# ==============================================================================
# 03 - Main Multinomial Model (Firth Penalized RRR) -----------------------------
# ==============================================================================

library(brglm2)
library(nnet)
library(broom)
library(dplyr)
library(gt)
library(readr)

# Directory safety: Ensure nested model directory exists
out_dir_models <- "03 - output/02 - models"
if (!dir.exists(out_dir_models)) dir.create(out_dir_models, recursive = TRUE)

# ------------------------------------------------------------------------------
# 1. Sample Preparation
# ------------------------------------------------------------------------------

plfs_eda_mnr <- plfs_youth %>% 
  filter(labor_force == 1) %>% 
  mutate(gen_edu_category = factor(gen_edu_category, ordered = FALSE)) %>%
  filter(region == "NER", gender != "Others") %>%
  mutate(labor_status = case_when(
    fe == 1 ~ "Formal",
    unemployed == 1 ~ "Unemployed",
    TRUE ~ "Informal"
  )) %>%
  mutate(labor_status = factor(labor_status, levels = c("Informal", "Formal", "Unemployed")))

# ------------------------------------------------------------------------------
# 2. Firth Penalized Multinomial Regression Model
# ------------------------------------------------------------------------------

# Uses brglm2 engine to resolve quasi-complete separation issues in state dummies
model_firth <- multinom(
  labor_status ~ gen_edu_category + gender + social_group + ner_state_name, 
  data = plfs_eda_mnr,
  method = "brglmFit",       # Applies the Firth penalty
  type = "AS_mean",          # Adjusted Score (Firth penalization)
  model = TRUE
)

# ------------------------------------------------------------------------------
# 3. Tidy Extraction & CSV Export
# ------------------------------------------------------------------------------

tidy_firth_precise <- tidy(model_firth, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)")

# Export raw estimates CSV to 03 - output/02 - models/
write_csv(tidy_firth_precise, file.path(out_dir_models, "m1_main_firth_multinomial_rrr.csv"))

# ------------------------------------------------------------------------------
# 4. Formatted GT Table Export (HTML)
# ------------------------------------------------------------------------------

gt_table_main <- tidy_firth_precise %>%
  gt(groupname_col = "y.level") %>%
  tab_header(
    title = "Firth Penalized Multinomial Regression Results (Main Model)",
    subtitle = "Relative Risk Ratios (RRR) relative to Informal Employment Baseline"
  ) %>%
  fmt_number(
    columns = c(estimate, std.error, statistic, conf.low, conf.high),
    decimals = 2
  ) %>%
  text_transform(
    locations = cells_body(columns = p.value),
    fn = function(x) {
      p <- as.numeric(x)
      ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p))
    }
  ) %>%
  cols_label(
    term = "Variable",
    estimate = "RRR",
    std.error = "S.E.",
    statistic = "z-stat",
    p.value = "p-value",
    conf.low = "95% CI (Low)",
    conf.high = "95% CI (High)"
  )

# Export rendered table to 03 - output/02 - models/
gtsave(gt_table_main, filename = file.path(out_dir_models, "m1_main_firth_multinomial_table.html"))


# 05 - Tests for Firth NER --------------------------------------------------------------

library(DescTools)
library(car)
library(survey)

# 1. McFadden's Pseudo R-Squared
# A value between 0.2 and 0.4 indicates an excellent fit.
mcfadden_r2 <- PseudoR2(model_firth, which = "McFadden")
print(paste("McFadden R2:", round(mcfadden_r2, 4)))

# 2. Likelihood Ratio Test
# Comparing your model to a null (intercept-only) model
model_null <- multinom(labor_status ~ 1, data = plfs_eda_mnr, weights = final_weight)
lr_test <- anova(model_null, model_firth)
print(lr_test)

# 3. Multicollinearity check (VIF)
# Since multinom doesn't always play nice with vif(), 
# we check a standard logit for the same predictors.
# If GVIF < 5, you are in the clear.
vif_check <- glm(fe ~ gen_edu_category + gender + social_group + ner_state_name, 
                 data = plfs_eda_mnr, family = binomial)
vif(vif_check)
