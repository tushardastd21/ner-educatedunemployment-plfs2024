# ==============================================================================
# 04 - NER District Structural Dichotomy & Sikkim Case Study
# ==============================================================================

library(dplyr)
library(srvyr)
library(survey)
library(readr)

# Directory Setup
out_csv_dir <- "03 - output/03 - sikkim"
if (!dir.exists(out_csv_dir)) dir.create(out_csv_dir, recursive = TRUE)

# ------------------------------------------------------------------------------
# 1. Base Preprocessing & Classification
# ------------------------------------------------------------------------------

plfs_prep <- plfs_youth %>%
  mutate(
    terrain = ifelse(is.na(terrain), "Uncoded", as.character(terrain)),
    pic = as.numeric(substr(as.character(Principal_Industry_Code), 1, 2)),
    
    broad_sector = case_when(
      pic %in% 1:3   ~ "Agriculture",
      pic == 4       ~ "Utilities (Elec/Gas/Water)",  # was falling into
      # "Uncategorised" silently
      pic %in% 5:43  ~ "Industry",
      pic %in% 45:99 ~ "Services",
      TRUE           ~ "Uncategorised"
    ),
    
    service_sector = case_when(
      pic %in% 45:47 ~ "Trade & Retail",
      pic %in% 49:53 ~ "Transport & Storage",
      pic %in% 55:56 ~ "Tourism (Hotel/Rest.)",
      pic %in% 84    ~ "Public Admin & Defence",
      pic %in% 85:88 ~ "Education & Health",
      pic %in% 45:99 ~ "Other Services",
      TRUE           ~ NA_character_
    ),
    
    mfg_sector = case_when(
      pic %in% 10:12 ~ "Consumables (Food/Bev)",
      pic %in% 13:18 ~ "Light Industry (Textiles/Wood)",
      pic %in% 21    ~ "Pharmaceuticals",
      pic %in% c(19:20, 22:33) ~ "Heavy Industry (Metals/Chem)",
      pic %in% 5:43  ~ "Other Industry",
      TRUE           ~ NA_character_
    )
  )

# --- Diagnostic: check how many NER youth fall in the pic==4 utilities gap ---
n_pic4 <- plfs_prep %>%
  filter(region == "NER") %>%
  as_tibble() %>%
  filter(pic == 4) %>%
  nrow()

message("NER rows with pic == 4 (utilities, previously mis-bucketed): ", n_pic4)
if (n_pic4 == 0) {
  message("-> No cases affected. Fix is precautionary only, safe to keep either way.")
} else {
  message("-> These rows were silently 'Uncategorised' in the original script. Re-check any totals that used broad_sector before this fix.")
}

# ------------------------------------------------------------------------------
# 2. Part A: NER District-Level Structural Analysis
# ------------------------------------------------------------------------------

# Table 1: District-Level Metrics
ner_dist_summary <- plfs_prep %>%
  filter(region == "NER") %>%
  mutate(
    state_short = ner_state_abbr[as.character(state_code)],
    full_label = paste0(dist_name, " (", state_short, ")")
  ) %>%
  group_by(
    State = state_short,
    `District Name` = dist_name,
    `Terrain` = terrain
  ) %>%
  summarise(
    `Unweighted n`        = unweighted(n()),
    `UG and Above (%)`    = round(survey_mean(ug_and_above, na.rm = TRUE, vartype = "se")[["coef"]] * 100, 2),
    `UG and Above SE`     = round(survey_mean(ug_and_above, na.rm = TRUE, vartype = "se")[["_se"]] * 100, 2),
    `FE Share (%)`        = round(survey_mean(fe, na.rm = TRUE, vartype = "se")[["coef"]] * 100, 2),
    `FE Share SE`         = round(survey_mean(fe, na.rm = TRUE, vartype = "se")[["_se"]] * 100, 2),
    `Unemployment (%)`    = round(survey_mean(unemployed, na.rm = TRUE, vartype = "se")[["coef"]] * 100, 2),
    `Unemployment SE`     = round(survey_mean(unemployed, na.rm = TRUE, vartype = "se")[["_se"]] * 100, 2),
    .groups = "drop"
  ) %>%
  mutate(
    `Small Cell Flag` = ifelse(`Unweighted n` < 30, "CAUTION: n<30", "")
  ) %>%
  arrange(State, `District Name`)

write_csv(ner_dist_summary, file.path(out_csv_dir, "table1_ner_district_employment_terrain.csv"))

# Table 2: Structural Variance Summary by Terrain (unweighted spread across
# districts — this describes dispersion of district-level rates, NOT a
# population-weighted regional average)
ner_terrain_variance <- ner_dist_summary %>%
  group_by(Terrain) %>%
  summarise(
    `District Count` = n(),
    `Districts Flagged Small Cell` = sum(`Small Cell Flag` != ""),
    `Mean FE (%)`    = round(mean(`FE Share (%)`, na.rm = TRUE), 2),
    `Median FE (%)`  = round(median(`FE Share (%)`, na.rm = TRUE), 2),
    `Min FE (%)`     = round(min(`FE Share (%)`, na.rm = TRUE), 2),
    `Max FE (%)`     = round(max(`FE Share (%)`, na.rm = TRUE), 2),
    `Std Dev`        = round(sd(`FE Share (%)`, na.rm = TRUE), 2),
    .groups = "drop"
  )

write_csv(ner_terrain_variance, file.path(out_csv_dir, "table2_ner_terrain_fe_summary.csv"))

# Table 3: State x Terrain FE Summary (nested boxplot metrics)
ner_state_terrain_summary <- ner_dist_summary %>%
  group_by(State, Terrain) %>%
  summarise(
    `District Count` = n(),
    `Mean FE (%)`    = round(mean(`FE Share (%)`, na.rm = TRUE), 2),
    `Min FE (%)`     = round(min(`FE Share (%)`, na.rm = TRUE), 2),
    `Max FE (%)`     = round(max(`FE Share (%)`, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(State, Terrain)

write_csv(ner_state_terrain_summary, file.path(out_csv_dir, "table3_ner_state_terrain_fe.csv"))

# ------------------------------------------------------------------------------
# 2b. Formal hill-vs-plains test statistic
# ------------------------------------------------------------------------------
# Two versions, since district count is small (non-normal-friendly caution):
#   (a) svyttest at the INDIVIDUAL level, properly design-weighted — this is
#       the statistically correct test given your survey design, and is what
#       should be reported/cited in the paper.
#   (b) A district-level Wilcoxon rank-sum on the unweighted district FE
#       rates, as a robustness / sensitivity check only — this ignores the
#       survey design and treats each district as one observation, so do NOT
#       report this as your primary test, but it's a useful sanity check.

ner_design_terrain <- plfs_prep %>%
  filter(region == "NER", terrain %in% c("Hills", "Plains"))

# (a) Primary test: design-weighted t-test comparing individual-level FE
# status between hill and plains districts
hill_plains_svyttest <- svyttest(fe ~ factor(terrain), design = ner_design_terrain)
print(hill_plains_svyttest)

# (b) Robustness check only: unweighted district-level Wilcoxon
hill_plains_wilcox <- wilcox.test(
  `FE Share (%)` ~ Terrain,
  data = ner_dist_summary %>% filter(Terrain %in% c("Hills", "Plains"))
)
print(hill_plains_wilcox)

# Save both results to a text file for the methods/appendix writeup
sink(file.path(out_csv_dir, "hill_plains_test_results.txt"))
cat("=== PRIMARY TEST: Design-weighted svyttest, individual-level FE status ===\n")
print(hill_plains_svyttest)
cat("\n\n=== ROBUSTNESS CHECK ONLY: Unweighted district-level Wilcoxon rank-sum ===\n")
cat("(Treats each district as one observation; ignores survey design.\n")
cat(" Do not cite this as the primary test -- for sanity-check purposes only.)\n\n")
print(hill_plains_wilcox)
sink()

# ------------------------------------------------------------------------------
# 3. Part B: Sikkim Case Study Deep-Dive
# ------------------------------------------------------------------------------

sikkim_fe <- plfs_prep %>%
  filter(ner_state_name == "Sikkim", fe == 1)

sikkim_fe_unweighted_n <- sikkim_fe %>% as_tibble() %>% nrow()
message("Sikkim formally-employed unweighted n = ", sikkim_fe_unweighted_n,
        " -- interpret sub-sector splits below with this in mind; ",
        "any sub-sector cell is necessarily smaller still.")

# Table 4: Sikkim Broad Sector Breakdown, NOW WITH SE and unweighted n
sikkim_broad_tbl <- sikkim_fe %>%
  group_by(`Broad Sector` = broad_sector) %>%
  summarise(
    `Unweighted n`        = unweighted(n()),
    `Estimated FE Count`  = round(survey_total(na.rm = TRUE, vartype = "se")[["coef"]]),
    `FE Count SE`         = round(survey_total(na.rm = TRUE, vartype = "se")[["_se"]]),
    .groups = "drop"
  ) %>%
  mutate(
    `Share of Total FE (%)` = round((`Estimated FE Count` / sum(`Estimated FE Count`)) * 100, 2),
    `Small Cell Flag` = ifelse(`Unweighted n` < 30, "CAUTION: n<30", "")
  ) %>%
  arrange(desc(`Estimated FE Count`))

write_csv(sikkim_broad_tbl, file.path(out_csv_dir, "table4_sikkim_broad_sector_fe.csv"))

# --- Sum check: does this total match the paper's implied Sikkim FE total (~50.1k)? ---
sikkim_table4_total <- sum(sikkim_broad_tbl$`Estimated FE Count`)
message("Table 4 total Sikkim FE (all sectors): ", sikkim_table4_total,
        " -- compare against paper's Table 2 Sikkim FE Count column sum (~50,116).")

# Table 5: Sikkim Services Sub-sector Breakdown, NOW WITH SE and unweighted n
sikkim_services_tbl <- sikkim_fe %>%
  filter(!is.na(service_sector)) %>%
  group_by(`Service Sub-Sector` = service_sector) %>%
  summarise(
    `Unweighted n`        = unweighted(n()),
    `Estimated FE Count`  = round(survey_total(na.rm = TRUE, vartype = "se")[["coef"]]),
    `FE Count SE`         = round(survey_total(na.rm = TRUE, vartype = "se")[["_se"]]),
    .groups = "drop"
  ) %>%
  mutate(
    `Share of Service FE (%)` = round((`Estimated FE Count` / sum(`Estimated FE Count`)) * 100, 2),
    `Small Cell Flag` = ifelse(`Unweighted n` < 30, "CAUTION: n<30", "")
  ) %>%
  arrange(desc(`Estimated FE Count`))

write_csv(sikkim_services_tbl, file.path(out_csv_dir, "table5_sikkim_service_sector_fe.csv"))

# Table 6: Sikkim Industrial Sub-sector Breakdown (Pharma Hub), NOW WITH SE
sikkim_mfg_tbl <- sikkim_fe %>%
  filter(!is.na(mfg_sector)) %>%
  group_by(`Industrial Sub-Sector` = mfg_sector) %>%
  summarise(
    `Unweighted n`        = unweighted(n()),
    `Estimated FE Count`  = round(survey_total(na.rm = TRUE, vartype = "se")[["coef"]]),
    `FE Count SE`         = round(survey_total(na.rm = TRUE, vartype = "se")[["_se"]]),
    .groups = "drop"
  ) %>%
  mutate(
    `Share of Industrial FE (%)` = round((`Estimated FE Count` / sum(`Estimated FE Count`)) * 100, 2),
    `Small Cell Flag` = ifelse(`Unweighted n` < 30, "CAUTION: n<30", "")
  ) %>%
  arrange(desc(`Estimated FE Count`))

write_csv(sikkim_mfg_tbl, file.path(out_csv_dir, "table6_sikkim_industrial_fe.csv"))


# Graph of Hills vs Plains (State-wise in NER) ----------------------------


# ==============================================================================
# Visualization: District FE Share (%) Boxplot
# ==============================================================================

library(ggplot2)
library(dplyr)
library(readr)
library(forcats)

# 1. Load Data
out_csv_dir <- "03 - output/03 - sikkim"
ner_dist_summary <- read_csv(file.path(out_csv_dir, "table1_ner_district_employment_terrain.csv"))

# 2. Data Preparation
plot_data <- ner_dist_summary %>%
  filter(!is.na(`District Name`)) %>%
  mutate(
    state_terrain = paste0(State, " (", Terrain, ")"),
    # Reorder state_terrain by median FE Share (%) in descending order
    state_terrain = fct_reorder(state_terrain, `FE Share (%)`, .fun = median, .desc = TRUE)
  )

# 2. Generate Plot
p_fe_terrain <- ggplot(plot_data, aes(x = state_terrain, y = `FE Share (%)`, fill = Terrain)) +
  geom_boxplot(
    color = "#000000",       # Black outline
    fatten = 1.2,
    outlier.color = "#000000",
    outlier.shape = 22,
    outlier.size = 1.5,
    width = 0.55
  ) +
  # Custom scale mapping: Dark Charcoal for Hills, Light Grey for Plains
  scale_fill_manual(values = c(
    "Hills"  = "grey", 
    "Plains" = "white"
  )) +
  labs(
    y = "FE Share (%)",
    x = "State"
    fill = "Terrain"
  ) + 
  theme_classic(base_family = "Times New Roman", size = 12)

print(p_fe_terrain)

