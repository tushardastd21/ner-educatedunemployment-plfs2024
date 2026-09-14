# ==============================================================================
# 02 - Descriptive Statistics --------------------------------------------------
# ==============================================================================

# Directory safety: Ensure nested directory exists
out_dir <- "03 - output/01 - descriptive"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Helper function to write out paired outputs cleanly
  export_pair <- function(df_region, df_state, file_prefix) {
  write_csv(df_region, file.path(out_dir, paste0(file_prefix, "_NERvIndia.csv")))
  write_csv(df_state,  file.path(out_dir, paste0(file_prefix, "_NER_statewise.csv")))
}

# ------------------------------------------------------------------------------
# 1. Sample Distribution & Population Totals
# ------------------------------------------------------------------------------

# Level 1: Regional
t1_pop_region <- plfs_youth %>%
  group_by(region) %>%
  summarise(
    sample_size   = unweighted(n()),
    pop_total     = survey_total(vartype = "se"),
    pop_share_pct = survey_mean(vartype = "ci") * 100
  )

# Level 2: NER State-wise
t1_pop_state <- plfs_youth %>%
  filter(region == "NER") %>%
  group_by(ner_state_name) %>%
  summarise(
    sample_size = unweighted(n()),
    pop_total   = survey_total(vartype = "se")
  ) %>%
  arrange(desc(pop_total))

export_pair(t1_pop_region, t1_pop_state, "t1_population_baseline")


# ------------------------------------------------------------------------------
# 2. Demographic Profile: Gender & Education Proportions
# ------------------------------------------------------------------------------

# Level 1: Gender Distribution
t2_gender_region <- plfs_youth %>%
  group_by(region, gender) %>%
  summarise(
    sample_size = unweighted(n()),
    prop        = survey_mean(vartype = "se") * 100,
    total_pop   = survey_total(vartype = NULL),
    .groups     = "drop"
  )

t2_gender_state <- plfs_youth %>%
  filter(region == "NER") %>%
  group_by(ner_state_name, gender) %>%
  summarise(
    sample_size = unweighted(n()),
    prop        = survey_mean(vartype = "se") * 100,
    total_pop   = survey_total(vartype = NULL),
    .groups     = "drop"
  )

export_pair(t2_gender_region, t2_gender_state, "t2_gender_distribution")

# Level 2: Education Category Distribution
t3_edu_region <- plfs_youth %>%
  group_by(region, gen_edu_category) %>%
  summarise(
    sample_size = unweighted(n()),
    prop        = survey_mean(vartype = "se") * 100,
    total_pop   = survey_total(vartype = NULL),
    .groups     = "drop"
  )

t3_edu_state <- plfs_youth %>%
  filter(region == "NER") %>%
  group_by(ner_state_name, gen_edu_category) %>%
  summarise(
    sample_size = unweighted(n()),
    prop        = survey_mean(vartype = "se") * 100,
    total_pop   = survey_total(vartype = NULL),
    .groups     = "drop"
  )

export_pair(t3_edu_region, t3_edu_state, "t3_education_distribution")


# ------------------------------------------------------------------------------
# 3. Macro Labor Market Indicators (LFPR, WPR, UR)
# ------------------------------------------------------------------------------

# Calculated over full population
t4_labor_macro_region <- plfs_youth %>%
  group_by(region) %>%
  summarise(
    lfpr = survey_mean(labor_force, vartype = "se") * 100,
    wpr  = survey_mean(employed, vartype = "se") * 100
  )

# Unemployment Rate (UR calculated conditional on being in Labor Force)
t4_ur_region <- plfs_youth %>%
  filter(labor_force == 1) %>%
  group_by(region) %>%
  summarise(ur = survey_mean(unemployed, vartype = "se") * 100)

t4_labor_macro_region <- left_join(t4_labor_macro_region, t4_ur_region, by = "region")

# State-wise Macro Indicators
t4_labor_macro_state <- plfs_youth %>%
  filter(region == "NER") %>%
  group_by(ner_state_name) %>%
  summarise(
    lfpr = survey_mean(labor_force, vartype = "se") * 100,
    wpr  = survey_mean(employed, vartype = "se") * 100
  )

t4_ur_state <- plfs_youth %>%
  filter(region == "NER", labor_force == 1) %>%
  group_by(ner_state_name) %>%
  summarise(ur = survey_mean(unemployed, vartype = "se") * 100)

t4_labor_macro_state <- left_join(t4_labor_macro_state, t4_ur_state, by = "ner_state_name")

export_pair(t4_labor_macro_region, t4_labor_macro_state, "t4_macro_labor_indicators")


# ------------------------------------------------------------------------------
# 4. Employment Quality: Formal Employment & Unemployment by Disaggregations
# ------------------------------------------------------------------------------

# Labor Force Sub-sample for conditional rates
plfs_youth_lf <- plfs_youth %>% filter(labor_force == 1)

# A. Education x Labor Market Quality
t5_edu_outcomes_region <- plfs_youth_lf %>%
  group_by(region, gen_edu_category) %>%
  summarise(
    sample_size = unweighted(n()),
    fe_rate     = survey_mean(fe, vartype = "se") * 100,
    ur          = survey_mean(unemployed, vartype = "se") * 100,
    .groups     = "drop"
  )

t5_edu_outcomes_state <- plfs_youth_lf %>%
  filter(region == "NER") %>%
  group_by(ner_state_name, gen_edu_category) %>%
  summarise(
    sample_size = unweighted(n()),
    fe_rate     = survey_mean(fe, vartype = "se") * 100,
    ur          = survey_mean(unemployed, vartype = "se") * 100,
    .groups     = "drop"
  )

export_pair(t5_edu_outcomes_region, t5_edu_outcomes_state, "t5_education_x_employment_quality")


# B. Education x Gender Intersectionality
t6_edu_gender_region <- plfs_youth_lf %>%
  group_by(region, gen_edu_category, gender) %>%
  summarise(
    sample_size = unweighted(n()),
    fe_rate     = survey_mean(fe, vartype = "se") * 100,
    ur          = survey_mean(unemployed, vartype = "se") * 100,
    .groups     = "drop"
  )

t6_edu_gender_state <- plfs_youth_lf %>%
  filter(region == "NER") %>%
  group_by(ner_state_name, gen_edu_category, gender) %>%
  summarise(
    sample_size = unweighted(n()),
    fe_rate     = survey_mean(fe, vartype = "se") * 100,
    ur          = survey_mean(unemployed, vartype = "se") * 100,
    .groups     = "drop"
  )

export_pair(t6_edu_gender_region, t6_edu_gender_state, "t6_intersection_edu_gender")


# C. Education x Social Group Intersectionality
t7_edu_social_region <- plfs_youth_lf %>%
  group_by(region, gen_edu_category, social_group) %>%
  summarise(
    sample_size = unweighted(n()),
    fe_rate     = survey_mean(fe, vartype = "se") * 100,
    ur          = survey_mean(unemployed, vartype = "se") * 100,
    .groups     = "drop"
  )

t7_edu_social_state <- plfs_youth_lf %>%
  filter(region == "NER") %>%
  group_by(ner_state_name, gen_edu_category, social_group) %>%
  summarise(
    sample_size = unweighted(n()),
    fe_rate     = survey_mean(fe, vartype = "se") * 100,
    ur          = survey_mean(unemployed, vartype = "se") * 100,
    .groups     = "drop"
  )

export_pair(t7_edu_social_region, t7_edu_social_state, "t7_intersection_edu_socialgroup")


# ------------------------------------------------------------------------------
# 5. Intra-NER Geographical & Terrain Dynamics (Hills vs. Plains)
# ------------------------------------------------------------------------------

# Filtered strictly for region == "NER" to isolate hill-plains dynamics
plfs_ner_youth    <- plfs_youth %>% filter(region == "NER")
plfs_ner_youth_lf <- plfs_youth_lf %>% filter(region == "NER")

# A. Macro Population & Sample Distribution across Terrains (NER Only)
t8_ner_terrain_pop <- plfs_ner_youth %>%
  group_by(terrain) %>%
  summarise(
    sample_size   = unweighted(n()),
    pop_total     = survey_total(vartype = "se"),
    pop_share_pct = survey_mean(vartype = "ci") * 100
  )

write_csv(t8_ner_terrain_pop, file.path(out_dir, "t8_ner_terrain_population_baseline.csv"))

# B. Macro Labor Market Indicators by Terrain (LFPR, WPR, UR — NER Only)
t9_ner_terrain_labor_macro <- plfs_ner_youth %>%
  group_by(terrain) %>%
  summarise(
    lfpr = survey_mean(labor_force, vartype = "se") * 100,
    wpr  = survey_mean(employed, vartype = "se") * 100
  )

t9_ner_terrain_ur <- plfs_ner_youth_lf %>%
  group_by(terrain) %>%
  summarise(ur = survey_mean(unemployed, vartype = "se") * 100)

t9_ner_terrain_macro <- left_join(t9_ner_terrain_labor_macro, t9_ner_terrain_ur, by = "terrain")

write_csv(t9_ner_terrain_macro, file.path(out_dir, "t9_ner_terrain_macro_labor_indicators.csv"))

# C. Employment Quality by Terrain & Education (NER Only)
t10_ner_terrain_edu_outcomes <- plfs_ner_youth_lf %>%
  group_by(terrain, gen_edu_category) %>%
  summarise(
    sample_size = unweighted(n()),
    fe_rate     = survey_mean(fe, vartype = "se") * 100,
    ur          = survey_mean(unemployed, vartype = "se") * 100,
    .groups     = "drop"
  )

write_csv(t10_ner_terrain_edu_outcomes, file.path(out_dir, "t10_ner_terrain_education_x_employment_quality.csv"))

# D. Gendered Labor Outcomes across Terrains (NER Only)
t11_ner_terrain_gender_outcomes <- plfs_ner_youth_lf %>%
  group_by(terrain, gender) %>%
  summarise(
    sample_size = unweighted(n()),
    fe_rate     = survey_mean(fe, vartype = "se") * 100,
    ur          = survey_mean(unemployed, vartype = "se") * 100,
    .groups     = "drop"
  )

write_csv(t11_ner_terrain_gender_outcomes, file.path(out_dir, "t11_ner_terrain_gender_outcomes.csv"))