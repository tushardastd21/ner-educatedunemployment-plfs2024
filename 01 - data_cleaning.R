# ==============================================================================
# 01 - Setting Up --------------------------------------------------------------
# ==============================================================================

# Libraries
library(tidyverse)
library(survey)
library(janitor)
library(readr)
library(ggplot2)
library(scales)
library(ggrepel)
library(patchwork)
library(readxl)

library(srvyr)
library(haven)
library(broom)
library(ggeffects)
library(gt)
library(brglm2)
library(VGAM)
library(nnet)

# NER state code
NER <- c("11", "12", "13", "14", "15", "16", "17", "18")

# NER state code mapped to state's name
ner_state_labels <- c(
  "11" = "Sikkim", "12" = "Arunachal Pradesh", "13" = "Nagaland",
  "14" = "Manipur", "15" = "Mizoram", "16" = "Tripura",
  "17" = "Meghalaya", "18" = "Assam"
)

# NER state code mapped to state's abbreviation
ner_state_abbr <- c(
  "11" = "SK", "12" = "AR", "13" = "NL",
  "14" = "MN", "15" = "MZ", "16" = "TR",
  "17" = "ML", "18" = "AS"
)


# ==============================================================================
# 02 - Load Files --------------------------------------------------------------
# ==============================================================================

plfs_p <- read_csv("01 - data/cperv1.csv") # person-level data
plfs_hh <- read_csv("01 - data/chhv1.csv") # household-level data
district_code <- read_excel("01 - data/district_code.xlsx") # district-code


# ==============================================================================
# 03 - Combining Household and Person-level data -------------------------------
# ==============================================================================

# Correction of variable name (to match with person level data)
plfs_hh <- plfs_hh %>%
  rename(
    Schedule = Schdule,
    State_UT_Code = State_Ut_Code,
    FOD_Sub_Region = Fod_Sub_Region
  )

join_keys <- c(
  "Quarter", "Visit", "Sector", "State_UT_Code", "District_Code",
  "NSS_Region", "Stratum", "Sub_Stratum", "Sub_Sample",
  "FOD_Sub_Region", "FSU", "Sample_Sg_Sb_No", 
  "Second_Stage_Stratum_No", "Sample_Household_Number", "Subsample_Multiplier"
)

plfs <- plfs_p %>%
  left_join(plfs_hh, by = join_keys)

# Integrity check of the combined file
stopifnot(nrow(plfs) == nrow(plfs_p))


# ==============================================================================
# 04 - Assigning weights and Selection ----------------------------------------
# ==============================================================================

## a. Assigning weights according to PLFS Manual ----
plfs <- plfs %>%
  mutate(
    final_weight = case_when(
      # CASE 1: When NSS = NSC (Only one sub-sample surveyed in the stratum)
      Ns_Count_Sector_Stratum_Substratum_Subsample == Ns_Count_Sector_Stratum_Substratum ~ 
        Subsample_Multiplier / (Contrib_Sample_Count * 100),
      
      # CASE 2: Standard case (Both Sub-samples 1 & 2 surveyed)
      TRUE ~ 
        Subsample_Multiplier / (Contrib_Sample_Count * 200)
    )
  )

## b. Selecting the variables for study ----
plfs_select <- plfs %>%
  select(!62:131)


# ==============================================================================
# 05 - Code to Name/Collapsing Data & Recoding ---------------------------------
# ==============================================================================

plfs_coded <- plfs_select %>% 
  mutate(
    # General education levels
    ge_value = as.numeric(General_Education_Level),
    gen_edu_category = case_when(
      ge_value == 1 ~ "Not Literate",
      ge_value %in% 2:5 ~ "Below Primary",
      ge_value %in% 6:7 ~ "Primary or Middle",
      ge_value == 8 ~ "Matriculation",
      ge_value %in% 10:11 ~ "HS or Diploma",
      ge_value == 12 ~ "UG",
      ge_value == 13 ~ "PG and above",
      TRUE ~ "Uncategorised"
    ),
    
    # Employment categories
    psc = as.numeric(Principal_Status_Code),
    emp_category = case_when(
      psc == 31 ~ "Formal Employment",
      psc %in% c(11, 12, 21) ~ "Self-Employed",
      psc %in% c(41, 51) ~ "Casual Wage Labour",
      psc == 81 ~ "Unemployed",
      psc %in% c(92, 93) ~ "Domestic Work",
      psc %in% c(94) ~ "Rent, Pension, Remittance",
      TRUE ~ "Outside Labour Force"
    )
  ) %>% 
  select(-ge_value, -psc) %>% 
  
  # Binary Category Indicators
  mutate(
    labor_force = ifelse(emp_category %in% c("Formal Employment", "Self-Employed", 
                                             "Casual Wage Labour", "Unemployed"), 1, 0),
    unemployed  = ifelse(emp_category == "Unemployed", 1, 0),
    employed    = ifelse(emp_category %in% c("Formal Employment", "Self-Employed", 
                                             "Casual Wage Labour"), 1, 0),
    fe          = ifelse(emp_category == "Formal Employment", 1, 0),
    ie          = ifelse(labor_force == 1 & fe == 0, 1, 0),
    noliteracy  = ifelse(gen_edu_category == "Not Literate", 1, 0),
    ug_and_above = ifelse(as.character(General_Education_Level) %in% c("12", "13"), 1, 0),
    pg_and_above = ifelse(as.character(General_Education_Level) == "13", 1, 0)
  ) %>% 
  
  # Demographic & Regional Recoding
  mutate(
    state_code_str = as.character(State_UT_Code),
    region         = ifelse(state_code_str %in% NER, "NER", "Rest of India"),
    ner_state_name = ner_state_labels[state_code_str],
    
    gender = case_when(
      as.character(Sex) %in% c("1", 1) ~ "Male",
      as.character(Sex) %in% c("2", 2) ~ "Female",
      TRUE ~ "Others"
    ),
    
    social_group = case_when(
      as.character(Social_Group) %in% c("1", 1) ~ "ST",
      as.character(Social_Group) %in% c("2", 2) ~ "SC",
      as.character(Social_Group) %in% c("3", 3) ~ "OBC",
      as.character(Social_Group) %in% c("9", 9) ~ "Others",
      TRUE ~ "Others"
    ),
    
    rural_urban = case_when(
      as.character(Sector) %in% c("1", 1) ~ "Rural",
      as.character(Sector) %in% c("2", 2) ~ "Urban"
    )
  ) %>% 
  
  # Ordering Education Factor
  mutate(
    gen_edu_category = factor(
      gen_edu_category, 
      levels = c("Not Literate", "Below Primary", "Primary or Middle", 
                 "Matriculation", "HS or Diploma", "UG", "PG and above", "Uncategorised"),
      ordered = TRUE
    )
  )

## Terrain Lookup Mapping ----
terrain_lookup <- tribble(
  ~state_code, ~dist_code, ~terrain,
  18, 15, "Hills", # Karbi Anglong (Assam)
  18, 16, "Hills", # Dima Hasao (Assam)
  18, 29, "Hills", # West Karbi Anglong (Assam)
  14, 04, "Plains", # Imphal West (Manipur)
  14, 05, "Plains", # Imphal East (Manipur)
  14, 06, "Plains", # Thoubal (Manipur)
  14, 07, "Plains"  # Bishnupur (Manipur)
)

# Clean district code metadata
district_code_clean <- district_code %>% 
  rename(
    state_code  = `State Code`,
    dist_code   = `DISTRICT CODE`,
    state_name  = `State Name`,
    dist_name   = `DISTRICT NAME`,
    dummy_hills = Hills
  ) %>% 
  mutate(
    state_code = as.numeric(state_code),
    dist_code  = as.numeric(dist_code)
  )

# Merge district metadata & terrain
plfs_coded <- plfs_coded %>%
  mutate(
    state_code = as.numeric(State_UT_Code),
    dist_code  = as.numeric(District_Code)
  ) %>% 
  left_join(district_code_clean, by = c("state_code", "dist_code")) %>%
  left_join(terrain_lookup, by = c("state_code", "dist_code")) %>%
  mutate(
    terrain = case_when(
      !is.na(terrain) ~ terrain,
      state_code %in% c(11, 12, 13, 15, 17) ~ "Hills", # SK, AR, NL, MZ, ML
      state_code == 16 ~ "Plains", # Tripura
      state_code == 18 ~ "Plains", # Default remaining Assam
      state_code == 14 ~ "Hills",  # Default remaining Manipur
      TRUE ~ "Other"
    )
  )


# ==============================================================================
# 07 - Survey Design & Domain Subsetting ---------------------------------------
# ==============================================================================

# 1. Handle single-PSU strata as certainty units (prevents Inf SEs)
options(survey.lonely.psu = "certainty")

# 2. Build Master Survey Design Object
plfs_clean <- plfs_coded %>%
  # Filter finite positive weights and valid keys
  filter(is.finite(final_weight), final_weight > 0, !is.na(FSU)) %>%
  
  # Construct unique Strata key
  mutate(
    strata_clean = paste(State_UT_Code, Sector, Stratum, Sub_Stratum, Quarter, sep = "_")
  ) %>%
  
  # Specify Survey Design
  as_survey_design(
    ids     = FSU,
    strata  = strata_clean,
    weights = final_weight,
    nest    = TRUE
  )

# 3. Domain Subset: Youth (15-29) Out of Education
plfs_youth <- plfs_clean %>%
  filter(
    Age >= 15 & Age <= 29,
    as.character(Current_Attendance_Status) %in% sprintf("%02d", c(1:5, 11:15))
  )


