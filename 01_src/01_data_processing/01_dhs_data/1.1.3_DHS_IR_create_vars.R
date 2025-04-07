# load libraries ----
rm(list = ls())
pacman::p_load(dplyr, janitor, data.table, fst, openxlsx, here)

# set paths ----
source(here("paths.R"))

# load data ----
## full data with missing caste values
df_IR_missing <- read_fst(here(path_processed,
  "1.1.1_DHS_IR_filtered_6mo.fst")) |>
  select(-hh_caste)
## imputed caste data
df_IR_impute <- readRDS(here(path_processed, 
  "1.1.2_DHS_IR_imputed_6mo.rds")) |>
  select(caseid, hh_caste)

# merge the datasets ----
df_IR_long_merged <- df_IR_missing |>
  left_join(df_IR_impute, by = "caseid")

# Create variables for full dataset ----
## Outcome variables ----
### Any contact with health system in last 3 months
df_IR_long_vars <- df_IR_long_merged |> 
  dplyr::mutate(dv_no_contact_3mo = ifelse(s359 == "yes" | s361 == "yes" | s368 == "yes", 0, 1))

tabyl(df_IR_long_vars, dv_no_contact_3mo) |> janitor::adorn_totals("row")

## SES Variables ----
### Wealth - Binary
df_IR_long_vars <- df_IR_long_vars |>
  # Two-level wealth variable cut at the median
  dplyr::mutate(ses_wealth_bi = ifelse(hh_wealth_score_ru_og > quantile(hh_wealth_score_ru_og, 0.5), "rich", "poor")) |> 
  # relevel the factor
  dplyr::mutate(ses_wealth_bi = forcats::fct_relevel(ses_wealth_bi, "rich", "poor")) |>
  # Richer-2 vs poorer-3
  dplyr::mutate(ses_wealth_bi_richer2 = ifelse(
    hh_wealth_quintile_ru_og == "richest" | hh_wealth_quintile_ru_og == "richer", "richer2", "poorer")) |> 
  # relevel the factor
  dplyr::mutate(ses_wealth_bi_richer2 = forcats::fct_relevel(ses_wealth_bi_richer2, "richer2", "poorer")) |>
  # Richer-3 vs poorer-2
  dplyr::mutate(ses_wealth_bi_richer3 = ifelse(
    hh_wealth_quintile_ru_og == "richest" | hh_wealth_quintile_ru_og == "richer" | 
    hh_wealth_quintile_ru_og == "middle", "richer3", "poorer")) |>
  # relevel the factor
  dplyr::mutate(ses_wealth_bi_richer3 = forcats::fct_relevel(ses_wealth_bi_richer3, "richer3", "poorer")) 

### Religion
df_IR_long_vars <- df_IR_long_vars |>
  # Two-level variables for religion: Hindu and other
  dplyr::mutate(ses_religion_2_hindu = if_else(hh_religion == "hindu", "Hindu", "Not-Hindu")) |>
  # Two-level variables for religion: Muslim and other
  dplyr::mutate(ses_religion_2_muslim = if_else(hh_religion == "muslim", "Muslim", "Other")) |> 
  # Three level variable for religion
  dplyr::mutate(ses_religion_3 = case_when(
      hh_religion == "hindu" ~ "Hindu",
      hh_religion == "muslim" ~ "Muslim",
      TRUE ~ "Other")) |> 
  # Four-level variable for religion
  dplyr::mutate(ses_religion_4 = case_when(
    hh_religion == "hindu" ~ "Hindu",
    hh_religion == "muslim" ~ "Muslim",
    hh_religion == "christian" ~ "Christian",
    TRUE ~ "Other")) |>
  ## Relevel Religion levels
  dplyr::mutate(ses_religion_2_hindu = forcats::fct_relevel(ses_religion_2_hindu, "Hindu", "Not-Hindu")) |> 
  dplyr::mutate(ses_religion_2_muslim = forcats::fct_relevel(ses_religion_2_muslim, "Muslim", "Other")) |> 
  dplyr::mutate(ses_religion_3 = forcats::fct_relevel(ses_religion_3, "Hindu", "Muslim", "Other")) |> 
  dplyr::mutate(ses_religion_4 = forcats::fct_relevel(ses_religion_4, "Hindu", "Muslim", "Christian", "Other")) 

### Caste
df_IR_long_vars <- df_IR_long_vars |>
  # Two-level variables for caste
  dplyr::mutate(ses_caste_2 = if_else(hh_caste == "none of them", "General", "SC/ST/OBC")) |> 
  # Three level variable for caste
  dplyr::mutate(ses_caste_3 = case_when(
    hh_caste == "obc" ~ "OBC",
    hh_caste == "none of them" ~ "General",
    TRUE ~ "SC/ST")) |> 
  # Four-level variable for caste
  dplyr::mutate(ses_caste_4 = case_when(
    hh_caste == "schedule caste" ~ "SC",
    hh_caste == "schedule tribe" ~ "ST",
    hh_caste == "obc" ~ "OBC",
    TRUE ~ "General")) |>
  ## Relevel Caste levels
  dplyr::mutate(ses_caste_2 = forcats::fct_relevel(ses_caste_2, "SC/ST/OBC", "General")) |> 
  dplyr::mutate(ses_caste_3 = forcats::fct_relevel(ses_caste_3, "SC/ST", "OBC", "General")) |>
  dplyr::mutate(ses_caste_4 = forcats::fct_relevel(ses_caste_4, "SC", "ST", "OBC", "General")) 

### Distance is a problem to access healthcare ----
df_IR_long_vars <- df_IR_long_vars |> 
  dplyr::mutate(ses_access_issue_distance = 
    ifelse(v467d == "big problem", "big-problem", "not-a-big-prob")) 

### Vehicle ownership
df_IR_long_vars <- df_IR_long_vars |> 
  dplyr::mutate(ses_vehicle_ownership_any = case_when(
    v123 == "yes" ~ "yes",
    v124 == "yes" ~ "yes",
    v125 == "yes" ~ "yes",
    TRUE ~ "no")) |>
  dplyr::mutate(ses_vehicle_ownership_motor = case_when(
    v124 == "yes" ~ "yes",
    v125 == "yes" ~ "yes",
    TRUE ~ "no"))

## Maternal characteristics ----
### Currently pregnant
df_IR_long_vars <- df_IR_long_vars |> 
  dplyr::mutate(mat_cur_preg = ifelse(v213 == "yes", 1, 0))

### Parity
df_IR_long_vars <- df_IR_long_vars |> 
  dplyr::mutate(mat_parity_bi = ifelse(mat_parity == 1, "Primiparous", "Multiparous")) |> 
  dplyr::mutate(mat_parity_bi = forcats::fct_relevel(mat_parity_bi, "Primiparous", "Multiparous"))

### Maternal age at interview
df_IR_long_vars <- df_IR_long_vars |> 
  # scale the continuous age variable
  dplyr::mutate(mat_age_at_int_scaled = scale(mat_age))  |> 
  # Create age groups
  dplyr::mutate(mat_age_at_int_cat = case_when(
    mat_age < 25  ~ "15-24",
    mat_age >= 25 & mat_age < 35  ~ "25-34",
    mat_age >= 35 & mat_age < 50  ~ "35-49")) |> 
  # Create binary age groups
  dplyr::mutate(mat_age_at_int_bi = ifelse(mat_age < 35, "15-34", "35-49")) |> 
  ## Relevel the factor
  dplyr::mutate(mat_age_at_int_cat = forcats::fct_relevel(mat_age_at_int_cat, "15-24", "25-34", "35-49")) |>
  dplyr::mutate(mat_age_at_int_bi = forcats::fct_relevel(mat_age_at_int_bi, "15-34", "35-49"))


tabyl(df_IR_long_vars, m14_1) |> janitor::adorn_totals("row")

### Exposure to mass media ----
df_IR_long_vars <- df_IR_long_vars |>
  mutate(
    media_newspaper = if_else(v157 == "not at all", 0, 1),
    media_radio = if_else(v158 == "not at all", 0, 1),
    media_tv = if_else(v159 == "not at all", 0, 1),
    media_internet = if_else(v171a == "never", 0, 1)
  )

#### create any exposure to mass media
df_IR_long_vars <- df_IR_long_vars |>
  mutate(mat_media_exp_any = case_when(
    media_newspaper == 1 | media_radio == 1 | media_tv == 1 | media_internet == 1 ~ "yes", 
    is.na(media_internet) ~ "no",
    TRUE ~ "no"
    ))

# Step-5: Select variables ----------------------------------------------------
df_selected <- df_IR_long_vars |>
  dplyr::select(caseid, starts_with("meta"), starts_with("dv"), 
    doi, v135, v213, v214, covid, m14_2,
    starts_with("ses"), starts_with("mat"),
    contains("zone"), contains("int"), contains("birth"))

# save the dataset
df_selected |> saveRDS(here(path_processed, "1.1.3_DHS_IR_vars_created.rds"))