pacman::p_load(dplyr, openxlsx, here, lme4, lmtest, msm)

# Set paths ----
source(here("paths.R"))

# Load weighted datasets ----
all_weighted_data <- readRDS(here(path_outputs, "models", "full_models", "iptw_weights", 
  "two_stage_gaussian", "2.1.1_weighted_datasets_all_datasets_5mo.rds"))

# Data processing ----
## Create JSY variable for all datasets 
all_weighted_data <- lapply(all_weighted_data, function(dataset) {
  lapply(dataset, function(df) {
    df <- df |> 
      dplyr::mutate(meta_jsy_bi = case_when(
        meta_state_name %in% c(
      "assam", "bihar", "chhattisgarh", "jharkhand", "madhya pradesh", 
      "odisha", "rajasthan", "uttar pradesh", "uttarakhand",
      "jammu & kashmir") ~ "JSY",
    TRUE ~ "Non-JSY"
  ))
  return(df)
  })
})

## convert all modifier variables to factors
all_weighted_data <- lapply(all_weighted_data, function(dataset) {
  lapply(dataset, function(df) {
    df <- df |>
      mutate(across(all_of(modifier_vars), as.factor))
    return(df)
  })
})

## Relevel modifier variables
all_weighted_data <- lapply(all_weighted_data, function(dataset) {
  lapply(dataset, function(df) {
    df <- df |>
      mutate(hh_wealth_bi_richest3 = relevel(ses_wealth_bi_richer3, ref = "richer3")) |>
      mutate(ses_access_issue_distance = relevel(ses_access_issue_distance, ref = "not-a-big-prob")) |>
      mutate(ses_caste_2 = relevel(ses_caste_2, ref = "General")) |>
      mutate(ses_religion_2_hindu = relevel(ses_religion_2_hindu, ref = "Hindu")) |>
      mutate(mat_edu_level_bi = relevel(mat_edu_level_bi, ref = "primary or higher")) |>
      mutate(meta_jsy_bi = relevel(meta_jsy_bi, ref = "JSY"))
    return(df)
  })
})

# set constants ----

## Create output directories
dir.create(here(path_outputs, "models", "effect_modification", "main_models"), recursive = TRUE, showWarnings = FALSE)
path_outcome_models <- here(path_outputs, "models", "effect_modification", "main_models")

## Define seasonal parameters
seasonal_params <- c("month_sin1", "month_sin2", "month_cos1", "month_cos2")

## Define modifier variables
modifier_vars <- c("meta_rural", "ses_access_issue_distance", 
    "ses_wealth_bi_richer3", "mat_edu_level_bi", "ses_caste_2", 
    "ses_religion_2_hindu", "meta_jsy_bi")

# Source utility functions ----
source(here("01_src", "02_models", "utils", "func_effect_modification_models.R"))
source(here("01_src", "02_models", "utils", "func_test_heterogeneity.R"))

# Run effect modification models ----
effect_mod_models <- run_effect_modification_models(
  weighted_data = all_weighted_data,
  modifier_vars = modifier_vars,
  seasonal_params = seasonal_params,
  path_output = path_outcome_models
)

# Test effect modification and get results
effect_mod_results <- test_effect_modification(
  model_list = effect_mod_models,
  path_output = path_outcome_models
)

# Print summary of results
cat("\nEffect Modification Analysis Complete\n")
cat("Results saved in:", path_outcome_models, "\n")
cat("Number of models analyzed:", length(effect_mod_models), "\n")
cat("Number of significant interactions (p < 0.05):", 
    sum(effect_mod_results$interaction_p_values$Interaction_P < 0.05), "\n")
