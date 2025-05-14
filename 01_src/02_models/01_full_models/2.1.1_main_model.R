# Load packages ----
pacman::p_load(dplyr, openxlsx, here, ggplot2, geepack, CBPS, splines, gridExtra, cobalt, lme4, lmtest)

# Set paths ----
## Project paths
source(here("paths.R"))

## create and set output paths
dir.create(here(path_outputs, "models", "full_models", "main_models"), recursive = TRUE, showWarnings = FALSE)
path_outputs_models_root <- here(path_outputs, "models", "full_models", "main_models")
dir.create(here(path_outputs_models_root, "love_plots"), recursive = TRUE, showWarnings = FALSE)
path_outputs_love_plots <- here(path_outputs_models_root, "love_plots")
dir.create(here(path_outputs_models_root, "models"), recursive = TRUE, showWarnings = FALSE)
path_outputs_models <- here(path_outputs_models_root, "models")

# Source utility functions ----
source(here("01_src", "02_models", "utils", "func_iptw_weights.R"))
source(here("01_src", "02_models", "utils", "func_love_plots.R"))
source(here("01_src", "02_models", "utils", "variable_labels_for_models.R"))
source(here("01_src", "02_models", "utils", "func_full_model.R"))

# Load prepared data ----
all_data <- readRDS(here(path_processed, "1.3.3_cumulative_bins_all_datasets_5mo.rds"))

# Set constants ----
## Define confounders
confounders <- c("mat_edu_level", "mat_age_at_int_cat", "mat_parity_bi",
                 "mat_media_exp_any", "ses_caste_2", "ses_religion_2_hindu",
                 "ses_access_issue_distance", "hh_wealth_quintile_ru_og", 
                 "meta_rural")

seasonal_params <- c("month_sin1", "month_sin2", "month_cos1", "month_cos2")

## Set trimming quantile 
trim_quantile <- 0.99

# Step-1: Create weighted datasets ----
## Create a list to store weighted datasets
all_weighted_data <- list()

## Run a loop to create weighted datasets for each exposure variable
for (dataset_name in names(all_data)) {
  cat("\nProcessing dataset:", dataset_name, "\n")
  
  # get cumulative exposure variables
  exposures <- names(all_data[[dataset_name]])[grep("^exp", names(all_data[[dataset_name]]))]

  # Skip datasets that don't have any exposure variables
  if (length(exposures) == 0) {
    cat("No exposure variables found in dataset", dataset_name, "\n")
    next
  }
  
  for (exposure_var in exposures) {
    cat("\n  Processing exposure:", exposure_var, "\n")
    
    # Get current dataset
    df <- all_data[[dataset_name]]
    
    # Create CBPS weights
    df_weighted <- create_iptw_weights(df, exposure_var, 
      confounders, trim_quantile, 
      second_stage_model = "gaussian",
      verbose = TRUE)

    # Save weighted dataset in a list
    all_weighted_data[[dataset_name]][[exposure_var]] <- df_weighted
  }
}

## Check summary of all trimmed weights 
for (dataset_name in names(all_weighted_data)) {
  for (exposure_var in names(all_weighted_data[[dataset_name]])) {
    print(dataset_name)
    print(exposure_var)
    print(summary(all_weighted_data[[dataset_name]][[exposure_var]]$iptw_weights_trimmed))
  }
}

# Step-2: Create and save love plots for each exposure variable ----
for (dataset_name in names(all_weighted_data)) {
  for (exposure_var in names(all_weighted_data[[dataset_name]])) {
    df_weighted <- all_weighted_data[[dataset_name]][[exposure_var]]
    create_love_plot(
      df_weighted = df_weighted, 
      exposure_var = exposure_var,
      confounders = confounders,
      dataset_name = dataset_name,
      output_dir = here(path_outputs_love_plots),
      exposure_label = exposure_labels_love_plots,
      var_labels = var_labels_love_plots)
  }
}

# Step-3: Fit full models ----
results <- fit_full_models(
  all_weighted_data = all_weighted_data,
  outcome_var = "dv_no_contact_3mo",
  outcome_type = "binary",
  random_effects = "meta_state_name",
  seasonal_params = seasonal_params,
  confounders = NULL,
  output_path = path_outputs_models
)
