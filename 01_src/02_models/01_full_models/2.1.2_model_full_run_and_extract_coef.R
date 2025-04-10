# ---------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @author: Arnab K. Dey, Anna Dimitrova
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script runs the full models for the paper and extracts coefficients for the final paper.
# @date: March 2025

# Load Libraries ----
rm(list = ls())
pacman::p_load(tidyverse, here, tictoc, beepr)
pacman::p_load(MatchIt, cobalt, sandwich, survey, gbm, geepack, WeightIt)

# Set paths ----
source(here("paths.R"))

# load datasets ----
df_paper_final <- readRDS(here(path_processed,
  "1.3.2.df_IR_temp_counts_merged.rds"))

# source functions to extract coefficients ----
source(here("01_src", "02_models", "utils", "function_to_extract_full_model_results.R"))

# set constants ----
## Define exposure variables ----
varlist_exposure_all <- dput(df_paper_final |> 
  select(starts_with("days_cutoff_")) |> 
  select(-contains("noaa")) |>
  colnames())

## Define covariates for direct matching ----
covariates_all <- c("mat_edu_level", "mat_age_at_int_cat", 
                "mat_parity_bi",
                "mat_media_exp_any",
                "ses_caste_4", "ses_religion_4", 
                "ses_access_issue_distance", 
                "hh_wealth_quintile_ru_og", 
                "meta_rural")

## Convert all covariates to factors
df_paper_final <- df_paper_final |>
  mutate(across(all_of(covariates_all), as.factor))

# Prepare datasets ----
df_effects <- list(df_paper_final)
names(df_effects) <- c("alldata")

# Setup progress tracking ----
total_dfs <- length(names(df_effects))
total_exposures <- length(varlist_exposure_all)
total_combinations <- total_dfs * total_exposures
processed_combinations <- 0
successful_models <- 0
skipped_models <- 0

cat("\n===============================================================\n")
cat("Starting model fitting for", total_combinations, "total combinations\n")
cat("===============================================================\n")
cat("Datasets:", total_dfs, "\n")
cat("Exposure variables:", total_exposures, "\n\n")

# Start overall timer
tic("Total execution time")

# Model fitting ----
## Initialize an empty list to store all models
all_models <- list()

## Loop over all combinations of df_effect, exposure_var
for (df_idx in 1:length(names(df_effects))) {
  df_name <- names(df_effects)[df_idx]
  
  cat("\n[", format(Sys.time(), "%H:%M:%S"), "] Processing dataset", df_idx, "of", total_dfs, ":", df_name, "\n")
  
  ### Make a copy of covariates_all to restore at the end of each dataset iteration
  covariates_current <- covariates_all
  
  ### Extract the current dataset
  df_cur <- df_effects[[df_name]]
  cat("  - Working with", format(nrow(df_cur), big.mark=","), "observations\n")
  
  # Start timer for this dataset
  tic(paste("Dataset:", df_name))
  
  # Track skipped combinations for this dataset
  dataset_skipped <- 0
  dataset_success <- 0
  
  for (exp_idx in 1:length(varlist_exposure_all)) {
      exposure_var <- varlist_exposure_all[exp_idx]
      processed_combinations <- processed_combinations + 1
      
      # Calculate overall progress percentage
      progress_pct <- round(processed_combinations / total_combinations * 100, 1)
      
      cat("  [", format(Sys.time(), "%H:%M:%S"), "] Processing exposure", exp_idx, "of", total_exposures, 
          ":", exposure_var, "(Overall progress:", progress_pct, "%)\n")
      
      # Check if exposure_var has at least two unique values
      exposure_values <- table(df_cur[[exposure_var]])
      if (length(exposure_values) < 2) {
        warning(paste("    Skipping combination:", df_name, exposure_var, 
                      "because exposure_var has only one unique value."))
        skipped_models <- skipped_models + 1
        dataset_skipped <- dataset_skipped + 1
        next  # Skip to the next iteration
      }
      
      # Show distribution of exposure variable
      cat("    - Exposure distribution:", paste(names(exposure_values), "=", exposure_values, collapse=", "), "\n")

      # Create weights ----
      cat("    - Fitting propensity score model using GBM...\n")
      
      ## Use weightit with GBM method to generate propensity scores and weights
      w_gbm <- tryCatch({
        weightit(as.formula(paste(exposure_var, "~", 
                      paste(covariates_current, collapse = " + "))),
                      data = df_cur,
                      method = "gbm",
                      estimand = "ATE",
                      stop.method = "p.mean")
      }, error = function(e) {
        cat("    *** ERROR in GBM weight generation:", conditionMessage(e), "\n")
        return(NULL)
      })
      
      if (is.null(w_gbm)) {
        skipped_models <- skipped_models + 1
        dataset_skipped <- dataset_skipped + 1
        next
      }
      
      ## Add weights to dataset
      df_cur$stabilized_weight <- w_gbm$weights
      
      # Check weight distribution
      weight_quantiles <- quantile(df_cur$stabilized_weight, c(0.01, 0.25, 0.5, 0.75, 0.99), na.rm = TRUE)
      cat("    - Weight distribution:", 
          sprintf("min=%.2f, q01=%.2f, median=%.2f, q99=%.2f, max=%.2f", 
                  min(df_cur$stabilized_weight, na.rm=TRUE),
                  weight_quantiles[1],
                  weight_quantiles[3],
                  weight_quantiles[5],
                  max(df_cur$stabilized_weight, na.rm=TRUE)), "\n")
      
      # Modelling ----
      cat("    - Fitting GEE model...\n")
      
      ## GEE model with stabilized weights
      gee_formula <- as.formula(paste("dv_no_contact_3mo ~", exposure_var, 
        " + month_sin1 + month_cos1 + month_sin2 + month_cos2"))
      
      gee_model <- tryCatch({
        geeglm(
          gee_formula,
          data = df_cur,
          id = meta_state_name, 
          weights = stabilized_weight,
          family = binomial(link = "logit"),
          corstr = "ar1"
        )
      }, error = function(e) {
        cat("    *** ERROR in GEE model:", conditionMessage(e), "\n")
        return(NULL)
      })
      
      if (is.null(gee_model)) {
        skipped_models <- skipped_models + 1
        dataset_skipped <- dataset_skipped + 1
        next
      }
      
      # Get model summary and show key statistics
      model_summary <- summary(gee_model)
      exposure_coef <- coef(model_summary)[exposure_var, ]
      
      cat("    - Model results for", exposure_var, ":", 
          sprintf("coef=%.3f, SE=%.3f, p=%.4f", 
                  exposure_coef[1], 
                  exposure_coef[2], 
                  exposure_coef[4]), "\n")
      
      # Store the model in the list with a unique name
      model_name <- paste("gee-model", df_name, exposure_var, sep = "-")
      all_models[[model_name]] <- gee_model
      successful_models <- successful_models + 1
      dataset_success <- dataset_success + 1
  }
  
  # End timer for this dataset
  toc()
  
  cat("Dataset", df_name, "completed:", 
      dataset_success, "successful models,", 
      dataset_skipped, "skipped models.\n")
}

# End total timer
toc()

# Get output paths
output_rds <- here(path_outputs, "models", "full_models", "models_all.rds")
output_xlsx <- here(path_outputs, "models", "full_models", "coefs_all_models.xlsx")

# Print final summary
cat("\n===============================================================\n")
cat("Final Summary\n")
cat("===============================================================\n")
cat("Total datasets processed:", total_dfs, "\n")
cat("Total exposure variables:", total_exposures, "\n")
cat("Total combinations:", total_combinations, "\n")
cat("Successful models:", successful_models, "\n")
cat("Skipped combinations:", skipped_models, "\n")
cat("\n")

# Save all models as a single list
cat("Saving all models to:", basename(output_rds), "\n")
saveRDS(all_models, output_rds)

# Extract coefficients ----
cat("\nExtracting coefficients and saving to Excel...\n")
tic("Coefficient extraction")
process_models_to_excel(output_rds, output_xlsx)
toc()

cat("\nComplete! All results saved to:", basename(output_xlsx), "\n")

