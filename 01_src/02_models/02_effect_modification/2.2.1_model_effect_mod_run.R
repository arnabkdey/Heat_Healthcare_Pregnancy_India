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

# set constants ----
## Define exposure variables ----
varlist_exposure_all <- dput(df_paper_final |> 
  select(starts_with("days_cutoff_")) |> 
  select(-contains("noaa")) |>
  colnames())

## Define modifier variables ----
modifier_vars <- c("meta_rural", "ses_access_issue_distance", 
    "ses_wealth_bi_richer3", "mat_edu_level_bi")

### convert all modifier variables to factors
df_paper_final <- df_paper_final |>
  mutate(across(all_of(modifier_vars), as.factor))

### Relevel modifier variables
df_paper_final$ses_wealth_bi_richer3 <- relevel(df_paper_final$ses_wealth_bi_richer3, ref = "richer3")
df_paper_final$ses_access_issue_distance <- relevel(df_paper_final$ses_access_issue_distance, ref = "not-a-big-prob")
df_paper_final$mat_edu_level_bi <- relevel(df_paper_final$mat_edu_level_bi, ref = "primary or higher")

# Create weights ----
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
 
# Run models in a loop ----
# Create empty list to store model results
model_results_list <- list()

# Track progress
total_exposures <- length(varlist_exposure_all)
total_modifiers <- length(modifier_vars)
total_combinations <- total_exposures * total_modifiers
processed_combinations <- 0

cat("\n===============================================================\n")
cat("Starting model fitting for", total_combinations, "total combinations\n")
cat("===============================================================\n")
cat("Exposure variables:", total_exposures, "\n")
cat("Modifier variables:", total_modifiers, "\n\n")

# Start overall timer
tic("Total execution time")

# Loop through each exposure variable
for (exp_idx in 1:length(varlist_exposure_all)) {
  exposure_var <- varlist_exposure_all[exp_idx]
  
  cat("\n[", format(Sys.time(), "%H:%M:%S"), "] Processing exposure", exp_idx, "of", total_exposures, 
      ":", exposure_var, "\n")
  
  # Check if exposure_var has at least two unique values
  exposure_values <- table(df_paper_final[[exposure_var]])
  if (length(exposure_values) < 2) {
    warning(paste("    Skipping exposure:", exposure_var, 
                  "because it has only one unique value."))
    next  # Skip to the next iteration
  }
  
  # Show distribution of exposure variable
  cat("    - Exposure distribution:", paste(names(exposure_values), "=", exposure_values, collapse=", "), "\n")
  
  # Create weights ----
  cat("    - Fitting propensity score model using GBM...\n")
  
  ## Use weightit with GBM method to generate propensity scores and weights
  w_gbm <- tryCatch({
    weightit(as.formula(paste(exposure_var, "~", 
                  paste(covariates_all, collapse = " + "))),
                  data = df_paper_final,
                  method = "gbm",
                  estimand = "ATE",
                  stop.method = "p.mean")
  }, error = function(e) {
    cat("    *** ERROR in GBM weight generation:", conditionMessage(e), "\n")
    return(NULL)
  })
  
  if (is.null(w_gbm)) {
    next
  }
  
  ## Add weights to dataset
  df_paper_final$stabilized_weight <- w_gbm$weights
  
  # Check weight distribution
  weight_quantiles <- quantile(df_paper_final$stabilized_weight, c(0.01, 0.25, 0.5, 0.75, 0.99), na.rm = TRUE)
  cat("    - Weight distribution:", 
      sprintf("min=%.2f, q01=%.2f, median=%.2f, q99=%.2f, max=%.2f", 
              min(df_paper_final$stabilized_weight, na.rm=TRUE),
              weight_quantiles[1],
              weight_quantiles[3],
              weight_quantiles[5],
              max(df_paper_final$stabilized_weight, na.rm=TRUE)), "\n")
  
  # Loop through each modifier variable
  for (mod_idx in 1:length(modifier_vars)) {
    modifier_var <- modifier_vars[mod_idx]
    processed_combinations <- processed_combinations + 1
    
    # Calculate overall progress percentage
    progress_pct <- round(processed_combinations / total_combinations * 100, 1)
    
    # Generate model name
    model_name <- paste0(exposure_var, "_X_", modifier_var)
    
    cat("    [", format(Sys.time(), "%H:%M:%S"), "] Processing modifier", mod_idx, "of", total_modifiers, 
        ":", modifier_var, "(Overall progress:", progress_pct, "%)\n")
    
    # Generate interaction model formula - update dependent variable to dv_home_del_num
    gee_formula <- as.formula(
      paste("dv_no_contact_3mo ~", exposure_var, "*", modifier_var,
        "+ month_sin1 + month_cos1 + month_sin2 + month_cos2"))
    
    # Try to fit the GEE model with error handling
    tryCatch({
      # Fit the GEE model with updated id variable to psu_fac
      model <- geeglm(
        gee_formula,
        data = df_paper_final,
        id = meta_state_name, 
        weights = stabilized_weight,
        family = binomial(link = "logit"),
        corstr = "ar1"
      )
      
      # Get model summary and show key statistics
      model_summary <- summary(model)
      interaction_term <- paste0(exposure_var, ":", modifier_var)
      if(interaction_term %in% rownames(coef(model_summary))) {
        interaction_coef <- coef(model_summary)[interaction_term, ]
        
        cat("      - Interaction result:", 
            sprintf("coef=%.3f, SE=%.3f, p=%.4f", 
                    interaction_coef[1], 
                    interaction_coef[2], 
                    interaction_coef[4]), "\n")
      }
      
      # Store the model in the list
      model_results_list[[model_name]] <- model
      
    }, error = function(e) {
      # Handle errors
      cat("      *** ERROR in GEE model:", conditionMessage(e), "\n")
    })
  }
}

# End total timer
toc()

# Print final summary
cat("\n===============================================================\n")
cat("Final Summary\n")
cat("===============================================================\n")
cat("Total exposure variables:", total_exposures, "\n")
cat("Total modifier variables:", total_modifiers, "\n")
cat("Total combinations:", total_combinations, "\n")
cat("Models stored:", length(model_results_list), "\n")
cat("Skipped combinations:", total_combinations - length(model_results_list), "\n")
cat("\n")

# Save the model results list
saveRDS(model_results_list, here(
  path_project, "outputs", "models", "effect_modification",
  "gee_interaction_models.rds"))

cat("\nComplete! All models saved to: outputs/models/effect_modification/gee_interaction_models.rds\n")