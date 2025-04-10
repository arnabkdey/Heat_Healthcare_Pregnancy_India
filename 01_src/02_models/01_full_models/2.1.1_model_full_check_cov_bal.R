# ---------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @author: Arnab K. Dey, Anna Dimitrova
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script runs the full models for the paper and checks covariate balance using Love plots.
# @date: March 2025

# Load Libraries ----
rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, here, tictoc, beepr)
pacman::p_load(MatchIt, cobalt, sandwich, survey, ggplot2, geepack, pscl, WeightIt)

# Set paths ----
source(here("paths.R"))

# read datasets ----
df_paper_final <- readRDS(here(path_processed,
  "1.3.2.df_IR_temp_counts_merged.rds"))
colnames(df_paper_final)

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

cat("Starting processing of", total_combinations, "total combinations\n")
cat("===========================================================\n")

# Model fitting ----
## Initialize an empty list to store all models
all_models <- list()

# Start overall timer
tic("Total execution time")

## Loop over all combinations of df_effect, exposure_var
for (df_idx in 1:length(names(df_effects))) {
  df_name <- names(df_effects)[df_idx]
  
  cat("\n[", format(Sys.time(), "%H:%M:%S"), "] Processing dataset", df_idx, "of", total_dfs, ":", df_name, "\n")
  
  ### Make a copy of covariates_all to restore at the end of each dataset iteration
  covariates_current <- covariates_all
  
  ### Extract the current dataset
  df_cur <- df_effects[[df_name]]
  
  # Start timer for this dataset
  tic(paste("Dataset:", df_name))
  
  # Track skipped combinations for this dataset
  skipped_count <- 0
  
  for (exp_idx in 1:length(varlist_exposure_all)) {
      exposure_var <- varlist_exposure_all[exp_idx]
      processed_combinations <- processed_combinations + 1
      
      # Calculate overall progress percentage
      progress_pct <- round(processed_combinations / total_combinations * 100, 1)
      
      cat("  [", format(Sys.time(), "%H:%M:%S"), "] Processing exposure", exp_idx, "of", total_exposures, 
          ":", exposure_var, "(Overall progress:", progress_pct, "%)\n")
      
      # Check if exposure_var has at least two unique values
      if (length(unique(df_cur[[exposure_var]])) < 2) {
        warning(paste("  Skipping combination:", df_name, exposure_var, 
                      "because exposure_var has only one unique value."))
        skipped_count <- skipped_count + 1
        next  # Skip to the next iteration
      }

      # Create weights using GBM approach ----
      cat("    - Fitting propensity score model using GBM...\n")
      
      ## Use weightit with GBM method to generate propensity scores and weights
      w_gbm <- weightit(as.formula(paste(exposure_var, "~", 
                      paste(covariates_current, collapse = " + "))),
                      data = df_cur,
                      method = "gbm",
                      estimand = "ATE",
                      stop.method = "p.mean")
      
      ## Add weights to dataset
      df_cur$weights_untrimmed <- w_gbm$weights
      
      ## Trim weights
      q <- quantile(df_cur$weights_untrimmed, c(0.01, 0.99))
      df_cur$weights_trimmed <- pmin(pmax(df_cur$weights_untrimmed, q[1]), q[2])

      # Check balance ----
      cat("    - Creating Love plot...\n")
      
      ## Define variable labels
      var_labels <- c(
        "ses_access_issue_distance_not-a-big-prob" = "Distance not a big problem for healthcare access",
        "ses_caste_4_SC" = "Caste: Scheduled Caste",
        "ses_caste_4_ST" = "Caste: Scheduled Tribe",
        "ses_caste_4_OBC" = "Caste: Other Backward Classes",
        "ses_caste_4_Other" = "Caste: Other",
        "ses_religion_4_Hindu" = "Religion: Hindu",
        "ses_religion_4_Muslim" = "Religion: Muslim",
        "ses_religion_4_Christian" = "Religion: Christian",
        "ses_religion_4_Other" = "Religion: Other",
        "hh_wealth_quintile_ru_og_middle" = "Household wealth: Middle quintile",
        "hh_wealth_quintile_ru_og_poorer" = "Household wealth: Poorer quintile",
        "hh_wealth_quintile_ru_og_poorest" = "Household wealth: Poorest quintile",
        "hh_wealth_quintile_ru_og_richer" = "Household wealth: Richer quintile",
        "hh_wealth_quintile_ru_og_richest" = "Household wealth: Richest quintile",
        "mat_age_grp_at_int_cat_15-24" = "Mother's age at birth: 15-24 years",
        "mat_age_grp_at_int_cat_25-34" = "Mother's age at birth: 25-34 years",
        "mat_age_grp_at_int_cat_35-49" = "Mother's age at birth: 35-49 years",
        "mat_edu_level_higher" = "Mother's education: Higher",
        "mat_edu_level_no education" = "Mother's education: No education",
        "mat_edu_level_primary" = "Mother's education: Primary",
        "mat_edu_level_secondary" = "Mother's education: Secondary",
        "mat_media_exp_any_yes" = "Mother exposed to media: Yes",
        "mat_parity_bi_Primiparous" = "Primiparous",
        "mat_parity_fac_Multiparous" = "Multiparous",
        "meta_rural_rural" = "Rural residence"
      )

      ## Define exposure labels
      exposure_labels <- c(
        "days_cutoff_tmax_wbgt_perc_800_greater" = "Tmax-WBGT >= 80th percentile",
        "days_cutoff_tmax_wbgt_perc_825_greater" = "Tmax-WBGT >= 82.5th percentile",
        "days_cutoff_tmax_wbgt_perc_850_greater" = "Tmax-WBGT >= 85th percentile",
        "days_cutoff_tmax_wbgt_perc_875_greater" = "Tmax-WBGT >= 87.5th percentile",
        "days_cutoff_tmax_wbgt_perc_900_greater" = "Tmax-WBGT >= 90th percentile",
        "days_cutoff_tmax_wbgt_perc_925_greater" = "Tmax-WBGT >= 92.5th percentile",
        "days_cutoff_tmax_wbgt_perc_950_greater" = "Tmax-WBGT >= 95th percentile",
        "days_cutoff_tmax_db_era5_perc_800_greater" = "Tmax-DBT >= 80th percentile",
        "days_cutoff_tmax_db_era5_perc_825_greater" = "Tmax-DBT >= 82.5th percentile",
        "days_cutoff_tmax_db_era5_perc_850_greater" = "Tmax-DBT >= 85th percentile",
        "days_cutoff_tmax_db_era5_perc_875_greater" = "Tmax-DBT >= 87.5th percentile",
        "days_cutoff_tmax_db_era5_perc_900_greater" = "Tmax-DBT >= 90th percentile",
        "days_cutoff_tmax_db_era5_perc_925_greater" = "Tmax-DBT >= 92.5th percentile",
        "days_cutoff_tmax_db_era5_perc_950_greater" = "Tmax-DBT >= 95th percentile",
        "days_cutoff_tmin_wbgt_perc_050_less" = "Tmin-WBGT <= 5th percentile",
        "days_cutoff_tmin_wbgt_perc_075_less" = "Tmin-WBGT <= 7.5th percentile",
        "days_cutoff_tmin_wbgt_perc_100_less" = "Tmin-WBGT <= 10th percentile",
        "days_cutoff_tmin_wbgt_perc_125_less" = "Tmin-WBGT <= 12.5th percentile",
        "days_cutoff_tmin_wbgt_perc_150_less" = "Tmin-WBGT <= 15th percentile",
        "days_cutoff_tmin_wbgt_perc_175_less" = "Tmin-WBGT <= 17.5th percentile",
        "days_cutoff_tmin_wbgt_perc_200_less" = "Tmin-WBGT <= 20th percentile",
        "days_cutoff_tmin_db_era5_perc_050_less" = "Tmin-DBT <= 5th percentile",
        "days_cutoff_tmin_db_era5_perc_075_less" = "Tmin-DBT <= 7.5th percentile",
        "days_cutoff_tmin_db_era5_perc_100_less" = "Tmin-DBT <= 10th percentile",
        "days_cutoff_tmin_db_era5_perc_125_less" = "Tmin-DBT <= 12.5th percentile",
        "days_cutoff_tmin_db_era5_perc_150_less" = "Tmin-DBT <= 15th percentile",
        "days_cutoff_tmin_db_era5_perc_175_less" = "Tmin-DBT <= 17.5th percentile",
        "days_cutoff_tmin_db_era5_perc_200_less" = "Tmin-DBT <= 20th percentile")

      ## Create the Love plot
      setDT(df_cur)
      love_plot <- love.plot(
        df_cur |> select(all_of(covariates_current)), 
        treat = df_cur[[exposure_var]],
        weights = df_cur$weights_trimmed,
        binary = "std",
        threshold = 0.1,
        abs = FALSE,
        var.order = "alphabetical",
        var.names = var_labels,
        colours = c("#E41A1C", "#377EB8"),  # Colors for unweighted and weighted
        shapes = c(16, 17),  # Shapes for unweighted and weighted
        size = 3,
        position = "center",
        title = paste0("Covariate Balance Before and After GBM Weighting\n", 
          "Exposure Variable: ",
          exposure_labels[exposure_var]),
        sample.names = c("Unweighted", "GBM Weighted"),  # Updated names for the legend
        line = FALSE
      )

      ## Customize the plot to move the legend to the bottom
      love_plot <- love_plot +
        theme(
          legend.position = "bottom",  # Move legend to the bottom
          legend.title = element_blank(),  # Remove legend title
          legend.text = element_text(size = 12),  # Adjust legend text size
          legend.key.size = unit(1.5, "lines")  # Adjust legend key size
        )
      
      # Save the plot ----
      output_path <- here(path_output, "figures", "love_plots", "gbm-trimmed",
             paste0("love-gbm-", df_name, "-", exposure_var, ".png"))
      
      cat("    - Saving plot to:", basename(output_path), "\n")
      
      ggsave(
        output_path, 
        plot = love_plot, 
        width = 20, 
        height = 16, 
        units = "in",
        dpi = 600,
        bg = "white"
      )
  }
  
  # End timer for this dataset
  toc()
  
  cat("Dataset", df_name, "completed. Skipped", skipped_count, "of", 
  total_exposures, "exposure variables.\n")
}

# End total timer
toc()

# Print final summary
cat("\n=== Final Summary ===\n")
cat("Total datasets processed:", total_dfs, "\n")
cat("Total exposure variables:", total_exposures, "\n")
cat("Total combinations processed:", processed_combinations, "\n")
cat("Complete!\n")

#add a beep to signal completion
try(beepr::beep(sound = 'ping'), silent = TRUE)