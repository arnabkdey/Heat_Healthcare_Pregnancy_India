# --------------------
# @project: Heat and healthcare contact during pregnancy in India
# @author: Arnab K. Dey, Anna Dimitrova
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script generates survey-weighted cross-tabs for key variables across exposure vars.
# @date: Dec 26, 2024

# Libraries ----
rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, here, survey)

# set paths ----
source(here("paths.R"))

# load the list with all datasets ----
df_merged <- readRDS(here(path_processed, "1.3.3_cumulative_bins_all_datasets_5mo.rds"))

# Create list of variables ----
## outcome and SES variables
varlist_ses <- c("dv_no_contact_3mo", "mat_edu_level", "mat_age_at_int_cat", "mat_parity_bi",
                 "mat_media_exp_any", "ses_caste_2", "ses_religion_2_hindu",
                 "ses_access_issue_distance", "hh_wealth_quintile_ru_og", 
                 "meta_rural")

## temperature exposures 
varlist_exp_tmax_wbgt <- df_merged$tmax_wbgt |> 
  select(starts_with("days_compu")) |> 
  select(!matches("(_5|_10|_15)")) |> 
  colnames()

varlist_exp_tmin_wbgt <- df_merged$tmin_wbgt |> 
  select(starts_with("days_compu")) |> 
  select(!matches("(_85|_90|_95)")) |> 
  colnames()

varlist_exp_tmax_db <- df_merged$tmax_db_era5 |> 
  select(starts_with("days_compu")) |> 
  select(!matches("(_5|_10|_15)")) |> 
  colnames()

varlist_exp_tmin_db <- df_merged$tmin_db_era5 |> 
  select(starts_with("days_compu")) |> 
  select(!matches("(_85|_90|_95)")) |> 
  colnames()

# Create variable labels ----
## Variable names
var_names <- c(
  "dv_no_contact_3mo" = "Healthcare utilization in last 3 months",
  "mat_edu_level" = "Maternal education level",
  "mat_age_at_int_cat" = "Maternal age at interview",
  "mat_parity_bi" = "Maternal parity",
  "mat_media_exp_any" = "Maternal media exposure",
  "ses_caste_2" = "Caste",
  "ses_religion_2_hindu" = "Religion",
  "ses_access_issue_distance" = "Distance as a barrier to access healthcare",
  "hh_wealth_quintile_ru_og" = "Household wealth",
  "meta_rural" = "Urban/Rural residence"
)

## Variable level labels
var_labels <- list(
  dv_no_contact_3mo = c(
    "0" = "Some healthcare utilization in 3 months",
    "1" = "No healthcare utilization in 3 months"
  ),
  mat_edu_level = c(
    "no education" = "No formal education",
    "primary" = "Primary education",
    "secondary" = "Secondary education",
    "higher" = "Higher education"
  ),
  mat_age_at_int_cat = c(
    "15-24" = "15-24 years",
    "25-34" = "25-34 years",
    "35-49" = "35-49 years"
  ),
  mat_parity_bi = c(
    "Primiparous" = "First birth",
    "Multiparous" = "Second or higher birth"
  ),
  mat_media_exp_any = c(
    "no" = "No media exposure",
    "yes" = "Media exposure"
  ),
  ses_caste_2 = c(
    "SC/ST/OBC" = "Scheduled Caste/Tribe/Other Backward Class",
    "General" = "General/Other castes"
  ),
  ses_religion_2_hindu = c(
    "Hindu" = "Hindu",
    "Not-Hindu" = "Other religions"
  ),
  ses_access_issue_distance = c(
    "big-problem" = "Distance is a big problem",
    "not-a-big-prob" = "Distance is not a big problem"
  ),
  hh_wealth_quintile_ru_og = c(
    "poorest" = "Poorest",
    "poorer" = "Poorer",
    "middle" = "Middle",
    "richer" = "Richer",
    "richest" = "Richest"
  ),
  meta_rural = c(
    "urban" = "Urban residence",
    "rural" = "Rural residence"
  )
)

exposure_labels_tmin <- c(
  "days_compu_bins_cumulative_5" = "below the 5th percentile",
  "days_compu_bins_cumulative_10" = "below the 10th percentile",
  "days_compu_bins_cumulative_15" = "below the 15th percentile"
)

exposure_labels_tmax <- c(
  "days_compu_bins_cumulative_95" = "above the 95th percentile",
  "days_compu_bins_cumulative_90" = "above the 90th percentile",
  "days_compu_bins_cumulative_85" = "above the 85th percentile"
)


# Convert list of variables to factor -----
for (i in names(df_merged)) {
  df_merged[[i]] <- df_merged[[i]] |> 
    mutate(across(all_of(varlist_ses), as.factor)) 
}

# Create a list of survey objects ----
svy_list <- list()
for (i in names(df_merged)) {
  svy_list[[i]] <- svydesign(ids = ~1,
                data = df_merged[[i]],
                weights = df_merged[[i]]$wt_final)
}
names(svy_list) <- names(df_merged)

# Create a function to generate tables for each exposure type in each dataset ----
create_crosstab <- function(exp_var, ses_var, design) {
  formula_str <- paste0("~", exp_var)
  by_str <- paste0("~", ses_var)
  
  # Get the results
  results <- svyby(as.formula(formula_str), 
        as.formula(by_str),
        design = design,
        svymean,
        na.rm = TRUE,
        vartype = "var")
  
  # Convert to tibble and add metadata
  results <- results |>
    dplyr::mutate(sd = sqrt(var),
                  exposure = exp_var,
                  ses_var = ses_var) |>
    as_tibble() |>
    dplyr::select(-var)
  
  # Rename the exposure column to match the variable name
  colnames(results)[colnames(results) == exp_var] <- "value"
  
  return(results)
}

# Process and create tables for each exposure type ----
process_results <- function(results, exposure_pattern) {
  # Get columns matching the pattern
  matching_cols <- grep(exposure_pattern, names(results), value = TRUE)
  
  # Print debugging information
  cat("Matching columns:", paste(matching_cols, collapse = ", "), "\n")
  cat("Number of rows in results:", nrow(results), "\n")
  
  results |>
    dplyr::mutate(
      levels = coalesce(dv_no_contact_3mo, mat_edu_level, mat_age_at_int_cat,
        mat_parity_bi, mat_media_exp_any, ses_caste_2, 
        ses_religion_2_hindu, ses_access_issue_distance, 
        hh_wealth_quintile_ru_og, meta_rural)
    ) |>
    dplyr::arrange(ses_var, exposure) |>
    dplyr::select(ses_var, levels, exposure, value, sd) |>
    dplyr::mutate(
      level_label = map2_chr(ses_var, levels, ~var_labels[[.x]][as.character(.y)])
    ) |>
    dplyr::mutate(result = paste0(round(value, 2), " (", round(sd, 2), ")")) |>
    tidyr::pivot_wider(
      id_cols = c(ses_var, levels, level_label),
      names_from = exposure, 
      values_from = result
    )
}

# Function to create and format GT table
create_gt_table <- function(results_pivot, exposure_type, file_name) {
  # Print column names for debugging
  print("Column names in results_pivot:")
  print(names(results_pivot))
  
  # Choose the right label set
  if (exposure_type %in% c("tmax_wbgt", "tmax_db_era5")) {
    label_map <- exposure_labels_tmax
  } else if (exposure_type %in% c("tmin_wbgt", "tmin_db_era5")) {
    label_map <- exposure_labels_tmin
  } else {
    label_map <- NULL
  }
  
  # Print label map for debugging
  print("Label map:")
  print(label_map)
  
  # Get columns to label
  data_cols <- setdiff(names(results_pivot), c("level_label", "ses_var", "levels"))
  
  # Map labels, fallback to raw name if not found
  col_labels <- c(level_label = "Level")
  col_labels[data_cols] <- sapply(data_cols, function(x) {
    if (!is.null(label_map) && x %in% names(label_map)) {
      gt::html(label_map[[x]])
    } else {
      x
    }
  })
  
  results_pivot |>
    gt::gt(
      groupname_col = "ses_var"
    ) |>
    # Add title and subtitle
    # gt::tab_header(
    #   title = "",
    #   subtitle = ""
    # ) |>
    # Format column headers based on exposure type
    gt::cols_label(
      .list = col_labels
    ) |>
    # Rename the group labels (variable names)
    gt::text_transform(
      locations = gt::cells_row_groups(),
      fn = function(x) unname(var_names[as.character(x)])
    ) |>
    # Add source notes
    gt::tab_source_note(
      source_note = ("Note 1: All values represent survey-weighted means with standard deviations in parentheses")
    ) |>
    gt::tab_source_note(
      source_note = ("Note 2: Values presented as mean (standard deviation)")
    ) |>
    # Style the table
    gt::tab_style(
      style = list(
        gt::cell_text(weight = "bold")
      ),
      locations = list(
        gt::cells_column_labels(),
        gt::cells_title(),
        gt::cells_row_groups()
      )
    ) |>
    # Add a spanner for temperature columns
    gt::tab_spanner(
      label = gt::html("<b>Mean (std. dev) of days</b>"),
      columns = starts_with("days_compu_bins_cumulative_"),
    ) |>
    # Style alternating rows
    gt::opt_row_striping() |>
    # Format the table with clean theme
    gt::opt_table_font(
      font = list(
        gt::google_font("Source Sans Pro")
      )
    ) |>
    # Add borders
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = "bottom",
          color = "gray85",
          weight = gt::px(1)
        )
      ),
      locations = list(
        gt::cells_body()
      )
    ) |>
    # Style group rows
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "gray95"),
        gt::cell_text(weight = "bold")
      ),
      locations = list(
        gt::cells_row_groups()
      )
    ) |>
    # Customize column alignment
    gt::cols_align(
      align = "left",
      columns = c(level_label)
    ) |>
    gt::cols_align(
      align = "center",
      columns = starts_with("days_compu_bins_cumulative_")
    ) |>
    # Hide original columns
    gt::cols_hide(columns = c(ses_var, levels)) |>
    # save the table
    gt::gtsave(here(path_outputs, "tables", file_name))
}

# Generate tables for each exposure type
## WBGT Tmax
results_tmax_wbgt <- expand.grid(exposure = varlist_exp_tmax_wbgt,
                                ses = varlist_ses) |>
  purrr::pmap_dfr(~create_crosstab(..1, ..2, svy_list$tmax_wbgt))

results_tmax_wbgt_pivot <- process_results(results_tmax_wbgt, "days_compu_bins_cumulative")
create_gt_table(results_tmax_wbgt_pivot, "tmax_wbgt", "temperature_exposure_table_tmax_wbgt.html")

## WBGT Tmin
results_tmin_wbgt <- expand.grid(exposure = varlist_exp_tmin_wbgt,
                                ses = varlist_ses) |>
  purrr::pmap_dfr(~create_crosstab(..1, ..2, svy_list$tmin_wbgt))

results_tmin_wbgt_pivot <- process_results(results_tmin_wbgt, "tmin_wbgt")
create_gt_table(results_tmin_wbgt_pivot, "tmin_wbgt", "temperature_exposure_table_tmin_wbgt.html")

## Dry Bulb Tmax
results_tmax_db <- expand.grid(exposure = varlist_exp_tmax_db,
                              ses = varlist_ses) |>
  purrr::pmap_dfr(~create_crosstab(..1, ..2, svy_list$tmax_db_era5))
results_tmax_db_pivot <- process_results(results_tmax_db, "tmax_db_era5")
create_gt_table(results_tmax_db_pivot, "tmax_db_era5", "temperature_exposure_table_tmax_db.html")

## Dry Bulb Tmin
results_tmin_db <- expand.grid(exposure = varlist_exp_tmin_db,
                              ses = varlist_ses) |>
  purrr::pmap_dfr(~create_crosstab(..1, ..2, svy_list$tmin_db_era5))
results_tmin_db_pivot <- process_results(results_tmin_db, "tmin_db_era5")
create_gt_table(results_tmin_db_pivot, "tmin_db_era5", "temperature_exposure_table_tmin_db.html")


