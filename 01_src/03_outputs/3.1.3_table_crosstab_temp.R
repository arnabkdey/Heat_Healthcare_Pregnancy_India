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

# load data ----
df_paper_final <- readRDS(here(path_processed, "1.3.2.df_IR_temp_counts_merged.rds"))

# Create list of variables ----
## outcome and SES variables
varlist_ses <- c("dv_no_contact_3mo", "ses_wealth_bi_richer3",
                  "mat_edu_level",
                  "ses_access_issue_distance", 
                  "meta_rural")

## temperature exposures 
varlist_exp_tmax_wbgt <- df_paper_final |> 
  select(contains("tmax_wbgt")) |> 
  colnames()

varlist_exp_tmin_wbgt <- df_paper_final |> 
  select(contains("tmin_wbgt")) |> 
  colnames()

varlist_exp_tmax_db <- df_paper_final |> 
  select(contains("tmax_db_era5")) |> 
  colnames()

varlist_exp_tmin_db <- df_paper_final |> 
  select(contains("tmin_db_era5")) |> 
  colnames()

# Create variable labels ----
## Variable names
var_names <- c(
  "dv_no_contact_3mo" = "Healthcare utilization in last 3 months",
  "ses_wealth_bi_richer3" = "Household wealth",
  "mat_edu_level" = "Maternal education level",
  "ses_access_issue_distance" = "Distance to facility barrier",
  "meta_rural" = "Urban/Rural residence"
)

## Variable level labels
var_labels <- list(
  dv_no_contact_3mo = c(
    "0" = "Some healthcare utilization in 3 months",
    "1" = "No healthcare utilization in 3 months"
  ),
  ses_wealth_bi_richer3 = c(
    "richer3" = "Higher wealth quintiles",
    "poorer" = "Lower wealth quintiles"
  ),
  mat_edu_level = c(
    "no education" = "No formal education",
    "primary" = "Primary education",
    "secondary" = "Secondary education",
    "higher" = "Higher education"
  ),
  ses_access_issue_distance = c(
    "big-problem" = "Distance is a big problem",
    "not-a-big-prob" = "Distance is not a big problem"
  ),
  meta_rural = c(
    "urban" = "Urban",
    "rural" = "Rural"
  )
)

# Convert list of variables to factor -----
df_paper_final <- df_paper_final |> 
  mutate(across(all_of(varlist_ses), as.factor)) 

# Create survey object ----
svy_object <- svydesign(ids = ~1,
                data = df_paper_final,
                weights = df_paper_final$wt_final)

# Create a function to generate tables ----
create_crosstab <- function(exp_var, ses_var, design) {
  formula_str <- paste0("~", exp_var)
  by_str <- paste0("~", ses_var)
  
  svyby(as.formula(formula_str), 
        as.formula(by_str),
        design = design,
        svymean,
        na.rm = TRUE,
        vartype = "var") |>
    dplyr::mutate(sd = sqrt(var),
                  exposure = exp_var,
                  ses_var = ses_var) |>
    as_tibble() |>
    dplyr::select(-var)
}

# Process and create tables for each exposure type ----

## Function to process results
process_results <- function(results, exposure_pattern) {
  # Get columns matching the pattern
  matching_cols <- grep(exposure_pattern, names(results), value = TRUE)
  
  results |>
    dplyr::mutate(
      value = coalesce(!!!syms(matching_cols))
    ) |>
    dplyr::mutate(
      levels = coalesce(dv_no_contact_3mo, ses_wealth_bi_richer3, 
                      mat_edu_level, ses_access_issue_distance, 
                      meta_rural)
    ) |>
    dplyr::select(-starts_with("days_cutoff")) |>
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
  # Function to convert column names to percentile labels
  format_percentile_label <- function(col_name) {
    # Extract numeric value
    num_val <- as.numeric(gsub(".*_(\\d+)_.*", "\\1", col_name)) / 10
    
    # Format the label
    if (grepl("_greater", col_name)) {
      sprintf("%.1fth percentile", num_val)
    } else if (grepl("_less", col_name)) {
      sprintf("%.1fth percentile", num_val)
    } else {
      col_name  # Return original if pattern doesn't match
    }
  }
  
  # Get column names except level_label, ses_var, and levels
  data_cols <- setdiff(names(results_pivot), c("level_label", "ses_var", "levels"))
  
  # Create named vector for column labels
  col_labels <- c(level_label = "Level")
  col_labels[data_cols] <- sapply(data_cols, format_percentile_label)
  
  results_pivot |>
    gt::gt(
      groupname_col = "ses_var"
    ) |>
    # Add title and subtitle
    gt::tab_header(
      title = "",
      subtitle = ""
    ) |>
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
      label = paste(exposure_type, "Temperature Exposure Categories"),
      columns = starts_with("days_cutoff")
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
      columns = starts_with("days_cutoff")
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
  purrr::pmap_dfr(~create_crosstab(..1, ..2, svy_object))
results_tmax_wbgt_pivot <- process_results(results_tmax_wbgt, "tmax_wbgt")
create_gt_table(results_tmax_wbgt_pivot, "Maximum Wet Bulb Globe", "temperature_exposure_table_tmax_wbgt.html")

## WBGT Tmin
results_tmin_wbgt <- expand.grid(exposure = varlist_exp_tmin_wbgt,
                                ses = varlist_ses) |>
  purrr::pmap_dfr(~create_crosstab(..1, ..2, svy_object))
results_tmin_wbgt_pivot <- process_results(results_tmin_wbgt, "tmin_wbgt")
create_gt_table(results_tmin_wbgt_pivot, "Minimum Wet Bulb Globe", "temperature_exposure_table_tmin_wbgt.html")

## Dry Bulb Tmax
results_tmax_db <- expand.grid(exposure = varlist_exp_tmax_db,
                              ses = varlist_ses) |>
  purrr::pmap_dfr(~create_crosstab(..1, ..2, svy_object))
results_tmax_db_pivot <- process_results(results_tmax_db, "tmax_db_era5")
create_gt_table(results_tmax_db_pivot, "Maximum Dry Bulb", "temperature_exposure_table_tmax_db.html")

## Dry Bulb Tmin
results_tmin_db <- expand.grid(exposure = varlist_exp_tmin_db,
                              ses = varlist_ses) |>
  purrr::pmap_dfr(~create_crosstab(..1, ..2, svy_object))
results_tmin_db_pivot <- process_results(results_tmin_db, "tmin_db_era5")
create_gt_table(results_tmin_db_pivot, "Minimum Dry Bulb", "temperature_exposure_table_tmin_db.html")
