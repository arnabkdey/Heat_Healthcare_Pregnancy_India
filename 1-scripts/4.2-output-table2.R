# -------------------------------------------------------------------------------
# @project: Heat and healthcare contact during pregnancy in India
# @author: Arnab K. Dey,  arnabxdey@gmail.com 
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script generates survey-weighted cross-tabs for key variables across exposure vars.
# @date: Dec 26, 2024

# Libraries ----
rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, here)
library(survey)
source(here("paths-mac.R"))

# load data ----
df_paper_final <- readRDS(here(path_project, "processed-data", "2.3-final-hv-data-6mo.rds"))
dim(df_paper_final)
colnames(df_paper_final |> select(starts_with("exp_bin")))
# process data ----
## de-scale all scaled variables by mutipling by 10
df_paper_final <- df_paper_final |> 
  mutate(exp_bin_below_10 = exp_bin_below_10_10 * 10,
         exp_bin_10_15 = exp_bin_10_15_10 * 10,
         exp_bin_15_20 = exp_bin_15_20_10 * 10,
         exp_bin_20_25 = exp_bin_20_25_10 * 10,
         exp_bin_25_30 = exp_bin_25_30_10 * 10,
         exp_bin_above_30 = exp_bin_above_30_10 * 10)

## merge bins for 15 to 20 and 20 to 25 
df_paper_final <- df_paper_final |> 
  mutate(exp_bin_15_25 = exp_bin_15_20 + exp_bin_20_25)

# Create list of variables ----
## outcome and SES variables
varlist_ses <- c("dv_no_contact_3mo", "ses_wealth_bi_richer",
                  "mat_edu_level",
                  "ses_access_issue_distance", 
                  "meta_rural")

## temperature exposures 
varlist_exp_bin <- c("exp_bin_below_10", "exp_bin_10_15", "exp_bin_15_25", "exp_bin_25_30", "exp_bin_above_30")


# Check the exposure variables ----
summary(df_paper_final |> select(all_of(varlist_exp_bin)))

# Convert list of variables to factor -----
df_paper_final <- df_paper_final |> 
  mutate(across(all_of(varlist_ses), as.factor)) 

# Create survey object ----
svy_object <- svydesign(ids = ~1,
                data = df_paper_final,
                weights = df_paper_final$wt_final)

# example code to create one table ----
svyby(~exp_bin_10_15, ~dv_no_contact_3mo, 
        design = svy_object, 
        svymean, 
        na.rm = T, 
        vartype = "var") |>
    dplyr::mutate(sd = sqrt(var)) |>
    as.data.frame() |>
    dplyr::select(-var) 
 
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

# Generate all combinations and run cross tabs ----
results <- expand.grid(exposure = varlist_exp_bin,
                      ses = varlist_ses) |>
  purrr::pmap_dfr(~create_crosstab(..1, ..2, svy_object))


# Data cleaning and reshaping ----
## create a vector of labels for all variables 
var_names <- c(
  "dv_no_contact_3mo" = "Healthcare utilization in last 3 months",
  "ses_wealth_bi_richer" = "Household wealth",
  "mat_edu_level" = "Maternal education level",
  "ses_access_issue_distance" = "Distance to facility barrier",
  "meta_rural" = "Urban/Rural residence"
)

## create a list of labels for all levels
var_labels <- list(
  dv_no_contact_3mo = c(
    "0" = "Some healthcare utilization in 3 months",
    "1" = "No healthcare utilization in 3 months"
  ),
  ses_wealth_bi_richer = c(
    "richer" = "Higher wealth quintiles",
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

## clean the results
results_clean <- results |>
  dplyr::mutate(
    value = coalesce(exp_bin_below_10, exp_bin_10_15, 
                     exp_bin_15_25, exp_bin_25_30, 
                     exp_bin_above_30)
  ) |>
  dplyr::mutate(
    levels = coalesce(dv_no_contact_3mo, ses_wealth_bi_richer, 
                      mat_edu_level, ses_access_issue_distance, 
                      meta_rural)
  ) |>
  dplyr::select(-starts_with("exp_bin_")) |>
  dplyr::arrange(ses_var, exposure) |>
  dplyr::select(ses_var, levels, exposure, value, sd) |>
  # Add labels based on the variable and level
  dplyr::mutate(
    level_label = map2_chr(ses_var, levels, ~var_labels[[.x]][as.character(.y)])
  ) |>
  dplyr::mutate(result = paste0(round(value, 2), " (", round(sd, 2), ")")) 

## Pivot the data to wide format ----
results_pivot <- results_clean |> 
  tidyr::pivot_wider(
    id_cols = c(ses_var, levels, level_label),
    names_from = exposure, 
    values_from = result)

View(results_pivot)


# Create GT table ----
# Create mapping for variable names and their labels
var_names <- c(
  "dv_no_contact_3mo" = "Healthcare utilization in last 3 months",
  "ses_wealth_bi_richer" = "Household wealth",
  "mat_edu_level" = "Maternal education level",
  "ses_access_issue_distance" = "Distance to facility barrier",
  "meta_rural" = "Urban/Rural residence"
)

# Create mapping for values and their labels
var_labels <- list(
  dv_no_contact_3mo = c(
    "0" = "Some healthcare utilization in 3 months",
    "1" = "No healthcare utilization in 3 months"
  ),
  ses_wealth_bi_richer = c(
    "richer" = "Higher wealth quintiles",
    "poorer" = "Lower wealth quintiles"
  ),
  mat_edu_level = c(
    "no education" = "No formal education",
    "primary" = "Primary education",
    "secondary" = "Secondary education",
    "higher" = "Higher education"
  ),
  ses_access_issue_distance = c(
    "0" = "No distance barrier",
    "1" = "Reports distance barrier"
  ),
  meta_rural = c(
    "0" = "Urban",
    "1" = "Rural"
  )
)

# Create GT table
results_pivot |>
  gt::gt(
    groupname_col = "ses_var"
  ) |>
  # Add title and subtitle
  gt::tab_header(
    title = "",
    subtitle = "Values shown as mean (standard deviation)"
  ) |>
  # Format column headers
  gt::cols_label(
    level_label = "Level",
    exp_bin_below_10 = "Below 10°C",
    exp_bin_10_15 = "10-15°C",
    exp_bin_15_25 = "15-25°C",
    exp_bin_25_30 = "25-30°C",
    exp_bin_above_30 = "Above 30°C"
  ) |>
  # Rename the group labels (variable names)
  gt::text_transform(
    locations = gt::cells_group(),
    fn = function(x) unname(var_names[as.character(x)])
  ) |>
  # Add source note
  gt::tab_source_note(
    source_note = "Note: All values represent survey-weighted means with standard deviations in parentheses"
  ) |>
  # Style the table
  gt::tab_style(
    style = list(
      gt::cell_text(weight = "bold")
    ),
    locations = list(
      gt::cells_column_labels(),
      gt::cells_title(),
      gt::cells_group()
    )
  ) |>
  # Add a spanner for temperature columns
  gt::tab_spanner(
    label = "Temperature Exposure Categories",
    columns = starts_with("exp_bin")
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
      gt::cells_group()
    )
  ) |>
  # Customize column alignment
  gt::cols_align(
    align = "left",
    columns = c(level_label)
  ) |>
  gt::cols_align(
    align = "center",
    columns = starts_with("exp_bin")
  ) |>
  # Hide original columns
  gt::cols_hide(columns = c(ses_var, levels)) |>
  # save the table
  gt::gtsave(here(path_project, "outputs", "tables", "temperature_exposure_table.html"))
