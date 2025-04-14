
# --------------------
# @project: Heat and healthcare contact during pregnancy in India
# @author: Arnab K. Dey, Anna Dimitrova
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script generates survey-weighted descriptive statistics tables for the study population.
# @date: Dec 12, 2024

# Libraries ----
rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, here, survey)

# set paths ----
source(here("paths.R"))

# Load datasets ----
df_paper_final <- readRDS(here(path_processed, "1.3.2.df_IR_temp_counts_merged.rds"))

# Load function for weighted tables ----
function_path <- here("01_src", "03_outputs", "utils", "function_wtd_comparegroups.R")
source(function_path)

# Create list of variables
varlist_ses <- c("ses_wealth_bi_richer3",
                  "mat_edu_level",
                  "ses_access_issue_distance", 
                  "meta_rural")

# Convert list of variables to factor -----
df_paper_final <- df_paper_final |> 
  mutate(across(all_of(varlist_ses), as.factor)) 

# Create survey object
svy_object <- svydesign(ids = ~1,
                data = df_paper_final,
                weights = df_paper_final$wt_final)

# Column percentages for total FLW visits ----
df_paper_final$dv_no_contact_3mo <- as.factor(df_paper_final$dv_no_contact_3mo)

table_flw_tot_col <- compareGroups_wtd(data = df_paper_final,
                            dep_var = "dv_no_contact_3mo",
                            varlist = c(varlist_ses),
                            survey_object = svy_object,
                            output_type = "full", 
                            n_digits = 1,
                            percentage_type = "column")

## Save output
path_output <- here(path_outputs, "tables")
openxlsx::write.xlsx(table_flw_tot_col, file = here(path_output, "table1_flw_tot_col.xlsx"))

# Get mean age of mothers
svyvar(~mat_age, design = svy_object, na.rm = T) |> as.data.frame() |> dplyr::mutate(sd = sqrt(mat_age))
svyby(~mat_age, ~dv_no_contact_3mo, 
        design = svy_object, 
        svymean, 
        na.rm = T, 
        vartype = "var") |>
    dplyr::mutate(sd = sqrt(var)) |>
    as.data.frame()

# Get weighted percentages for the outcome variables
svytable(~dv_no_contact_3mo, design = svy_object) |> prop.table() |> round(4) * 100
tabyl(df_paper_final, dv_no_contact_3mo)
nrow(df_paper_final)
