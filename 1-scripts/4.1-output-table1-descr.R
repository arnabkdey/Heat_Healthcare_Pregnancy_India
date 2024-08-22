# Libraries ----
rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, here)
library(survey)
source(here(".Rprofile"))

# Load datasets ----
path_processed <- here(path_project, "processed-data")
df_paper_final <- readRDS(here(path_processed, "2.3-final-hv-data.rds"))


# Load function for weighted tables ----
function_path <- here("1-scripts", "6.2-function-wtd-comparegroups.R")
source(function_path)
ls()

# Create a folder for the output
path_output <- here(path_project, "outputs", "descriptives")

# Create list of variables
varlist_ses <- c("ses_caste_3", "ses_religion_3", "ses_wealth_bi", "ses_access_issue_distance", 
		            "mat_age", "mat_cur_preg", "mat_edu_level")



# Convert list of variables to factor -----
df_paper_final <- df_paper_final |> 
  mutate(across(all_of(varlist_ses), as.factor)) |> 
  mutate(across(all_of(varlist_exp_wb_abs), as.factor)) |> 
  mutate(across(all_of(varlist_exp_wb_ntile_doy), as.factor)) 

# Create survey object
svy_object <- svydesign(ids = ~1,
                data = df_paper_final,
              weights = df_paper_final$wt_final)


# Column percentages for total FLW visits ----
df_paper_final$dv_met_flw_3mo <- as.factor(df_paper_final$dv_met_flw_3mo)
df_paper_final$mat_cur_preg <- as.factor(df_paper_final$mat_cur_preg)

table_flw_tot_col <- compareGroups_wtd(data = df_paper_final,
                            dep_var = "dv_met_flw_3mo",
                            varlist = c(varlist_ses),
                            survey_object = svy_object,
                            output_type = "full", 
                            n_digits = 1,
                            percentage_type = "column")

## Save output
openxlsx::write.xlsx(table_flw_tot_col, file = here(path_output, "table1_flw_tot_col.xlsx"))


# Column percentages for home visits ----
df_paper_final$dv_met_flw_3mo_home <- as.factor(df_paper_final$dv_met_flw_3mo_home)
table_flw_hv_col <- compareGroups_wtd(data = df_paper_final,
                            dep_var = "dv_met_flw_3mo_home",
                            varlist = c(varlist_ses),
                            survey_object = svy_object,
                            output_type = "full", 
                            n_digits = 1,
                            percentage_type = "column")

## Save output
openxlsx::write.xlsx(table_flw_hv_col, file = here(path_output, "table1_flw_hv_col.xlsx"))

# Get weighted percentages for the outcome variables
svytable(~no_pnc_b, design = svy_object) |> prop.table() |> round(4) * 100
svytable(~no_pnc_m, design = svy_object) |> prop.table() |> round(4) * 100
