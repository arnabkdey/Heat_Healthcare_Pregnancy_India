# -------------------------------------------------------------------------------
# @project: Heat and healthcare contact during pregnancy in India
# @author: Arnab K. Dey,  arnabxdey@gmail.com 
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script generates survey-weighted descriptive statistics tables for the study population.
# @date: Dec 12, 2024

# Libraries ----
rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, here)
library(survey)
source("paths-mac.R")

# Load datasets ----
path_processed <- here(path_project, "processed-data")
df_paper_final <- readRDS(here(path_processed, "2.3-final-hv-data-6mo.rds"))

# Load function for weighted tables ----
function_path <- here("1-scripts", "6.2-function-wtd-comparegroups.R")
source(function_path)
ls()

# Create list of variables
varlist_ses <- c("ses_wealth_bi_richer",
                  "mat_edu_level",
                  "ses_access_issue_distance", 
                  "meta_rural")            

varlist_exp_bin <- c("exp_bin_below_10_10", "exp_bin_10_15_10", "exp_bin_25_30_10", "exp_bin_above_30_10")
colnames(df_paper_final |> select(starts_with("mat")))

# Convert list of variables to factor -----
df_paper_final <- df_paper_final |> 
  mutate(across(all_of(varlist_ses), as.factor)) |> 
  mutate(across(all_of(varlist_exp_bin), as.factor))  

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

tabyl(df_paper_final, mat_age)
## Save output
path_output <- here(path_project, "outputs", "descriptives")
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
