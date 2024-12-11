# -------------------------------------------------------------------------------
# @project: Heat and healthcare contact during pregnancy in India
# @author: Arnab K. Dey,  arnabxdey@gmail.com 
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script calculates temperature exposure variables for specific time periods related to healthcare visits.
# @date: Dec 12, 2024

rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive, here)
library(climExposuR)
source("paths-mac.R")

# Read datasets -----
## Long-term WBGT data ----
df_lt_vars_2014 <- read_fst(here(path_project, "processed-data", "2.2-df_wbgt_hw_exceed_vars.fst"), as.data.table = T)
df_lt_vars_2014$psu <- as.factor(df_lt_vars_2014$psu)

### Address missing wbgt data ----
df_missing <- df_lt_vars_2014 |> filter(is.na(max_temp_wb))
tabyl(df_missing, psu) # we see that some PSUs dont have temp data at all
# Drop these PSUs
psus_missing <- unique(df_lt_vars_2014 |> filter(is.na(max_temp_wb)) |> pull(psu))
df_lt_vars_2014 <- df_lt_vars_2014 |> filter(!psu %in% psus_missing)
sum(is.na(df_lt_vars_2014)) # no missing values

## Health datasets ----
df_IR_full <- readRDS(here(path_project, "processed-data", "1.4-processed-IR-data-6mo.rds"))
df_IR_full$psu <- df_IR_full$meta_psu

# Function to calculate bins ----
source(here("1-scripts", "6.1-function-to-calc-temp-bins.R"))

# Calculate the period specific exposures ----
colnames(df_lt_vars_2014)
vec_identifiers <- c("consec", "ident", "mag")

## Home visits (90 days ) ----
### Counts and sums ----
df_hv_90_exp <- func_calc_descr_period(
    df_lt_clim = df_lt_vars_2014, 
    df_health = df_IR_full, 
    start_date_var = "doi", 
    add_days = NULL,
    subtract_days = 90, 
    psu_var = "psu",
    vec_identifiers = vec_identifiers,
    sum = TRUE,
    count_non_zero = TRUE,
    max = FALSE,
    count_d = TRUE, 
    d = 2
)

### Bins ----
df_hv_90_exp <- func_calc_bins_period(
                                df_hv_90_exp, 
                                df_lt_vars_2014, 
                                start_date_var = "doi", 
                                add_days = NULL, 
                                subtract_days = 90,
                                psu_var = "psu", 
                                clim_var = "max_temp_wb")


# Scale exposures as needed ----
summary(df_hv_90_exp |> select(starts_with("bin")))
summary(df_hv_90_exp |> select(starts_with("sum")))
summary(df_hv_90_exp |> select(starts_with("count")) |> select(-starts_with("count_d")))
# Note sum and count (not coun_d) values for all 'ident' vars should be the same
sum(df_hv_90_exp$sum_ident_abs_28 - df_hv_90_exp$count_ident_abs_28) # should be 0
summary(df_hv_90_exp |> select(starts_with("exp_count_d")))

df_hv_90_exp_scaled <- df_hv_90_exp |> 
        # Scale temperature bins
        dplyr::mutate(across(starts_with("bin"), 
                ~ .x / 10, 
                .names = "exp_{.col}_10")) |>
        # Count number of exceedance days
        ## Scale sum_indents by 10 
        dplyr::mutate(across(starts_with("sum_ident"), 
                ~ .x / 10, 
                .names = "exp_{.col}_10")) |>
        # count of number of 5 day heatwave events
        ## Add a prefix of exp_ to all vars starting with "cound_d_consec"
        dplyr::mutate(across(starts_with("count_d_consec"), 
                ~ .x, 
                .names = "exp_{.col}")) |> 
        # count total number of consecutive days above exceedance for the period
        ## scale count_consec by 10
        dplyr::mutate(across(starts_with("count_consec"), 
                ~ .x / 10, 
                .names = "exp_{.col}_10")) |>
        # Sum of exceedance (magnitude)
        ## scale sum_mag by standard deviation
        dplyr::mutate(across(starts_with("sum_mag"), 
                ~ .x / sd(.x), 
                .names = "exp_{.col}_sd"))

# Save as RDS ----
## Save as rds file
saveRDS(df_hv_90_exp_scaled, here(path_project, "processed-data", "2.3-final-hv-data-6mo.rds"))
## Save as dta file
df_selected <- df_hv_90_exp_scaled |>        
  dplyr::select(
        starts_with("exp_bin"),
        starts_with("meta"), starts_with("dv"), 
        doi, starts_with("ses"), starts_with("mat"),
        contains("zone"), contains("int"), contains("birth"))

haven::write_dta(df_selected, here(path_project, "processed-data", "2.3-final-hv-data-6mo.dta"))

# df_hv_90_exp_scaled <- readRDS(here(path_project, "processed-data", "2.3-final-hv-data.rds"))
# colnames(df_hv_90_exp_scaled)
