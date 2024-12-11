# -------------------------------------------------------------------------------
# @project: Heat and healthcare contact during pregnancy in India
# @author: Arnab K. Dey,  arnabxdey@gmail.com 
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script creates heat wave and temperature exceedance variables using absolute temperature thresholds and consecutive day criteria.
# @date: Dec 12, 2024

rm(list = ls())
pacman::p_load(dplyr, janitor, data.table, fst, openxlsx, here, googledrive)
library(climExposuR)
source("paths-mac.R")

# Read datasets -----
## Long-term WBGT data ----
df_lt_vars_2014 <- read_fst(here(path_project, "processed-data", "2.1.1-df_psu_wbgt_2014-na-removed.fst"), as.data.table = T)

# create cutoff vars ----
df_lt_vars_2014$cutoff_abs_28 <- 28
df_lt_vars_2014$cutoff_abs_30 <- 30
df_lt_vars_2014$cutoff_abs_32 <- 32

# create vectors ---
vec_threshold_vars <- c("cutoff_abs_28", "cutoff_abs_30", "cutoff_abs_32")
vec_consec_days <- c(2, 3, 4, 5)

# Create heatwave variables ----
df_lt_2014_hw <- climExposuR::create_heatwave_vars(df_lt_vars_2014, vec_threshold_vars, vec_consec_days, date_var = "date", clim_var = "max_temp_wb", psu_var = "psu")

# Create exceedence variables ----
vec_threshold_abs <- c(28, 30, 32)
df_lt_hw_mag_ident <- climExposuR::func_ident_mag_calc_mag(df_lt_2014_hw, vec_threshold_abs, vec_varnames_perc = NULL, clim_var = "max_temp_wb")
colnames(df_lt_hw_mag_ident)
dim(df_lt_hw_mag_ident)
write.fst(df_lt_hw_mag_ident, here(path_project, "processed-data", "2.2-df_wbgt_hw_exceed_vars.fst"))

