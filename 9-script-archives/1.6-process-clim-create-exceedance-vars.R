rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive, here)
library(climExposuR)

# Constants ----
path_processed <- here("2-data", "2.2-processed-data")

# Read datasets --------
# df_lt_vars <- read_fst(here(path_processed, "1.6a-clim-lt-vars-max_temp_wb-crude.fst"), as.data.table = T)
df_lt_vars <- read_fst(here(path_processed, "1.5.1-df_psu_wbgt.fst"), as.data.table = T)
df_paper_final <- readRDS(here(path_processed, "1.4-dhs-IR-merged-w-clim-zones.rds"))

# Filter Long-Term climate data --------------------------------
## Include only dates from January 2014 onwards
df_lt_vars_2014 <- df_lt_vars[date >= as.Date("2014-01-01")]
rm(df_lt_vars)
nrow(df_lt_vars_2014)
print("loading complete")

# Address missing wbgt data ----
df_missing <- df_lt_vars_2014 |> filter(is.na(max_temp_wb))
tabyl(df_missing, psu) # we see that some PSUs dont have temp data at all
## Drop these PSUs
psus_missing <- unique(df_lt_vars_2014 |> filter(is.na(max_temp_wb)) |> pull(psu))
df_lt_vars_2014 <- df_lt_vars_2014 |> filter(!psu %in% psus_missing)
sum(is.na(df_lt_vars_2014)) # no missing values


# Set constants ----
vec_threshold_abs <- c(30, 31, 32)
# vec_varnames_perc <- colnames(df_lt_vars_2014[, .SD, .SDcols = patterns("^cutoff")])
clim_var <- "max_temp_wb"

# identify exceedance days and calculate magnitude ----
df_exceed <- func_ident_mag_calc_mag(df_lt_vars_2014, vec_threshold_abs, vec_varnames_perc = NULL, clim_var)

## Check the results ----
colnames(df_exceed)
tabyl(df_exceed, ident_abs_30)
# head(df_exceed |> select(psu, date, max_temp_wb, starts_with("ident"), starts_with("mag")) |> filter(ident_abs_31 == 1))

# Save the results -----
write_fst(df_exceed, here(path_processed, "1.6-clim-exceedance-vars-max_temp_wb.fst"))
print("saving complete")


# Models
## Read datasets --------
df_