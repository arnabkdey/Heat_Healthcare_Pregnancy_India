# load libraries ----
rm(list = ls())
pacman::p_load(dplyr, janitor, data.table, fst, here, future.apply, tictoc)

# set paths ----
source(here("paths_win.R"))

# source functions ----
source(here("01_src", "01_data_processing", "utils", "func_calc_percentile_cutoffs_daily.R"))

# dataset paths ----
## Tmax 
### WBGT
path_tmax_wbgt <- here(path_processed, "1.2.1.a_df_psu_tmax_wbgt.fst")
### NOAA
path_tmax_db_noaa <- here(path_processed, "1.2.1.b_df_psu_tmax_db_noaa.fst")
### ERA5
path_tmax_db_era5 <- here(path_processed, "1.2.1.c_df_psu_tmax_db_era5.fst")

## Tmin
### WBGT
path_tmin_wbgt <- here(path_processed, "1.2.1.d_df_psu_tmin_wbgt.fst")

### NOAA
path_tmin_db_noaa <- here(path_processed, "1.2.1.e_df_psu_tmin_db_noaa.fst")
### ERA5
path_tmin_db_era5 <- here(path_processed, "1.2.1.f_df_psu_tmin_db_era5.fst")

# define vector cutoffs ----
vec_cutoffs_tmax <- c(80, 82.5, 85, 87.5, 90, 92.5, 95)
vec_cutoffs_tmin <- c(5, 7.5, 10, 12.5, 15, 17.5, 20)

# Process Tmax vars and cutoffs ----
## Tmax
### WBGT
df_tmax_wbgt_lt <- calculate_cutoffs(path_tmax_wbgt, "tmax_wbgt", vec_cutoffs_tmax)
## save the dataset ----
df_tmax_wbgt_lt |> write_fst(here(path_processed, "1.2.2.a1_df_psu_tmax_wbgt_lt.fst"))
## filter to 2014 onwards
df_tmax_wbgt_lt_2014 <- df_tmax_wbgt_lt[date >= as.Date("2014-01-01")]
rm(df_tmax_wbgt_lt)
df_tmax_wbgt_lt_2014 |> write_fst(here(path_processed, "1.2.2.a2_df_psu_tmax_wbgt_lt_2014.fst"))
rm(df_tmax_wbgt_lt_2014)

### NOAA
df_tmax_db_noaa_lt <- calculate_cutoffs(path_tmax_db_noaa, "tmax_db_noaa", vec_cutoffs_tmax)
## save the dataset ----
df_tmax_db_noaa_lt |> write_fst(here(path_processed, "1.2.2.b1_df_psu_tmax_db_noaa_lt.fst"))
## filter to 2014 onwards
df_tmax_db_noaa_lt_2014 <- df_tmax_db_noaa_lt[date >= as.Date("2014-01-01")]
rm(df_tmax_db_noaa_lt)
df_tmax_db_noaa_lt_2014 |> write_fst(here(path_processed, "1.2.2.b2_df_psu_tmax_db_noaa_lt_2014.fst"))
rm(df_tmax_db_noaa_lt_2014)

### ERA5
df_tmax_db_era5_lt <- calculate_cutoffs(path_tmax_db_era5, "tmax_db_era5", vec_cutoffs_tmax)
## save the dataset ----
df_tmax_db_era5_lt |> write_fst(here(path_processed, "1.2.2.c1_df_psu_tmax_db_era5_lt.fst"))
## filter to 2014 onwards
df_tmax_db_era5_lt_2014 <- df_tmax_db_era5_lt[date >= as.Date("2014-01-01")]
rm(df_tmax_db_era5_lt)
df_tmax_db_era5_lt_2014 |> write_fst(here(path_processed, "1.2.2.c2_df_psu_tmax_db_era5_lt_2014.fst"))
rm(df_tmax_db_era5_lt_2014)

## Tmin
### WBGT
df_tmin_wbgt_lt <- calculate_cutoffs(path_tmin_wbgt, "tmin_wbgt", vec_cutoffs_tmin)
## save the dataset ----
df_tmin_wbgt_lt |> write_fst(here(path_processed, "1.2.2.d1_df_psu_tmin_wbgt_lt.fst"))
## filter to 2014 onwards 
df_tmin_wbgt_lt_2014 <- df_tmin_wbgt_lt[date >= as.Date("2014-01-01")]
rm(df_tmin_wbgt_lt)
df_tmin_wbgt_lt_2014 |> write_fst(here(path_processed, "1.2.2.d2_df_psu_tmin_wbgt_lt_2014.fst"))
rm(df_tmin_wbgt_lt_2014)

### NOAA
df_tmin_db_noaa_lt <- calculate_cutoffs(path_tmin_db_noaa, "tmin_db_noaa", vec_cutoffs_tmin)
## save the dataset ----
df_tmin_db_noaa_lt |> write_fst(here(path_processed, "1.2.2.e1_df_psu_tmin_db_noaa_lt.fst"))
## filter to 2014 onwards
df_tmin_db_noaa_lt_2014 <- df_tmin_db_noaa_lt[date >= as.Date("2014-01-01")]
rm(df_tmin_db_noaa_lt)
df_tmin_db_noaa_lt_2014 |> write_fst(here(path_processed, "1.2.2.e2_df_psu_tmin_db_noaa_lt_2014.fst"))
rm(df_tmin_db_noaa_lt_2014)

### ERA5
df_tmin_db_era5_lt <- calculate_cutoffs(path_tmin_db_era5, "tmin_db_era5", vec_cutoffs_tmin)
## save the dataset ----
df_tmin_db_era5_lt |> write_fst(here(path_processed, "1.2.2.f1_df_psu_tmin_db_era5_lt.fst"))
## filter to 2014 onwards
df_tmin_db_era5_lt_2014 <- df_tmin_db_era5_lt[date >= as.Date("2014-01-01")]
rm(df_tmin_db_era5_lt)
df_tmin_db_era5_lt_2014 |> write_fst(here(path_processed, "1.2.2.f2_df_psu_tmin_db_era5_lt_2014.fst"))
rm(df_tmin_db_era5_lt_2014)
