# --------------------
# @project: Heat and healthcare contact during pregnancy in India
# @author: Arnab K. Dey
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script counts the number of days in temperature bins for each PSU.
# @date: Apr, 2025

rm(list = ls())
pacman::p_load(dplyr, janitor, data.table, fst, here, tictoc)

# set paths ----
source("paths.R")

# source functions ----
source(here("01_src", "01_data_processing", "utils", 
  "func_count_num_days_bins.R"))

# count number of days in bins ----
## WBGT ----
### Tmax
print("Counting days in bins for Tmax WBGT...")
tic()

#### load DHS data with PSU level cutoffs
df_dhs_tmax_wbgt_psu <- fst::read_fst(here(path_processed, "1.3.1.a_df_dhs_tmax_wbgt_psu.fst"), 
  as.data.table = TRUE)

#### load daily tmax_wbgt data
df_tmax_wbgt_daily <- fst::read_fst(here(path_processed, "1.2.1.a_df_psu_tmax_wbgt.fst"), 
  as.data.table = TRUE)

#### Filter to 2014 onwards
df_tmax_wbgt_daily <- df_tmax_wbgt_daily |> filter(date >= as.Date("2014-01-01"))

#### count number of days in bins
result_tmax_wbgt <- count_days_in_bins(
  df_dhs = df_dhs_tmax_wbgt_psu,
  df_daily = df_tmax_wbgt_daily,
  temp_var = "tmax_wbgt"
)
toc()
print(Sys.time())

#### save the result
result_tmax_wbgt |> fst::write_fst(here("outputs", "1.3.2.a_df_bins_tmax_wbgt.fst"))
rm(result_tmax_wbgt, df_tmax_wbgt_daily, df_dhs_tmax_wbgt_psu)
print("Tmax WBGT bins saved.")

### Tmin
print("Counting days in bins for Tmin WBGT...")
tic()

#### load DHS data with PSU level cutoffs
df_dhs_tmin_wbgt_psu <- fst::read_fst(here(path_processed, "1.3.1.b_df_dhs_tmin_wbgt_psu.fst"), 
  as.data.table = TRUE)

#### load daily tmin_wbgt data
df_tmin_wbgt_daily <- fst::read_fst(here(path_processed, "1.2.1.b_df_psu_tmin_wbgt.fst"), 
  as.data.table = TRUE)

#### Filter to 2014 onwards
df_tmin_wbgt_daily <- df_tmin_wbgt_daily |> filter(date >= as.Date("2014-01-01"))

#### count number of days in bins
result_tmin_wbgt <- count_days_in_bins(
  df_dhs = df_dhs_tmin_wbgt_psu,
  df_daily = df_tmin_wbgt_daily,
  temp_var = "tmin_wbgt"
)
toc()
print(Sys.time())

#### save the result
result_tmin_wbgt |> fst::write_fst(here("outputs", "1.3.2.b_df_bins_tmin_wbgt.fst"))
rm(result_tmin_wbgt, df_tmin_wbgt_daily, df_dhs_tmin_wbgt_psu)
print("Tmin WBGT bins saved.")

### Tmean
print("Counting days in bins for Tmean WBGT...")
tic()

#### load DHS data with PSU level cutoffs
df_dhs_tmean_wbgt_psu <- fst::read_fst(here(path_processed, "1.3.1.c_df_dhs_tmean_wbgt_psu.fst"), 
  as.data.table = TRUE)

#### load daily tmean_wbgt data
df_tmean_wbgt_daily <- fst::read_fst(here(path_processed, "1.2.1.c_df_psu_tmean_wbgt.fst"), 
  as.data.table = TRUE)

#### Filter to 2014 onwards
df_tmean_wbgt_daily <- df_tmean_wbgt_daily |> filter(date >= as.Date("2014-01-01"))

#### count number of days in bins
result_tmean_wbgt <- count_days_in_bins(
  df_dhs = df_dhs_tmean_wbgt_psu,
  df_daily = df_tmean_wbgt_daily,
  temp_var = "tmean_wbgt"
)
toc()
print(Sys.time())

#### save the result
result_tmean_wbgt |> fst::write_fst(here("outputs", "1.3.2.c_df_bins_tmean_wbgt.fst"))
rm(result_tmean_wbgt, df_tmean_wbgt_daily, df_dhs_tmean_wbgt_psu)
print("Tmean WBGT bins saved.")

## ERA5 ----
### Tmax
print("Counting days in bins for Tmax ERA5...")
tic()

#### load DHS data with PSU level cutoffs
df_dhs_tmax_era5_psu <- fst::read_fst(here(path_processed, "1.3.1.d_df_dhs_tmax_db_era5_psu.fst"), 
  as.data.table = TRUE)

#### load daily tmax_era5 data
df_tmax_era5_daily <- fst::read_fst(here(path_processed, "1.2.1.d_df_psu_tmax_db_era5.fst"), 
  as.data.table = TRUE)

#### Filter to 2014 onwards
df_tmax_era5_daily <- df_tmax_era5_daily |> filter(date >= as.Date("2014-01-01"))

#### count number of days in bins
result_tmax_era5 <- count_days_in_bins(
  df_dhs = df_dhs_tmax_era5_psu,
  df_daily = df_tmax_era5_daily,
  temp_var = "tmax_db_era5"
)
toc()
print(Sys.time())

#### save the result
result_tmax_era5 |> fst::write_fst(here("outputs", "1.3.2.d_df_bins_tmax_db_era5.fst"))
rm(result_tmax_era5, df_tmax_era5_daily, df_dhs_tmax_era5_psu)
print("Tmax ERA5 bins saved.")

### Tmin
print("Counting days in bins for Tmin ERA5...")
tic()

#### load DHS data with PSU level cutoffs
df_dhs_tmin_era5_psu <- fst::read_fst(here(path_processed, "1.3.1.e_df_dhs_tmin_db_era5_psu.fst"), 
  as.data.table = TRUE)

#### load daily tmin_era5 data
df_tmin_era5_daily <- fst::read_fst(here(path_processed, "1.2.1.e_df_psu_tmin_db_era5.fst"),  
  as.data.table = TRUE)

#### Filter to 2014 onwards
df_tmin_era5_daily <- df_tmin_era5_daily |> filter(date >= as.Date("2014-01-01"))

#### count number of days in bins
result_tmin_era5 <- count_days_in_bins( 
  df_dhs = df_dhs_tmin_era5_psu,
  df_daily = df_tmin_era5_daily,
  temp_var = "tmin_db_era5"
)
toc()
print(Sys.time())

#### save the result
result_tmin_era5 |> fst::write_fst(here("outputs", "1.3.2.e_df_bins_tmin_db_era5.fst"))
rm(result_tmin_era5, df_tmin_era5_daily, df_dhs_tmin_era5_psu)
print("Tmin ERA5 bins saved.")

### Tmean
print("Counting days in bins for Tmean ERA5...")
tic()

#### load DHS data with PSU level cutoffs
df_dhs_tmean_era5_psu <- fst::read_fst(here(path_processed, "1.3.1.f_df_dhs_tmean_db_era5_psu.fst"), 
  as.data.table = TRUE)

#### load daily tmean_era5 data
df_tmean_era5_daily <- fst::read_fst(here(path_processed, "1.2.1.f_df_psu_tmean_db_era5.fst"),  
  as.data.table = TRUE)

#### Filter to 2014 onwards
df_tmean_era5_daily <- df_tmean_era5_daily |> filter(date >= as.Date("2014-01-01"))

#### count number of days in bins
result_tmean_era5 <- count_days_in_bins(
  df_dhs = df_dhs_tmean_era5_psu,
  df_daily = df_tmean_era5_daily,
  temp_var = "tmean_db_era5"
)
toc()
print(Sys.time())

#### save the result
result_tmean_era5 |> fst::write_fst(here("outputs", "1.3.2.f_df_bins_tmean_db_era5.fst"))
rm(result_tmean_era5, df_tmean_era5_daily, df_dhs_tmean_era5_psu)
print("Tmean ERA5 bins saved.")

## NOAA ----
### Tmax
print("Counting days in bins for Tmax NOAA...")
tic()

#### load DHS data with PSU level cutoffs
df_dhs_tmax_noaa_psu <- fst::read_fst(here(path_processed, "1.3.1.g_df_dhs_tmax_db_noaa_psu.fst"), 
  as.data.table = TRUE)

#### load daily tmax_noaa data
df_tmax_noaa_daily <- fst::read_fst(here(path_processed, "1.2.1.g_df_psu_tmax_db_noaa.fst"),  
  as.data.table = TRUE)

#### Filter to 2014 onwards
df_tmax_noaa_daily <- df_tmax_noaa_daily |> filter(date >= as.Date("2014-01-01"))

#### count number of days in bins
result_tmax_noaa <- count_days_in_bins(
  df_dhs = df_dhs_tmax_noaa_psu,
  df_daily = df_tmax_noaa_daily,
  temp_var = "tmax_db_noaa"
)
toc()
print(Sys.time())

#### save the result
result_tmax_noaa |> fst::write_fst(here("outputs", "1.3.2.g_df_bins_tmax_db_noaa.fst"))
rm(result_tmax_noaa, df_tmax_noaa_daily, df_dhs_tmax_noaa_psu)
print("Tmax NOAA bins saved.")

### Tmin
print("Counting days in bins for Tmin NOAA...")
tic()

#### load DHS data with PSU level cutoffs
df_dhs_tmin_noaa_psu <- fst::read_fst(here(path_processed, "1.3.1.h_df_dhs_tmin_db_noaa_psu.fst"), 
  as.data.table = TRUE)

#### load daily tmin_noaa data
df_tmin_noaa_daily <- fst::read_fst(here(path_processed, "1.2.1.h_df_psu_tmin_db_noaa.fst"),  
  as.data.table = TRUE)

#### Filter to 2014 onwards
df_tmin_noaa_daily <- df_tmin_noaa_daily |> filter(date >= as.Date("2014-01-01"))

#### count number of days in bins
result_tmin_noaa <- count_days_in_bins(
  df_dhs = df_dhs_tmin_noaa_psu,
  df_daily = df_tmin_noaa_daily,
  temp_var = "tmin_db_noaa"
)
toc()
print(Sys.time())

#### save the result
result_tmin_noaa |> fst::write_fst(here("outputs", "1.3.2.h_df_bins_tmin_db_noaa.fst"))
rm(result_tmin_noaa, df_tmin_noaa_daily, df_dhs_tmin_noaa_psu)
print("Tmin NOAA bins saved.")
