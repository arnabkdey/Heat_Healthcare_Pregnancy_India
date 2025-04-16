# load libraries ----
rm(list = ls())
pacman::p_load(dplyr, janitor, data.table, fst, here, future.apply, tictoc)

# set paths ----
source(here("paths.R"))

# source functions ----
source(here("01_src", "01_data_processing", "utils", "func_calc_percentile_cutoffs_psu.R"))

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
vec_cutoffs <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 95)

# Process Tmax vars and cutoffs ----
## Tmax
### WBGT
df_tmax_wbgt_lt <- calculate_cutoffs(path_tmax_wbgt, "tmax_wbgt", vec_cutoffs)
## save the dataset ----
df_tmax_wbgt_lt |> write_fst(here(path_processed, "1.2.3.a_df_psu_tmax_wbgt_lt.fst"))
rm(df_tmax_wbgt_lt)
print("WBGT Tmax cutoffs calculated and saved.")


### ERA5
df_tmax_db_era5_lt <- calculate_cutoffs(path_tmax_db_era5, "tmax_db_era5", vec_cutoffs)
## save the dataset ----
df_tmax_db_era5_lt |> write_fst(here(path_processed, "1.2.3.c_df_psu_tmax_db_era5_lt.fst"))
rm(df_tmax_db_era5_lt)
print("ERA5 Tmax cutoffs calculated and saved.")

### NOAA
df_tmax_db_noaa_lt <- calculate_cutoffs(path_tmax_db_noaa, "tmax_db_noaa", vec_cutoffs)
## save the dataset ----
df_tmax_db_noaa_lt |> write_fst(here(path_processed, "1.2.3.b_df_psu_tmax_db_noaa_lt.fst"))
rm(df_tmax_db_noaa_lt)
print("NOAA Tmax cutoffs calculated and saved.")

## Tmin
### WBGT
df_tmin_wbgt_lt <- calculate_cutoffs(path_tmin_wbgt, "tmin_wbgt", vec_cutoffs)
## save the dataset ----
df_tmin_wbgt_lt |> write_fst(here(path_processed, "1.2.3.d_df_psu_tmin_wbgt_lt.fst"))
rm(df_tmin_wbgt_lt)
print("WBGT Tmin cutoffs calculated and saved.")

### ERA5
df_tmin_db_era5_lt <- calculate_cutoffs(path_tmin_db_era5, "tmin_db_era5", vec_cutoffs)
## save the dataset ----
df_tmin_db_era5_lt |> write_fst(here(path_processed, "1.2.3.f_df_psu_tmin_db_era5_lt.fst"))
rm(df_tmin_db_era5_lt)
print("ERA5 Tmin cutoffs calculated and saved.")

### NOAA
df_tmin_db_noaa_lt <- calculate_cutoffs(path_tmin_db_noaa, "tmin_db_noaa", vec_cutoffs)
## save the dataset ----
df_tmin_db_noaa_lt |> write_fst(here(path_processed, "1.2.3.e_df_psu_tmin_db_noaa_lt.fst"))
rm(df_tmin_db_noaa_lt)
print("NOAA Tmin cutoffs calculated and saved.")
