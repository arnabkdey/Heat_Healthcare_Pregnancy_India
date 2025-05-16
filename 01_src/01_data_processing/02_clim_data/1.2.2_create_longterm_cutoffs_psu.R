# --------------------
# @project: Heat and healthcare contact during pregnancy in India
# @author: Arnab K. Dey
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script creates long-term cutoffs for WBGT, dry bulb temperature for each DHS PSU location.
# @date: Apr, 2025

# Load libraries ----
rm(list = ls())
pacman::p_load(dplyr, janitor, data.table, fst, here, future.apply, tictoc)

# Set paths ----
source(here("paths.R"))

# Source functions ----
source(here("01_src", "01_data_processing", "utils", "func_calc_percentile_cutoffs_psu.R"))

# Set constants ----
## Dataset paths ----
### WBGT ----
#### Tmax 
path_tmax_wbgt <- here(path_processed, "1.2.1.a_df_psu_tmax_wbgt.fst")
#### Tmin 
path_tmin_wbgt <- here(path_processed, "1.2.1.b_df_psu_tmin_wbgt.fst")
#### Tmean 
path_tmean_wbgt <- here(path_processed, "1.2.1.c_df_psu_tmean_wbgt.fst")

### Dry Bulb Temperature - ERA5 ----
#### Tmax
path_tmax_db_era5 <- here(path_processed, "1.2.1.d_df_psu_tmax_db_era5.fst")
#### Tmin
path_tmin_db_era5 <- here(path_processed, "1.2.1.e_df_psu_tmin_db_era5.fst")
#### Tmean
path_tmean_db_era5 <- here(path_processed, "1.2.1.f_df_psu_tmean_db_era5.fst")

### Dry Bulb Temperature - NOAA ----
#### Tmax
path_tmax_db_noaa <- here(path_processed, "1.2.1.g_df_psu_tmax_db_noaa.fst")
#### Tmin
path_tmin_db_noaa <- here(path_processed, "1.2.1.h_df_psu_tmin_db_noaa.fst")

### Heat Index - ERA5 ----
#### Tmax
path_tmax_hi_era5 <- here(path_processed, "1.2.1.a.1_df_psu_tmax_hi_era5.fst")
#### Tmin
path_tmin_hi_era5 <- here(path_processed, "1.2.1.a.2_df_psu_tmin_hi_era5.fst")

## Define vector cutoffs ----
vec_cutoffs <- seq(5, 95, by = 5)

# Create long-term cutoffs ----
## WBGT data ----
### Tmax
df_tmax_wbgt_lt <- calculate_cutoffs(path_tmax_wbgt, "tmax_wbgt", vec_cutoffs)

#### save the dataset 
df_tmax_wbgt_lt |> write_fst(here(path_processed, "1.2.2.a_df_psu_tmax_wbgt_lt.fst"))
rm(df_tmax_wbgt_lt)
print("WBGT Tmax cutoffs calculated and saved.")

### Tmin
df_tmin_wbgt_lt <- calculate_cutoffs(path_tmin_wbgt, "tmin_wbgt", vec_cutoffs)

#### save the dataset 
df_tmin_wbgt_lt |> write_fst(here(path_processed, "1.2.2.b_df_psu_tmin_wbgt_lt.fst"))
rm(df_tmin_wbgt_lt)
print("WBGT Tmin cutoffs calculated and saved.")

### Tmean
df_tmean_wbgt_lt <- calculate_cutoffs(path_tmean_wbgt, "tmean_wbgt", vec_cutoffs)

#### save the dataset 
df_tmean_wbgt_lt |> write_fst(here(path_processed, "1.2.2.c_df_psu_tmean_wbgt_lt.fst"))
rm(df_tmean_wbgt_lt)
print("WBGT Tmean cutoffs calculated and saved.")

## ERA5 data ----
### Tmax
df_tmax_db_era5_lt <- calculate_cutoffs(path_tmax_db_era5, "tmax_db_era5", vec_cutoffs)

#### save the dataset 
df_tmax_db_era5_lt |> write_fst(here(path_processed, "1.2.2.d_df_psu_tmax_db_era5_lt.fst"))
rm(df_tmax_db_era5_lt)
print("ERA5 Tmax cutoffs calculated and saved.")

### Tmin
df_tmin_db_era5_lt <- calculate_cutoffs(path_tmin_db_era5, "tmin_db_era5", vec_cutoffs)

#### save the dataset 
df_tmin_db_era5_lt |> write_fst(here(path_processed, "1.2.2.e_df_psu_tmin_db_era5_lt.fst"))
rm(df_tmin_db_era5_lt)
print("ERA5 Tmin cutoffs calculated and saved.")

### Tmean
df_tmean_db_era5_lt <- calculate_cutoffs(path_tmean_db_era5, "tmean_db_era5", vec_cutoffs)

#### save the dataset 
df_tmean_db_era5_lt |> write_fst(here(path_processed, "1.2.2.f_df_psu_tmean_db_era5_lt.fst"))
rm(df_tmean_db_era5_lt)
print("ERA5 Tmean cutoffs calculated and saved.")

## NOAA data ----
### Tmax
df_tmax_db_noaa_lt <- calculate_cutoffs(path_tmax_db_noaa, "tmax_db_noaa", vec_cutoffs)

#### save the dataset 
df_tmax_db_noaa_lt |> write_fst(here(path_processed, "1.2.2.g_df_psu_tmax_db_noaa_lt.fst"))
rm(df_tmax_db_noaa_lt)
print("NOAA Tmax cutoffs calculated and saved.")

### Tmin
df_tmin_db_noaa_lt <- calculate_cutoffs(path_tmin_db_noaa, "tmin_db_noaa", vec_cutoffs)

#### save the dataset 
df_tmin_db_noaa_lt |> write_fst(here(path_processed, "1.2.2.h_df_psu_tmin_db_noaa_lt.fst"))
rm(df_tmin_db_noaa_lt)
print("NOAA Tmin cutoffs calculated and saved.")

## Heat Index data ----
### Tmax
df_tmax_hi_era5_lt <- calculate_cutoffs(path_tmax_hi_era5, "tmax_hi_era5", vec_cutoffs)

#### save the dataset 
df_tmax_hi_era5_lt |> write_fst(here(path_processed, "1.2.2.i_df_psu_tmax_hi_era5_lt.fst"))
rm(df_tmax_hi_era5_lt)
print("Heat Index Tmax cutoffs calculated and saved.")

### Tmin
df_tmin_hi_era5_lt <- calculate_cutoffs(path_tmin_hi_era5, "tmin_hi_era5", vec_cutoffs)

#### save the dataset 
df_tmin_hi_era5_lt |> write_fst(here(path_processed, "1.2.2.j_df_psu_tmin_hi_era5_lt.fst"))
rm(df_tmin_hi_era5_lt)
print("Heat Index Tmin cutoffs calculated and saved.")

