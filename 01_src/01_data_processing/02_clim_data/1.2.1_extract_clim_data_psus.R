# --------------------
# @project: Heat and healthcare contact during pregnancy in India
# @author: Arnab K. Dey,  arnabxdey@gmail.com 
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script extracts daily WBGT, dry bulb temperature, and precipitation data for each DHS PSU location.
# @date: Dec 12, 2024

# Load Packages ----
rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, here)
pacman::p_load(sf, sp, raster, terra, tidyterra, ncdf4, rnaturalearth)

# set paths ----
source("paths_mac.R")

# load functions ----
source(here("01_src", "01_data_processing", "utils", "function_to_extract_clim_data_psus.R"))

# load-datasets ---- 
df_dhs_psu_geo_sf <- readRDS(here(path_processed, "1.1.4.a_df_dhs_psu_geo.rds"))
india_boundary_buf <- readRDS(here(path_processed, "1.1.4.b_ind_boundary_0_buf.rds"))

# Step-1: Run the function to extract climate data for each PSU ----
## Tmax - WBGT ----
df_psu_tmax_wbgt <- merge_dhs_climate(path = here(path_rmax_wbgt_raw), 
  clim_var = "tmax_wbgt")
write_fst(df_psu_tmax_wbgt, path = here(path_processed, "1.2.1.a_df_psu_tmax_wbgt.fst"))
rm(df_psu_tmax_wbgt)
print("finished Step-1a: tmax-wbgt")

## Tmax - dry bulb -NOAA ----
df_psu_tmax_db_noaa <- merge_dhs_climate(path = here(path_tmax_db_noaa), 
  clim_var = "tmax_db_noaa")
write_fst(df_psu_tmax_db_noaa, path = here(path_processed, "1.2.1.b_df_psu_tmax_db_noaa.fst"))
rm(df_psu_tmax_db_noaa)
print("finished Step-1b: tmax db - noaa")

## Tmax - dry bulb -ERA5 ----
df_psu_tmax_db_era5 <- merge_dhs_climate(path = here(path_tmax_db_era5), 
  clim_var = "tmax_db_era5")

write_fst(df_psu_tmax_db_era5, path = here(path_processed, "1.2.1.c_df_psu_tmax_db_era5.fst"))
rm(df_psu_tmax_db_era5)
print("finished Step-1c: tmax db - era5")

## Tmin - WBGT ----
df_psu_tmin_wbgt <- merge_dhs_climate(path = here(path_tmin_wbgt_raw), 
  clim_var = "tmin_wbgt")
write_fst(df_psu_tmin_wbgt, path = here(path_processed, "1.2.1.d_df_psu_tmin_wbgt.fst"))
rm(df_psu_tmin_wbgt)
print("finished Step-1d: tmin-wbgt")

## Tmin - dry bulb ----
df_psu_tmin_db_noaa <- merge_dhs_climate(path = here(path_tmin_db_noaa), 
  clim_var = "tmin_db_noaa")
write_fst(df_psu_tmin_db_noaa, path = here(path_processed, "1.2.1.e_df_psu_tmin_db_noaa.fst"))
rm(df_psu_tmin_db_noaa)
print("finished Step-1e: tmin-db-noaa")

## ## Tmin - dry bulb -ERA5 ----
df_psu_tmin_db_era5 <- merge_dhs_climate(path = here(path_tmin_db_era5), 
  clim_var = "tmin_db_era5")
write_fst(df_psu_tmin_db_era5, path = here(path_processed, "1.2.1.f_df_psu_tmin_db_era5.fst"))
rm(df_psu_tmin_db_era5)
print("finished Step-1f: tmin-db-era5")

# step-2: read individual datasets ----
rm(list = ls())
## read all saved datasets
### Tmax - WBGT
df_psu_tmax_wbgt <- read_fst(here(path_processed, "1.2.1.a_df_psu_tmax_wbgt.fst"), 
  columns = c("psu", "date", "tmax_wbgt"))
### Tmax - DB
df_psu_tmax_db_noaa <- read_fst(here(path_processed, "1.2.1.b_df_psu_tmax_db_noaa.fst"), 
  columns = c("psu", "date", "tmax_db_noaa"))

### Tmax - DB - ERA5
df_psu_tmax_db_era5 <- read_fst(here(path_processed, "1.2.1.c_df_psu_tmax_db_era5.fst"), 
  columns = c("psu", "date", "tmax_db_era5"))

### Tmin - WBGT
df_psu_tmin_wbgt <- read_fst(here(path_processed, "1.2.1.d_df_psu_tmin_wbgt.fst"), 
  columns = c("psu", "date", "tmin_wbgt"))

### Tmin - DB
df_psu_tmin_db_noaa <- read_fst(here(path_processed, "1.2.1.e_df_psu_tmin_db_noaa.fst"), 
  columns = c("psu", "date", "tmin_db_noaa"))

### Tmin - DB - ERA5
df_psu_tmin_db_era5 <- read_fst(here(path_processed, "1.2.1.f_df_psu_tmin_db_era5.fst"), 
  columns = c("psu", "date", "tmin_db_era5"))

# step-3: merge all datasets
df_list <- list(
  df_psu_tmax_wbgt,
  df_psu_tmax_db_noaa,
  df_psu_tmax_db_era5,
  df_psu_tmin_wbgt,
  df_psu_tmin_db_noaa,
  df_psu_tmin_db_era5
)

df_psu_clim_merged <- reduce(df_list, full_join, by = c("psu", "date"))
setDT(df_psu_clim_merged)

## convert psu to factor
df_psu_clim_merged <- df_psu_clim_merged[, psu := as.factor(psu)]
## arrange the data by psu and date
df_psu_clim_merged <- df_psu_clim_merged[order(psu, date)]

## check number of rows with and without missing values
df_psu_clim_merged |> 
  summarise(
    n_missing = sum(is.na(tmax_wbgt) | is.na(tmax_db_noaa) | is.na(tmax_db_era5) |
      is.na(tmin_wbgt) | is.na(tmin_db_noaa) | is.na(tmin_db_era5)),
    n_total = n()
  ) |> 
  mutate(n_missing = n_total - n_missing)

## remove missing values
df_psu_clim_merged <- df_psu_clim_merged[complete.cases(df_psu_clim_merged), ]

# save the merged dataset using data.table ----
df_psu_clim_merged |> write_fst(here(path_processed, "1.2.1.df_psu_clim_merged.fst"))
print("finished! merged all datasets")