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
source("paths.R")

# load functions ----
source(here("01_src", "01_data_processing", "utils", "function_to_extract_clim_data_psus.R"))

# load-datasets ----
df_dhs_psu_geo_sf <- readRDS(here(path_processed, "1.1.4.a_df_dhs_psu_geo.rds"))
india_boundary_buf <- readRDS(here(path_processed, "1.1.4.b_ind_boundary_0_buf.rds"))

# Step-1: Run the function to extract climate data for each PSU ----
## Tmax - WBGT ----
df_psu_tmax_wbgt <- merge_dhs_climate(
  path = here(path_tmax_wbgt_raw),
  clim_var = "tmax_wbgt",
  from_index = 1, to_index = 42
)
### basic cleaning
setDT(df_psu_tmax_wbgt)
df_psu_tmax_wbgt <- df_psu_tmax_wbgt[
  , .(psu = as.factor(psu), date, tmax_wbgt)][
    order(psu, date)]

### save the data    
df_psu_tmax_wbgt |> write_fst(path = here(path_processed, "1.2.1.a_df_psu_tmax_wbgt.fst"))
rm(df_psu_tmax_wbgt)
print("finished Step-1a: tmax-wbgt")

## Tmax - dry bulb -NOAA ----
df_psu_tmax_db_noaa <- merge_dhs_climate(
  path = here(path_tmax_db_noaa),
  clim_var = "tmax_db_noaa",
  from_index = 1, to_index = 42
)

### basic cleaning
setDT(df_psu_tmax_db_noaa)
df_psu_tmax_db_noaa <- df_psu_tmax_db_noaa[
  , .(psu = as.factor(psu), date, tmax_db_noaa)][
    order(psu, date)]

#### save the data
df_psu_tmax_db_noaa |> write_fst(path = here(path_processed, "1.2.1.b_df_psu_tmax_db_noaa.fst"))
rm(df_psu_tmax_db_noaa)
print("finished Step-1b: tmax db - noaa")

## Tmax - dry bulb -ERA5 ----
df_psu_tmax_db_era5 <- merge_dhs_climate(
  path = here(path_tmax_db_era5),
  clim_var = "tmax_db_era5",
  from_index = 1, to_index = 42  
)
### basic cleaning
setDT(df_psu_tmax_db_era5)
df_psu_tmax_db_era5 <- df_psu_tmax_db_era5[
  , .(psu = as.factor(psu), date, tmax_db_era5 = tmax_db_era5 - 273.15)][
    order(psu, date)]

### save the data
df_psu_tmax_db_era5 |> write_fst(path = here(path_processed, "1.2.1.c_df_psu_tmax_db_era5.fst"))
rm(df_psu_tmax_db_era5)
print("finished Step-1c: tmax db - era5")

## Tmin - WBGT ----
df_psu_tmin_wbgt <- merge_dhs_climate(
  path = here(path_tmin_wbgt_raw),
  clim_var = "tmin_wbgt",
  from_index = 1, to_index = 42
)

### basic cleaning
setDT(df_psu_tmin_wbgt)
df_psu_tmin_wbgt <- df_psu_tmin_wbgt[
  , .(psu = as.factor(psu), date, tmin_wbgt)][
    order(psu, date)]

### save the data
df_psu_tmin_wbgt |> write_fst(path = here(
  path_processed, 
  "1.2.1.d_df_psu_tmin_wbgt.fst"))
rm(df_psu_tmin_wbgt)
print("finished Step-1d: tmin-wbgt")

## Tmin - dry bulb ----
df_psu_tmin_db_noaa <- merge_dhs_climate(
  path = here(path_tmin_db_noaa),
  clim_var = "tmin_db_noaa",
  from_index = 1, to_index = 42
)

### basic cleaning
setDT(df_psu_tmin_db_noaa)
df_psu_tmin_db_noaa <- df_psu_tmin_db_noaa[
  , .(psu = as.factor(psu), date, tmin_db_noaa)][
    order(psu, date)]

### save the data
df_psu_tmin_db_noaa |> write_fst(path = here(path_processed, 
  "1.2.1.e_df_psu_tmin_db_noaa.fst"))
rm(df_psu_tmin_db_noaa)
print("finished Step-1e: tmin-db-noaa")

## Tmin - dry bulb -ERA5 ----
df_psu_tmin_db_era5 <- merge_dhs_climate(
  path = here(path_tmin_db_era5),
  clim_var = "tmin_db_era5",
  from_index = 1, to_index = 42
)

### basic cleaning
setDT(df_psu_tmin_db_era5)
df_psu_tmin_db_era5 <- df_psu_tmin_db_era5[
  , .(psu = as.factor(psu), date, tmin_db_era5 = tmin_db_era5 - 273.15)][
    order(psu, date)]

### save the data
df_psu_tmin_db_era5 |> write_fst(path = here(
  path_processed,
  "1.2.1.f_df_psu_tmin_db_era5.fst"
))
rm(df_psu_tmin_db_era5)
print("finished Step-1f: tmin-db-era5")
