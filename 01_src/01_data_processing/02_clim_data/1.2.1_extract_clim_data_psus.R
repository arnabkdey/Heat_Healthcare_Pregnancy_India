# -------------------------------------------------------------------------------
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
df_dhs_psu_geo_sf <- readRDS(here(path_processed, "1.1.2.a_df_dhs_psu_geo.rds"))
india_boundary_buf <- readRDS(here(path_processed, "1.1.2.b_ind_boundary_0_buf.rds"))

# Step-1: Run the function to extract climate data for each PSU ----
## Tmax - WBGT ----
df_psu_tmax_wbgt <- merge_dhs_climate(path = here(path_rmax_wbgt_raw), 
  clim_var = "max_temp_wb")
write_fst(df_psu_tmax_wbgt, path = here(path_processed, "1.2.1.a_df_psu_tmax_wb.fst"))
rm(df_psu_tmax_wbgt)
print("finished Step-1a: tmax-wbgt")

## Tmax - dry bulb ----
df_psu_tmax_db <- merge_dhs_climate(path = here(path_tmax__db_raw), 
  clim_var = "max_temp")
write_fst(df_psu_tmax, path = here(path_processed, "1.2.1.b_df_psu_tmax_db.fst"))
rm(df_psu_tmax)
print("finished Step-1b: tmax")

## Tmin - WBGT ----
df_psu_tmin_wbgt <- merge_dhs_climate(path = here(path_tmin_wbgt_raw), 
  clim_var = "min_temp_wbgt")
write_fst(df_psu_tmin_wbgt, path = here(path_processed, "1.2.1.c_df_psu_tmin_wbgt.fst"))
rm(df_psu_tmin_wbgt)
print("finished Step-1c: tmin-wbgt")

## Tmin - dry bulb ----
df_psu_tmin_db <- merge_dhs_climate(path = here(path_tmin_db_raw), 
  clim_var = "min_temp_db")
write_fst(df_psu_tmin_db, path = here(path_processed, "1.2.1.d_df_psu_tmin_db.fst"))
rm(df_psu_tmin_db)
print("finished Step-1d: tmin-db")

# step-2: merge all the datasets ----
rm(list = ls())
## read all saved datasets
### Tmax - WBGT
df_psu_tmax_wb <- read_fst(here(path_processed, "1.2.1.a_df_psu_tmax_wb.fst"), 
  columns = c("psu", "date", "max_temp_wb"))
### Tmax - DB
df_psu_tmax_db <- read_fst(here(path_processed, "1.2.1.b_df_psu_tmax_db.fst"), 
  columns = c("psu", "date", "max_temp_db"))
### Tmin - WBGT
df_psu_tmin_wbgt <- read_fst(here(path_processed, "1.2.1.c_df_psu_tmin_wbgt.fst"), 
  columns = c("psu", "date", "min_temp_wbgt"))
### Tmin - DB
df_psu_tmin_db <- read_fst(here(path_processed, "1.2.1.d_df_psu_tmin_db.fst"), 
  columns = c("psu", "date", "min_temp_db"))

## merge all datasets
df_list <- list(df_psu_tmax_wb, df_psu_tmax_db, df_psu_tmin_wbgt, df_psu_tmin_db)
df_psu_clim_merged <- reduce(df_list, full_join, by = c("psu", "date"))

# process the merged dataset ----
setDT(df_psu_clim_merged)
## convert psu to factor
df_psu_clim_merged <- df_psu_clim_merged[, psu := as.factor(psu)]
## remove missing values
df_psu_clim_merged <- df_psu_clim_merged[complete.cases(df_psu_clim_merged), ]
## arrange the data by psu and date
df_psu_clim_merged <- df_psu_clim_merged[order(psu, date)]

# save the merged dataset ----
df_psu_clim_merged |> write_fst(here(path_processed, "1.2.1.df_psu_clim_merged.fst"))
print("finished! merged all datasets")

