# title: "Link the daily temp data with IR data"
# This script reads the geocoded PSU data from DHS and extracts daily gridded climate data for the past n years for each PSU. 
# The result is a HUGE dataset where each PSU has n\*365 rows of data, where n is the number of years of climate data available.

# Load Packages ----
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive, here)
pacman::p_load(sf, sp, raster, terra, tidyterra, ncdf4, rnaturalearth)
library(climExposuR)
rm(list = ls())
source("paths-win.R")

# Step-1: Load-datasets ---- 
df_dhs_psu_geo_sf <- readRDS(here(path_project, "processed-data", "1.2-a-df-dhs-psu-geo.rds"))
india_boundary_buf <- readRDS(here(path_project, "processed-data", "1.2-b-ind-boundary-0-buf.rds"))
# head(df_dhs_psu_geo_sf)

# Step-2: load-function to extract climate data to DHS PSUs ----
# source(here("1-scripts", "6.1-function-to-extract-climate-data-for-psus.R"))

# Step-3: Run the function to extract climate data for each PSU ----
## Tmax - WB ----
df_psu_tmax_wb <- merge_dhs_climate(path = path_tmax_wb_raw, clim_var = "max_temp_wb")
write_fst(df_psu_tmax_wb, path = here(path_project, "processed-data", "2.1.1-df_psu_wbgt.fst"))

### Create a shorter dataframe for 2014 onwards ----
setDT(df_psu_tmax_wb)
df_psu_wbgt_2014 <- df_psu_tmax_wb[date >= as.Date("2014-01-01")]
write.fst(df_psu_wbgt_2014, here(path_project, "processed-data", "2.1.1-df_psu_wbgt_2014.fst"))
rm(df_psu_tmax_wb)
rm(df_psu_wbgt_2014)

### Address missing wbgt data ----
df_missing <- df_psu_wbgt_2014 |> filter(is.na(max_temp_wb))
tabyl(df_missing, psu) # we see that some PSUs dont have temp data at all
# Drop these PSUs
psus_missing <- unique(df_psu_wbgt_2014 |> filter(is.na(max_temp_wb)) |> pull(psu))
df_psu_wbgt_2014 <- df_psu_wbgt_2014 |> filter(!psu %in% psus_missing)
sum(is.na(df_psu_wbgt_2014)) # no missing values

write.fst(df_psu_wbgt_2014, here(path_project, "processed-data", "2.1.1-df_psu_wbgt_2014-na-removed.fst"))
print("finished Step-3: tmax-wb")

## Precipitation
df_psu_precip <- merge_dhs_climate(path = path_precip_raw, clim_var = "mean_precip")
write_fst(df_psu_precip, path = here(path_project, "processed-data", "2.1.2-df_psu_precip.fst"))
rm(df_psu_precip)
print("finished Step-3: precip")

## Tmax - DB
df_psu_tmax_db <- merge_dhs_climate(path = path_tmax_db_raw, clim_var = "max_temp_db")
write_fst(df_psu_tmax_db, path = here(path_project, "processed-data", "2.1.3-df_psu_tmax.fst"))
rm(df_psu_tmax_db)

print("finished Step-3: tmax-db")