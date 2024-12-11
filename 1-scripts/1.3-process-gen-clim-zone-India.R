# -------------------------------------------------------------------------------
# @project: Heat and healthcare contact during pregnancy in India
# @author: Arnab K. Dey,  arnabxdey@gmail.com 
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script assigns climate zones to Indian districts using Beck's classification system and DHS spatial boundaries.
# @date: Dec 12, 2024

# Data source (Beck et al)
#https://www.nature.com/articles/sdata2018214/tables/3
#https://www.nature.com/articles/sdata2018214#data-records

# Load Libraries ---- 
rm(list= ls())
pacman::p_load(dplyr, janitor, data.table, fst, openxlsx, here, googledrive)
pacman::p_load(sf, sp, raster, terra, tidyterra, ncdf4, rnaturalearth)
pacman::p_load(smoothr, tiff, rasterVis, tmap, rdhs, googledrive)
library(parallel)
library(plyr)
source("paths-mac.R")


# Read datasets  ---------
## Read and rasterize Beck_KG files ----
file <- here::here(path_beck, "Beck_KG_V1_present_0p0083.tif")
KG.rd <- terra::rast(file)
plot(KG.rd)

## Spatial boundaries for India (DHS spatial repository)
## Create a df with districts and climate zones
### manually download zip files for India from https://spatialdata.dhsprogram.com/boundaries/
bord <- download_boundaries(surveyId = "IA2020DHS", method = "sf")
### Unlist the list
list2env(bord ,.GlobalEnv)

# plot(bord[[1]])
# plot(sdr_subnational_boundaries[1]) # State boundaries
# plot(sdr_subnational_boundaries2[1]) # District boundaries

# Extract climate zones for districts in India ----
## Ensure that both the raster and the spatial boundaries are in the same CRS
sdr_subnational_boundaries2 <- st_transform(sdr_subnational_boundaries2, crs(KG.rd))

## Convert sf object to SpatVector
india_districts <- vect(sdr_subnational_boundaries2)

## Extract climate zone information for each district
climate_zones <- extract(KG.rd, india_districts)
str(climate_zones)
head(climate_zones)
length(unique(climate_zones$ID))
nrow(sdr_subnational_boundaries2)
class(sdr_subnational_boundaries2)

# Toolbox
## New approach
### Step 1: Summarize climate zones for each district
climate_summary <-  plyr::ddply(climate_zones, .(ID),summarize,Beck_KG_V1_present_0p0083={
  tt <- table(Beck_KG_V1_present_0p0083)
  names(tt)[which.max(tt)]
})
head(climate_summary)

### Step 2: Prepare the district information
district_info <- sdr_subnational_boundaries2 %>%
  st_drop_geometry() %>%
  select(OTHREGNA, REGNAME, REGCODE)
# Add row numbers manually
district_info$row_number <- 1:nrow(district_info)

head(district_info)

# Step 3: Merge climate summary with district information
df_clim_zones <- district_info %>%
  left_join(climate_summary, by = c("row_number" = "ID")) |> 
  mutate(clim_code = Beck_KG_V1_present_0p0083)


# Assign labels to climate zones -- Detailed --------
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 1] <- "Af: Tropical, rainforest"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 2] <- "Am: Tropical, monsoon "
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 3] <- "Aw: Tropical, savannah, dry winter"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 4] <- "As: Tropical, savannah, dry summer"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 5] <- "BWh: Arid, desert, hot"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 6] <- "BWk: Arid, desert, cold"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 7] <- "BSh: Arid, steppe, hot"   
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 8] <- "BSk: Arid, steppe, cold"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 9] <- "Csa: Temperate, dry summer, hot summer"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 10] <- "Csb: Temperate, dry summer, warm summer"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 11] <- "Csc: Temperate, dry summer, cold summer"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 12] <- "Cwa: Temperate, dry winter, hot summer"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 13] <- "Cwb: Temperate, dry winter, warm summer"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 14] <- "Cwc: Temperate, dry winter, cold summer"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 15] <- "Cfa: Temperate, no dry season, hot summer"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 16] <- "Cfb: Temperate, no dry season, warm summer"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 17] <- "Cfc: Temperate, no dry season, cold summer"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 18] <- "Dsa: Cold, dry summer, hot summer"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 19] <- "Dsb: Cold, dry summer, warm summer"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 20] <- "Dsc: Cold, dry summer, cold summer"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 21] <- "Dsd: Cold, dry summer, very cold summer"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 22] <- "Dwa: Cold, dry winter, hot summer"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 23] <- "Dwb: Cold, dry winter, warm summer"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 24] <- "Dwc: Cold, dry winter, cold summer"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 25] <- "Dwd: Cold, dry winter, very cold summer"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 26] <- "Dfa: Cold, no dry season, hot summer"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 27] <- "Dfb: Cold, no dry season, warm summer"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 28] <- "Dfc: Cold, no dry season, cold summer"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 29] <- "Dfd: Cold, no dry season, very cold summer"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 30] <- "ET: Polar, tundra"
df_clim_zones$clim_zone_full[df_clim_zones$clim_code == 31] <- "EF: Polar, ice cap"


# Assign shorter climate code (from Anna's paper) ----
df_clim_zones <- df_clim_zones |>
  mutate(clim_zone_short = case_when(
    clim_code < 3 ~ "trop_wet",
    clim_code == 3 ~ "trop_wet_dry",
    clim_code == 4 ~ "arid",
    clim_code == 6 ~ "semi-Arid",
    clim_code == 8 | clim_code == 11 | clim_code == 12 | clim_code ==14 ~ "hum_sub-trop",
    clim_code > 18 ~ "mountain")) |> 
  mutate(monsoon_zone = case_when(
    clim_zone_short == "trop_wet" ~ "monsoon",
    clim_zone_short == "trop_wet_dry" ~ "monsoon",
    clim_zone_short == "hum_sub-trop" ~ "monsoon",
    TRUE ~ "non-monsoon"))


tabyl(df_clim_zones$clim_zone_short)
tabyl(df_clim_zones$monsoon_zone)
nrow(df_clim_zones)

# Step-11: Save dataset with districts and climate zones for India ----
## Check if the directory exists and create if not
path_processed_data <- here::here(path_project, "processed-data")

## Save the file
write_fst(df_clim_zones, path = here::here(path_processed_data, "1.3-india-dist-climate-zones.fst"))

