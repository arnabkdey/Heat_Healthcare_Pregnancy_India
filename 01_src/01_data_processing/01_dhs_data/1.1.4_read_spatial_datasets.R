# --------------------
# @project: Heat and healthcare contact during pregnancy in India
# @author: Arnab K. Dey,  arnabxdey@gmail.com 
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script reads and processes DHS geocoded PSU data and creates buffered India boundary files for spatial analysis.
# @date: Dec 12, 2024

# load Packages -----
rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, here)
pacman::p_load(sf, sp, raster, terra, tidyterra, ncdf4, rnaturalearth)

# set paths ----
source("paths_mac.R")

# Read Geo Coded Datasets
## Step-1: Read geo-coded PSU data from DHS
### Load India shape file
df_dhs_geo_raw <- read_sf(here(path_dhs_india_2019_shp, "IAGE7AFL.shp"))

### Rename variables 
df_dhs_geo_raw <- df_dhs_geo_raw %>% 
  dplyr::select(psu = DHSCLUST, dist_name = DHSREGNA,
                lat = LATNUM, long = LONGNUM)
nrow(df_dhs_geo_raw)

### Filter missing values
df_dhs_psu_geo <- df_dhs_geo_raw %>%
  dplyr::filter(!is.na(lat)) %>% 
  dplyr::filter(!is.na(long)) %>% 
  dplyr::filter(lat!=0 | long!=0) %>%                  #LAT=0 and LONG=0 are missing coordinates  
  dplyr::filter(lat <= -0.00005 | long >= 0.00005)      #missing obs. - remove

nrow(df_dhs_psu_geo) #118 psus dropped

## Step-2: Get India Administrative Boundaries
### Load adm-1 for India
india_boundary <- ne_countries(scale = "medium", returnclass = "sf") %>% 
              filter(admin == "India")
# plot(st_geometry(india_boundary))

#### Add Buffer
india_boundary_buf <- st_buffer(india_boundary, dist = 50000)
# plot(st_geometry(india_boundary_buf))

# Save output ----
saveRDS(df_dhs_psu_geo, file = here(path_processed, "1.1.4.a-df-dhs-psu-geo.rds"))
saveRDS(india_boundary_buf, file = here(path_processed, "1.1.4.b-ind-boundary-0-buf.rds"))
