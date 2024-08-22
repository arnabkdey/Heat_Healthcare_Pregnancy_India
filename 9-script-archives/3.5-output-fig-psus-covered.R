# title: "Link the daily temp data with IR data"
# This script reads the geocoded PSU data from DHS and extracts daily gridded climate data for the past n years for each PSU. 
# The result is a HUGE dataset where each PSU has n\*365 rows of data, where n is the number of years of climate data available.

# Load Packages ----
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive, here)
pacman::p_load(sf, sp, raster, terra, tidyterra, ncdf4, rnaturalearth)
rm(list = ls())

# Step-0: Point to folders that contain the raw WBGT and precip data ----
path_processed <- here("2-data", "2.2-processed-data")

# Step-1: Load-datasets ---- 
df_dhs_psu_geo_sf <- readRDS(here(path_processed, "1.2-a-df-dhs-psu-geo.rds"))



# Step-2: Create a plot for all PSUs covered
plot_psus <- ggplot(df_dhs_psu_geo_sf) + 
              geom_sf(aes(fill = psu), color = "brown", size = 0.005) + 
              theme_minimal() + 
              theme(legend.position = "none") + 
              # remove grid lines
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
              # remove x and y axis lines
              theme(axis.line = element_blank())

# Step-3: Save the plot
path_out <- here("3-outputs", "figures")
ggsave(here(path_out, "all_psus_covered.jpeg"), plot_psus, width = 8, height = 10, dpi = 600)
