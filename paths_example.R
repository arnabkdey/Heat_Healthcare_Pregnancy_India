library(here)

# Paths for the project on the shared drive
path_project <- "path/to/your/project/directory" # Replace with your actual project path
path_processed <- here(path_project, "data/processed_data/")
path_output <- here(path_project, "outputs/")

# Paths for datasets from the common-datasets folder
root_dir <- "path/to/your/common-datasets/directory" # Replace with your actual common datasets path

## DHS datasets
path_dhs_india_2019 <- here(root_dir, "dhs-raw-datasets/India/2019-21/individual-recode/IAIR7EDT/")
path_dhs_india_2019_shp <- here(root_dir, "dhs-raw-datasets/India/2019-21/geographic-data/IAGE7AFL/")

## KG Beck Climate zones
path_beck <- here(root_dir, "climate-datasets/global/climate-region-maps/Beck_KG")

## Temperature
path_tmax_wbgt_raw <- here(root_dir, "climate-datasets/global/wbgt-brimicombe/world-temp-wbgt-max/")
path_tmin_wbgt_raw <- here(root_dir, "climate-datasets/global/wbgt-brimicombe/world-temp-wbgt-min/")
path_tmean_wbgt_raw <- here(root_dir, "climate-datasets/global/wbgt-brimicombe/world-temp-wbgt-mean/")
path_tmax_db_noaa <- here(root_dir, "climate-datasets/global/air-temperature-noaa/world-temp-drybulb-max/1979-2023/")
path_tmin_db_noaa <- here(root_dir, "climate-datasets/global/air-temperature-noaa/world-temp-drybulb-min/1979-2023/")
path_tmax_db_era5 <- here(root_dir, "climate-datasets/global/air-temperature-era5/tmax/")
path_tmin_db_era5 <- here(root_dir, "climate-datasets/global/air-temperature-era5/tmin/")
path_tmean_db_era5 <- here(root_dir, "climate-datasets/global/air-temperature-era5/tmean/")
