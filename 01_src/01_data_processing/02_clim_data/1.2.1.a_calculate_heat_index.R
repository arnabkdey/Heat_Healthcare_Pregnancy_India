# load libraries ----
rm(list = ls())
pacman::p_load(dplyr, janitor, data.table, fst, openxlsx, here)

# set paths ----
source(here("paths.R"))

# source functions ----
source(here("01_src", "01_data_processing", "utils", "func_calculate_heat_index.R"))

# load data ----
## Air temperature
### Max temperature
df_psu_tmax_era5 <- read_fst(here(
  path_processed, 
  "1.2.1.d_df_psu_tmax_db_era5.fst"), 
  as.data.table = TRUE)

### Min temperature
df_psu_tmin_era5 <- read_fst(here(
  path_processed, 
  "1.2.1.d_df_psu_tmin_db_era5.fst"), 
  as.data.table = TRUE)

## Dewpoint temperatures
### Max temperature
df_psu_tmax_dew_era5 <- read_fst(here(
  path_processed, 
  "1.2.1.i_df_psu_tmax_dew_era5.fst"), 
  as.data.table = TRUE)

### Min temperature
df_psu_tmin_dew_era5 <- read_fst(here(
  path_processed, 
  "1.2.1.j_df_psu_tmin_dew_era5.fst"), 
  as.data.table = TRUE)

# Merge datasets using inner join ----
## Toolbox
# df_psu_tmax_era5 <- df_psu_tmax_era5[date >= as.Date("1994-01-01") & date <= as.Date("1994-12-31"), ]
# df_psu_tmax_dew_era5 <- df_psu_tmax_dew_era5[date >= as.Date("1994-01-01") & date <= as.Date("1994-12-31"), ]

df_psu_tmax_ta_tdew <- merge(df_psu_tmax_era5, df_psu_tmax_dew_era5, by = c("psu", "date"), all.y = TRUE)
num_rows_before <- nrow(df_psu_tmax_ta_tdew)
df_psu_tmax_ta_tdew <- df_psu_tmax_ta_tdew[complete.cases(df_psu_tmax_ta_tdew), ]
num_rows_after <- nrow(df_psu_tmax_ta_tdew)
cat("Number of rows removed:", num_rows_before - num_rows_after)

df_psu_tmin_ta_tdew <- merge(df_psu_tmin_era5, df_psu_tmin_dew_era5, by = c("psu", "date"), all.y = TRUE)
num_rows_before <- nrow(df_psu_tmin_ta_tdew)
df_psu_tmin_ta_tdew <- df_psu_tmin_ta_tdew[complete.cases(df_psu_tmin_ta_tdew), ]
num_rows_after <- nrow(df_psu_tmin_ta_tdew)
cat("Number of rows removed:", num_rows_before - num_rows_after)

# calculate heat index ----
## Max temperature 
df_psu_tmax_ta_tdew <- calculate_heat_index(df_psu_tmax_ta_tdew, "tmax_db_era5", "tmax_dew_era5", 
                              air_temp_celsius = TRUE, 
                              dew_point_celsius = TRUE,
                              heat_index_celsius = TRUE,
                              heat_index_col = "tmax_hi_era5")

head(df_psu_tmax_ta_tdew)
glimpse(df_psu_tmax_ta_tdew)
sum(is.na(df_psu_tmax_ta_tdew$tmax_hi_era5))
mean(df_psu_tmax_ta_tdew$tmax_hi_era5)

## Min temperature
df_psu_tmin_ta_tdew <- calculate_heat_index(df_psu_tmin_ta_tdew, "tmin_db_era5", "tmin_dew_era5", 
                              air_temp_celsius = TRUE, 
                              dew_point_celsius = TRUE,
                              heat_index_celsius = TRUE,
                              heat_index_col = "tmin_hi_era5")

head(df_psu_tmin_ta_tdew)
glimpse(df_psu_tmin_ta_tdew)
sum(is.na(df_psu_tmin_ta_tdew$tmin_hi_era5))
mean(df_psu_tmin_ta_tdew$tmin_hi_era5)

# Save results ----
write_fst(df_psu_tmax_ta_tdew, here(path_processed, "1.2.1.a.1_df_psu_tmax_hi_era5.fst"))
write_fst(df_psu_tmin_ta_tdew, here(path_processed, "1.2.1.a.2_df_psu_tmin_hi_era5.fst"))
