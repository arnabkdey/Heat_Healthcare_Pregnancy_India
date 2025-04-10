# --------------------
# @project: Heat and healthcare contact during pregnancy in India
# @author: Arnab K. Dey,  arnabxdey@gmail.com 
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script creates heat wave and temperature exceedance variables using absolute temperature thresholds and consecutive day criteria.
# @date: Dec 12, 2024

rm(list = ls())
pacman::p_load(dplyr, janitor, data.table, fst, here, tictoc)

# set paths ----
source("paths.R")

# source functions ----
source(here("01_src", "01_data_processing", "utils", 
  "func_count_num_days_thresh_period.R"))

# load DHS datasets ----
df_IR_6mo <- readRDS(here(path_processed, "1.1.3_DHS_IR_vars_created.rds"))

## create a minimal health dataset
df_IR_short <- df_IR_6mo |> select(caseid, psu = meta_psu, doi) 

# count num_days when temp was above/below cutoffs over 90 days from date of interview ---
## Tmax ----
### WBGT ----

#### load data
df_tmax_wbgt_lt_2014 <- read_fst(here(path_processed, "1.2.2.a2_df_psu_tmax_wbgt_lt_2014.fst"), as.data.table = TRUE)

#### vector of cutoff columns
vec_cutoff_tmax_wbgt <- dput(df_tmax_wbgt_lt_2014 |> select(starts_with("cutoff")) |> colnames())

#### Run the function to count days
tic()
df_IR_tmax_wbgt <- count_temp_exposure(
  df_health = df_IR_short, 
  df_temp = df_tmax_wbgt_lt_2014, 
  ref_temp = "tmax_wbgt", 
  vec_cutoff = vec_cutoff_tmax_wbgt, 
  lookback_days = 90, 
  date_col = "doi", 
  psu_col = "psu",
  comparison = "greater"
)
toc()
print("finished tmax_wbgt")

#### save output
df_IR_tmax_wbgt |> write.fst(here(path_processed, "1.3.1.a.df_IR_temp_counts_tmax_wbgt.fst"))
rm(df_temp_cutoffs_2014 df_IR_tmax_wbgt)

### Tmax DB - NOAA ----
#### load data
df_tmax_db_noaa_lt_2014 <- read_fst(here(path_processed, "1.2.2.b2_df_psu_tmax_db_noaa_lt_2014.fst"), as.data.table = TRUE)

#### vector of cutoff columns
vec_cutoff_tmax_db_noaa <-  dput(df_tmax_db_noaa_lt_2014 |> select(starts_with("cutoff")) |> colnames())

#### Run the function to count days
tic()
df_IR_tmax_db_noaa <- count_temp_exposure(
  df_health = df_IR_short, 
  df_temp = df_tmax_db_noaa_lt_2014, 
  ref_temp = "tmax_db_noaa", 
  vec_cutoff = vec_cutoff_tmax_db_noaa, 
  lookback_days = 90, 
  date_col = "doi", 
  psu_col = "psu",
  comparison = "greater"
)
toc()
print("finished tmax_db_noaa")

#### save output
df_IR_tmax_db_noaa |> write.fst(here(path_processed, "1.3.1.b.df_IR_temp_counts_tmax_db_noaa.fst"))
rm(df_tmax_db_noaa_lt_2014, df_IR_tmax_db_noaa)

### Tmax DB - ERA5 ----

#### load data
df_tmax_db_era5_lt_2014 <- read_fst(here(path_processed, "1.2.2.c2_df_psu_tmax_db_era5_lt_2014.fst"), as.data.table = TRUE)

#### vector of cutoff columns
vec_cutoff_tmax_db_era5 <- dput(df_tmax_db_era5_lt_2014 |> select(starts_with("cutoff")) |> colnames())

#### Run the function to count days
tic()
df_IR_tmax_db_era5 <- count_temp_exposure(
  df_health = df_IR_short, 
  df_temp = df_tmax_db_era5_lt_2014, 
  ref_temp = "tmax_db_era5", 
  vec_cutoff = vec_cutoff_tmax_db_era5, 
  lookback_days = 90, 
  date_col = "doi", 
  psu_col = "psu",
  comparison = "greater"
)

toc()
print("finished tmax_db_era5")

#### save output
df_IR_tmax_db_era5 |> write.fst(here(path_processed, "1.3.1.c.df_IR_temp_counts_tmax_db_era5.fst"))
rm(df_tmax_db_era5_lt_2014, df_IR_tmax_db_era5)

## Tmin ----
### WBGT ----
#### load data
df_tmin_wbgt_lt_2014 <- read_fst(here(path_processed, "1.2.2.d2_df_psu_tmin_wbgt_lt_2014.fst"), as.data.table = TRUE)

#### vector of cutoff columns
vec_cutoff_tmin_wbgt <- dput(df_tmin_wbgt_lt_2014 |> select(starts_with("cutoff")) |> colnames())

#### Run the function to count days
tic()
df_IR_tmin_wbgt <- count_temp_exposure(
  df_health = df_IR_short, 
  df_temp = df_tmin_wbgt_lt_2014, 
  ref_temp = "tmin_wbgt", 
  vec_cutoff = vec_cutoff_tmin_wbgt, 
  lookback_days = 90, 
  date_col = "doi", 
  psu_col = "psu",
  comparison = "less"
)
toc()

print("finished tmin_wbgt")

#### save output
df_IR_tmin_wbgt |> write.fst(here(path_processed, "1.3.1.d.df_IR_temp_counts_tmin_wbgt.fst"))
rm(df_tmin_wbgt_lt_2014, df_IR_tmin_wbgt)

### Tmin DB - NOAA ----
#### load data
df_tmin_db_noaa_lt_2014 <- read_fst(here(path_processed, "1.2.2.e2_df_psu_tmin_db_noaa_lt_2014.fst"), as.data.table = TRUE)

#### vector of cutoff columns
vec_cutoff_tmin_db_noaa <- dput(df_tmin_db_noaa_lt_2014 |> select(starts_with("cutoff")) |> colnames())

#### Run the function to count days
tic()
df_IR_tmin_db_noaa <- count_temp_exposure(
  df_health = df_IR_short, 
  df_temp = df_tmin_db_noaa_lt_2014, 
  ref_temp = "tmin_db_noaa", 
  vec_cutoff = vec_cutoff_tmin_db_noaa, 
  lookback_days = 90, 
  date_col = "doi", 
  psu_col = "psu",
  comparison = "less"
)

toc()
print("finished tmin_db_noaa")

#### save output
df_IR_tmin_db_noaa |> write.fst(here(path_processed, "1.3.1.e.df_IR_temp_counts_tmin_db_noaa.fst"))
rm(df_tmin_db_noaa_lt_2014, df_IR_tmin_db_noaa)

### Tmin DB - ERA5 ----
#### load data
df_tmin_db_era5_lt_2014 <- read_fst(here(path_processed, "1.2.2.f2_df_psu_tmin_db_era5_lt_2014.fst"), as.data.table = TRUE)

#### vector of cutoff columns
vec_cutoff_tmin_db_era5 <- dput(df_tmin_db_era5_lt_2014 |> select(starts_with("cutoff")) |> colnames())

#### Run the function to count days
tic()
df_IR_tmin_db_era5 <- count_temp_exposure(
  df_health = df_IR_short, 
  df_temp = df_tmin_db_era5_lt_2014, 
  ref_temp = "tmin_db_era5", 
  vec_cutoff = vec_cutoff_tmin_db_era5, 
  lookback_days = 90, 
  date_col = "doi", 
  psu_col = "psu",
  comparison = "less"
)
toc()

print("finished tmin_db_era5")

#### save output
df_IR_tmin_db_era5 |> write.fst(here(path_processed, "1.3.1.f.df_IR_temp_counts_tmin_db_era5.fst"))
rm(df_tmin_db_era5_lt_2014, df_IR_tmin_db_era5)