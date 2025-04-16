# --------------------
# @project: Heat and healthcare contact during pregnancy in India
# @author: Arnab K. Dey,  arnabxdey@gmail.com 
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script creates variables to count number of days in percentile bins of daily temperature data.
# @date: Apr, 2025

rm(list = ls())
pacman::p_load(dplyr, janitor, data.table, fst, here, tictoc)

# set paths ----
source("paths.R")

# source functions ----
source(here("01_src", "01_data_processing", "utils", 
  "func_count_num_days_bins.R"))

# load data  ----
## DHS data ----
df_IR_6mo <- readRDS(here(path_processed, "1.1.3_DHS_IR_vars_created.rds"))

### create a minimal health dataset
df_IR_short <- df_IR_6mo |> select(caseid, psu = meta_psu, doi) |>
  mutate(start_date = doi - 90, end_date = doi)  
glimpse(df_IR_short)

## LT cutoffs data ----
###  Tmax - WBGT ----
df_tmax_wbgt_lt <- fst::read_fst(here(path_processed, "1.2.3.a_df_psu_tmax_wbgt_lt.fst"), 
  as.data.table = TRUE)

#### drop columns that are not needed
df_tmax_wbgt_lt <- df_tmax_wbgt_lt |> select(-starts_with("days"))

#### identify psus with missing data
df_tmax_wbgt_lt |> filter(is.na(cutoff_tmax_wbgt_100)) |> pull(psu) |> unique()

#### drop psus with missing data
df_tmax_wbgt_lt <- df_tmax_wbgt_lt |> filter(!is.na(cutoff_tmax_wbgt_100)) 
sum(is.na(df_tmax_wbgt_lt))

###  Tmax - ERA5 ----
df_tmax_era5_lt <- fst::read_fst(here(path_processed, "1.2.3.c_df_psu_tmax_db_era5_lt.fst"), 
  as.data.table = TRUE)

#### drop columns that are not needed
df_tmax_era5_lt <- df_tmax_era5_lt |> select(-starts_with("days"))

#### identify psus with missing data
df_tmax_era5_lt |> filter(is.na(cutoff_tmax_db_era5_100)) |> pull(psu) |> unique()

#### drop psus with missing data
df_tmax_era5_lt <- df_tmax_era5_lt |> filter(!is.na(cutoff_tmax_db_era5_100)) 
sum(is.na(df_tmax_era5_lt))

### Tmin - WBGT ----
df_tmin_wbgt_lt <- fst::read_fst(here(path_processed, "1.2.3.d_df_psu_tmin_wbgt_lt.fst"), 
  as.data.table = TRUE)

#### drop columns that are not needed
df_tmin_wbgt_lt <- df_tmin_wbgt_lt |> select(-starts_with("days"))

#### identify psus with missing data
df_tmin_wbgt_lt |> filter(is.na(cutoff_tmin_wbgt_100)) |> pull(psu) |> unique()

#### drop psus with missing data
df_tmin_wbgt_lt <- df_tmin_wbgt_lt |> filter(!is.na(cutoff_tmin_wbgt_100)) 
sum(is.na(df_tmin_wbgt_lt))

### Tmin - ERA5 ----
df_tmin_era5_lt <- fst::read_fst(here(path_processed, "1.2.3.f_df_psu_tmin_db_era5_lt.fst"), 
  as.data.table = TRUE)

#### drop columns that are not needed
df_tmin_era5_lt <- df_tmin_era5_lt |> select(-starts_with("days"))

#### identify psus with missing data
df_tmin_era5_lt |> filter(is.na(cutoff_tmin_db_era5_100)) |> pull(psu) |> unique()

#### drop psus with missing data
df_tmin_era5_lt <- df_tmin_era5_lt |> filter(!is.na(cutoff_tmin_db_era5_100)) 
sum(is.na(df_tmin_era5_lt))

# merge dhs and clim data ----
## Tmax - WBGT ----
df_dhs_tmax_wbgt_psu <- left_join(df_IR_short, df_tmax_wbgt_lt, by = c("psu"))

nrow(df_IR_short) # 8178
nrow(df_dhs_tmax_wbgt_psu) # 8178

### identify psus with missing data
psus_missing <- df_dhs_tmax_wbgt_psu |> filter(is.na(cutoff_tmax_wbgt_100)) |> pull(psu) |> unique()

### check if the psus with missing data are also missing in the tmax_wbgt_lt data
df_tmax_wbgt_lt |> filter(psu %in% psus_missing)

### drop the psus with missing data
df_dhs_tmax_wbgt_psu <- df_dhs_tmax_wbgt_psu |> filter(!psu %in% psus_missing)
nrow(df_dhs_tmax_wbgt_psu) # 8151 

## Tmax - ERA5 ----
df_dhs_tmax_era5_psu <- left_join(df_IR_short, df_tmax_era5_lt, by = c("psu"))
nrow(df_IR_short) # 8178
nrow(df_dhs_tmax_era5_psu) # 8178

### identify psus with missing data
psus_missing <- df_dhs_tmax_era5_psu |> filter(is.na(cutoff_tmax_db_era5_100)) |> pull(psu) |> unique()

### check if the psus with missing data are also missing in the tmax_era5_lt data
df_tmax_era5_lt |> filter(psu %in% psus_missing)

### drop the psus with missing data
df_dhs_tmax_era5_psu <- df_dhs_tmax_era5_psu |> filter(!psu %in% psus_missing)
nrow(df_dhs_tmax_era5_psu) # 8151 

## Tmin - WBGT ----
df_dhs_tmin_wbgt_psu <- left_join(df_IR_short, df_tmin_wbgt_lt, by = c("psu"))
nrow(df_IR_short) # 8178
nrow(df_dhs_tmin_wbgt_psu) # 8178

### identify psus with missing data
psus_missing <- df_dhs_tmin_wbgt_psu |> filter(is.na(cutoff_tmin_wbgt_100)) |> pull(psu) |> unique()

### check if the psus with missing data are also missing in the tmin_wbgt_lt data
df_tmin_wbgt_lt |> filter(psu %in% psus_missing)

### drop the psus with missing data
df_dhs_tmin_wbgt_psu <- df_dhs_tmin_wbgt_psu |> filter(!psu %in% psus_missing)
nrow(df_dhs_tmin_wbgt_psu) # 8151 

## Tmin - ERA5 ----
df_dhs_tmin_era5_psu <- left_join(df_IR_short, df_tmin_era5_lt, by = c("psu"))
nrow(df_IR_short) # 8178
nrow(df_dhs_tmin_era5_psu) # 8178

### identify psus with missing data
psus_missing <- df_dhs_tmin_era5_psu |> filter(is.na(cutoff_tmin_db_era5_100)) |> pull(psu) |> unique()

### check if the psus with missing data are also missing in the tmin_era5_lt data
df_tmin_era5_lt |> filter(psu %in% psus_missing)

### drop the psus with missing data
df_dhs_tmin_era5_psu <- df_dhs_tmin_era5_psu |> filter(!psu %in% psus_missing)
nrow(df_dhs_tmin_era5_psu) # 8151 

# count number of days in bins ----
## Tmax - WBGT ----
### load daily tmax_wbgt data
df_tmax_wbgt_daily <- fst::read_fst(here(path_processed, "1.2.2.a2_df_psu_tmax_wbgt_lt_2014.fst"), 
  as.data.table = TRUE,
  columns = c("psu", "date", "tmax_wbgt"))

glimpse(df_tmax_wbgt_daily)

head(df_dhs_tmax_wbgt_psu)
head(df_tmax_wbgt_daily)

### count number of days in bins
print("Counting days in bins for Tmax WBGT...")
tic()
result_tmax_wbgt <- count_days_in_bins(
  df_dhs = df_dhs_tmax_wbgt_psu,
  df_daily = df_tmax_wbgt_daily,
  temp_var = "tmax_wbgt"
)
toc()
print(Sys.time())

### save the result
result_tmax_wbgt |> fst::write_fst(here(path_processed, "1.3.3.x1_df_bins_tmax_wbgt.fst"))
rm(result_tmax_wbgt)
print("Tmax WBGT bins saved.")

## Tmax - ERA5 ----
### load daily tmax_era5 data
df_tmax_era5_daily <- fst::read_fst(here(path_processed, "1.2.2.c2_df_psu_tmax_db_era5_lt_2014.fst"), 
  as.data.table = TRUE,
  columns = c("psu", "date", "tmax_db_era5"))

### count number of days in bins
tic()
result_tmax_era5 <- count_days_in_bins(
  df_dhs = df_dhs_tmax_era5_psu,
  df_daily = df_tmax_era5_daily,
  temp_var = "tmax_db_era5"
)
toc()
print(Sys.time())

### save the result
result_tmax_era5 |> fst::write_fst(here(path_processed, "1.3.3.x2_df_bins_tmax_era5.fst"))
rm(result_tmax_era5)
print("Tmax ERA5 bins saved.")

## Tmin - WBGT ----
### load daily tmin_wbgt data
df_tmin_wbgt_daily <- fst::read_fst(here(path_processed, "1.2.2.b2_df_psu_tmin_wbgt_lt_2014.fst"), 
  as.data.table = TRUE,
  columns = c("psu", "date", "tmin_wbgt"))

### count number of days in bins
tic()
result_tmin_wbgt <- count_days_in_bins(
  df_dhs = df_dhs_tmin_wbgt_psu,
  df_daily = df_tmin_wbgt_daily,
  temp_var = "tmin_wbgt"
)
toc()
print(Sys.time())

### save the result
result_tmin_wbgt |> fst::write_fst(here(path_processed, "1.3.3.x3_df_bins_tmin_wbgt.fst"))
rm(result_tmin_wbgt)
print("Tmin WBGT bins saved.")

## Tmin - ERA5 ----
### load daily tmin_era5 data
df_tmin_era5_daily <- fst::read_fst(here(path_processed, "1.2.2.d2_df_psu_tmin_db_era5_lt_2014.fst"),  
  as.data.table = TRUE,
  columns = c("psu", "date", "tmin_db_era5"))

### count number of days in bins
result_tmin_era5 <- count_days_in_bins( 
  df_dhs = df_dhs_tmin_era5_psu,
  df_daily = df_tmin_era5_daily,
  temp_var = "tmin_db_era5"
)
toc()
print(Sys.time())

### save the result
result_tmin_era5 |> fst::write_fst(here(path_processed, "1.3.3.x4_df_bins_tmin_era5.fst"))
rm(result_tmin_era5)
print("Tmin ERA5 bins saved.")

