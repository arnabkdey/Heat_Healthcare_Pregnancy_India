# --------------------
# @project: Heat and healthcare contact during pregnancy in India
# @author: Arnab K. Dey
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script creates variables to count number of days in percentile bins of daily temperature data.
# @date: Apr, 2025

# Load libraries ----
pacman::p_load(dplyr, janitor, data.table, fst, here, tictoc)

# Set paths ----
source("paths.R")

# Source functions ----
source(here("01_src", "01_data_processing", "utils", 
  "func_count_num_days_bins.R"))

# Load data  ----
## DHS data ----
df_IR_4mo <- readRDS(here(path_processed, "1.1.3_DHS_IR_vars_created_4mo.rds"))
# df_IR_5mo <- readRDS(here(path_processed, "1.1.3_DHS_IR_vars_created_5mo.rds"))
# df_IR_6mo <- readRDS(here(path_processed, "1.1.3_DHS_IR_vars_created_6mo.rds"))

### create a minimal health dataset
df_IR_short <- df_IR_4mo |> select(caseid, psu = meta_psu, doi) |>
  mutate(start_date = doi - 90, end_date = doi)  
glimpse(df_IR_short)

## LT cutoffs data ----
### WBGT ----
#### Tmax
df_tmax_wbgt_lt <- fst::read_fst(here(path_processed, "1.2.2.a_df_psu_tmax_wbgt_lt.fst"), 
  as.data.table = TRUE)
colnames(df_tmax_wbgt_lt)

##### drop columns that are not needed
df_tmax_wbgt_lt <- df_tmax_wbgt_lt |> select(-starts_with("days"))

##### identify psus with missing data
df_tmax_wbgt_lt |> filter(is.na(cutoff_tmax_wbgt_100)) |> pull(psu) |> unique()

##### drop psus with missing data
df_tmax_wbgt_lt <- df_tmax_wbgt_lt |> filter(!is.na(cutoff_tmax_wbgt_100)) 
sum(is.na(df_tmax_wbgt_lt))

#### Tmin
df_tmin_wbgt_lt <- fst::read_fst(here(path_processed, "1.2.2.b_df_psu_tmin_wbgt_lt.fst"), 
  as.data.table = TRUE)

##### drop columns that are not needed
df_tmin_wbgt_lt <- df_tmin_wbgt_lt |> select(-starts_with("days"))

##### identify psus with missing data
df_tmin_wbgt_lt |> filter(is.na(cutoff_tmin_wbgt_100)) |> pull(psu) |> unique()

##### drop psus with missing data
df_tmin_wbgt_lt <- df_tmin_wbgt_lt |> filter(!is.na(cutoff_tmin_wbgt_100)) 
sum(is.na(df_tmin_wbgt_lt))

#### Tmean
df_tmean_wbgt_lt <- fst::read_fst(here(path_processed, "1.2.2.c_df_psu_tmean_wbgt_lt.fst"), 
  as.data.table = TRUE)

##### drop columns that are not needed
df_tmean_wbgt_lt <- df_tmean_wbgt_lt |> select(-starts_with("days"))

##### identify psus with missing data
df_tmean_wbgt_lt |> filter(is.na(cutoff_tmean_wbgt_100)) |> pull(psu) |> unique()

##### drop psus with missing data
df_tmean_wbgt_lt <- df_tmean_wbgt_lt |> filter(!is.na(cutoff_tmean_wbgt_100)) 
sum(is.na(df_tmean_wbgt_lt))

### ERA5 ----
#### Tmax
df_tmax_era5_lt <- fst::read_fst(here(path_processed, "1.2.2.d_df_psu_tmax_db_era5_lt.fst"), 
  as.data.table = TRUE)

##### drop columns that are not needed
df_tmax_era5_lt <- df_tmax_era5_lt |> select(-starts_with("days"))

##### identify psus with missing data
df_tmax_era5_lt |> filter(is.na(cutoff_tmax_db_era5_100)) |> pull(psu) |> unique()

##### drop psus with missing data
df_tmax_era5_lt <- df_tmax_era5_lt |> filter(!is.na(cutoff_tmax_db_era5_100)) 
sum(is.na(df_tmax_era5_lt))

#### Tmin
df_tmin_era5_lt <- fst::read_fst(here(path_processed, "1.2.2.e_df_psu_tmin_db_era5_lt.fst"), 
  as.data.table = TRUE)

##### drop columns that are not needed
df_tmin_era5_lt <- df_tmin_era5_lt |> select(-starts_with("days"))

##### identify psus with missing data
df_tmin_era5_lt |> filter(is.na(cutoff_tmin_db_era5_100)) |> pull(psu) |> unique()

##### drop psus with missing data
df_tmin_era5_lt <- df_tmin_era5_lt |> filter(!is.na(cutoff_tmin_db_era5_100)) 
sum(is.na(df_tmin_era5_lt))

#### Tmean
df_tmean_era5_lt <- fst::read_fst(here(path_processed, "1.2.2.f_df_psu_tmean_db_era5_lt.fst"), 
  as.data.table = TRUE)

##### drop columns that are not needed
df_tmean_era5_lt <- df_tmean_era5_lt |> select(-starts_with("days"))

##### identify psus with missing data
df_tmean_era5_lt |> filter(is.na(cutoff_tmean_db_era5_100)) |> pull(psu) |> unique()

##### drop psus with missing data
df_tmean_era5_lt <- df_tmean_era5_lt |> filter(!is.na(cutoff_tmean_db_era5_100)) 
sum(is.na(df_tmean_era5_lt))

### NOAA ----
#### Tmax
df_tmax_noaa_lt <- fst::read_fst(here(path_processed, "1.2.2.g_df_psu_tmax_db_noaa_lt.fst"), 
  as.data.table = TRUE)

##### drop columns that are not needed
df_tmax_noaa_lt <- df_tmax_noaa_lt |> select(-starts_with("days"))

##### identify psus with missing data
df_tmax_noaa_lt |> filter(is.na(cutoff_tmax_db_noaa_100)) |> pull(psu) |> unique()

##### drop psus with missing data
df_tmax_noaa_lt <- df_tmax_noaa_lt |> filter(!is.na(cutoff_tmax_db_noaa_100)) 
sum(is.na(df_tmax_noaa_lt))

#### Tmin
df_tmin_noaa_lt <- fst::read_fst(here(path_processed, "1.2.2.h_df_psu_tmin_db_noaa_lt.fst"), 
  as.data.table = TRUE)

##### drop columns that are not needed
df_tmin_noaa_lt <- df_tmin_noaa_lt |> select(-starts_with("days"))

##### identify psus with missing data
df_tmin_noaa_lt |> filter(is.na(cutoff_tmin_db_noaa_100)) |> pull(psu) |> unique()

##### drop psus with missing data
df_tmin_noaa_lt <- df_tmin_noaa_lt |> filter(!is.na(cutoff_tmin_db_noaa_100)) 
sum(is.na(df_tmin_noaa_lt))

### Heat Index - ERA5 ----
#### Tmax
df_tmax_hi_era5_lt <- fst::read_fst(here(path_processed, "1.2.2.i_df_psu_tmax_hi_era5_lt.fst"), 
  as.data.table = TRUE)

##### drop columns that are not needed
df_tmax_hi_era5_lt <- df_tmax_hi_era5_lt |> 
  select(-starts_with("days")) |>
  select(-contains("dew_era5")) |>
  select(-contains("db_era5"))

##### identify psus with missing data
df_tmax_hi_era5_lt |> filter(is.na(cutoff_tmax_hi_era5_100)) |> pull(psu) |> unique()

##### drop psus with missing data
df_tmax_hi_era5_lt <- df_tmax_hi_era5_lt |> filter(!is.na(cutoff_tmax_hi_era5_100)) 
sum(is.na(df_tmax_hi_era5_lt))

#### Tmin
df_tmin_hi_era5_lt <- fst::read_fst(here(path_processed, "1.2.2.j_df_psu_tmin_hi_era5_lt.fst"), 
  as.data.table = TRUE)

##### drop columns that are not needed
df_tmin_hi_era5_lt <- df_tmin_hi_era5_lt |> select(-starts_with("days"))

##### identify psus with missing data
df_tmin_hi_era5_lt |> filter(is.na(cutoff_tmin_hi_era5_100)) |> pull(psu) |> unique()

##### drop psus with missing data
df_tmin_hi_era5_lt <- df_tmin_hi_era5_lt |> filter(!is.na(cutoff_tmin_hi_era5_100)) 
sum(is.na(df_tmin_hi_era5_lt))

# Merge DHS and clim data ----
## WBGT ----
### Tmax
df_dhs_tmax_wbgt_psu <- left_join(df_IR_short, df_tmax_wbgt_lt, by = c("psu"))

nrow(df_IR_short) # 8178
nrow(df_dhs_tmax_wbgt_psu) # 8178

#### identify psus with missing data
psus_missing <- df_dhs_tmax_wbgt_psu |> filter(is.na(cutoff_tmax_wbgt_100)) |> pull(psu) |> unique()

#### check if the psus with missing data are also missing in the tmax_wbgt_lt data
df_tmax_wbgt_lt |> filter(psu %in% psus_missing)

#### drop the psus with missing data
df_dhs_tmax_wbgt_psu <- df_dhs_tmax_wbgt_psu |> filter(!psu %in% psus_missing)
nrow(df_dhs_tmax_wbgt_psu) # 8151 

#### save Tmax
df_dhs_tmax_wbgt_psu |> write_fst(here(path_processed, "1.3.1.a_df_dhs_tmax_wbgt_psu_4mo.fst"))
rm(df_tmax_wbgt_lt, df_dhs_tmax_wbgt_psu)
print("WBGT Tmax merged dataset saved.")

### Tmin
df_dhs_tmin_wbgt_psu <- left_join(df_IR_short, df_tmin_wbgt_lt, by = c("psu"))
nrow(df_IR_short) # 8178
nrow(df_dhs_tmin_wbgt_psu) # 8178

#### identify psus with missing data
psus_missing <- df_dhs_tmin_wbgt_psu |> filter(is.na(cutoff_tmin_wbgt_100)) |> pull(psu) |> unique()

#### check if the psus with missing data are also missing in the tmin_wbgt_lt data
df_tmin_wbgt_lt |> filter(psu %in% psus_missing)

#### drop the psus with missing data
df_dhs_tmin_wbgt_psu <- df_dhs_tmin_wbgt_psu |> filter(!psu %in% psus_missing)
nrow(df_dhs_tmin_wbgt_psu) # 8151 

#### save Tmin
df_dhs_tmin_wbgt_psu |> write_fst(here(path_processed, "1.3.1.b_df_dhs_tmin_wbgt_psu_4mo.fst"))
rm(df_tmin_wbgt_lt, df_dhs_tmin_wbgt_psu)
print("WBGT Tmin merged dataset saved.")

### Tmean
df_dhs_tmean_wbgt_psu <- left_join(df_IR_short, df_tmean_wbgt_lt, by = c("psu"))
nrow(df_IR_short) # 8178
nrow(df_dhs_tmean_wbgt_psu) # 8178

#### identify psus with missing data
psus_missing <- df_dhs_tmean_wbgt_psu |> filter(is.na(cutoff_tmean_wbgt_100)) |> pull(psu) |> unique()

#### check if the psus with missing data are also missing in the tmean_wbgt_lt data
df_tmean_wbgt_lt |> filter(psu %in% psus_missing)

#### drop the psus with missing data
df_dhs_tmean_wbgt_psu <- df_dhs_tmean_wbgt_psu |> filter(!psu %in% psus_missing)
nrow(df_dhs_tmean_wbgt_psu) # 8151 

#### save Tmean
df_dhs_tmean_wbgt_psu |> write_fst(here(path_processed, "1.3.1.c_df_dhs_tmean_wbgt_psu_4mo.fst"))
rm(df_tmean_wbgt_lt, df_dhs_tmean_wbgt_psu)
print("WBGT Tmean merged dataset saved.")

## ERA5 ----
### Tmax
df_dhs_tmax_era5_psu <- left_join(df_IR_short, df_tmax_era5_lt, by = c("psu"))
nrow(df_IR_short) # 8178
nrow(df_dhs_tmax_era5_psu) # 8178

#### identify psus with missing data
psus_missing <- df_dhs_tmax_era5_psu |> filter(is.na(cutoff_tmax_db_era5_100)) |> pull(psu) |> unique()

#### check if the psus with missing data are also missing in the tmax_era5_lt data
df_tmax_era5_lt |> filter(psu %in% psus_missing)

#### drop the psus with missing data
df_dhs_tmax_era5_psu <- df_dhs_tmax_era5_psu |> filter(!psu %in% psus_missing)
nrow(df_dhs_tmax_era5_psu) # 8151 

#### save Tmax
df_dhs_tmax_era5_psu |> write_fst(here(path_processed, "1.3.1.d_df_dhs_tmax_db_era5_psu_4mo.fst"))
rm(df_tmax_era5_lt, df_dhs_tmax_era5_psu)
print("ERA5 Tmax merged dataset saved.")

### Tmin
df_dhs_tmin_era5_psu <- left_join(df_IR_short, df_tmin_era5_lt, by = c("psu"))
nrow(df_IR_short) # 8178
nrow(df_dhs_tmin_era5_psu) # 8178

#### identify psus with missing data
psus_missing <- df_dhs_tmin_era5_psu |> filter(is.na(cutoff_tmin_db_era5_100)) |> pull(psu) |> unique()

#### check if the psus with missing data are also missing in the tmin_era5_lt data
df_tmin_era5_lt |> filter(psu %in% psus_missing)

#### drop the psus with missing data
df_dhs_tmin_era5_psu <- df_dhs_tmin_era5_psu |> filter(!psu %in% psus_missing)
nrow(df_dhs_tmin_era5_psu) # 8151 

#### save Tmin
df_dhs_tmin_era5_psu |> write_fst(here(path_processed, "1.3.1.e_df_dhs_tmin_db_era5_psu_4mo.fst"))
rm(df_tmin_era5_lt, df_dhs_tmin_era5_psu)
print("ERA5 Tmin merged dataset saved.")

### Tmean
df_dhs_tmean_era5_psu <- left_join(df_IR_short, df_tmean_era5_lt, by = c("psu"))
nrow(df_IR_short) # 8178
nrow(df_dhs_tmean_era5_psu) # 8178

#### identify psus with missing data
psus_missing <- df_dhs_tmean_era5_psu |> filter(is.na(cutoff_tmean_db_era5_100)) |> pull(psu) |> unique()

#### check if the psus with missing data are also missing in the tmean_era5_lt data
df_tmean_era5_lt |> filter(psu %in% psus_missing)

#### drop the psus with missing data
df_dhs_tmean_era5_psu <- df_dhs_tmean_era5_psu |> filter(!psu %in% psus_missing)
nrow(df_dhs_tmean_era5_psu) # 8151 

#### save Tmean
df_dhs_tmean_era5_psu |> write_fst(here(path_processed, "1.3.1.f_df_dhs_tmean_db_era5_psu_4mo.fst"))
rm(df_tmean_era5_lt, df_dhs_tmean_era5_psu)
print("ERA5 Tmean merged dataset saved.")

## NOAA ----
### Tmax
df_dhs_tmax_noaa_psu <- left_join(df_IR_short, df_tmax_noaa_lt, by = c("psu"))
nrow(df_IR_short) # 8178
nrow(df_dhs_tmax_noaa_psu) # 8178

#### identify psus with missing data
psus_missing <- df_dhs_tmax_noaa_psu |> filter(is.na(cutoff_tmax_db_noaa_100)) |> pull(psu) |> unique()

#### check if the psus with missing data are also missing in the tmax_noaa_lt data
df_tmax_noaa_lt |> filter(psu %in% psus_missing)

#### drop the psus with missing data
df_dhs_tmax_noaa_psu <- df_dhs_tmax_noaa_psu |> filter(!psu %in% psus_missing)
nrow(df_dhs_tmax_noaa_psu) # 8151 

#### save Tmax
df_dhs_tmax_noaa_psu |> write_fst(here(path_processed, "1.3.1.g_df_dhs_tmax_db_noaa_psu_4mo.fst"))
rm(df_tmax_noaa_lt, df_dhs_tmax_noaa_psu)
print("NOAA Tmax merged dataset saved.")

### Tmin
df_dhs_tmin_noaa_psu <- left_join(df_IR_short, df_tmin_noaa_lt, by = c("psu"))
nrow(df_IR_short) # 8178
nrow(df_dhs_tmin_noaa_psu) # 8178

#### identify psus with missing data
psus_missing <- df_dhs_tmin_noaa_psu |> filter(is.na(cutoff_tmin_db_noaa_100)) |> pull(psu) |> unique()

#### check if the psus with missing data are also missing in the tmin_noaa_lt data
df_tmin_noaa_lt |> filter(psu %in% psus_missing)

#### drop the psus with missing data
df_dhs_tmin_noaa_psu <- df_dhs_tmin_noaa_psu |> filter(!psu %in% psus_missing)
nrow(df_dhs_tmin_noaa_psu) # 8151 

#### save Tmin
df_dhs_tmin_noaa_psu |> write_fst(here(path_processed, "1.3.1.h_df_dhs_tmin_db_noaa_psu_4mo.fst"))
rm(df_tmin_noaa_lt, df_dhs_tmin_noaa_psu)
print("NOAA Tmin merged dataset saved.")


## Heat Index - ERA5 ----
### Tmax
df_dhs_tmax_hi_era5_psu <- left_join(df_IR_short, df_tmax_hi_era5_lt, by = c("psu"))
nrow(df_IR_short) # 8178
nrow(df_dhs_tmax_hi_era5_psu) # 8178

#### identify psus with missing data
psus_missing <- df_dhs_tmax_hi_era5_psu |> filter(is.na(cutoff_tmax_hi_era5_100)) |> pull(psu) |> unique()

#### check if the psus with missing data are also missing in the tmax_hi_era5_lt data
df_tmax_hi_era5_lt |> filter(psu %in% psus_missing)

#### drop the psus with missing data
df_dhs_tmax_hi_era5_psu <- df_dhs_tmax_hi_era5_psu |> filter(!psu %in% psus_missing)
nrow(df_dhs_tmax_hi_era5_psu) # 8151 

#### save Tmax
df_dhs_tmax_hi_era5_psu |> write_fst(here(path_processed, "1.3.1.i_df_dhs_tmax_hi_era5_psu_4mo.fst"))
rm(df_tmax_hi_era5_lt, df_dhs_tmax_hi_era5_psu)
print("ERA5 Tmax HI merged dataset saved.")

### Tmin
df_dhs_tmin_hi_era5_psu <- left_join(df_IR_short, df_tmin_hi_era5_lt, by = c("psu"))
nrow(df_IR_short) # 8178
nrow(df_dhs_tmin_hi_era5_psu) # 8178

#### identify psus with missing data
psus_missing <- df_dhs_tmin_hi_era5_psu |> filter(is.na(cutoff_tmin_hi_era5_100)) |> pull(psu) |> unique()

#### check if the psus with missing data are also missing in the tmin_hi_era5_lt data
df_tmin_hi_era5_lt |> filter(psu %in% psus_missing)

#### drop the psus with missing data
df_dhs_tmin_hi_era5_psu <- df_dhs_tmin_hi_era5_psu |> filter(!psu %in% psus_missing)
nrow(df_dhs_tmin_hi_era5_psu) # 8151 

#### save Tmin
df_dhs_tmin_hi_era5_psu |> write_fst(here(path_processed, "1.3.1.j_df_dhs_tmin_hi_era5_psu_4mo.fst"))
rm(df_tmin_hi_era5_lt, df_dhs_tmin_hi_era5_psu)
print("ERA5 Tmin HI merged dataset saved.")

