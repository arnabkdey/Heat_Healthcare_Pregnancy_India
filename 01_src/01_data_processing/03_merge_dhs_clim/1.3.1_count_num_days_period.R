# --------------------
# @project: Heat and healthcare contact during pregnancy in India
# @author: Arnab K. Dey,  arnabxdey@gmail.com 
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script creates heat wave and temperature exceedance variables using absolute temperature thresholds and consecutive day criteria.
# @date: Dec 12, 2024

rm(list = ls())
pacman::p_load(dplyr, janitor, data.table, fst, here, tictoc)

# set paths ----
source("paths_mac.R")

# source functions ----
source(here("01_src", "01_data_processing", "utils", 
  "func_count_num_days_thresh_period.R"))

# load datasets ----
## temperature data with long-term cutoffs 
df_temp_cutoffs_2014 <- 
  read_fst(here(path_processed, 
    "1.2.2.e.df_psu_clim_merged_tmax_tmin_cutoffs_2014.fst"), 
  as.data.table = T)

## DHS IR dataset 
df_IR_6mo <- readRDS(here(path_processed, "1.1.3_DHS_IR_vars_created.rds"))

# data processing ----
## temperature dataset ----
### rename columns in the temperature dataset to standardize 
colnames(df_temp_cutoffs_2014) <- c(
  "psu", "date", "tmax_wbgt", "tmax_db", "tmin_wbgt", "tmin_db", 
  ## Tmax-WBGT
  "cutoff_tmax_wbgt_perc_800", "cutoff_tmax_wbgt_perc_825", "cutoff_tmax_wbgt_perc_850", 
  "cutoff_tmax_wbgt_perc_875", "cutoff_tmax_wbgt_perc_900", "cutoff_tmax_wbgt_perc_925", 
  "cutoff_tmax_wbgt_perc_950", "cutoff_tmax_db_perc_800", "cutoff_tmax_db_perc_825", 
  ## Tmax-DB
  "cutoff_tmax_db_perc_850", "cutoff_tmax_db_perc_875", "cutoff_tmax_db_perc_900", 
  "cutoff_tmax_db_perc_925", "cutoff_tmax_db_perc_950", 
  ## Tmin-WBGT
  "cutoff_tmin_wbgt_perc_50", "cutoff_tmin_wbgt_perc_75", "cutoff_tmin_wbgt_perc_100", 
  "cutoff_tmin_wbgt_perc_125", "cutoff_tmin_wbgt_perc_150", "cutoff_tmin_wbgt_perc_175", 
  "cutoff_tmin_wbgt_perc_200", 
  ## Tmin-DB
  "cutoff_tmin_db_perc_50", "cutoff_tmin_db_perc_75", "cutoff_tmin_db_perc_100", 
  "cutoff_tmin_db_perc_125", "cutoff_tmin_db_perc_150", "cutoff_tmin_db_perc_175", 
  "cutoff_tmin_db_perc_200")

### create cutoff vars for absolute temperatures
df_temp_cutoffs_2014 <- df_temp_cutoffs_2014 |> 
  ## Tmax-WBGT
  mutate(
    cutoff_tmax_wbgt_abs_28 = 28,
    cutoff_tmax_wbgt_abs_30 = 30,
    cutoff_tmax_wbgt_abs_32 = 32
  ) |>
  ## Tmax-DB
  mutate(
    cutoff_tmax_db_abs_30 = 30,
    cutoff_tmax_db_abs_32 = 32,
    cutoff_tmax_db_abs_34 = 34
  ) |>
  ## Tmin-WBGT
  mutate(
    cutoff_tmin_wbgt_abs_0 = 0,
    cutoff_tmin_wbgt_abs_2 = 2,
    cutoff_tmin_wbgt_abs_4 = 4
  ) |>
  ## Tmin-DB
  mutate(
    cutoff_tmin_db_abs_6 = 6,
    cutoff_tmin_db_abs_8 = 8,
    cutoff_tmin_db_abs_10 = 10
  )

## health dataset ----
### create a minimal health dataset
df_IR_short <- df_IR_6mo |> select(caseid, psu = meta_psu, doi) 

# count num_days when temp was above/below cutoffs over 90 days from date of interview ---
## Tmax WBGT ----
### vector of cutoff columns
vec_cutoff_tmax_wbgt <- c(
  "cutoff_tmax_wbgt_perc_800", "cutoff_tmax_wbgt_perc_825", "cutoff_tmax_wbgt_perc_850",
  "cutoff_tmax_wbgt_perc_875", "cutoff_tmax_wbgt_perc_900", "cutoff_tmax_wbgt_perc_925",
  "cutoff_tmax_wbgt_perc_950", "cutoff_tmax_wbgt_abs_28", "cutoff_tmax_wbgt_abs_30",
  "cutoff_tmax_wbgt_abs_32")

### Run the function to count days
tic()
df_IR_tmax_wbgt <- count_temp_exposure(
  df_health = df_IR_short, 
  df_temp = df_temp_cutoffs_2014, 
  ref_temp = "tmax_wbgt", 
  vec_cutoff = vec_cutoff_tmax_wbgt, 
  lookback_days = 90, 
  date_col = "doi", 
  psu_col = "psu",
  comparison = "greater"
)
toc()
print("finished tmax_wbgt")
### save output
df_IR_tmax_wbgt |> write.fst(here(path_processed, "1.3.1.a.df_IR_temp_counts_tmax_wbgt.fst"))
rm(df_IR_tmax_wbgt)

## Tmax DB ----
### vector of cutoff columns
vec_cutoff_tmax_db <- c(
  "cutoff_tmax_db_perc_800", "cutoff_tmax_db_perc_825", "cutoff_tmax_db_perc_850",
  "cutoff_tmax_db_perc_875", "cutoff_tmax_db_perc_900", "cutoff_tmax_db_perc_925",
  "cutoff_tmax_db_perc_950", "cutoff_tmax_db_abs_30", "cutoff_tmax_db_abs_32",
  "cutoff_tmax_db_abs_34")

## Run the function to count days
tic()
df_IR_tmax_db <- count_temp_exposure(
  df_health = df_IR_short, 
  df_temp = df_temp_cutoffs_2014, 
  ref_temp = "tmax_db", 
  vec_cutoff = vec_cutoff_tmax_db, 
  lookback_days = 90, 
  date_col = "doi", 
  psu_col = "psu",
  comparison = "greater"
)
toc()
print("finished tmax_db")
### save output
df_IR_tmax_db |> write.fst(here(path_processed, "1.3.1.b.df_IR_temp_counts_tmax_db.fst"))
rm(df_IR_tmax_db)

## Tmin WBGT ----
### vector of cutoff columns
vec_cutoff_tmin_wbgt <- c(
  "cutoff_tmin_wbgt_perc_50", "cutoff_tmin_wbgt_perc_75", "cutoff_tmin_wbgt_perc_100",
  "cutoff_tmin_wbgt_perc_125", "cutoff_tmin_wbgt_perc_150", "cutoff_tmin_wbgt_perc_175",
  "cutoff_tmin_wbgt_perc_200", 
  "cutoff_tmin_wbgt_abs_0", "cutoff_tmin_wbgt_abs_2", "cutoff_tmin_wbgt_abs_4")

## Run the function to count days 
tic()
df_IR_tmin_wbgt <- count_temp_exposure(
  df_health = df_IR_short, 
  df_temp = df_temp_cutoffs_2014, 
  # ref_temp = ref_temp_tmax_wbgt, 
  ref_temp = "tmin_wbgt", 
  vec_cutoff = vec_cutoff_tmin_wbgt, 
  lookback_days = 90, 
  date_col = "doi", 
  psu_col = "psu",
  comparison = "less"
)
toc()
print("finished tmin_wbgt")

### save output
df_IR_tmin_wbgt |> write.fst(here(path_processed, "1.3.1.c.df_IR_temp_counts_tmin_wbgt.fst"))
rm(df_IR_tmin_wbgt)

## Tmin DB ----
### vector of cutoff columns
vec_cutoff_tmin_db <- c(
  "cutoff_tmin_db_perc_50", "cutoff_tmin_db_perc_75", "cutoff_tmin_db_perc_100",
  "cutoff_tmin_db_perc_125", "cutoff_tmin_db_perc_150", "cutoff_tmin_db_perc_175",
  "cutoff_tmin_db_perc_200", 
  "cutoff_tmin_db_abs_6", "cutoff_tmin_db_abs_8", "cutoff_tmin_db_abs_10")

### Run the function to count days
tic()
df_IR_tmin_db <- count_temp_exposure(
  df_health = df_IR_short, 
  df_temp = df_temp_cutoffs_2014, 
  ref_temp = "tmin_db", 
  vec_cutoff = vec_cutoff_tmin_db, 
  lookback_days = 90, 
  date_col = "doi", 
  psu_col = "psu",
  comparison = "less"
)
toc()
print("finished tmin_db")

### save output
df_IR_tmin_db |> write.fst(here(path_processed, "1.3.1.d.df_IR_temp_counts_tmin_db.fst"))
rm(df_IR_tmin_db)

