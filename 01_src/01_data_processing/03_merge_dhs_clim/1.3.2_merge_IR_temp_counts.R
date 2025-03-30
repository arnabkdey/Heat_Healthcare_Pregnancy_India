# load libraries ----
rm(list = ls())
pacman::p_load(dplyr, janitor, data.table, fst, openxlsx, here)

# set paths ----
source(here("paths_mac.R"))

# load data ----
## processed IR data with temp counts 
df_IR_tmax_wbgt <-  read.fst(here(path_processed, "1.3.1.a.df_IR_temp_counts_tmax_wbgt.fst"))
df_IR_tmax_db <-  read.fst(here(path_processed, "1.3.1.b.df_IR_temp_counts_tmax_db.fst"))
df_IR_tmin_wbgt <-  read.fst(here(path_processed, "1.3.1.c.df_IR_temp_counts_tmin_wbgt.fst"))
df_IR_tmin_db <-  read.fst(here(path_processed, "1.3.1.d.df_IR_temp_counts_tmin_db.fst"))

## IR data
df_IR_6mo <- readRDS(here(path_processed, "1.1.3_DHS_IR_vars_created.rds"))

# Merge IR data with temperature counts data ----
### First merge all temperature count datasets based on caseid, psu, doi, and start and end dates columns
df_temp_counts_merged <- df_IR_tmax_wbgt |>
  inner_join(df_IR_tmax_db, by = c("caseid", "psu", "doi", "start_date", "end_date")) |>
  inner_join(df_IR_tmin_wbgt, by = c("caseid", "psu", "doi", "start_date", "end_date")) |>
  inner_join(df_IR_tmin_db, by = c("caseid", "psu", "doi", "start_date", "end_date")) |>
  select(-c("start_date", "end_date"))

nrow(df_temp_counts_merged) # 8178

### filter missing data
df_temp_counts_merged <- df_temp_counts_merged |>
  filter(!is.na(days_cutoff_tmax_wbgt_perc_800_greater))

sum(is.na(df_temp_counts_merged))

nrow(df_temp_counts_merged) # 8065; 113 cases removed due to missing climate data for those PSU/date combinations

# Merge IR data with temperature counts data ----
### Merge IR data with temperature counts data
df_merged <- df_IR_6mo |>
  mutate(psu = meta_psu) |>
  inner_join(df_temp_counts_merged, by = c("caseid", "psu", "doi"))

# save data ----
df_merged |> saveRDS(here(path_processed, "1.3.2.df_IR_temp_counts_merged.rds"))

