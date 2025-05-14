# Load required packages
rm(list = ls())
pacman::p_load(dplyr, data.table, fst, here, purrr)

# Set paths
source(here("paths.R"))

# Source functions
source(here("01_src", "01_data_processing", "utils", "func_aggregate_bin_vars.R"))
source(here("01_src", "01_data_processing", "utils", "func_merge_datasets.R"))

# Load data ----
## DHS data ----
df_IR_4mo <- readRDS(here(path_processed, "1.1.3_DHS_IR_vars_created_4mo.rds"))
# df_IR_5mo <- readRDS(here(path_processed, "1.1.3_DHS_IR_vars_created_5mo.rds"))
# df_IR_6mo <- readRDS(here(path_processed, "1.1.3_DHS_IR_vars_created_6mo.rds"))
df_IR_4mo <- df_IR_4mo |> mutate(psu = meta_psu)
# df_IR_6mo <- df_IR_6mo |> mutate(psu = meta_psu)
# df_IR_5mo <- df_IR_5mo |> mutate(psu = meta_psu)


## Load temperature bins data
temp_files <- list(
  tmax_wbgt = "1.3.2.a_df_bins_tmax_wbgt_4mo.fst",
  tmin_wbgt = "1.3.2.b_df_bins_tmin_wbgt_4mo.fst",
  # tmean_wbgt = "1.3.2.c_df_bins_tmean_wbgt_4mo.fst",
  tmax_db_era5 = "1.3.2.d_df_bins_tmax_db_era5_4mo.fst",
  tmin_db_era5 = "1.3.2.e_df_bins_tmin_db_era5_4mo.fst"
  # tmean_db_era5 = "1.3.2.f_df_bins_tmean_db_era5_4mo.fst"
)

# Merge datasets
# df_merged <- merge_datasets(df_IR_6mo, temp_files, path_processed)
df_merged <- merge_datasets(df_IR_4mo, temp_files, path_processed)
# df_merged <- merge_datasets(df_IR_5mo, temp_files, path_processed)

# Create aggregated bins for each dataset
for (dataset_name in names(df_merged)) {
  # Extract temperature variable name from dataset name
  temp_var <- dataset_name
  # Create aggregated bins
  df_merged[[dataset_name]] <- create_aggregated_bins(
    df_merged[[dataset_name]], 
    temp_var
  )
}

# estimate number and percentage of zeros in cumulative bins in each dataset
# number of zeros
# df_merged |> map(~ .x |> 
#   select(starts_with("exp")) |> 
#   summarise(across(everything(), ~sum(.x == 0, na.rm = TRUE))))

# percentage of zeros
# df_merged |> map(~ {
#   zeros <- .x |> 
#     select(starts_with("exp")) |> 
#     summarise(across(everything(), ~sum(.x == 0, na.rm = TRUE)))
#   zeros / nrow(.x)
# })

# Save processed data
saveRDS(df_merged, here(path_processed, "1.3.3_cumulative_bins_all_datasets_4mo.rds")) 






