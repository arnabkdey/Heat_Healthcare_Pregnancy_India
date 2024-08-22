# Read anna's datasets
pacman::p_load(tidyverse, data.table, janitor, fst, openxlsx, googledrive, here, beepr, Hmisc)
# source(here(".Rprofile"))
# Constants ----
path_anna_raw_data <- here(path_raw_data, "WBGT_data_for_analysis_India")

# Read datasets --------
# Get a list of all CSV files in the folder
csv_files <- list.files(path = path_anna_raw_data, pattern = "*.csv", full.names = TRUE)

# Function to read CSV and convert all columns to character
read_csv_to_chr <- function(file) {
  df <- read_csv(file, col_types = cols(.default = "c"))
  return(df)
}

# Read and combine all CSV files
combined_df <- csv_files %>%
  map_dfr(read_csv_to_chr)

# Print the first few rows of the combined dataframe
print(head(combined_df))

dim(combined_df)

combined_df <- read.fst(here(path_processed_data, "7.2_combined_df_anna.fst"), as.data.table = TRUE)
# Create a shorter dataframe for df_final
df_final <- combined_df |> dplyr::select(-contains("lag"), -contains("Tmean"), -contains("minus"))

dim(combined_df)
dim(df_final)

# Save the combined dataframe as an RDS file
write.fst(combined_df, here(path_processed_data, "7.2_combined_df_anna.fst"))
write.fst(df_final, here(path_processed_data, "7.2_combined_df_anna_short.fst"))

