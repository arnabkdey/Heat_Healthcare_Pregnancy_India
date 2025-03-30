# load libraries ----
rm(list = ls())
pacman::p_load(dplyr, janitor, data.table, fst, here, future.apply, tictoc)

# set paths ----
source(here("paths_mac.R"))

# load data ----
df_psu_clim_merged <- read_fst(here(path_processed, 
  "1.2.1.df_psu_clim_merged.fst"),
  # from = 1, to = 1e6,
  columns = c("psu", "date"),
  as.data.table = T)
head(df_psu_clim_merged)
typeof(df_psu_clim_merged$psu)

df_test <- df_psu_clim_merged |> filter(psu == "37942")
nrow(df_test)
max(df_test$date)
# Your vectors
vec_tmax <- c("tmax_wbgt", "tmax_db") 
vec_tmin <- c("tmin_wbgt", "tmin_db") 

vec_cutoffs_tmax <- c(80, 82.5, 85, 87.5, 90, 92.5, 95)
vec_cutoffs_tmin <- c(5, 7.5, 10, 12.5, 15, 17.5, 20)

# Function to calculate all cutoffs for one variable
calculate_cutoffs <- function(df, variable, cutoffs) {
  # Use variable directly as it's already the column name
  col_name <- variable
  result <- df
  for (cutoff in cutoffs) {
    # Format cutoff for column name - replace decimal point
    if (cutoff < 10) {
      cutoff_str <- as.character(cutoff)
      # Replace decimal point if present
      cutoff_str <- gsub("\\.", "", cutoff_str)
    } else {
      cutoff_str <- gsub("\\.", "", sprintf("%.1f", cutoff))
    }
    
    new_col_name <- paste0("cutoff_", variable, "_", cutoff_str)
    
    # Calculate quantile by PSU
    result[, (new_col_name) := quantile(get(col_name), probs = cutoff/100, na.rm = TRUE), by = "psu"]
  }
  
  return(result)
}


# Process Tmax vars and cutoffs ----
## Set up parallel processing (for windows PC)
plan(multisession, workers = parallel::detectCores() - 4)  

## run the function ----
tic()
df_tmax <- copy(df_psu_clim_merged)
for (var in vec_tmax) {
  df_tmax <- calculate_cutoffs(df_tmax, var, vec_cutoffs_tmax)
}
toc()
head(df_tmax)
typeof(df_psu_clim_merged$psu)
typeof(df_tmax$psu)

## save the dataset ----
df_tmax |> write_fst(here(path_processed, "1.2.2.a.df_psu_clim_merged_tmax_cutoffs.fst"))
df_tmax_2014 <- df_tmax[date >= as.Date("2014-01-01")]
rm(df_tmax)
df_tmax_2014 |> write_fst(here(path_processed, "1.2.2.b.df_psu_clim_merged_tmax_cutoffs_2014.fst"))
rm(df_tmax_2014)
print("Tmax done")

## close parallel workers ----
plan(sequential)

# Process Tmin vars and cutoffs ----
## Set up parallel processing (for windows PC)
plan(multisession, workers = parallel::detectCores() - 4)  

## run the function ----
tic()
df_tmin <- copy(df_psu_clim_merged)
for (var in vec_tmin) {
  df_tmin <- calculate_cutoffs(df_tmin, var, vec_cutoffs_tmin)
}
toc()
colnames(df_psu_clim_merged)
colnames(df_tmin)
## save the dataset ----
df_tmin |> write_fst(here("1.2.2.c.df_psu_clim_merged_tmin_cutoffs.fst"))
df_tmin_2014 <- df_tmin[date >= as.Date("2014-01-01")]
rm(df_tmin)
df_tmin_2014 |> write_fst(here(path_processed, "1.2.2.d.df_psu_clim_merged_tmin_cutoffs_2014.fst"))
rm(df_tmin_2014)
print("Tmin done")

## close parallel workers ----
plan(sequential)

# Merge Tmax and Tmin datasets ----

## 2014 dataset
tic()
df_tmax_2014 <- read_fst(here(path_processed, "1.2.2.b.df_psu_clim_merged_tmax_cutoffs_2014.fst"))
df_tmin_2014 <- read_fst(here(path_processed, "1.2.2.d.df_psu_clim_merged_tmin_cutoffs_2014.fst"))
## keep only the columns date, psu, and starting with the cutoffs 
df_tmin_2014 <- df_tmin_2014 |> select(date, psu, starts_with("cutoff"))
df_tmax_tmin_2014 <- merge(df_tmax_2014, df_tmin_2014, by = c("psu", "date"))
df_tmax_tmin_2014 |> write_fst(here(path_processed, "1.2.2.e.df_psu_clim_merged_tmax_tmin_cutoffs_2014.fst"))
toc()

## full dataset
tic()
df_tmax <- read_fst(here(path_processed, "1.2.2.a.df_psu_clim_merged_tmax_cutoffs.fst"))
df_tmin <- read_fst(here(path_processed, "1.2.2.c.df_psu_clim_merged_tmin_cutoffs.fst"))
### keep only the columns date, psu, and starting with the cutoffs 
df_tmin <- df_tmin |> select(date, psu, starts_with("cutoff"))

df_tmax_tmin <- merge(df_tmax, df_tmin, by = c("psu", "date"))
df_tmax_tmin |> write_fst(here(path_processed, "1.2.2.f.df_psu_clim_merged_tmax_tmin_cutoffs_full.fst"))
toc()
rm(df_tmax, df_tmin, df_tmax_tmin)
print("Tmax and Tmin FULL done")

rm(df_tmax_2014, df_tmin_2014, df_tmax_tmin_2014)
print("Tmax and Tmin 2014 done")

print("All done!")