# load libraries ----
rm(list = ls())
pacman::p_load(dplyr, janitor, data.table, fst, openxlsx, here, tidyr, knitr)

# set paths ----
source(here("paths.R"))

# load data ----
df_paper_final <- readRDS(here(path_processed,
  "1.3.2.df_IR_temp_counts_merged.rds"))

# identify exposure variables ----

## WBGT Tmax
vec_wbgt_tmax <- df_paper_final |>
  select(contains("tmax_wbgt")) |>
  colnames() |>
  unique()

## WBGT Tmin
vec_wbgt_tmin <- df_paper_final |>
  select(contains("tmin_wbgt")) |>
  colnames() |>
  unique()

## Dry bulb Tmax
vec_db_tmax <- df_paper_final |>
  select(contains("tmax_db_era5")) |>
  colnames() |>
  unique()

## Dry bulb Tmin
vec_db_tmin <- df_paper_final |>
  select(contains("tmin_db_era5")) |>
  colnames() |>
  unique()

# Create summary tables ----

## Function to extract percentile from column name
extract_percentile <- function(col_name) {
  as.numeric(gsub(".*_([0-9]+)_.*", "\\1", col_name)) / 10
}

## Function to calculate mean and sd
calculate_stats <- function(x) {
  mean_val <- mean(x, na.rm = TRUE)
  sd_val <- sd(x, na.rm = TRUE)
  sprintf("%.1f (%.1f)", mean_val, sd_val)
}

## Calculate statistics for each temperature type
calculate_summary_stats <- function(df, columns) {
  result <- data.frame(
    percentile = sapply(columns, extract_percentile),
    stats = sapply(columns, function(col) calculate_stats(df[[col]]))
  )
  result[order(result$percentile), ]
}

## Tmax Summary Table
tmax_wbgt_summary <- calculate_summary_stats(df_paper_final, vec_wbgt_tmax)
tmax_db_summary <- calculate_summary_stats(df_paper_final, vec_db_tmax)

tmax_table <- merge(tmax_wbgt_summary, tmax_db_summary, 
                   by = "percentile", suffixes = c("_wbgt", "_db"))

## Tmin Summary Table
tmin_wbgt_summary <- calculate_summary_stats(df_paper_final, vec_wbgt_tmin)
tmin_db_summary <- calculate_summary_stats(df_paper_final, vec_db_tmin)

tmin_table <- merge(tmin_wbgt_summary, tmin_db_summary, 
                   by = "percentile", suffixes = c("_wbgt", "_db"))

## Format tables
format_table <- function(df) {
  df |>
    rename(
      "Percentile" = "percentile",
      "WBGT" = "stats_wbgt",
      "Dry Bulb" = "stats_db"
    ) |>
    mutate(
      Percentile = ifelse(Percentile %% 1 == 0,
                         paste0(Percentile, "th"),
                         paste0(Percentile, "th"))
    ) |>
    distinct()  # Remove any duplicates
}

tmax_table_formatted <- format_table(tmax_table)
tmin_table_formatted <- format_table(tmin_table)

# Save tables ----
write.xlsx(list(Tmax = tmax_table_formatted, Tmin = tmin_table_formatted),
           file = here(path_outputs, "tables", "temperature_distributions.xlsx"))

# Print tables ----
cat("\nMaximum Temperature Distribution (Mean (SD)):\n")
print(knitr::kable(tmax_table_formatted, format = "pipe", align = c('l', 'r', 'r')))

cat("\nMinimum Temperature Distribution (Mean (SD)):\n")
print(knitr::kable(tmin_table_formatted, format = "pipe", align = c('l', 'r', 'r')))
