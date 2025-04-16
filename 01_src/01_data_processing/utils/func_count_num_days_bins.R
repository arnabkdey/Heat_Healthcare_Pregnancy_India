# ==============================================================================
# Script:      count_num_days_bins.R
# Description: Function to count number of days in temperature bins between
#              start and end dates for each individual
# 
# Function:    count_days_in_bins()
# Purpose:     Calculates number of days in each temperature bin for each individual
#              based on their start and end dates
#
# Inputs:
#   - df_dhs: Dataframe containing individual data with start_date and end_date
#   - df_daily: Dataframe containing daily temperature data
#   - temp_var: Name of the temperature variable in df_daily
#   - is_tmax: Boolean indicating if this is maximum temperature (TRUE) or minimum (FALSE)
#
# Output:
#   - Returns a dataframe with same rows as df_dhs plus new columns for days in each bin
#
# Features:
#   - Handles both maximum and minimum temperature bins
#   - Creates non-overlapping bins (e.g., 10-20th percentile, 20-30th percentile)
#   - Includes days below 10th percentile and above 95th percentile
#   - Efficiently processes data using data.table
#
# Dependencies:
#   - data.table
#   - dplyr
#
# Author:      Arnab K. Dey
# Created:     March 2024
# ==============================================================================

library(data.table)
library(dplyr)

count_days_in_bins <- function(df_dhs, df_daily, temp_var) {
  # Convert to data.table for efficiency
  setDT(df_dhs)
  setDT(df_daily)
  
  # Check if temperature variable exists in df_daily
  if (!temp_var %in% names(df_daily)) {
    stop("Temperature variable '", temp_var, "' not found in df_daily. Available variables: ", 
         paste(names(df_daily), collapse = ", "))
  }
  
  # Convert PSU columns to character for consistent comparison
  df_dhs$psu <- as.character(df_dhs$psu)
  df_daily$psu <- as.character(df_daily$psu)
  
  # Get cutoff column names and sort by percentile
  cutoff_cols <- grep("^cutoff_.*_\\d{3}$", names(df_dhs), value = TRUE)
  cutoff_cols <- cutoff_cols[order(as.numeric(gsub(".*_(\\d{3})$", "\\1", cutoff_cols)))]
  
  # Create result dataframe
  result <- copy(df_dhs)
  
  # For each row in df_dhs
  for (i in 1:nrow(df_dhs)) {
    # Get PSU and date range
    cur_psu <- df_dhs$psu[i]
    start_date <- df_dhs$start_date[i]
    end_date <- df_dhs$end_date[i]
    
    # Filter daily data for this PSU and date range using simple data.table syntax
    daily_subset <- df_daily[df_daily$psu == cur_psu & df_daily$date >= start_date & df_daily$date <= end_date, ]
    
    if (nrow(daily_subset) > 0) {
      # Get cutoff values for this PSU
      cutoffs <- sapply(cutoff_cols, function(col) df_dhs[[col]][i])
      
      # Count days below first cutoff
      first_percentile <- as.numeric(gsub(".*_(\\d{3})$", "\\1", cutoff_cols[1]))
      days_below_first <- sum(daily_subset[[temp_var]] < cutoffs[1], na.rm = TRUE)
      result[i, paste0("days_", temp_var, "_less_", first_percentile) := days_below_first]
      
      # Count days in each bin
      for (j in 1:(length(cutoffs)-1)) {
        current_percentile <- as.numeric(gsub(".*_(\\d{3})$", "\\1", cutoff_cols[j]))
        next_percentile <- as.numeric(gsub(".*_(\\d{3})$", "\\1", cutoff_cols[j+1]))
        bin_name <- paste0("days_", temp_var, "_", 
                         sprintf("%03d", current_percentile), "_", 
                         sprintf("%03d", next_percentile))
        days_in_bin <- sum(daily_subset[[temp_var]] >= cutoffs[j] & 
                          daily_subset[[temp_var]] < cutoffs[j+1], na.rm = TRUE)
        result[i, (bin_name) := days_in_bin]
      }
      
      # Count days above last cutoff
      last_percentile <- as.numeric(gsub(".*_(\\d{3})$", "\\1", tail(cutoff_cols, 1)))
      days_above_last <- sum(daily_subset[[temp_var]] >= cutoffs[length(cutoffs)], na.rm = TRUE)
      result[i, paste0("days_", temp_var, "_more_", last_percentile) := days_above_last]
    } else {
      # If no data available, set all counts to NA
      for (j in 1:(length(cutoff_cols)-1)) {
        current_percentile <- as.numeric(gsub(".*_(\\d{3})$", "\\1", cutoff_cols[j]))
        next_percentile <- as.numeric(gsub(".*_(\\d{3})$", "\\1", cutoff_cols[j+1]))
        bin_name <- paste0("days_", temp_var, "_", 
                         sprintf("%03d", current_percentile), "_", 
                         sprintf("%03d", next_percentile))
        result[i, (bin_name) := NA_real_]
      }
      first_percentile <- as.numeric(gsub(".*_(\\d{3})$", "\\1", cutoff_cols[1]))
      result[i, paste0("days_", temp_var, "_less_", first_percentile) := NA_real_]
      last_percentile <- as.numeric(gsub(".*_(\\d{3})$", "\\1", tail(cutoff_cols, 1)))
      result[i, paste0("days_", temp_var, "_more_", last_percentile) := NA_real_]
    }
  }
  
  return(result)
}

# Example usage
# df_dhs <- data.frame(
#   psu = as.factor(c(1, 2, 3)),
#   start_date = as.Date(c("2023-01-01", "2023-01-05", "2023-01-10")),
#   end_date = as.Date(c("2023-01-05", "2023-01-10", "2023-01-20")),
#   # cutoff_tmax_wbgt_100 = c(20, 25, 30),
#   cutoff_tmax_wbgt_150 = c(10, 15, 20),
#   cutoff_tmax_wbgt_200 = c(20, 25, 30),
#   cutoff_tmax_wbgt_300 = c(30, 35, 40),
#   cutoff_tmax_wbgt_450 = c(35, 40, 45),
#   cutoff_tmax_wbgt_500 = c(40, 45, 50)
# )

# # View(df_dhs)
# glimpse(df_dhs)
# head(df_dhs, 10)

# # create a dataframe with daily temperature data for each psu between Jan 1 and Jan 31 2023
# df_daily <- data.frame(
#   psu = as.factor(rep(1:3, each = 31)),
#   date = as.Date(rep(seq(as.Date("2023-01-01"), as.Date("2023-01-31"), by = "day"), times = 3)),
#   tmax_wbgt = runif(93, 10, 40)
# )

# View(df_daily)
# glimpse(df_daily)
# head(df_daily, 10)

# result <- count_days_in_bins(df_dhs, df_daily, "tmax_wbgt")
# head(result)

# # Test on real data
# df_dhs <- df_dhs_tmax_wbgt_psu[1:3,]
# head(df_dhs)

# glimpse(df_dhs)
# df_dhs_psus <- df_dhs |> pull(psu) |> unique()

# # check if all psus in df_dhs are present in df_tmax_wbgt_daily
# df_test <- df_tmax_wbgt_daily |> filter(psu %in% df_dhs_psus) 
# df_test <- df_tmax_wbgt_daily |> 
#   # filter(psu == 130) |> 
#   filter(date >= as.Date("2019-09-04") & date <= as.Date("2019-12-03")) 

# head(df_test)
# df_test |> filter(tmax_wbgt > 22.49084)  |> nrow()
# df_test |> filter(tmax_wbgt <= 22.49084 & tmax_wbgt >= 21.47521) 

# result_real <- count_days_in_bins(df_dhs, df_tmax_wbgt_daily, "tmax_wbgt")
# head(result_real)



