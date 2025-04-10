# ==============================================================================
# Script:      calculate_cutoffs.R
# Description: Function to calculate multiple percentile cutoffs for a specified
#              variable grouped by PSU (primary sampling unit) from FST-format data
# 
# Function:    calculate_cutoffs()
# Purpose:     Calculates multiple percentile cutoffs for a given variable across
#              different PSUs in panel/longitudinal data
#
# Inputs:
#   - df_path:  Path to FST file containing the dataset
#   - variable: Name of the variable for which to calculate cutoffs (character)
#   - cutoffs:  Vector of percentile cutoffs to calculate (e.g., c(95, 97.5, 99))
#
# Output:
#   - Returns a data.table with original columns (psu, date, variable) plus
#     additional columns for each calculated cutoff (named cutoff_[var]_[percentile])
#
# Features:
#   - Efficiently reads only required columns from FST file
#   - Handles both integer and decimal cutoff values
#   - Automatically formats column names for cutoffs
#   - Processes data by PSU and maintains original date ordering
#
# Dependencies:
#   - data.table
#   - fst
#
# Author:      Arnab K. Dey
# Created:     March 2024
# ==============================================================================

# Load required packages
library(data.table)
library(fst)

# Function to calculate all cutoffs for one variable
calculate_cutoffs <- function(df_path, variable, cutoffs) {
  # columns to read
  cols_to_read <- c("psu", "date", variable)  
  
  # Read the dataset
  df <- read_fst(df_path, as.data.table = TRUE, columns = cols_to_read)
  
  # Filter for dates before March 31st, 2020
  df <- df[date < as.Date("2020-03-31")]
  
  # Order the dataset by date and psu
  setorder(df, psu, date)

  # Use variable directly as it's already the column name
  col_name <- variable
  result <- df
  
  for (cutoff in cutoffs) {
    # Format cutoff to always have 3 digits (including decimal part)
    cutoff_str <- sprintf("%03d", cutoff * 10)  # Multiply by 10 to shift decimal

    new_col_name <- paste0("cutoff_", variable, "_", cutoff_str)
    
    # Calculate quantile by PSU
    result[, (new_col_name) := quantile(get(col_name), probs = cutoff/100, na.rm = TRUE), by = "psu"]
  }
  
  return(result)
}
