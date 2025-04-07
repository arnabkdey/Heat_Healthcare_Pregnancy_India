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

# Function to calculate all cutoffs for one variable
calculate_cutoffs <- function(df_path, variable, cutoffs) {
  # columns to read
  cols_to_read <- c("psu", "date", variable)  
  # Read the dataset
  df <- read_fst(df_path, as.data.table = TRUE, columns = cols_to_read)
  # Order the dataset by date and psu
  setorder(df, psu, date)
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
