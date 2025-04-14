library(tidyverse)
library(openxlsx)

# Helper functions to process data to and from excel files ----

## Function to read all sheets from an Excel file
read_excel_sheets <- function(file_path) {
  # Get sheet names
  sheet_names <- excel_sheets(file_path)
  
  # Read all sheets, add sheet name as column, and combine
  sheet_names |>
    lapply(function(sheet) read_excel(file_path, sheet = sheet) |>
             mutate(Modifier = sheet)) |>
    bind_rows() |> 
    drop_na()
}

## Function to standardize modifier names
standardize_modifiers <- function(df) {
  # Create mapping for modifiers
  modifier_mapping <- c(
    "meta_rural" = "A. Residence",
    "ses_access_issue_distance" = "B. Distance is a big problem",
    "ses_wealth_bi_richer3" = "C. Household wealth",
    "mat_edu_level_bi" = "D. Woman's education"
  )
  # Apply mapping
  df$Modifier <- modifier_mapping[df$Modifier]
  
  return(df)
}

## Function to standardize modifier levels
standardize_levels <- function(df) {
  # Create mapping for modifier levels
  level_mapping <- c(
    "urban" = "Urban",
    "rural" = "Rural",
    "big-problem" = "Yes",
    "not-a-big-prob" = "No",
    "poorer" = "Poorer",
    "richer3" = "Richer",
    "primary or higher" = "Some",
    "no education" = "None"
  )
  
  # Apply mapping
  df$ModifierLevel <- level_mapping[df$ModifierLevel]
  
  return(df)
}

## function to standardize exposures
standardize_exposures <- function(df) {
  # Create mapping for exposures
  exposure_mapping <- c(
    "days_cutoff_tmax_wbgt_800_greater" = "Tmax-WBGT >= 80th percentile",
    "days_cutoff_tmax_wbgt_825_greater" = "Tmax-WBGT >= 82.5th percentile",
    "days_cutoff_tmax_wbgt_850_greater" = "Tmax-WBGT >= 85th percentile",
    "days_cutoff_tmax_wbgt_875_greater" = "Tmax-WBGT >= 87.5th percentile",
    "days_cutoff_tmax_wbgt_900_greater" = "Tmax-WBGT >= 90th percentile",
    "days_cutoff_tmax_wbgt_925_greater" = "Tmax-WBGT >= 92.5th percentile",
    "days_cutoff_tmax_wbgt_950_greater" = "Tmax-WBGT >= 95th percentile",
    "days_cutoff_tmax_db_era5_800_greater" = "Tmax-DBT >= 80th percentile",
    "days_cutoff_tmax_db_era5_825_greater" = "Tmax-DBT >= 82.5th percentile",
    "days_cutoff_tmax_db_era5_850_greater" = "Tmax-DBT >= 85th percentile",
    "days_cutoff_tmax_db_era5_875_greater" = "Tmax-DBT >= 87.5th percentile",
    "days_cutoff_tmax_db_era5_900_greater" = "Tmax-DBT >= 90th percentile",
    "days_cutoff_tmax_db_era5_925_greater" = "Tmax-DBT >= 92.5th percentile",
    "days_cutoff_tmax_db_era5_950_greater" = "Tmax-DBT >= 95th percentile",
    "days_cutoff_tmin_wbgt_050_less" = "Tmin-WBGT <= 5th percentile",
    "days_cutoff_tmin_wbgt_075_less" = "Tmin-WBGT <= 7.5th percentile",
    "days_cutoff_tmin_wbgt_100_less" = "Tmin-WBGT <= 10th percentile",
    "days_cutoff_tmin_wbgt_125_less" = "Tmin-WBGT <= 12.5th percentile",
    "days_cutoff_tmin_wbgt_150_less" = "Tmin-WBGT <= 15th percentile",
    "days_cutoff_tmin_wbgt_175_less" = "Tmin-WBGT <= 17.5th percentile",
    "days_cutoff_tmin_wbgt_200_less" = "Tmin-WBGT <= 20th percentile",
    "days_cutoff_tmin_db_era5_050_less" = "Tmin-DBT <= 5th percentile",
    "days_cutoff_tmin_db_era5_075_less" = "Tmin-DBT <= 7.5th percentile",
    "days_cutoff_tmin_db_era5_100_less" = "Tmin-DBT <= 10th percentile",
    "days_cutoff_tmin_db_era5_125_less" = "Tmin-DBT <= 12.5th percentile",
    "days_cutoff_tmin_db_era5_150_less" = "Tmin-DBT <= 15th percentile",
    "days_cutoff_tmin_db_era5_175_less" = "Tmin-DBT <= 17.5th percentile",
    "days_cutoff_tmin_db_era5_200_less" = "Tmin-DBT <= 20th percentile")
  
  # Apply mapping
  df$Exposure <- exposure_mapping[df$Exposure]
  
  return(df)
}

save_df_to_excel_by_column <- function(
  df, 
  file_path, 
  split_column, 
  pivot_wider = FALSE) {
  
  # Check if the column exists in the dataframe
  if (!split_column %in% names(df)) {
    stop(paste("Column", split_column, "not found in the dataframe"))
  }
  
  # Get unique values from the specified column
  unique_values <- unique(df[[split_column]])
  
  # Create a new workbook
  wb <- createWorkbook()
  
  # For each unique value, create a sheet and add the filtered data
  for (value in unique_values) {
    # Filter data for this value
    sheet_data <- df |> 
      filter(!!sym(split_column) == value)
    
    if (pivot_wider) {
      # Store the split column name
      split_col_name <- split_column
    
      sheet_data <- as.data.table(sheet_data) |> 
          unique(by = c("Exposure", "ModifierLevel")) |>
            melt(id.vars = c("Exposure", "ModifierLevel"), 
            measure.vars = c("OR", "CI_Lower", "CI_Upper")) |>
      dcast(Exposure ~ ModifierLevel + variable, value.var = "value")

      # Add back the split column value as a constant
      # 'value' is already available from the for loop
      sheet_data[[split_col_name]] <- value
  
      # Move the column to the beginning
      col_order <- c(split_col_name, setdiff(names(sheet_data), split_col_name))
      setcolorder(sheet_data, col_order)
    }

    # Create sheet name (use the value)
    sheet_name <- as.character(value)
    
    # Some characters are not allowed in Excel sheet names
    # Replace any problematic characters if needed
    sheet_name <- gsub("[\\[\\]\\*\\?\\:\\/\\\\]", "_", sheet_name)
    
    # Limit sheet name length to 31 characters (Excel limitation)
    if (nchar(sheet_name) > 31) {
      sheet_name <- substr(sheet_name, 1, 31)
    }
    
    # Add worksheet
    addWorksheet(wb, sheet_name)
    
    # Write data to worksheet
    writeData(wb, sheet_name, sheet_data)
    
    # Auto-fit columns for better readability
    setColWidths(wb, sheet_name, cols = 1:ncol(sheet_data), 
                 widths = "auto")
  }
  
  # Save the workbook
  saveWorkbook(wb, file_path, overwrite = TRUE)
  
  # Return success message
  return(paste("Excel file saved successfully at:", file_path, 
               "with sheets separated by", split_column))
}
