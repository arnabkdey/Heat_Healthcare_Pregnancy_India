#' Count number of days exceeding a temperature threshold within a period
#'
#' @param df_health Health dataset containing interview dates (df_IR_long)
#' @param df_temp Temperature dataset with daily values (df_lt_vars_2014)
#' @param ref_temp Reference temperature column (e.g., "tmax_wbgt")
#' @param vec_cutoff Vector of cutoff columns to compare against
#' @param lookback_days Number of days before reference date to analyze
#' @param date_col Column name in health dataset containing reference dates
#' @param psu_col Column name for PSU/location identifiers (must exist in both datasets)
#' @param comparison Type of comparison: "greater" or "less"
#'
#' @return Health dataset with added columns for exposure counts, start dates, and end dates

count_temp_exposure <- function(df_health, df_temp, 
                                ref_temp, vec_cutoff, 
                                lookback_days, 
                                date_col = "doi", 
                                psu_col = "psu",
                                comparison = "greater") {
  
  # Load data.table if not already loaded
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required for this function")
  }
  library(data.table)
  
  # Validate inputs
  if (!all(c(date_col, psu_col) %in% colnames(df_health))) {
    stop("Health dataset must contain specified date and PSU columns")
  }
  
  if (!all(c("date", psu_col, ref_temp) %in% colnames(df_temp))) {
    stop("Temperature dataset must contain date, PSU, and reference temperature columns")
  }
  
  if (!all(vec_cutoff %in% colnames(df_temp))) {
    stop("All cutoff variables must exist in temperature dataset")
  }
  
  if (!comparison %in% c("greater", "less")) {
    stop("Comparison must be either 'greater' or 'less'")
  }
  
  # Convert data frames to data.tables if they aren't already
  if (!data.table::is.data.table(df_health)) {
    DT_health <- data.table::as.data.table(df_health)
  } else {
    DT_health <- data.table::copy(df_health)
  }
  
  if (!data.table::is.data.table(df_temp)) {
    DT_temp <- data.table::as.data.table(df_temp)
  } else {
    DT_temp <- data.table::copy(df_temp)
  }
  
  # Convert date columns to Date type if they aren't already
  if (!inherits(DT_health[[date_col]], "Date")) {
    DT_health[, (date_col) := as.Date(get(date_col))]
  }
  
  if (!inherits(DT_temp[["date"]], "Date")) {
    DT_temp[, date := as.Date(date)]
  }
  
  # Ensure PSU columns have the same type in both datasets (convert to factor)
  DT_health[, (psu_col) := as.character(get(psu_col))]
  DT_temp[, (psu_col) := as.character(get(psu_col))]
  
  # Create columns for start dates
  DT_health[, start_date := get(date_col) - lookback_days]
  DT_health[, end_date := get(date_col)]
  
  # Set keys for faster joins - fix the get(psu_col) issue
  data.table::setkeyv(DT_temp, c("date", psu_col))
  
  # Create comparison function based on user selection
  compare_func <- if (comparison == "greater") {
    function(x, y) x >= y
  } else {
    function(x, y) x <= y
  }
  
  # Calculate for each row in health data
  for (i in 1:nrow(DT_health)) {
    # Get individual's dates and PSU
    current_psu <- DT_health[i, get(psu_col)]
    current_start_date <- DT_health[i, start_date]
    current_end_date <- DT_health[i, end_date]
    
    # Create a filtering expression that properly handles column names
    filter_expr <- substitute(
      DT[col == val & date >= start & date <= end],
      list(
        DT = as.name("DT_temp"),
        col = as.name(psu_col),
        val = current_psu,
        start = current_start_date,
        end = current_end_date
      )
    )
    temp_subset <- eval(filter_expr)
    
    # Skip if no temperature data available for this period
    if (nrow(temp_subset) == 0) {
      for (cutoff in vec_cutoff) {
        col_name <- paste0("days_", cutoff, "_", comparison)
        DT_health[i, (col_name) := NA_real_]
      }
      next
    }
    
    # Calculate count for each cutoff
    for (cutoff in vec_cutoff) {
      # Count days where the condition is met
      days_count <- sum(compare_func(temp_subset[[ref_temp]], temp_subset[[cutoff]]), na.rm = TRUE)
      
      # Add new column with count
      col_name <- paste0("days_", cutoff, "_", comparison)
      DT_health[i, (col_name) := days_count]
    }
  }
  
  # Return the result (convert back to data.frame if the input was one)
  if (!data.table::is.data.table(df_health)) {
    return(as.data.frame(DT_health))
  } else {
    return(DT_health)
  }
}