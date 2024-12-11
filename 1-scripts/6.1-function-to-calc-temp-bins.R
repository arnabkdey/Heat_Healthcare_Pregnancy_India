# -------------------------------------------------------------------------------
# @project: Heat and healthcare contact during pregnancy in India
# @author: Arnab K. Dey,  arnabxdey@gmail.com 
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script defines functions for calculating temperature bin exposures over specified time periods.
# @date: Dec 12, 2024

func_calc_bins_period <- function(df_health, df_lt_clim,
                                  start_date_var, add_days, subtract_days, 
                                  psu_var = "psu", clim_var = "max_temp_wb") {
    # Ensure that the input and target dataframes are data.table
    setDT(df_lt_clim)
    setDT(df_health)
    
    # Ensure that the start_date_var is a Date object and create row_id and psu columns
    df_health[, `:=`(
        start_date = as.Date(get(start_date_var)),
        row_id = .I,
        psu = get(psu_var)
    )]
    
    # Calculate the end date of the period
    if (!is.null(add_days)) {
        df_health[, start_date := as.Date(get(start_date_var))]
        df_health[, end_date := start_date + add_days]
    } else if (!is.null(subtract_days)) {
        df_health[, end_date := as.Date(get(start_date_var))]
        df_health[, start_date := end_date - subtract_days]
    } else {
        stop("Either add_days or subtract_days must be provided")
    }
    
    # Calculate the number of instances for each bin
    result <- df_lt_clim[df_health, 
        .(
            bin_below_10 = sum(get(clim_var) < 10, na.rm = TRUE),
            bin_10_15 = sum(get(clim_var) >= 10 & get(clim_var) < 15, na.rm = TRUE),
            bin_15_20 = sum(get(clim_var) >= 15 & get(clim_var) < 20, na.rm = TRUE),
            bin_20_25 = sum(get(clim_var) >= 20 & get(clim_var) < 25, na.rm = TRUE),
            bin_25_30 = sum(get(clim_var) >= 25 & get(clim_var) < 30, na.rm = TRUE),
            bin_above_30 = sum(get(clim_var) >= 30, na.rm = TRUE)
        ),
        by = .EACHI,
        on = .(psu, date >= start_date, date <= end_date)
    ]
    
    # Merge result back to the target dataframe
    df_health <- cbind(df_health, result[, .(bin_below_10, bin_10_15, bin_15_20, bin_20_25, bin_25_30, bin_above_30)])
    
    return(df_health)
}