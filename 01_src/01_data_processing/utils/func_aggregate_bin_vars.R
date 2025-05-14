#' Create aggregated temperature bins
#' 
#' This function creates aggregated bins at 10th, 20th, and 25th percentiles
#' from the original temperature bin data.
#' 
#' @param df The dataframe containing temperature bin data
#' @param temp_var The name of the temperature variable (e.g., 'tmax_wbgt', 'tmin_wbgt')
#' @return A dataframe with the original data plus the new aggregated bin variables

create_aggregated_bins <- function(df, temp_var) {
  # Create a copy of the dataframe to avoid modifying the original
  df_merged <- df
  
  # Compute bins at every 5th, 10th, 15th, 85th, 90th, 95th percentiles
  df_merged <- df_merged |>
    # compute cumulative bins
    mutate(days_compu_bins_cumulative_5 = get(paste0("days_", temp_var, "_less_50"))) |>
    mutate(days_compu_bins_cumulative_10 = get(paste0("days_", temp_var, "_less_50")) + 
             get(paste0("days_", temp_var, "_050_100"))) |>
    mutate(days_compu_bins_cumulative_15 = get(paste0("days_", temp_var, "_less_50")) + 
             get(paste0("days_", temp_var, "_050_100")) + 
             get(paste0("days_", temp_var, "_100_150"))) |>
    mutate(days_compu_bins_cumulative_85 = get(paste0("days_", temp_var, "_850_900")) + 
             get(paste0("days_", temp_var, "_900_950")) + 
             get(paste0("days_", temp_var, "_more_950"))) |>
    mutate(days_compu_bins_cumulative_90 =  
             get(paste0("days_", temp_var, "_900_950")) + 
             get(paste0("days_", temp_var, "_more_950"))) |>
    mutate(days_compu_bins_cumulative_95 = get(paste0("days_", temp_var, "_more_950")))

    # Create scaled exposure variables
    ## if temp_var contains tmax
    if (grepl("tmin", temp_var)) {
      df_merged <- df_merged |>
      mutate(exp_cumu_5_scaled10 = days_compu_bins_cumulative_5 / 10) |>
      mutate(exp_cumu_10_scaled10 = days_compu_bins_cumulative_10 / 10) |>
      mutate(exp_cumu_15_scaled10 = days_compu_bins_cumulative_15 / 10)
    } else if (grepl("tmax", temp_var)) {
      df_merged <- df_merged |>
      mutate(exp_cumu_85_scaled10 = days_compu_bins_cumulative_85 / 10) |>
      mutate(exp_cumu_90_scaled10 = days_compu_bins_cumulative_90 / 10) |>
      mutate(exp_cumu_95_scaled10 = days_compu_bins_cumulative_95 / 10)
    } else if (grepl("tmean", temp_var)) {
      df_merged <- df_merged |>
      mutate(exp_cumu_5_scaled10 = days_compu_bins_cumulative_5 / 10) |>
      mutate(exp_cumu_10_scaled10 = days_compu_bins_cumulative_10 / 10) |>
      mutate(exp_cumu_15_scaled10 = days_compu_bins_cumulative_15 / 10) |>
      mutate(exp_cumu_85_scaled10 = days_compu_bins_cumulative_85 / 10) |>
      mutate(exp_cumu_90_scaled10 = days_compu_bins_cumulative_90 / 10) |>
      mutate(exp_cumu_95_scaled10 = days_compu_bins_cumulative_95 / 10)
    } else {
      stop("Temperature variable not recognized")
    }

  return(df_merged)
}
