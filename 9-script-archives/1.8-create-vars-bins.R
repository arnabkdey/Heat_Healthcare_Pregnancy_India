rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive, here)
# Constants ----
path_processed <- here("2-data", "2.2-processed-data")

# Read datasets --------
# df_lt_vars <- read_fst(here(path_processed, "1.6a-clim-lt-vars-max_temp_wb-crude.fst"), as.data.table = T)
df_lt_vars <- read_fst(here(path_processed, "1.5.1-df_psu_wbgt_2014.fst"), as.data.table = T)
df_paper_final <- readRDS(here(path_processed, "1.4-dhs-IR-merged-w-clim-zones.rds"))
ls()

# Process datasets --------
colnames(df_lt_vars)
## Create a shorter dataframe for df_final
colnames(df_paper_final)
df_paper_final <- df_paper_final |>
                        dplyr::select(psu, state_name, dist_name, clim_zone_short, 
                                        wt_raw, doi, dob, rural, 
                                        contains("caste"), contains("religion"),
                                        contains("wealth"), contains("edu"), contains("age"),
                                        contains("parity"), 
                                        dv_breech_pres_num, dv_prolonged_labour_num,
                                        dv_excessive_bleeding_num, 
                                        any_del_comp_sum, any_del_comp_bi,
                                        dv_caesarean)
                    

ls()                    

## Create variables for date of lmp and trimesters
df_paper_final <- df_paper_final |>
                    mutate(
                           dob = as.Date(dob, format = "%d-%b-%y"),
                           lmp = dob - 280,
                           trimester = case_when(dob - lmp < 91 ~ 1,
                                                 dob - lmp < 182 ~ 2,
                                                 TRUE ~ 3)) 

# Function
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
            bin_below_23 = sum(get(clim_var) < 23, na.rm = TRUE),
            bin_23_25 = sum(get(clim_var) >= 23 & get(clim_var) < 25, na.rm = TRUE),
            bin_25_28 = sum(get(clim_var) >= 25 & get(clim_var) < 28, na.rm = TRUE),
            bin_28_30 = sum(get(clim_var) >= 28 & get(clim_var) < 30, na.rm = TRUE),
            bin_30_33 = sum(get(clim_var) >= 30 & get(clim_var) < 33, na.rm = TRUE),
            bin_above_33 = sum(get(clim_var) >= 33, na.rm = TRUE)
        ),
        by = .EACHI,
        on = .(psu, date >= start_date, date <= end_date)
    ]
    
    # Merge result back to the target dataframe
    df_health <- cbind(df_health, result[, .(bin_below_23, bin_23_25, bin_25_28, bin_28_30, bin_30_33, bin_above_33)])
    
    return(df_health)
}

# Test the function ----

df_count_bins_preg <- func_calc_bins_period(df_health = df_paper_final, df_lt_clim = df_lt_vars, 
                                 start_date_var = "dob", subtract_days = 280, add_days = NULL,
                                 psu_var = "psu", clim_var = "max_temp_wb")


# Save the result ----
write_fst(df_count_bins_preg, here(path_processed, "1.8-df_count_bins_preg.fst"))