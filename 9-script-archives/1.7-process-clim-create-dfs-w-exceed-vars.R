rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive, here)
# devtools::install_github("axdey/climExposuR")
library(climExposuR)
path_processed <- here("2-data", "2.2-processed-data")

# load datasets needed for this script --------------------------------
df_lt_vars_2014 <- read_fst(here(path_processed, "1.6-clim-exceedance-vars-max_temp_wb.fst"), as.data.table = T)
colnames(df_lt_vars_2014)
nrow(df_lt_vars_2014)

df_paper_final <- readRDS(here(path_processed, "1.4-dhs-IR-merged-w-clim-zones.rds"))
colnames(df_paper_final)

print("loading complete")
print(Sys.time())


# Run the function for different variables
## For PNC variables with 7 days after date of discharge
df_pnc_7 <- func_calc_sum_count_exceed_period(df_input = df_lt_vars_2014, 
                                            df_target = df_paper_final, 
                                            start_date_var = "dod", 
                                            add_days = 7, 
                                            subtract_days = NULL, 
                                            psu_var = "psu")

### Scale all variables to IQR ----
input_vec <- grep("^sum", colnames(df_pnc_7), value = T)
for (var in input_vec) {
  df_pnc_7[[paste0(var, "_scale_iqr")]] <- rescale_mid(df_pnc_7[[var]], mid = median(df_pnc_7[[var]]), range = IQR(df_pnc_7[[var]]))
}

### Save the final dataset ------
write.fst(df_pnc_7, here(path_processed, "1.7-final-data-for-paper-pnc_7.fst"))

print("PNC - 7 dataset created")
print(Sys.time())

## For PNC variables with 30 days after date of discharge
df_pnc_30 <- func_calc_sum_count_exceed_period(df_input = df_lt_vars_2014, 
                                            df_target = df_paper_final, 
                                            start_date_var = "dod", 
                                            add_days = 30, 
                                            subtract_days = NULL, 
                                            psu_var = "psu")

### Scale all variables to IQR ----
input_vec <- grep("^sum", colnames(df_pnc_30), value = T)
for (var in input_vec) {
  df_pnc_30[[paste0(var, "_scale_iqr")]] <- rescale_mid(df_pnc_30[[var]], mid = median(df_pnc_30[[var]]), range = IQR(df_pnc_30[[var]]))
}

print("PNC - 30 dataset created")
print(Sys.time())
## For pregnancy duration 9 months before date of delivery
df_preg_9 <- func_calc_sum_count_exceed_period(df_input = df_lt_vars_2014, 
                                            df_target = df_paper_final, 
                                            start_date_var = "dob", 
                                            add_days = NULL, 
                                            subtract_days = 280, 
                                            psu_var = "psu")

### Scale all variables to IQR ----
input_vec <- grep("^sum", colnames(df_preg_9), value = T)
for (var in input_vec) {
  df_preg_9[[paste0(var, "_scale_iqr")]] <- rescale_mid(df_preg_9[[var]], mid = median(df_preg_9[[var]]), range = IQR(df_preg_9[[var]]))
}

### Save the final dataset ------
write.fst(df_preg_9, here(path_processed, "1.7-final-data-for-paper-preg_9.fst"))

## For pregnancy - trimester 1
df_paper_final$lmp <- df_paper_final$dob - lubridate::days(280)
df_preg_t1 <- func_calc_sum_count_exceed_period(df_input = df_lt_vars_2014, 
                                            df_target = df_paper_final, 
                                            start_date_var = "lmp", 
                                            add_days = 93, 
                                            subtract_days = NULL, 
                                            psu_var = "psu")

### Scale all variables to IQR ----
input_vec <- grep("^sum", colnames(df_preg_t1), value = T)
for (var in input_vec) {
  df_preg_t1[[paste0(var, "_scale_iqr")]] <- rescale_mid(df_preg_t1[[var]], mid = median(df_preg_t1[[var]]), range = IQR(df_preg_t1[[var]]))
}

### Save the final dataset ------
write.fst(df_preg_t1, here(path_processed, "1.7-final-data-for-paper-preg_t1.fst"))

## For pregnancy - trimester 2
df_paper_final$t1_date <- df_paper_final$lmp + lubridate::days(93)
df_preg_t2 <- func_calc_sum_count_exceed_period(df_input = df_lt_vars_2014, 
                                            df_target = df_paper_final, 
                                            start_date_var = "t1_date", 
                                            add_days = 93, 
                                            subtract_days = NULL, 
                                            psu_var = "psu")

### Scale all variables to IQR ----
input_vec <- grep("^sum", colnames(df_preg_t2), value = T)
for (var in input_vec) {
  df_preg_t2[[paste0(var, "_scale_iqr")]] <- rescale_mid(df_preg_t2[[var]], mid = median(df_preg_t2[[var]]), range = IQR(df_preg_t2[[var]]))
}

### Save the final dataset ------
write.fst(df_preg_t2, here(path_processed, "1.7-final-data-for-paper-preg_t2.fst"))


## For pregnancy - trimester 3
df_preg_t3 <- func_calc_sum_count_exceed_period(df_input = df_lt_vars_2014, 
                                            df_target = df_paper_final, 
                                            start_date_var = "dob", 
                                            add_days = NULL, 
                                            subtract_days = 94, 
                                            psu_var = "psu")

### Scale all variables to IQR ----
input_vec <- grep("^sum", colnames(df_preg_t3), value = T)
for (var in input_vec) {
  df_preg_t3[[paste0(var, "_scale_iqr")]] <- rescale_mid(df_preg_t3[[var]], mid = median(df_preg_t3[[var]]), range = IQR(df_preg_t3[[var]]))
}

### Save the final dataset ------
write.fst(df_preg_t3, here(path_processed, "1.7-final-data-for-paper-preg_t3.fst"))


print("pregnancy dataset created")
print(Sys.time())



# Check
## First confirm dimensions
dim(df_paper_final)
dim(df_pnc_7)
dim(df_pnc_30)
dim(df_preg_9)

## Then check the summary
summary(df_pnc_7 |> select(starts_with("sum")))
summary(df_pnc_30 |> select(starts_with("sum")))
summary(df_preg_9 |> select(starts_with("sum")))


## Now look at specific PSUs that have exceedances
View(df_pnc_7 |> 
        select(psu, dob, start_date, end_date, starts_with("sum")) |> 
        filter(sum_ident_abs_31 > 0))

View(df_lt_vars_2014 |> 
        mutate(year = year(date),
                month = month(date)) |>
    select(psu, date, max_temp_wb, year, month, ident_abs_31) |> 
    filter(psu == 55340 & year == 2019 & month == 5))
colnames(df_lt_vars_2014)

