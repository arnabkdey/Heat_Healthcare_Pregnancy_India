# Preparatory ----
## load-packages ----- 
rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive, here)

# Load and process datasets -----
## IR data ------ 
### Load IR data 
path_processed <- here("2-data", "2.2-processed-data")
df_IR_long <- read_fst(here(path_processed, "1.1-dhs-IR-vars-created.fst"), as.data.table = TRUE)

### Create variables for year, month, and weekday 
df_IR <- df_IR_long |>
  mutate(
    dob_year = year(dob),
    dob_month = month(dob),
    dob_weekday = lubridate::wday(dob, label = TRUE)
  )

### Retain only the necessary variables
df_IR <- df_IR |>
  select(psu, wt_final, dob, dob_year, dob_month, dob_weekday, 
        no_pnc_b, no_pnc_m, no_s2s, dv_home_del, place_del_type, 
        dv_caesarean, any_sba, any_comp, 
        rural, hh_caste_bi, hh_religion_bi, mat_parity_bi, hh_wealth_bi)

### Prepare shorter datasets for a list of variables ----
varlist_dep <- c("no_pnc_b", "no_pnc_m", "no_s2s", "dv_caesarean", 
                  "any_sba", "any_comp")

#### Create a list of datasets and ID groups in a loop ----
df_list_depvar_dfs <- list()
for (i in varlist_dep) {
  df_list_depvar_dfs[[i]] <- df_IR |>
    select(psu, wt_final, dob, dob_year, dob_month, dob_weekday, 
          no_pnc_b, no_pnc_m, no_s2s, dv_caesarean, 
          place_del_type, any_sba, any_comp) |>
    filter(!!sym(i) == 1) |> 
    mutate(ID_grp = row_number()) 
    # assign name to the dataset
    names(df_list_depvar_dfs)[names(df_list_depvar_dfs) == i] <- paste0("df_", i)
}
#### Check the datasets ----
names(df_list_depvar_dfs) # 6
# colnames(df_list_depvar_dfs[[6]])
# head(df_list_depvar_dfs$df_no_pnc_b)


## Climate data ------
### Load climate data
df_climate_final <- read_fst(here(path_processed, "1.5-dhs-psu-paper.fst"), as.data.table = TRUE)

### Create variables for year, month, and weekday
df_climate_final <- df_climate_final |>
  mutate(
    year = year(date),
    month = month(date),
    weekday = lubridate::wday(date, label = TRUE))
  
### Retain only the necessary variables
df_climate_final <- df_climate_final |>
  select(psu = psu, date, year, month, weekday, 
        hotday_wb_30, hotday_wb_31, hotday_wb_32,
        hw_wb_30_2d, hw_wb_30_3d, hw_wb_30_5d,
        hw_wb_31_2d, hw_wb_31_3d, hw_wb_31_5d,
        hw_wb_32_2d, hw_wb_32_3d, hw_wb_32_5d,
        hotday_wb_90_doy, hotday_wb_95_doy, hotday_wb_97_doy,
        hw_wb_90_doy_2d, hw_wb_90_doy_3d, hw_wb_90_doy_5d,
        hw_wb_95_doy_2d, hw_wb_95_doy_3d, hw_wb_95_doy_5d,
        hw_wb_97_doy_2d, hw_wb_97_doy_3d, hw_wb_97_doy_5d,
        hotday_wb_90_harmo, hotday_wb_95_harmo, hotday_wb_97_harmo,
        hw_wb_90_harmo_2d, hw_wb_90_harmo_3d, hw_wb_90_harmo_5d,
        hw_wb_95_harmo_2d, hw_wb_95_harmo_3d, hw_wb_95_harmo_5d,
        hw_wb_97_harmo_2d, hw_wb_97_harmo_3d, hw_wb_97_harmo_5d)

colnames(df_climate_final)


# Merge Health outcomes and Climate data ----
## Loop through list of pervar dfs and merge with climate data ----
df_list_merged_dfs <- list()
for (i in names(df_list_depvar_dfs)) {
  setDT(df_list_depvar_dfs[[i]])
  df_list_merged_dfs[[i]] <- merge(df_list_depvar_dfs[[i]], df_climate_final, 
                                by.x = c("psu", "dob_year", "dob_month", "dob_weekday"), 
                                by.y = c("psu", "year", "month", "weekday")) |> 
                              mutate(case = ifelse(format(dob, "%Y-%m-%d") == format(date, "%Y-%m-%d"), 
                                1, 0))
  names(df_list_merged_dfs)[names(df_list_merged_dfs) == i] <- paste0(i, "_merged")
}

## Check the datasets ----
names(df_list_merged_dfs) 


# Save the merged datasets ----
path_out <- here("2-data", "2.2-processed-data")
saveRDS(df_list_merged_dfs, file = here(path_out, "1.7-case-crossover-datasets.rds"))


# Usage -----------
# Models for case crossover ----
names(df_case_crossover)
df_caesarean <- df_case_crossover[["df_dv_caesarean_merged"]]
df_no_pnc_m <- df_case_crossover[["df_no_pnc_m_merged"]]
df_comp <- df_case_crossover[["df_any_comp_merged"]]
nrow(df_comp)

model_cco <- lme4::glmer(case ~ hw_wb_30_2d + 
                    (1 | ID_grp), 
                data = df_comp, 
                # subset = hh_caste_bi == "SC/ST/OBC",
                family = binomial)
summary(model_cco)

