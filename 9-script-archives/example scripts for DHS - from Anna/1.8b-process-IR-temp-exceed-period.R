pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive, here)
# devtools::install_github("axdey/climExposuR")
library(climExposuR)

# Read datasets -----
## Long-term WBGT data ----
df_lt_vars_2014 <- read_fst(here(path_project, "processed-data", "1.5.1-df_psu_wbgt_2014.fst"), as.data.table = T)
df_lt_vars_2014$psu <- as.factor(df_lt_vars_2014$psu)

### Address missing wbgt data ----
df_missing <- df_lt_vars_2014 |> filter(is.na(max_temp_wb))
tabyl(df_missing, psu) # we see that some PSUs dont have temp data at all
# Drop these PSUs
psus_missing <- unique(df_lt_vars_2014 |> filter(is.na(max_temp_wb)) |> pull(psu))
df_lt_vars_2014 <- df_lt_vars_2014 |> filter(!psu %in% psus_missing)
sum(is.na(df_lt_vars_2014)) # no missing values

## Health datasets ----
df_list <- readRDS(here(path_project, "processed-data", "1.4-list-of-processed-datasets.rds"))

# Calculate exceedance variables in LT dataset ----
vec_threshold_abs <- c(30)
df_lt_vars_ident_exceed <- func_ident_mag_calc_mag(df_lt_vars_2014, vec_threshold_abs, vec_varnames_perc = NULL, clim_var = "max_temp_wb")
df_lt_vars_ident_exceed <- df_lt_vars_ident_exceed |> select(-starts_with("mag"))

# Count exceedance in health datasets ----
## Last 3 months ANC ----
names(df_list)
df_anc_3mo <- df_list[["df_anc_last_3mo"]]
df_anc_3mo$psu <-   df_anc_3mo$meta_psu
head(df_exceed)
dim(df_exceed)
df_anc_3mo_ex <- func_calc_sum_count_exceed_period(df_lt_vars_ident_exceed, df_anc_3mo, "dob", add_days = NULL, subtract_days = 90, psu_var = "psu")
summary(df_anc_3mo_ex |> select(starts_with("sum")))

# Scale exceedance count ----
df_anc_3mo_ex <- df_anc_3mo_ex |> 
  mutate(sum_ident_abs_30_scaled = sum_ident_abs_30/10)

# Run models ----
## ANC last 3mo ----
colnames(df_anc_3mo_ex)
model_anc_3mo_ex_fe_int <- glm(dv_not_met_prov_last_3mo ~ sum_ident_abs_30_scaled +
                              ses_religion_3 + ses_caste_3 + ses_wealth_bi + ses_access_issue_distance +
                              mat_age_at_birth_scaled +
                        season_birth*meta_state_name, 
                        data = df_anc_3mo_ex,
                        family = poisson(link = "log"))
View(broom::tidy(model_anc_3mo_ex_fe_int, exp = T) |> head())
range()

# Create datasets ----


# Model ----
tabyl(df_no_pnc_7, no_pnc_7)
tabyl(df_no_pnc_7, sum_ident_abs_30)
model_pnc_7 <- glmer(no_pnc_7 ~ sum_ident_abs_30 + 
                    season_birth + (1 | clim_zone_short), 
                     data = df_no_pnc_7, 
                     family = poisson(link = "log"))
summary(model_pnc_7)


ls()
colnames(df_lt_vars_2014)
head(df_no_pnc_7 |> select(psu, dob, dod, starts_with("sum")) |> filter(sum_ident_abs_30 > 0))
colnames(df_dob_7)


View(df_lt_vars_2014 |> filter(psu == 55340) |> 
            mutate(year = year(date), month = month(date), day = day(date)) |>
            filter(year == 2019 & month == 5) |> 
            select(psu, date, max_temp_wb))
