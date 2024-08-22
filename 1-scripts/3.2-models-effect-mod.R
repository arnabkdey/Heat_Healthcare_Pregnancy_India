rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, here)
pacman::p_load(parallel, future, furrr, doParallel, foreach, future.apply)
library(scales)
source("paths-mac.R")

# Read ----
## All dataesets ----
df_hv <- readRDS(here(path_project, "processed-data", "2.3-final-hv-data.rds"))
names(list_dfs)
print("finished reading")
print(Sys.time())

# Variables ----
colnames(df_hv |> dplyr::select(starts_with("dv")))
colnames(df_hv |> dplyr::select(starts_with("exp_")))
colnames(df_hv |> dplyr::select(starts_with("ses_")))
colnames(df_hv |> dplyr::select(starts_with("mat_")))

range(df_hv$sum_mag_abs_30)
range(df_hv$dv_num_visits_home)

# EM by access is a big problem ----
model_glmer_hv_em_access <- glmer(dv_met_flw_3mo ~ 
                        exp_bin_below_10_10*ses_access_issue_distance +
                        # exp_bin_10_15_10 + 
                        # exp_bin_15_20_10 + 
                        # exp_bin_20_25_10 + 
                        # exp_bin_25_30_10 + 
                        # exp_bin_above_30_10 +
                          ses_wealth_bi +
                          ses_caste_3 +
                          ses_religion_3 +
                        #   ses_access_issue_distance + 
                        as.factor(mat_edu_level) +
                        mat_age_at_int_scaled +
                        as.factor(mat_cur_preg) +
                          month_int + 
                          meta_rural + 
                        (1 | meta_dist_name), 
                          data = df_hv,                         
                          family = binomial(link = "logit"))

beepr::beep(sound = 4)
View(broom.mixed::tidy(model_glmer_hv_em_access, effects = "fixed", conf.int = TRUE) |> 
        # filter(str_detect(term, "^exp_")) |> 
        mutate(estimate = exp(estimate), conf.low = exp(conf.low), conf.high = exp(conf.high)) |>
        select(term, estimate, p.value, conf.low, conf.high))

# EM by Rural ----
model_glmer_hv_em_rural <- glmer(dv_met_flw_3mo ~ 
                        exp_bin_below_10_10*meta_rural +
                        # exp_bin_10_15_10 + 
                        # exp_bin_15_20_10 + 
                        # exp_bin_20_25_10 + 
                        # exp_bin_25_30_10 + 
                        # exp_bin_above_30_10 +
                          ses_wealth_bi +
                          ses_caste_3 +
                          ses_religion_3 +
                          ses_access_issue_distance + 
                        as.factor(mat_edu_level) +
                        mat_age_at_int_scaled +
                        as.factor(mat_cur_preg) +
                          month_int + 
                        #   meta_rural + 
                        (1 | meta_dist_name), 
                          data = df_hv,
                          family = binomial(link = "logit"))

beepr::beep(sound = 4)
View(broom.mixed::tidy(model_glmer_hv_em_rural, effects = "fixed", conf.int = TRUE) |> 
        # filter(str_detect(term, "^exp_")) |> 
        # mutate(estimate = exp(estimate), conf.low = exp(conf.low), conf.high = exp(conf.high)) |>
        select(term, estimate, p.value, conf.low, conf.high))

# 