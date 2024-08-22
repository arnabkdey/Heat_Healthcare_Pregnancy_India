rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, here)
pacman::p_load(parallel, future, furrr, doParallel, foreach, future.apply)
library(mgcv)
library(gratia)
source("paths-mac.R")

# Read ----
## All dataesets ----
df_hv <- readRDS(here(path_project, "processed-data", "2.3-final-hv-data.rds"))
names(list_dfs)
print("finished reading")
print(Sys.time())

# Variables ----
colnames(df_hv |> dplyr::select(starts_with("dv")))
colnames(df_hv |> dplyr::select(starts_with("bin")))
colnames(df_hv |> dplyr::select(starts_with("ses_")))
colnames(df_hv |> dplyr::select(starts_with("mat_")))

range(df_hv$sum_mag_abs_30)
range(df_hv$dv_num_visits_home)


# Full model ----
gam_model_hv <- mgcv::bam(dv_num_visits_total ~ 
                          s(bin_below_10, bs = "cr") +
                          s(bin_10_15, bs = "cr") +
                          s(bin_15_20, bs = "cr") +
                          s(bin_20_25, bs = "cr") +
                          s(bin_25_30, bs = "cr") +
                          s(bin_above_30, bs = "cr", k = 20) +
                          month_int +
                          mat_age_at_int_scaled + 
                          mat_cur_preg +
                          mat_edu_level +
                          ses_wealth_bi +
                          ses_caste_3 + 
                          ses_religion_3 +
                          ses_access_issue_distance +
                          meta_rural,
                        #   meta_dist_name,
                          data = df_hv, 
                        #   family = poisson(link = "log"), 
                          method = "fREML",
                          select = TRUE)
source("paths-mac.R")

summary(gam_model_hv)
gam.check(gam_model_hv)
plot_gam <- plot.gam(gam_model_hv, seWithMean = TRUE, pages = 1, scheme = 1, shade = TRUE)
ggsave(here(path_project, "outputs", "figures", "hv_gam_model.png"), plot_gam, width = 8, height = 12, dpi = 600)
