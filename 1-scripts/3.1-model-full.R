rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, here)
pacman::p_load(parallel, future, furrr, doParallel, foreach, future.apply)
# library(scales)
library(merDeriv)
source("paths-mac.R")

# Read ----
df_paper <- readRDS(here(path_project, "processed-data", "2.3-final-hv-data.rds"))

# Load Function ---- 
source(here("1-scripts", "6.4-function-to-calc-cis.R"))

# Variables ----
colnames(df_paper |> dplyr::select(starts_with("dv")))
colnames(df_paper |> dplyr::select(starts_with("exp_")))
colnames(df_paper |> dplyr::select(starts_with("ses_")))
colnames(df_paper |> dplyr::select(starts_with("mat_")))

range(df_paper$sum_mag_abs_30)
range(df_paper$dv_num_visits_home)

# Full model_glmer_full - glmer ----
model_glmer_full <- glmer(dv_no_contact_3mo ~ 
                        exp_bin_below_10_10 + 
                        exp_bin_10_15_10 + 
                        # exp_bin_15_20_10 + 
                        exp_bin_20_25_10 + 
                        exp_bin_25_30_10 + 
                        exp_bin_above_30_10 +
                          ses_wealth_bi +
                          ses_caste_2 +
                          ses_religion_2_hindu +
                          ses_access_issue_distance + 
                        # as.factor(mat_edu_level) +
                        mat_age_at_int_scaled +
                          meta_rural + 
                          month_int + 
                        (1 | meta_state_name), 
                          data = df_paper,                         
                          family = poisson(link = "log"))

beepr::beep(sound = 4)
# View(broom.mixed::tidy(model_glmer_full, effects = "fixed", conf.int = TRUE) |> 
#         filter(str_detect(term, "^exp_")) |> 
#         mutate(estimate = exp(estimate), conf.low = exp(conf.low), conf.high = exp(conf.high)) |>
#         select(term, estimate, p.value, conf.low, conf.high))

# Generate robust CIs ----  
m_vcov <- vcov.glmerMod(model_glmer_full)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glmer_full@beta
tidy_output <- coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column()
# View(tidy_output)

# Save the output as an excel sheet ----
openxlsx::write.xlsx(tidy_output, 
                     file = here(path_project, "outputs", "models", "model-glmer-full.xlsx"), 
                     asTable = T)



