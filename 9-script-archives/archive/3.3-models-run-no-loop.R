rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, here)
pacman::p_load(parallel, future, furrr, doParallel, foreach, future.apply)
library(mgcv)
source(here(".Rprofile"))

# Read ----
## All dataesets ----
list_dfs <- readRDS(here(path_project, "processed-data", "2.3-list-of-scaled-exposures.rds"))

## Formulas ----
list_fmlas <- readRDS(file = here(path_project, "processed-data", "3.1-fmlas.RDS"))
names(list_fmlas)
print("finished reading")
print(Sys.time())

# Create Depvarlist ----
depvarlist_anc_pnc30 <- c("dv_no_four_anc_bi", "dv_no_pnc_7", "dv_no_pnc_30")

# Load necessary libraries
library(doParallel)
library(foreach)
library(lme4)

# Define your dataframes (replace these with your actual dataframe names)
df_anc90 <- list_dfs[["df_anc_90_exp_scaled"]]
dim(df_anc90)
df_pnc7 <- list_dfs[["df_pnc_7_exp_scaled"]]
dim(df_pnc7)
df_pnc30 <- list_dfs[["df_pnc_30_exp_scaled"]]
dim(df_pnc30)


# Run models ----
## ANC90 ----
m_anc90_bin <- glmer(as.formula(list_fmlas[["f_anc90_bin_glmer"]]), data = df_anc90, family = poisson(link = "log"))
print("finished anc-1")
print(Sys.time())
m_anc90_exp_sum_ident_abs_30_10 <- glmer(as.formula(list_fmlas[["f_anc90_exp_sum_ident_abs_30_10_glmer"]]), data = df_anc90, family = poisson(link = "log"))
print("finished anc-2")
print(Sys.time())
m_anc90_exp_count_d_consec_days_cutoff_abs_30 <- glmer(as.formula(list_fmlas[["f_anc90_exp_count_d_consec_days_cutoff_abs_30_glmer"]]), 
                        data = df_anc90, 
                        subset = year > 2017,
                        family = poisson(link = "log"))

print("finished anc-3")
print(Sys.time())

m_anc90_exp_count_consec_days_cutoff_abs_30_10 <- glmer(as.formula(list_fmlas[["f_anc90_exp_count_consec_days_cutoff_abs_30_10_glmer"]]), data = df_anc90, family = poisson(link = "log"))
print("finished anc-4")
print(Sys.time())

m_anc90_exp_sum_mag_abs_30_sd <- glmer(as.formula(list_fmlas[["f_anc90_exp_sum_mag_abs_30_sd_glmer"]]), data = df_anc90, family = poisson(link = "log"))
print("finished anc-5")
print(Sys.time())

## PNC- 7 ----
m_pnc07_bin <- glmer(as.formula(list_fmlas[["f_pnc07_bin_glmer"]]), data = df_pnc7, family = poisson(link = "log"))
print("finished pnc-1")
print(Sys.time())

colnames(df_pnc7 |> select(starts_with("exp_")))

m_pnc07_exp_sum_ident_abs_30 <- glmer(as.formula(list_fmlas[["f_pnc07_exp_sum_ident_abs_30_glmer"]]), data = df_pnc7, family = poisson(link = "log"))
print("finished pnc-2")
print(Sys.time())

m_pnc07_exp_count_d_consec_days_cutoff_abs_30 <- glmer(as.formula(list_fmlas[["f_pnc07_exp_count_d_consec_days_cutoff_abs_30_glmer"]]), data = df_pnc7, family = poisson(link = "log"))
print("finished pnc-3")
print(Sys.time())

m_pnc07_exp_count_consec_days_cutoff_abs_30 <- glmer(as.formula(list_fmlas[["f_pnc07_exp_count_consec_days_cutoff_abs_30_glmer"]]), data = df_pnc7, family = poisson(link = "log"))
print("finished pnc-4")
print(Sys.time())

m_pnc07_exp_sum_mag_abs_30_sd <- glmer(as.formula(list_fmlas[["f_pnc07_exp_sum_mag_abs_30_sd_glmer"]]), data = df_pnc7, family = poisson(link = "log"))
print("finished pnc-5")
print(Sys.time())

## PNC- 30 ----
m_pnc30_bin <- glmer(as.formula(list_fmlas[["f_pnc30_bin_glmer"]]), data = df_pnc30, family = poisson(link = "log"))
print("finished pnc-6")
print(Sys.time())

m_pnc30_exp_sum_ident_abs_30 <- glmer(as.formula(list_fmlas[["f_pnc30_exp_sum_ident_abs_30_10_glmer"]]), data = df_pnc30, family = poisson(link = "log"))
print("finished pnc-7")
print(Sys.time())
m_pnc30_exp_count_d_consec_days_cutoff_abs_30 <- glmer(as.formula(list_fmlas[["f_pnc30_exp_count_d_consec_days_cutoff_abs_30_glmer"]]), data = df_pnc30, family = poisson(link = "log"))
print("finished pnc-8")
print(Sys.time())
m_pnc30_exp_count_consec_days_cutoff_abs_30 <- glmer(as.formula(list_fmlas[["f_pnc30_exp_count_consec_days_cutoff_abs_30_10_glmer"]]), data = df_pnc30, family = poisson(link = "log"))
print("finished pnc-9")
print(Sys.time())
m_pnc30_exp_sum_mag_abs_30_sd <- glmer(as.formula(list_fmlas[["f_pnc30_exp_sum_mag_abs_30_sd_glmer"]]), data = df_pnc30, family = poisson(link = "log"))
print("finished pnc-10")
print(Sys.time())
# Save all objects starting with m_ in a list ----
list_models <- ls(pattern = "m_")
list_models <- mget(list_models)
saveRDS(list_models, file = here(path_project, "processed-data", "3.3-list-of-models.RDS"))


# Single Model ----
df_anc90 <- df_anc90 |> mutate(year = year(dob))
model <- glmer(dv_not_met_prov_last_3mo ~ exp_sum_ident_cutoff_abs_30 +
                # ses_wealth_bi +
                # ses_religion_3 + 
                ses_caste_3 +
                ses_access_issue_distance + 
                mat_age_at_birth + 
                mat_edu_level + 
                month_birth +
                meta_rural +
                (1 | meta_psu),
            data = df_anc90, 
            subset = year > 2017,
            family = binomial(link = "logit"))
summary(model)

model$coefficients[2]

library(mgcv)  # Make sure to load the mgcv package for GAMs
colnames(df_anc90 |> select(starts_with("exp_")))
range(df_anc90$sum_ident_abs_30)
gam_model_anc <- mgcv::gam(dv_not_met_prov_last_3mo ~ s(sum_ident_abs_30) +
                 ses_caste_3 +
                 ses_access_issue_distance + 
                 mat_age_at_birth + 
                 mat_edu_level + 
                 month_birth +
                 meta_rural,
                #  s(meta_psu, bs = "re"),
                 data = df_anc90,
                #  subset = year > 2017,
                 family = binomial(link = "logit"))
plot_anc <- plot(gam_model_anc)
ggsave(here(path_project, "outputs", "anc_sum_ident_abs_30.png"), plot_anc)
# for PNC7

df_pnc7 <- df_pnc7 |> mutate(year = year(dob))
colnames(df_pnc7 |> select(starts_with("exp_")))
range(df_pnc7$exp_sum_ident_abs_30)
model_pnc7_gam <- gam::gam(as.factor(dv_no_pnc_7) ~ s(sum_mag_abs_30, 5),
            data = df_pnc7, 
            family = binomial)

plot_pnc_7_mag <- mgcv::plot.gam(model_pnc7_gam)
ggsave(here(path_project, "outputs", "pnc7_sum_mag_abs_30.png"), plot_pnc_7_mag)
