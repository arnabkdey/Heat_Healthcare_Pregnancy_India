rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, openxlsx, googledrive, here, beepr, Hmisc)
source(here(".Rprofile"))


# Create Varlists ----
## Varlist for exposure ----XS
varlist_exp_bins_anc_pnc30 <- c("exp_bin_below_10_10", "exp_bin_20_30_10", "exp_bin_above_30_10")
varlist_exp_bins_pnc7 <- c("exp_bin_below_10", "exp_bin_20_30", "exp_bin_above_30")

varlist_exp_descr_anc_pnc30 <- c("exp_sum_ident_abs_30_10", "exp_count_d_consec_days_cutoff_abs_30", "exp_count_consec_days_cutoff_abs_30_10", "exp_sum_mag_abs_30_sd")
varlist_exp_descr_pnc7 <- c("exp_sum_ident_abs_30", "exp_count_d_consec_days_cutoff_abs_30", "exp_count_consec_days_cutoff_abs_30", "exp_sum_mag_abs_30_sd")

## Varlist for covariates ----
varlist_cov <- c("ses_caste_3", "ses_religion_3", "ses_wealth_bi", "mat_age_at_birth_scaled", "meta_rural", "ses_access_issue_distance", "season_birth")


# Create Formulas ----
list_fmlas <- list()

## For ANC_90 ----
### Temperature Bins ----
f_anc90_bin_glmer <- as.formula(paste("dv_not_met_prov_last_3mo", "~", 
                                      paste(varlist_exp_bins_anc_pnc30, collapse = " + "), "+", 
                                      paste(varlist_cov, collapse = " + "), "+", 
                                      " (1 | meta_dist_name/meta_psu)"))

list_fmlas[["f_anc90_bin_glmer"]] <- f_anc90_bin_glmer

### Descriptive exposures ----
for (exp in varlist_exp_descr_anc_pnc30) {
    fmla_name <- paste0("f_anc90_", exp, "_glmer")
    fmla_value <- as.formula(paste("dv_not_met_prov_last_3mo", "~", 
                                    exp, "+", 
                                    paste(varlist_cov, collapse = " + "), "+", 
                                    " (1 | meta_dist_name/meta_psu)"))
    list_fmlas[[fmla_name]] <- fmla_value
}

names(list_fmlas)
list_fmlas[["f_anc90exp_sum_mag_abs_30_sd_glmer"]]

## For PNC_7 ----
### Temperature Bins ----
f_pnc7_bin_glmer <- as.formula(paste("dv_no_pnc_7", "~", 
                                      paste(varlist_exp_bins_pnc7, collapse = " + "), "+", 
                                      paste(varlist_cov, collapse = " + "), "+", 
                                      " (1 | meta_dist_name/meta_psu)"))

list_fmlas[["f_pnc07_bin_glmer"]] <- f_pnc7_bin_glmer

### Descriptive exposures ----
for (exp in varlist_exp_descr_pnc7) {
    fmla_name <- paste0("f_pnc07_", exp, "_glmer")
    fmla_value <- as.formula(paste("dv_no_pnc_7", "~", 
                                    exp, "+", 
                                    paste(varlist_cov, collapse = " + "), "+", 
                                    " (1 | meta_dist_name/meta_psu)"))
    list_fmlas[[fmla_name]] <- fmla_value
}

names(list_fmlas)

## For PNC_30 ----
### Temperature Bins ----
f_pnc30_bin_glmer <- as.formula(paste("dv_no_pnc_30", "~", 
                                      paste(varlist_exp_bins_anc_pnc30, collapse = " + "), "+", 
                                      paste(varlist_cov, collapse = " + "), "+", 
                                      " (1 | meta_dist_name/meta_psu)"))

list_fmlas[["f_pnc30_bin_glmer"]] <- f_pnc30_bin_glmer

### Descriptive exposures ----
for (exp in varlist_exp_descr_anc_pnc30) {
    fmla_name <- paste0("f_pnc30_", exp, "_glmer")
    fmla_value <- as.formula(paste("dv_no_pnc_30", "~", 
                                    exp, "+", 
                                    paste(varlist_cov, collapse = " + "), "+", 
                                    " (1 | meta_dist_name/meta_psu)"))
    list_fmlas[[fmla_name]] <- fmla_value
}

names(list_fmlas)

# Save all lists as RDS----
saveRDS(list_fmlas, file = here(path_project, "processed-data", "3.1-fmlas.RDS"))