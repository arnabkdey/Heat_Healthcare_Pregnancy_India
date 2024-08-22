rm(list = setdiff(ls(), c("path_processed_data", "path_raw_data")))
pacman::p_load(tidyverse, data.table, janitor, fst, openxlsx, googledrive, here, beepr, Hmisc)

# Create formula for models --- 
## Varlist for exposure ----
varlist_exp_abs_base <- c("Tmax_below_15", "Tmax_15_20", "Tmax_25_30", "Tmax_above_30")
varlist_exp_pc_base <- c("Tmax_0_10", "Tmax_10_40", "Tmax_60_90", "Tmax_90_100")

## Varlist for covariates ----
# varlist_cov <- c("hh_caste_tri", "hh_religion_tri", "hh_wealth_bi", "mat_parity", "residence", "edu", "fertAge")
varlist_cov <- c("fertAge")

## Varlist for fixed effects ----
# varlist_fixed <- c("season_birth", "region")
varlist_fixed <- c("season_birth", "clim_zone")

## Varlist for trimester ----
varlist_trimester_abs <- c("iv_abs_t1", "iv_abs_t2", "iv_abs_t3", "iv_abs_full")
varlist_trimester_pc <- c("iv_pc_t1", "iv_pc_t2", "iv_pc_t3", "iv_pc_full")

## Depvarlist ----
depvarlist <- c("comp_breech_bi", "comp_prolonged_bi", "comp_excessive_bi", "no_four_anc_bi")


## Create Formulas ----
### GLM 
#### Absolute ----
list_fmlas_glm_abs <- list()
for (i in depvarlist) {
    for (k in varlist_trimester_abs) {
            formula_name <- paste0("fmla_", i, "_", k)
            formula_value <- as.formula(paste(i, "~", 
                                              paste(paste0(k, "_", varlist_exp_abs_base), collapse = " + "), "+", 
                                              paste(varlist_cov, collapse = " + "), "+", 
                                              paste(varlist_fixed, collapse = " + ")))
            assign(formula_name, formula_value)
            list_fmlas_glm_abs[[i]] <- c(list_fmlas_glm_abs[[i]], list(formula_value))
        }
}
#### Percentile ----
list_fmlas_glm_pc <- list()
for (i in depvarlist) {
    for (k in varlist_trimester_pc) {
            formula_name <- paste0("fmla_", i, "_", k)
            formula_value <- as.formula(paste(i, "~", 
                                              paste(paste0(k, "_", varlist_exp_pc_base), collapse = " + "), "+", 
                                              paste(varlist_cov, collapse = " + "), "+", 
                                              paste(varlist_fixed, collapse = " + ")))
            assign(formula_name, formula_value)
            list_fmlas_glm_pc[[i]] <- c(list_fmlas_glm_pc[[i]], list(formula_value))
        }
}


### GLMER
#### Absolute ----
list_fmlas_glmer_abs <- list()

for (i in depvarlist) {
    list_fmlas_glmer_abs[[i]] <- list()  # Initialize the nested list
    for (k in varlist_trimester_abs) {
        formula_name <- paste0("fmla_", i, "_", k)
        formula_value <- as.formula(paste(i, "~", 
                                          paste(paste0(k, "_", varlist_exp_abs_base), collapse = " + "), "+", 
                                          paste(varlist_cov, collapse = " + "), "+", 
                                          paste(varlist_fixed, collapse = " + "), " + (1 | psu)"))
        list_fmlas_glmer_abs[[i]][[formula_name]] <- formula_value
    }
}

# list_fmlas_glmer_abs[["comp_breech_bi"]][[1]]

#### Percentile ----
list_fmlas_glmer_pc <- list()

for (i in depvarlist) {
    list_fmlas_glmer_pc[[i]] <- list()  # Initialize the nested list
    for (k in varlist_trimester_pc) {
        formula_name <- paste0("fmla_", i, "_", k)
        formula_value <- as.formula(paste(i, "~", 
                                          paste(paste0(k, "_", varlist_exp_pc_base), collapse = " + "), "+", 
                                          paste(varlist_cov, collapse = " + "), "+", 
                                          paste(varlist_fixed, collapse = " + "), " + (1 | psu)"))
        list_fmlas_glmer_pc[[i]][[formula_name]] <- formula_value
    }
}

# Save all lists ----
save(list_fmlas_glm_abs, list_fmlas_glm_pc, list_fmlas_glmer_abs, list_fmlas_glmer_pc, file = here(path_processed_data, "7.5-fmlas.RData"))


