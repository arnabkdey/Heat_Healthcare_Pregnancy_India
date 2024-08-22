# This code takes about 5 hours to run on a 32 core machine with 128 GB of RAM

# Load Libraries ---- 
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, here)
pacman::p_load(parallel, future, furrr, doParallel, foreach, future.apply)
rm(list = ls())

# Read dataset ----
path_processed <- here("2-data", "2.2-processed-data")
df_paper_final <- read.fst(here(path_processed, "1.7-final-data-for-paper-preg_9.fst"))
tabyl(df_paper_final$mat_age_grp)
print("finished loading")
print(Sys.time())

# Specify varlist, formulas, and stratified datasets ----
## Covariates
varlist_cov_base <- c("rural", "mat_age_grp", "mat_edu_level", "hh_wealth_bi", "hh_caste_bi", "hh_religion_tri")
### Interaction variables
varlist_interaction <- c("mat_age_grp", "hh_wealth_bi", "hh_caste_bi", "hh_religion_tri")
### For exposure variables -----
varlist_exp_tot <- c("sum_mag_abs_30_scale_iqr")
### Define your outcome variable
vec_outcome_var <- c("any_preg_comp_sum", "any_del_comp_sum")

# Run Interaction models ----
## Initialize an empty list to store the formulas
formulas_list <- list()

## Loop through each exposure variable and interaction term to generate formulas
for (outcome_var in vec_outcome_var) {
  for (var_int in varlist_interaction) {
    varlist_cov_current <- setdiff(varlist_cov_base, var_int)
      for (exp_var in varlist_exp_tot) {
        # Create a list of covariates
        # Construct the formula for this exposure variable including interactions
        formula <- as.formula(paste(outcome_var, "~", 
                                      paste(paste0(exp_var, "*", var_int),
                                        "+", paste(varlist_cov_current, collapse = " + "), 
                                        "+ (1 | clim_zone_short)")))
        # Store the formula in the list
        formulas_list[[paste(outcome_var, exp_var, var_int, sep = "_")]] <- formula
    }
  }
}
names(formulas_list) <- gsub("any_|_sum_sum_mag|_comp|_scale_iqr|_bi|_mat|_hh|_30", "", names(formulas_list))
print("finished generating formulas")
# Run the models in parrallel ----

# ## Register parallel backend
no_cores <- detectCores() - 18
registerDoParallel(cores = no_cores)

# Use for_each to run the models in parallel
print(Sys.time())
model_outputs <- foreach(fmla = formulas_list, .combine = c) %dopar% {
  print(paste0("Now processing", fmla))
  print(Sys.time())
  model <- lme4::glmer(formula = as.formula(fmla), data = df_paper_final, family = poisson(link = "log"))
  return(model)
}

names(model_outputs) <- names(formulas_list)

# Save the list as an RDS object
saveRDS(model_outputs, here(path_processed, "2.3-models-interactions.rds"))
print("finished saving all models")
print(Sys.time())

