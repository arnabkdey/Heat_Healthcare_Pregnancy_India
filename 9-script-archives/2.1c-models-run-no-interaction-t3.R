rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, here)
pacman::p_load(parallel, future, furrr, doParallel, foreach, future.apply)
library(scales)

# Read datasets ----
## Final paper dataset
path_processed <- here("2-data", "2.2-processed-data")
df_paper_final <- read.fst(here(path_processed, "1.7-final-data-for-paper-preg_t3.fst"))
print("finished loading")
print(Sys.time())

# Process datasets ----
## Scale all variables to IQR ----
input_vec <- grep("^sum", colnames(df_paper_final), value = T)
for (var in input_vec) {
  df_paper_final[[paste0(var, "_scale_iqr")]] <- rescale_mid(df_paper_final[[var]], mid = median(df_paper_final[[var]]), range = IQR(df_paper_final[[var]]))
}

# Specify varlist, formulas, and stratified datasets ----
## Combination of varlists -----
### For potential confounders -----
varlist_cov_base_rc <- c("rural", "mat_age_grp", "mat_edu_level", "hh_wealth_bi", "hh_caste_bi", "hh_religion_tri")
 
### For exposure variables -----
varlist_exp_wb_all <- grep("iqr$", names(df_paper_final), value = TRUE)

# Run the models and save outputs ----

## Register parallel backend
no_cores <- detectCores() - 4
registerDoParallel(cores = no_cores)
## Use foreach to iterate over exposures in parallel
print(Sys.time())

vec_dep_var <- c("any_preg_comp_sum", "any_del_comp_sum")

# Helper function to construct and run models
run_model <- function(dep_var, exposure, covariate_list, data) {
  formula_string <- paste(dep_var, "~", exposure, "+", 
                          paste(covariate_list, collapse = " + "),
                          "+ (1 | clim_zone_short)")
  print(formula_string)
  model <- lme4::glmer(as.formula(formula_string), data = data, family = poisson(link = "log"))
  # return(model)
}

# Initialize an empty list to store all model outputs
all_model_outputs <- list()

print(Sys.time())

# Nested foreach loops: outer loop for dependent variables, inner loop for exposures
all_model_outputs <- foreach(dep_var = vec_dep_var, .packages = c("lme4", "broom.mixed"), .combine = 'c') %:%
  foreach(exposure = varlist_exp_wb_all, .combine = 'c') %dopar% {
    model <- run_model(dep_var, exposure, varlist_cov_base_rc, df_paper_final)
    setNames(list(model), paste(dep_var, exposure, sep = "-"))
  }

length(all_model_outputs)
names(all_model_outputs)

# Save the list as an RDS object
saveRDS(all_model_outputs, here(path_processed, "2.1c-models-no-interaction-comp-t3.rds"))
print("Finished saving all models")
print(Sys.time())
