rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, here)
pacman::p_load(parallel, future, furrr, doParallel, foreach, future.apply)
library(scales)
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

# Create a named vector to map formula prefixes to dataframes
df_map <- c(
  dv_no_four_anc_bi = "df_anc90",
  dv_no_pnc_7 = "df_pnc7",
  dv_no_pnc_30 = "df_pnc30"
)

# Function to get the appropriate dataframe based on formula name
get_df <- function(dv_name) {
  # prefix <- substr(fmla_name, 1, 7)
  # print(prefix)
  df_name <- df_map[dv_name]
  dim(df_name)
  if (is.na(df_name)) stop("Unknown formula prefix")
  get(df_name, envir = .GlobalEnv)
}

# Set up parallel processing
num_cores <- detectCores() - 5  # Use all but one core
registerDoParallel(cores = num_cores)

# Run the models in parallel
results <- foreach(fmla = list_fmlas, .packages = c("lme4")) %dopar% {
  df <- get_df(as.character(fmla)[2])  # Get the appropriate dataframe
  tryCatch({
    glmer(fmla, data = df, family = poisson(link = "log"))
  }, error = function(e) {
    list(error = e, formula = fmla)
  })
}

# Stop parallel processing
stopImplicitCluster()

# Process results
for (i in seq_along(results)) {
  if (inherits(results[[i]], "glmerMod")) {
    cat("Model", i, "successful\n")
    # Add any additional processing for successful models
  } else {
    cat("Model", i, "failed:", results[[i]]$error$message, "\n")
    # Add any additional error handling
  }
}

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
saveRDS(all_model_outputs, here(path_processed, "2.1-models-no-interaction-comp-age.rds"))
print("Finished saving all models")
print(Sys.time())


# Not run: Single model

model_null <- glmer(dv_not_met_prov_last_3mo ~ 1 + (1 | meta_dist_name/meta_psu) , data = df_test, family = poisson(link = "log"))
performance::icc(model_null)

model <- glmer(dv_not_met_prov_last_3mo ~ count_d_consec_days_cutoff_abs_30 +
                    ses_religion_3 + ses_caste_3 + ses_wealth_bi + 
                    ses_access_issue_distance +
                    mat_age_at_birth_scaled + 
                    month_birth + 
                    (1 | meta_dist_name/meta_psu),
            data = df_test, 
            family = poisson(link = "log"))

View(broom.mixed::tidy(model, effects = "fixed") |> filter(term != "(Intercept)"))

View(broom::tidy(model, exp = T) |> head())
hist(df_test$count_hw_cutoff_abs_30_3d)
head(df_test)
summary(df_test |> select(starts_with("count")))


