rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, here)
library(survival)
library(doParallel)
library(foreach)


# Read datasets ----
path_processed <- here("2-data", "2.2-processed-data")
df_merged_pnc_b <- read_fst(here(path_processed, "1.7-df-merged-b.fst"))
df_merged_pnc_m <- read_fst(here(path_processed, "1.7-df-merged-m.fst"))
df_merged_s2s <- read_fst(here(path_processed, "1.7-df-merged-s2s.fst"))

print("finished loading")
print(Sys.time())

# Create vectors ----
## Create vector for dataframes ----
vec_df <- c("df_merged_pnc_b", "df_merged_pnc_m", "df_merged_s2s")

## Create vector for dependent variables ----
vec_dep_var <- c("dv_pnc_b", "dv_pnc_m", "dv_s2s")

## Create vectors for exposures ----

## For absolute variables -----
varlist_exp_wb_abs <- c(
  "hotday_wb_30", "hw_wb_30_2d", "hw_wb_30_3d", "hw_wb_30_5d",
  "hotday_wb_31", "hw_wb_31_2d", "hw_wb_31_3d", "hw_wb_31_5d",
  "hotday_wb_32", "hw_wb_32_2d", "hw_wb_32_3d", "hw_wb_32_5d"
)

## For ntile variables based on doy -----
varlist_exp_wb_ntile_doy <- c(
  "hotday_wb_90_doy", "hw_wb_90_doy_2d", "hw_wb_90_doy_3d", "hw_wb_90_doy_5d",
  "hotday_wb_95_doy", "hw_wb_95_doy_2d", "hw_wb_95_doy_3d", "hw_wb_95_doy_5d",
  "hotday_wb_97_doy", "hw_wb_97_doy_2d", "hw_wb_97_doy_3d", "hw_wb_97_doy_5d"
)

## For ntile variables based on harmonic -----
varlist_exp_wb_ntile_harmo <- c(
  "hotday_wb_90_harmo", "hw_wb_90_harmo_2d", "hw_wb_90_harmo_3d", "hw_wb_90_harmo_5d",
  "hotday_wb_95_harmo", "hw_wb_95_harmo_2d", "hw_wb_95_harmo_3d", "hw_wb_95_harmo_5d",
  "hotday_wb_97_harmo", "hw_wb_97_harmo_2d", "hw_wb_97_harmo_3d", "hw_wb_97_harmo_5d"
)

## Combine all exposure variables -----
varlist_exp_wb_all <- c(varlist_exp_wb_ntile_doy, varlist_exp_wb_ntile_harmo, varlist_exp_wb_abs)


# Run the models and save outputs ----
# Helper function to construct and run models
run_model <- function(dep_var, exposure, data) {
  formula_string <- paste(dep_var, "~", exposure, "+", 
                          "(1 | psu:ID_grp)")
  print(formula_string)
  data$psu <- as.factor(data$psu)
  model <- lme4::glmer(as.formula(formula_string), data = data, family = binomial)
  # return(model)
}

## Register parallel backend
no_cores <- detectCores() - 18
registerDoParallel(cores = no_cores)
## Use foreach to iterate over exposures in parallel
print(Sys.time())


## For PNC-mother
## Initialize an empty list to store all model outputs
all_model_outputs_pnc_m <- list()

## Run Nested loops
all_model_outputs_pnc_m <- foreach(exposure = varlist_exp_wb_all, .combine = 'c') %dopar% {
                              model <- run_model("dv_pnc_m", exposure, df_merged_pnc_m)
                              setNames(list(model), paste0("pnc_m-", exposure))
                            }

print("finished running PNC mother models")
## For PNC-baby
all_model_outputs_pnc_b <- list()
all_model_outputs_pnc_b <- foreach(exposure = varlist_exp_wb_all, .combine = 'c') %dopar% {
                              model <- run_model("dv_pnc_b", exposure, df_merged_pnc_b)
                              setNames(list(model), paste0("pnc_b-", exposure))
                            }

print("finished running PNC baby models")
## For S2S
all_model_outputs_s2s <- list()
all_model_outputs_s2s <- foreach(exposure = varlist_exp_wb_all, .combine = 'c') %dopar% {
                              model <- run_model("dv_s2s", exposure, df_merged_s2s)
                              setNames(list(model), paste0("s2s-", exposure))
                            }
print("finished running S2S models")

# After execution, stop the cluster
stopImplicitCluster()


# Save the lists as an RDS objects
saveRDS(all_model_outputs_pnc_b, here(path_processed, "2.5-models-unwt-case-crossover-pnc-b-psu.rds"))
saveRDS(all_model_outputs_pnc_m, here(path_processed, "2.5-models-unwt-case-crossover-pnc-m-psu.rds"))
saveRDS(all_model_outputs_s2s, here(path_processed, "2.5-models-unwt-case-crossover-s2s-psu.rds"))

print("Finished saving all models")
print(Sys.time())
