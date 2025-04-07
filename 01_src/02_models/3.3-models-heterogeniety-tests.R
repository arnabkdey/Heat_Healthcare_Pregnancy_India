# --------------------
# @project: Heat and healthcare contact during pregnancy in India
# @author: Arnab K. Dey,  arnabxdey@gmail.com 
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script analyzes effect heterogeneity by comparing model coefficients across different population subgroups.
# @date: Dec 12, 2024

rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, here)
pacman::p_load(parallel, future, furrr, doParallel, foreach, future.apply)
# library(scales)
library(merDeriv)
source("paths.R")

# Read ----
df_paper <- readRDS(here(path_project, "processed-data", "2.3-final-hv-data-6mo.rds"))

# Load Function ---- 
source(here("1-scripts", "6.3-function-to-plot-models.R"))
source(here("1-scripts", "6.4-function-to-calc-cis.R"))

# Read all models ----
models_all <- readRDS(here(path_project, "processed-data", "3.2-list-models-6mo.rds"))
names(models_all)

# Create dataframes for plotting ----
varlist_exp <- c("exp_bin_below_10_10", 
                 "exp_bin_10_15_10",
                 "exp_bin_25_30_10", 
                 "exp_bin_above_30_10")

## Full model ----
df_full <- compare_model_coefficients(models_all[["model_full"]], models_all[["model_full"]], varlist_exp)
df_full <- df_full |>
  rename("coef" = `Model 1_estimate`,
        "lci" = `Model 1_conf.low`,
        "uci" = `Model 1_conf.high`
  ) |>
  select(term, coef, lci, uci)

## Relevel factors
df_full$term <- factor(df_full$term, levels = c("Above 30 0C", "25-30 0C", "20-25 0C", "10-15 0C",  "Below 10 0C"))
head(df_full)

## Dataset for EM Models ----
df_rural_urban <- compare_model_coefficients(models_all[["model_rural"]], models_all[["model_urban"]], varlist_exp, "Rural", "Urban")
df_access <- compare_model_coefficients(models_all[["model_access_yes"]], models_all[["model_access_no"]], varlist_exp, "big-prob", "not-big-prob")
df_wealth <- compare_model_coefficients(models_all[["model_wealth_poor"]], models_all[["model_wealth_rich"]], varlist_exp, "poorer", "richer")
df_edu <- compare_model_coefficients(models_all[["model_edu_no"]], models_all[["model_edu_yes"]], varlist_exp, "no-education", "some-education") 


# Function for wald test ----
func_z_test <- function(df) {
        ## Z test
        z_test <- function(b1, se1, b2, se2) {
        z <- (b1 - b2) / sqrt(se1^2 + se2^2)
        print(z)
        p <- 2 * (1 - pnorm(abs(z)))
        return(c(z = z, p = p))
        }

        ## Apply the Z-test to each pair of coefficients
        df_z <- data.frame(z = numeric(nrow(df)), p = numeric(nrow(df)))
        for (i in 1:nrow(df)) {
        term <- df$term[i]
        b1 <- as.numeric(df[i,2])
        print(b1)
        se1 <- as.numeric(df[i,4])
        print(se1)
        b2 <- as.numeric(df[i,3])
        print(b2)
        se2 <- as.numeric(df[i,5])
        print(se2)  
        results <- z_test(b1, se1, b2, se2)
        df_z[i,] <- results
        }
    return(df_z)
}

# Apply the Z-test function ----
df_z_rural_urban <- func_z_test(df_rural_urban)
df_z_access <- func_z_test(df_access)
df_z_wealth <- func_z_test(df_wealth)
df_z_edu <- func_z_test(df_edu)

# Save ----
df_z_rural_urban |> write_csv(here(path_project, "outputs", "models", "stata-models", "z_test_rural_urban.csv"))
df_z_access |> write_csv(here(path_project, "outputs", "models", "stata-models", "z_test_access.csv"))
df_z_wealth |> write_csv(here(path_project, "outputs", "models", "stata-models", "z_test_wealth.csv"))
df_z_edu |> write_csv(here(path_project, "outputs", "models", "stata-models", "z_test_edu.csv"))

