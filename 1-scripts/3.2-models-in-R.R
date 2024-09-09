rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, here)
pacman::p_load(parallel, future, furrr, doParallel, foreach, future.apply)
# library(scales)
library(merDeriv)
source("paths-mac.R")

# Read ----
df_paper <- readRDS(here(path_project, "processed-data", "2.3-final-hv-data-6mo.rds"))
df_paper$month_int_fac <- as.factor(df_paper$month_int)

# Load Function ---- 
source(here("1-scripts", "6.4-function-to-calc-cis.R"))

# Create formulae ----
## Variables ----
varlist_exp <- c("exp_bin_below_10_10", 
                 "exp_bin_10_15_10",
                 "exp_bin_25_30_10", 
                 "exp_bin_above_30_10")

varlist_fixed <- c("ses_wealth_bi", 
                   "ses_access_issue_distance", 
                   "mat_edu_level", 
                   "meta_rural", 
                   "month_int_fac")

## Formulae ----
fmla_full <- as.formula(paste("dv_no_contact_3mo ~ ", 
                              paste(varlist_exp, collapse = " + "), " + ", 
                              paste(varlist_fixed, collapse = " + "), " + ", 
                              "(1 | meta_state_name)")) 

fmla_rural <- as.formula(paste("dv_no_contact_3mo ~ ", 
                               paste(varlist_exp, collapse = " + "), " + ", 
                               paste(setdiff(varlist_fixed, "meta_rural"), collapse = " + "), " + ", 
                               "(1 | meta_state_name)"))

fmla_wealth <- as.formula(paste("dv_no_contact_3mo ~ ", 
                                paste(varlist_exp, collapse = " + "), " + ", 
                                paste(setdiff(varlist_fixed, "ses_wealth_bi"), collapse = " + "), " + ", 
                                "(1 | meta_state_name)"))

fmla_access <- as.formula(paste("dv_no_contact_3mo ~ ", 
                                paste(varlist_exp, collapse = " + "), " + ", 
                                paste(setdiff(varlist_fixed, "ses_access_issue_distance"), collapse = " + "), " + ", 
                                "(1 | meta_state_name)"))


# Models ----
## Full model ----
model_full <- glmer(fmla_full, 
                      data = df_paper,                         
                      family = binomial(link = "logit"))

summary(model_full)                      

# View(broom.mixed::tidy(model_full, effects = "fixed", conf.int = TRUE) |> 
#         filter(str_detect(term, "^exp_")) |> 
#         mutate(estimate = exp(estimate), conf.low = exp(conf.low), conf.high = exp(conf.high)) |>
#         select(term, estimate, p.value, conf.low, conf.high))

## Rural and urban model ----
model_rural <- glmer(fmla_rural, 
                      data = df_paper,
                      subset = meta_rural == "rural",          
                      family = binomial(link = "logit"))

model_urban <- glmer(fmla_rural, 
                      data = df_paper,
                      subset = meta_rural == "urban",          
                      family = binomial(link = "logit"))

## Rich / poor model ----
model_wealth_rich <- glmer(fmla_wealth, 
                      data = df_paper,
                      subset = ses_wealth_bi == "rich",
                      family = binomial(link = "logit"))

model_wealth_poor <- glmer(fmla_wealth, 
                      data = df_paper,
                      subset = ses_wealth_bi == "poor",
                      family = binomial(link = "logit"))


## Access issue models ----
model_access_no <- glmer(fmla_access, 
                      data = df_paper,
                      subset = ses_access_issue_distance == "not-a-big-prob",
                      family = binomial(link = "logit"))

model_access_yes <- glmer(fmla_access,
                        data = df_paper,
                        subset = ses_access_issue_distance == "big-problem",
                        family = binomial(link = "logit"))


# Save all objects starting with model as an RDS ----
## Get names of all objects that start with "model_"
model_names <- ls(pattern = "^model_")
## Create a list of the actual model objects
list_models <- mget(model_names)
# names(list_models)

## Save the list of models
saveRDS(list_models, here(path_project, "processed-data", "list-models-6mo.rds"))
