# ---------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @author: Arnab K. Dey, Anna Dimitrova
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script extracts the odds ratios and RERI from the effect modification models for the paper.
# @date: March 2025

# load libraries ----
rm(list = ls())
pacman::p_load(dplyr, janitor, data.table, fst, openxlsx, here)

# set paths ----
source(here("paths.R"))

# source functions ----
source(here("01_src", "02_models", "utils", "function_to_extract_EM_results.R"))

# load data ----
model_results_list <- readRDS(here(
  path_outputs, "models", "effect_modification", 
  "gee_interaction_models.rds"))

# extract results ----
path_out <- here(path_outputs, "models", 
  "effect_modification")

process_models(model_results_list, path_output = path_out)