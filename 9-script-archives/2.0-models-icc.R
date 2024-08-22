pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive, here)
library(performance)

# Read datasets ----
rm(list = ls())
## Final paper dataset
path_processed <- here("2-data", "2.2-processed-data")
df_paper_final <- readRDS(here(path_processed, "1.6-final-data-for-paper.rds"))
colnames(df_paper_final)
print(Sys.time())

# Model
model_dist <- lme4::glmer(no_pnc_m ~ 1 + (1 | dist_code), data = df_paper_final, family = binomial)
icc_dist <- performance::icc(model_dist)

model_state <- lme4::glmer(no_pnc_m ~ 1 + (1 | state_name), data = df_paper_final, family = binomial)
icc_state <- performance::icc(model_state)

model_psu <- lme4::glmer(no_pnc_m ~ 1 + (1 | psu_fac), data = df_paper_final, family = binomial)
icc_psu <- performance::icc(model_psu)
