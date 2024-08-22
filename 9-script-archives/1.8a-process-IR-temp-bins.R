pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive, here)
# devtools::install_github("axdey/climExposuR")
library(climExposuR)

# Read datasets -----
## Long-term WBGT data ----
df_lt_vars_2014 <- read_fst(here(path_project, "processed-data", "1.5.1-df_psu_wbgt_2014.fst"), as.data.table = T)
df_lt_vars_2014$psu <- as.factor(df_lt_vars_2014$psu)

### Address missing wbgt data ----
df_missing <- df_lt_vars_2014 |> filter(is.na(max_temp_wb))
tabyl(df_missing, psu) # we see that some PSUs dont have temp data at all
# Drop these PSUs
psus_missing <- unique(df_lt_vars_2014 |> filter(is.na(max_temp_wb)) |> pull(psu))
df_lt_vars_2014 <- df_lt_vars_2014 |> filter(!psu %in% psus_missing)
sum(is.na(df_lt_vars_2014)) # no missing values

## Health datasets ----
df_list <- readRDS(here(path_project, "processed-data", "1.4-list-of-processed-datasets.rds"))

# Calculate bins ----
## Last 3 months ANC ----
names(df_list)
df_anc_3mo <- df_list[["df_anc_last_3mo"]]
df_anc_3mo$psu <- df_anc_3mo$meta_psu
df_anc_3mo_bins <- climExposuR::func_calc_bins_period(df_anc_3mo, df_lt_vars_2014, 
                                "dob", add_days = NULL, subtract_days = 90, 
                                psu_var = "psu", clim_var = "max_temp_wb")  

head(df_anc_3mo_bins)

# Scale bins ----
df_anc_3mo_bins <- df_anc_3mo_bins |> 
  mutate(bin_below_10_scaled = bin_below_10/10,
            bin_10_20_scaled = bin_10_20/10,
            bin_20_30_scaled = bin_20_30/10,
            bin_above_30_scaled = bin_above_30/10)

# Run models ----
## ANC last 3mo ----
### Fixed effect model with interaction
model_anc_3mo_bins_fe_int <- glm(dv_not_met_prov_last_3mo ~ bin_below_10_scaled + bin_20_30_scaled + bin_above_30_scaled + 
                        season_birth*meta_state_name, 
                        data = df_anc_3mo_bins,
                        family = poisson(link = "log"))
View(broom::tidy(model_anc_3mo_bins_fe_int, exp = T) |> head())

### Fixed effect model without interaction

### Random effect model
#### at clim_zone level 
model_anc_3mo_bins_re_clim <- glmer(dv_not_met_prov_last_3mo ~ bin_below_10_scaled + bin_20_30_scaled + bin_above_30_scaled + 
                        season_birth + (1 | clim_zone_short), 
                        data = df_anc_3mo_bins,
                        family = poisson(link = "log"))


View(broom.mixed::tidy(model_anc_3mo_bins_re_clim, effects = "fixed") |> filter(term != "(Intercept)"))

#### at district level 
model_anc_3mo_bins_re_dist <- glmer(dv_not_met_prov_last_3mo ~ bin_below_10_scaled + bin_20_30_scaled + bin_above_30_scaled + 
                        season_birth + (1 | meta_dist_name), 
                        data = df_anc_3mo_bins,
                        family = poisson(link = "log"))

View(broom.mixed::tidy(model_anc_3mo_bins_re_dist, effects = "fixed") |> filter(term != "(Intercept)"))

#### at state level
model_anc_3mo_bins_re_state <- glmer(dv_not_met_prov_last_3mo ~ bin_below_10_scaled + bin_20_30_scaled + bin_above_30_scaled + 
                        season_birth + (1 | meta_state_name), 
                        data = df_anc_3mo_bins,
                        family = poisson(link = "log"))
View(broom.mixed::tidy(model_anc_3mo_bins_re_state, effects = "fixed") |> filter(term != "(Intercept)"))
#### at psu level
model_anc_3mo_bins_re_psu <- glmer(dv_not_met_prov_last_3mo ~ bin_below_10_scaled + bin_20_30_scaled + bin_above_30_scaled + 
                        season_birth + (1 | psu), 
                        data = df_anc_3mo_bins,
                        family = poisson(link = "log"))

View(broom.mixed::tidy(model_anc_3mo_bins_re_psu, effects = "fixed") |> filter(term != "(Intercept)"))
