# --------------------
# @project: Heat and healthcare contact during pregnancy in India
# @author: Arnab K. Dey,  arnabxdey@gmail.com
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script processes the DHS Individual Recode dataset to create analysis
## variables including healthcare contact, socioeconomic status, and maternal characteristics.
# @date: Dec 12, 2024

# load packages ----
rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive, here)

# set paths ----
source("paths.R")

# Step-1: Identify variables for the paper ----
## Meta variables
varlist_meta <- c("caseid", "v021", "v025", "v023", "sdist", "v024", "v005", "v008a")

## Birth History variables
varlist_birth_history <- c(
  c(paste0("bidx_0", c(1:9))),
  c(paste0("bidx_", c(10:20))),
  c(paste0("b18_0", c(1:9))),
  c(paste0("b18_", c(10:20)))
)

varlist_index_to_birth <- c("midx_1", "midx_2", "midx_3", "midx_4", "midx_5", "midx_6")

## ANC related variables
varlist_anc <- c(
  "m14_1", "m14_2", "m14_3", "m14_4", "m14_5", "m14_6", # number of ANC visits
  "s438_1", "s438_2", "s438_3", "s438_4", "s438_5", "s438_6" # met healthcare provider last 3 months
)

## Delivery related variables
varlist_inst_del <- c(
  "m15_1", "m15_2", "m15_3", "m15_4", "m15_5", "m15_6", # place of delivery
  "m61_1", "m61_2", "m61_3", "m61_4", "m61_5", "m61_6", # time stayed in hospital
  "m17_1", "m17_2", "m17_3", "m17_4", "m17_5", "m17_6", # caesarion section
  "m3a_1", "m3a_2", "m3a_3", "m3a_4", "m3a_5", "m3a_6", # assissted: doctor
  "m3b_1", "m3b_2", "m3b_3", "m3b_4", "m3b_5", "m3b_6", # assissted: anm/nurse/midwife
  "m3c_1", "m3c_2", "m3c_3", "m3c_4", "m3c_5", "m3c_6" # assissted: other health worker
)
## Home visit variables
varlist_home_visit <- c(
  "s359", "s360a", "s360b", "s360c", "s360d",
  "s361", "s363a", "s363b", "s363c", "s363d",
  "s368", "s369", "s370c"
)

## Pregnancy complication variables
varlist_preg_comp <- c(
  "v213", "v214", "v215", "v216", # current pregnancy related vars
  "m47_1", "m47_2", "m47_3", "m47_4", "m47_5", "m47_6", # Daylight vision problem
  "s434_1", "s434_2", "s434_3", "s434_4", "s434_5", "s434_6", # Convulsions w/o fever
  "s435_1", "s435_2", "s435_3", "s435_4", "s435_5", "s435_6" # swelling of legs
)

## delivery complication variables
varlist_del_comp <- c(
  "s441_1", "s441_2", "s441_3", "s441_4", "s441_5", "s441_6", # Breech Presentation
  "s442_1", "s442_2", "s442_3", "s442_4", "s442_5", "s442_6", # Prolonged Labour
  "s443_1", "s443_2", "s443_3", "s443_4", "s443_5", "s443_6" # Excessive Bleeding
)

## Skin to Skin contact
varlist_skin_care <- c(
  "s464_1", "s464_2", "s464_3", "s464_4", "s464_5", "s464_6", # child put to chest
  "s465_1", "s465_2", "s465_3", "s465_4", "s465_5", "s465_6" # Bare Skin
)

### PNC checkup before discharge
varlist_pnc_checkup <- c(
  "s470_1", "s470_2", "s470_3", "s470_4", "s470_5", "s470_6", # PNC checkup on baby before discharge
  "m62_1", "m62_2", "m62_3", "m62_4", "m62_5", "m62_6", # PNC checkup on mother before discharge
  "m66_1", "m66_2", "m66_3", "m66_4", "m66_5", "m66_6", # PNC checkup after discharge
  "m67_1", "m67_2", "m67_3", "m67_4", "m67_5", "m67_6", # Timing of PNC checkup after discharge
  "m68_1", "m68_2", "m68_3", "m68_4", "m68_5", "m68_6", # Who dod PNC checkup after discharge
  "m69_1", "m69_2", "m69_3", "m69_4", "m69_5", "m69_6", # where did PNC checkup take place after discharge
  "s486_1", "s486_2", "s486_3", "s486_4", "s486_5", "s486_6" # PNC checkup in first 10 days after delivery
)

## SES variables
varlist_ses <- c("v012", "v218", "v201", "v106", "v130", 
  "s116", "v190", "v191", "v190a", "v191a", "v135")

## Access related variables
varlist_access <- c("v467d", "v123", "v124", "v125")

## Mass media exposure
varlist_media <- c("v157", "v158", "v159", "v171a")

## Combine the individual varlists
varlist_select <- c(
  varlist_meta, varlist_birth_history, varlist_index_to_birth,
  varlist_anc, varlist_inst_del, varlist_home_visit,
  varlist_preg_comp, varlist_del_comp, varlist_skin_care, varlist_pnc_checkup,
  varlist_ses, varlist_access, varlist_media
)

# Step-2: Read the data ----
## load raw dataset
df_dhs_IR_raw <- haven::read_dta(here(path_dhs_india_2019, "IAIR7EFL.DTA"),
  col_select = all_of(varlist_select)
)
## Convert to factors
df_dhs_IR_raw <- as_factor(df_dhs_IR_raw)
setDT(df_dhs_IR_raw)
nrow(df_dhs_IR_raw) # 724,115

# Step-3: Convert from wide to long -------------------------------------------
df_IR_long_full <- df_dhs_IR_raw |>
  dplyr::select(
    "caseid",
    date_int_cdc = v008a,
    wt_raw = v005,
    meta_psu = v021,
    meta_rural = v025,
    strata = v023,
    meta_dist_name = sdist,
    meta_state_name = v024,
    mat_num_living_children = v218,
    mat_age = v012,
    mat_parity = v201,
    mat_edu_level = v106,
    hh_religion = v130,
    hh_caste = s116,
    hh_wealth_quintile = v190,
    hh_wealth_score = v191,
    hh_wealth_quintile_ru_og = v190a,
    hh_wealth_score_ru_og = v191a,
    v467d, # distance is a big problem in accessing healthcare
    v213, v214, v215, v216,
    s359, s360a, s360b, s360c, s360d, # met ANM/LHV in last 3mo
    s361, s363a, s363b, s363c, s363d, # visited FLW last 3mo
    s368, s369, s370c, # visited health facility in last 3mo
    v123, v124, v125, # personal vehicle related variables
    everything()
  )
nrow(df_IR_long_full) # 724,115
rm(df_dhs_IR_raw)

# Create basic variables ----
## Meta variables ----
df_IR_long_full <- df_IR_long_full |> 
  dplyr::mutate(meta_wt_final = wt_raw / 1000000) |> 
  dplyr::mutate(meta_psu = as.factor(meta_psu)) |>
  dplyr::mutate(meta_rural = as.factor(meta_rural)) |>
  dplyr::mutate(meta_dist_name = as.factor(meta_dist_name)) |>
  dplyr::mutate(meta_state_name = as.factor(meta_state_name))

## Birth related vars ----
df_IR_long_full <- df_IR_long_full |>
  ### Calculate Date of Interview
  dplyr::mutate(doi = as.Date(date_int_cdc, origin = "1900-01-01")) |>
  ### Get Day of the Week, month and year
  #### For DOI
  dplyr::mutate(month_int = lubridate::month(doi)) |>
  dplyr::mutate(year_int = lubridate::year(doi)) |>
  #### For DOB
  dplyr::mutate(year_int = lubridate::year(doi)) |> 
  dplyr::mutate(month_int = lubridate::month(doi)) |>
  dplyr::mutate(season_int = case_when(
    month_int %in% c(1, 2) ~ "winter",
    month_int %in% c(3, 4, 5) ~ "pre-monsson",
    month_int %in% c(6, 7, 8, 9) ~ "monsoon",
    month_int %in% c(10, 11, 12) ~ "post-monsoon"))

## COVID related vars ----
df_IR_long_full <- df_IR_long_full |>
  dplyr::mutate(covid = ifelse(
    doi >= as.Date("2020-03-24"), "post-covid", "pre-covid"))     

# Step-4: Filter cases --------------------------------------------------------
tabyl(df_IR_long_full, v135) |> janitor::adorn_totals("row") # usual resident
tabyl(df_IR_long_full, v213) |> janitor::adorn_totals("row") # currently pregnant
tabyl(df_IR_long_full, v214) |> janitor::adorn_totals("row") # month of pregnancy
ls()

## Filter to usual residents who are in their last trimester pregnancy ----
df_filtered_6mo <- df_IR_long_full |>
  # Filter to women who are in their last trimester of pregnancy
  dplyr::filter(v213 == "yes" & v214 >= 6) |> # 11482 cases remaining
  # Filter to usual residents
  dplyr::filter(v135 == "usual resident") |> # 18,312 cases dropped; 14116060 cases remaining
  # Filter post covid cases
  dplyr::filter(covid == "pre-covid") # 3304 case dropped

nrow(df_filtered_6mo) # 8,178

## Filter to usual residents who are in their last two months of pregnancy ----
df_filtered_7mo <- df_IR_long_full |>
  # Filter to women who are in their last two months of pregnancy
  dplyr::filter(v213 == "yes" & v214 >= 7) |>
  # Filter to usual residents
  dplyr::filter(v135 == "usual resident") |> # 18,312 cases dropped; 14116060 cases remaining
  # Filter post covid cases
  dplyr::filter(covid == "pre-covid")

nrow(df_filtered_7mo) # 5,874

# Step-7: Save the files ------------------------------------------------------
write_fst(df_filtered_6mo, path = here(path_processed, "1.1.1_DHS_IR_filtered_6mo.fst"))
write_fst(df_filtered_7mo, path = here(path_processed, "1.1.1_DHS_IR_filtered_7mo.fst"))
