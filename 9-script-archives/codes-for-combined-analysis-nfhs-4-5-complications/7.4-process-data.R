rm(list = setdiff(ls(), c("path_processed_data", "path_raw_data")))
pacman::p_load(tidyverse, data.table, janitor, fst, openxlsx, googledrive, here, beepr, Hmisc)
library(lme4)

# Read merged data
df_merge <- read.fst(here(path_processed_data, "7.3-dhs-5-IR-merged.fst"))
df_merge_proc <- df_merge
# Filter cases for the analysis ----
df_merge_proc <- df_merge_proc |> dplyr::filter(!is.na(m15))
df_merge_proc <- df_merge_proc |> dplyr::filter(midx == 1)
df_merge_proc <- df_merge_proc |> dplyr::filter(v135 == "usual resident")
df_merge_proc <- df_merge_proc |> dplyr::filter(!is.na(caste))


# Create outcomes ----
## Pregnancy complications ----
df_merge_proc <- df_merge_proc |> 
                mutate(dv_comp_vision_bi = ifelse(comp_vision == "yes", 1, 0)) |>
                mutate(dv_comp_convulsions_bi = ifelse(comp_convulsions == "yes", 1, 0)) |> 
                mutate(dv_comp_swelling_bi = ifelse(comp_swelling == "yes", 1, 0))

## Delivery Complications ----
df_merge_proc <- df_merge_proc |> 
                mutate(dv_comp_breech_bi = ifelse(comp_breech == "yes", 1, 0)) |>
                mutate(dv_comp_excessive_bi = ifelse(comp_excessive == "yes", 1, 0)) |>
                mutate(dv_comp_prolonged_bi = ifelse(comp_prolonged == "yes", 1, 0))

summary(df_merge_proc |> select(dv_comp_breech_bi, dv_comp_excessive_bi, dv_comp_prolonged_bi))                

## ANC ----
df_merge_proc <- df_merge_proc |> 
    dplyr::mutate(four_anc_dk = case_when(
      str_detect(m14, "no antenatal visits") ~ "no_anc",
      str_detect(m14, "don't know") ~ "don't know",
      str_detect(m14, "1") ~ "less_than_four",
      str_detect(m14, "2") ~ "less_than_four",
      str_detect(m14, "3") ~ "less_than_four",
      TRUE ~ "four_or_more"
    )) |> 
  dplyr::mutate(dv_no_four_anc_bi = ifelse(four_anc_dk == "four_or_more", 0, 1))

summary(df_merge_proc$dv_no_four_anc_bi)

# Create exposures ----
## Convert all tmax variables to numeric ---- 
vec_tmax <- grep("Tmax", colnames(df_merge_proc), value = T)
for (i in vec_tmax) {
    df_merge_proc[[i]] <- as.numeric(df_merge_proc[[i]])
}

## Create variables full pregnancy ----
df_merge_proc <- df_merge_proc |> 
                ## absolute values
                mutate(Tmax_below_5_full = (Tmax_below_5_trim1 + Tmax_below_5_trim2 + Tmax_below_5_trim3)) |>
                mutate(Tmax_5_10_full = (Tmax_5_10_trim1 + Tmax_5_10_trim2 + Tmax_5_10_trim3)) |> 
                mutate(Tmax_10_15_full = (Tmax_10_15_trim1 + Tmax_10_15_trim2 + Tmax_10_15_trim3)) |>
                mutate(Tmax_15_20_full = (Tmax_15_20_trim1 + Tmax_15_20_trim2 + Tmax_15_20_trim3)) |>
                mutate(Tmax_20_23_full = (Tmax_20_23_trim1 + Tmax_20_23_trim2 + Tmax_20_23_trim3)) |>                
                mutate(Tmax_23_25_full = (Tmax_23_25_trim1 + Tmax_23_25_trim2 + Tmax_23_25_trim3)) |>
                mutate(Tmax_25_28_full = (Tmax_25_28_trim1 + Tmax_25_28_trim2 + Tmax_25_28_trim3)) |>
                mutate(Tmax_28_30_full = (Tmax_28_30_trim1 + Tmax_28_30_trim2 + Tmax_28_30_trim3)) |>
                mutate(Tmax_30_33_full = (Tmax_30_33_trim1 + Tmax_30_33_trim2 + Tmax_30_33_trim3)) |>
                mutate(Tmax_above_33_full = (Tmax_above_33_trim1 + Tmax_above_33_trim2 + Tmax_above_33_trim3)) |>
                ## percentile values
                mutate(Tmax_pc_0_10_full = (Tmax_pc_0_10_trim1 + Tmax_pc_0_10_trim2 + Tmax_pc_0_10_trim3)) |>
                mutate(Tmax_pc_10_20_full = (Tmax_pc_10_20_trim1 + Tmax_pc_10_20_trim2 + Tmax_pc_10_20_trim3)) |>
                mutate(Tmax_pc_20_30_full = (Tmax_pc_20_30_trim1 + Tmax_pc_20_30_trim2 + Tmax_pc_20_30_trim3)) |>
                mutate(Tmax_pc_30_40_full = (Tmax_pc_30_40_trim1 + Tmax_pc_30_40_trim2 + Tmax_pc_30_40_trim3)) |>
                mutate(Tmax_pc_40_50_full = (Tmax_pc_40_50_trim1 + Tmax_pc_40_50_trim2 + Tmax_pc_40_50_trim3)) |>
                mutate(Tmax_pc_50_60_full = (Tmax_pc_50_60_trim1 + Tmax_pc_50_60_trim2 + Tmax_pc_50_60_trim3)) |>
                mutate(Tmax_pc_60_70_full = (Tmax_pc_60_70_trim1 + Tmax_pc_60_70_trim2 + Tmax_pc_60_70_trim3)) |>
                mutate(Tmax_pc_70_80_full = (Tmax_pc_70_80_trim1 + Tmax_pc_70_80_trim2 + Tmax_pc_70_80_trim3)) |>
                mutate(Tmax_pc_80_90_full = (Tmax_pc_80_90_trim1 + Tmax_pc_80_90_trim2 + Tmax_pc_80_90_trim3)) |>
                mutate(Tmax_pc_90_100_full = (Tmax_pc_90_100_trim1 + Tmax_pc_90_100_trim2 + Tmax_pc_90_100_trim3))

### Check variables ----
#### Look at their freq distribution ----
varlist_full <- grep("trim1$", colnames(df_merge_proc), value = T)
varlist_pc <- grep("pc", varlist_full, value = T)
varlist_abs <- setdiff(varlist_full, varlist_pc)

#### Look at the distrubution of non-zero values in each variable
# df_merge_proc |> 
#   select(all_of(varlist_abs)) |> 
#   summarise_all(~sum(. > 0)) |> 
#   gather() |> 
#   ggplot(aes(x = key, y = value)) +
#   geom_bar(stat = "identity") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Aggregate temperature bins ----
### Absolute values ----
df_merge_proc <- df_merge_proc |>
                # Full Pregnancy
                mutate(iv_abs_full_bin1 = (Tmax_below_5_full + Tmax_5_10_full)) |>
                mutate(iv_abs_full_bin2 = (Tmax_10_15_full + Tmax_15_20_full)) |>
                mutate(iv_abs_full_bin3 = (Tmax_20_23_full + Tmax_23_25_full)) |>
                mutate(iv_abs_full_bin4 = (Tmax_25_28_full + Tmax_28_30_full)) |>
                mutate(iv_abs_full_bin5 = (Tmax_30_33_full + Tmax_above_33_full)) |>
                # Trimester 1
                mutate(iv_abs_t1_bin1 = (Tmax_below_5_trim1 + Tmax_5_10_trim1)) |>
                mutate(iv_abs_t1_bin2 = (Tmax_10_15_trim1 + Tmax_15_20_trim1)) |>
                mutate(iv_abs_t1_bin3 = (Tmax_20_23_trim1 + Tmax_23_25_trim1)) |>
                mutate(iv_abs_t1_bin4 = (Tmax_25_28_trim1 + Tmax_28_30_trim1)) |>
                mutate(iv_abs_t1_bin5 = (Tmax_30_33_trim1 + Tmax_above_33_trim1)) |>
                # Trimester 2
                mutate(iv_abs_t2_bin1 = (Tmax_below_5_trim2 + Tmax_5_10_trim2)) |>
                mutate(iv_abs_t2_bin2 = (Tmax_10_15_trim2 + Tmax_15_20_trim2)) |>
                mutate(iv_abs_t2_bin3 = (Tmax_20_23_trim2 + Tmax_23_25_trim2)) |>
                mutate(iv_abs_t2_bin4 = (Tmax_25_28_trim2 + Tmax_28_30_trim2)) |>
                mutate(iv_abs_t2_bin5 = (Tmax_30_33_trim2 + Tmax_above_33_trim2)) |>
                # Trimester 3
                mutate(iv_abs_t3_bin1 = (Tmax_below_5_trim3 + Tmax_5_10_trim3)) |>
                mutate(iv_abs_t3_bin2 = (Tmax_10_15_trim3 + Tmax_15_20_trim3)) |>
                mutate(iv_abs_t3_bin3 = (Tmax_20_23_trim3 + Tmax_23_25_trim3)) |>
                mutate(iv_abs_t3_bin4 = (Tmax_25_28_trim3 + Tmax_28_30_trim3)) |>
                mutate(iv_abs_t3_bin5 = (Tmax_30_33_trim3 + Tmax_above_33_trim3))

#### Check variables ----
##### Check the distribution of the variables 
# varlist_iv_abs_full <- grep("iv_abs", colnames(df_merge_proc), value = T)
# sapply(varlist_iv_abs_full, function(x) range(df_merge_proc[[x]]))
# df_merge_proc |> 
#   select(all_of(varlist_iv_abs_full)) |> 
#   summarise_all(~sum(. > 0)) |> 
#   gather() |> 
#   ggplot(aes(x = key, y = value)) +
#   geom_bar(stat = "identity") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Percentile values ----
df_merge_proc <- df_merge_proc |> 
                # Full Pregnancy                
                mutate(iv_pc_full_bin1 = (Tmax_pc_0_10_full + Tmax_pc_10_20_full)) |>
                mutate(iv_pc_full_bin2 = (Tmax_pc_20_30_full + Tmax_pc_30_40_full)) |>
                mutate(iv_pc_full_bin3 = (Tmax_pc_40_50_full + Tmax_pc_50_60_full)) |>
                mutate(iv_pc_full_bin4 = (Tmax_pc_60_70_full + Tmax_pc_70_80_full)) |>
                mutate(iv_pc_full_bin5 = (Tmax_pc_80_90_full + Tmax_pc_90_100_full)) |>
                # Trimester 1
                mutate(iv_pc_t1_bin1 = (Tmax_pc_0_10_trim1 + Tmax_pc_10_20_trim1)) |>
                mutate(iv_pc_t1_bin2 = (Tmax_pc_20_30_trim1 + Tmax_pc_30_40_trim1)) |>
                mutate(iv_pc_t1_bin3 = (Tmax_pc_40_50_trim1 + Tmax_pc_50_60_trim1)) |>
                mutate(iv_pc_t1_bin4 = (Tmax_pc_60_70_trim1 + Tmax_pc_70_80_trim1)) |>
                mutate(iv_pc_t1_bin5 = (Tmax_pc_80_90_trim1 + Tmax_pc_90_100_trim1)) |>
                # Trimester 2
                mutate(iv_pc_t2_bin1 = (Tmax_pc_0_10_trim2 + Tmax_pc_10_20_trim2)) |>
                mutate(iv_pc_t2_bin2 = (Tmax_pc_20_30_trim2 + Tmax_pc_30_40_trim2)) |>
                mutate(iv_pc_t2_bin3 = (Tmax_pc_40_50_trim2 + Tmax_pc_50_60_trim2)) |>
                mutate(iv_pc_t2_bin4 = (Tmax_pc_60_70_trim2 + Tmax_pc_70_80_trim2)) |>
                mutate(iv_pc_t2_bin5 = (Tmax_pc_80_90_trim2 + Tmax_pc_90_100_trim2)) |>
                # Trimester 3
                mutate(iv_pc_t3_bin1 = (Tmax_pc_0_10_trim3 + Tmax_pc_10_20_trim3)) |>
                mutate(iv_pc_t3_bin2 = (Tmax_pc_20_30_trim3 + Tmax_pc_30_40_trim3)) |>
                mutate(iv_pc_t3_bin3 = (Tmax_pc_40_50_trim3 + Tmax_pc_50_60_trim3)) |>
                mutate(iv_pc_t3_bin4 = (Tmax_pc_60_70_trim3 + Tmax_pc_70_80_trim3)) |>
                mutate(iv_pc_t3_bin5 = (Tmax_pc_80_90_trim3 + Tmax_pc_90_100_trim3))

#### Check variables ----
##### Look at the distribution of the variables
varlist_iv_pc_full <- grep("iv_pc", colnames(df_merge_proc), value = T)

## Scale all variables starting with 'iv'  ----
### Identify all variables starting with 'iv'
varlist_iv <- grep("iv_", colnames(df_merge_proc), value = T)
### Divide all variables by 10
df_merge_proc <- df_merge_proc |> mutate_at(vars(all_of(varlist_iv)), ~./10)
# df_merge_proc <- df_merge_proc |> mutate_at(vars(all_of(varlist_iv)), scale)

# Create covariates ----
## Wealth - Binary -----
df_merge_proc <- df_merge_proc |> mutate(wealth_score = as.numeric(wealth_score))
### Convert wealth score to binary using quantiles 
df_merge_proc <- df_merge_proc |> 
              mutate(ses_wealth_bi = 
                ifelse(wealth_score > quantile(wealth_score, 0.5), "rich", "poor"))
tabyl(df_merge_proc, ses_wealth_bi)

## Religion ----
df_merge_proc <- df_merge_proc |>
  # Religion classification
  mutate(
    ses_religion_club = case_when(
      religion == "hindu" ~ "hindu",
      religion == "muslim" ~ "muslim",
      religion == "christian" ~ "christian",
      TRUE ~ "other"
    )
  ) |>
  # Relevel Religion levels
  mutate(
    ses_religion_club = fct_relevel(ses_religion_club, "hindu", "muslim", "christian", "other")
  ) |>
  # Two level variables for religion
  mutate(
    ses_religion_bi_hindu = if_else(religion == "hindu", "hindu", "not-hindu")
  ) |>
  mutate(
    ses_religion_bi_islam = if_else(religion == "muslim", "muslim", "not-muslim")
  ) |> 
  # Three level variable for religion
  mutate(
    ses_religion_tri = case_when(
      religion == "hindu" ~ "hindu",
      religion == "muslim" ~ "muslim",
      TRUE ~ "other"
      ))

tabyl(df_merge_proc, ses_religion_tri)

## Caste ---- 
df_merge_proc <- df_merge_proc |>
  # Caste classification
  mutate(
    ses_caste_club = case_when(
      caste == "schedule caste" ~ "SC",
      caste == "schedule tribe" ~ "ST",
      caste == "obc" ~ "OBC",
      TRUE ~ "Other"
    )
  ) |>
  # Two level variables for caste
  mutate(
    ses_caste_bi = if_else(caste == "none of them", "General", "SC/ST/OBC")
  ) |> 
  # Three level variable for caste
  mutate(
    ses_caste_tri = case_when(
      caste == "obc" ~ "OBC",
      caste == "none of them" ~ "General",
      TRUE ~ "SC/ST"
    ))

## Parity ----
df_merge_proc <- df_merge_proc |> 
  mutate(ses_parity_bi = ifelse(mat_parity == 1, "Primiparous", "Multiparous")) 

## Season of birth ----
df_merge_proc <- df_merge_proc |> 
    dplyr::mutate(ses_season_birth = case_when(
      fertMo %in% c(1, 2) ~ "winter",
      fertMo %in% c(3, 4, 5) ~ "pre-monsson",
      fertMo %in% c(6, 7, 8, 9) ~ "monsoon",
      fertMo %in% c(10, 11, 12) ~ "post-monsoon")) 

## Age at birth ---- 
df_merge_proc$ses_fertAge <- as.numeric(df_merge_proc$fertAge)

## Education ----
df_merge_proc$ses_edu <- df_merge_proc$edu

## Residence ----
df_merge_proc$ses_residence <- df_merge_proc$residence

## Create a PSU ID variable ----
df_merge_proc <- df_merge_proc |> mutate(psu = paste0(clust, "_", SurveyId))

# Select relevant variables ----
df_merge_proc <- df_merge_proc |> 
                    select(starts_with("comp_"), starts_with("dv_"), starts_with("iv_"), 
                    starts_with("ses_"), psu, clim_zone, region, district)

# subset data for the three complications ---
df_model_vision <- df_merge_proc |> filter(comp_vision != "don't know")
df_model_convulsions <- df_merge_proc |> filter(comp_convulsions != "don't know")
df_model_swelling <- df_merge_proc |> filter(comp_swelling != "don't know")
df_model_breech <- df_merge_proc |> filter(comp_breech != "don't know")
df_model_prolonged <- df_merge_proc |> filter(comp_prolonged != "don't know")
df_model_excessive <- df_merge_proc |> filter(comp_excessive != "don't know")

# Save the datasets as dta files ----
foreign::write.dta(df_merge_proc, here(path_processed_data, "7.4-dhs-5-IR-merged-processed.dta"))
foreign::write.dta(df_model_vision, here(path_processed_data, "7.4-dhs-5-IR-merged-vision.dta"))
foreign::write.dta(df_model_convulsions, here(path_processed_data, "7.4-dhs-5-IR-merged-convulsions.dta"))
foreign::write.dta(df_model_swelling, here(path_processed_data, "7.4-dhs-5-IR-merged-swelling.dta"))
foreign::write.dta(df_model_breech, here(path_processed_data, "7.4-dhs-5-IR-merged-breech.dta"))
foreign::write.dta(df_model_prolonged, here(path_processed_data, "7.4-dhs-5-IR-merged-prolonged.dta"))
foreign::write.dta(df_model_excessive, here(path_processed_data, "7.4-dhs-5-IR-merged-excessive.dta"))

unique(df_merge_proc$region)
