rm(list = setdiff(ls(), c("path_raw_data", "path_processed_data")))
pacman::p_load(tidyverse, data.table, janitor, fst, openxlsx, googledrive, here, beepr, Hmisc)
library(lme4)

# Read merged data
df_merge <- read.fst(here("2-data", "2.2-processed-data", "7.3-dhs-5-IR-merged.fst"))

# Process datasets for analysis --------
## Filter last birth only
df_merge <- df_merge |> dplyr::filter(!is.na(m15))
df_merge <- df_merge |> dplyr::filter(midx == 1)
df_merge <- df_merge |> dplyr::filter(v135 == "usual resident")
df_merge <- df_merge |> dplyr::filter(!is.na(caste))

tabyl(df_merge, m15)
tabyl(df_merge, midx)
tabyl(df_merge, v135)

tabyl(df_merge, comp_breech, SurveyId)
tabyl(df_merge, comp_excessive, SurveyId)
tabyl(df_merge, comp_prolonged, SurveyId)

## Create binary variables for outcomes
df_merge <- df_merge |> 
                mutate(comp_breech_bi = ifelse(comp_breech == "yes", 1, 0)) |>
                mutate(comp_excessive_bi = ifelse(comp_excessive == "yes", 1, 0)) |>
                mutate(comp_prolonged_bi = ifelse(comp_prolonged == "yes", 1, 0))

## Convert all tmax variables to numeric
vec_tmax <- grep("Tmax", colnames(df_merge), value = T)
for (i in vec_tmax) {
    df_merge[[i]] <- as.numeric(df_merge[[i]])
}

## Create exposures ----
### Trimester specific Tmax
df_merge <- df_merge |> 
                mutate(Tmax_below_15_t1 = (Tmax_below_5_trim1 + Tmax_5_10_trim1 + Tmax_10_15_trim1)/10) |>
                mutate(Tmax_15_20_t1 = (Tmax_15_20_trim1)/10) |>
                mutate(Tmax_20_25_t1 = (Tmax_20_23_trim1 + Tmax_20_23_trim1)/10) |>
                mutate(Tmax_25_28_t1 = (Tmax_25_28_trim1)/10) |>
                mutate(Tmax_28_30_t1 = (Tmax_28_30_trim1)/10) |>
                mutate(Tmax_above_30_t1 = (Tmax_30_33_trim1 + Tmax_above_33_trim1)/10) |>
                # Trim-2
                mutate(Tmax_below_15_t2 = (Tmax_below_5_trim2 + Tmax_5_10_trim2 + Tmax_10_15_trim2)/10) |>
                mutate(Tmax_15_20_t2 = (Tmax_15_20_trim2)/10) |>
                mutate(Tmax_20_25_t2 = (Tmax_20_23_trim2 + Tmax_20_23_trim2)/10) |>
                mutate(Tmax_25_28_t2 = (Tmax_25_28_trim2)/10) |>
                mutate(Tmax_28_30_t2 = (Tmax_28_30_trim2)/10) |>
                mutate(Tmax_above_30_t2 = (Tmax_30_33_trim2 + Tmax_above_33_trim2)/10) |>
                # Trim-3
                mutate(Tmax_below_15_t3 = (Tmax_below_5_trim3 + Tmax_5_10_trim3 + Tmax_10_15_trim3)/10) |>
                mutate(Tmax_15_20_t3 = (Tmax_15_20_trim3)/10) |>
                mutate(Tmax_20_25_t3 = (Tmax_20_23_trim3 + Tmax_20_23_trim3)/10) |>
                mutate(Tmax_25_28_t3 = (Tmax_25_28_trim3)/10) |>
                mutate(Tmax_28_30_t3 = (Tmax_28_30_trim3)/10) |>
                mutate(Tmax_above_30_t3 = (Tmax_30_33_trim3 + Tmax_above_33_trim3)/10)

### For full pregnancy ----
df_merge <- df_merge |> 
                mutate(Tmax_below_15_full = (Tmax_below_15_t1 + Tmax_below_15_t2 + Tmax_below_15_t3)/3) |>
                mutate(Tmax_15_20_full = (Tmax_15_20_t1 + Tmax_15_20_t2 + Tmax_15_20_t3)/3) |>
                mutate(Tmax_20_25_full = (Tmax_20_25_t1 + Tmax_20_25_t2 + Tmax_20_25_t3)/3) |>
                mutate(Tmax_25_28_full = (Tmax_25_28_t1 + Tmax_25_28_t2 + Tmax_25_28_t3)/3) |>
                mutate(Tmax_28_30_full = (Tmax_28_30_t1 + Tmax_28_30_t2 + Tmax_28_30_t3)/3) |>
                mutate(Tmax_above_30_full = (Tmax_above_30_t1 + Tmax_above_30_t2 + Tmax_above_30_t3)/3)



## Create covariates ----
### Wealth - Binary -----
tabyl(df_merge, wealth)
df_merge <- df_merge |>
              mutate(hh_wealth_bi = 
                ifelse(wealth %in% c("richer", "richest"), "rich", "poor"))

tabyl(df_merge, hh_wealth_bi)

### Religion ----
df_merge <- df_merge |>
  # Religion classification
  mutate(
    hh_religion_club = case_when(
      religion == "hindu" ~ "hindu",
      religion == "muslim" ~ "muslim",
      religion == "christian" ~ "christian",
      TRUE ~ "other"
    )
  ) |>
  # Relevel Religion levels
  mutate(
    hh_religion_club = fct_relevel(hh_religion_club, "hindu", "muslim", "christian", "other")
  ) |>
  # Two level variables for religion
  mutate(
    hh_religion_bi_hindu = if_else(religion == "hindu", "hindu", "not-hindu")
  ) |>
  mutate(
    hh_religion_bi_islam = if_else(religion == "muslim", "muslim", "not-muslim")
  ) |> 
  # Three level variable for religion
  mutate(
    hh_religion_tri = case_when(
      religion == "hindu" ~ "hindu",
      religion == "muslim" ~ "muslim",
      TRUE ~ "other"
      ))

tabyl(df_merge, hh_religion_tri)

### Caste ---- 
df_merge <- df_merge |>
  # Caste classification
  mutate(
    hh_caste_club = case_when(
      caste == "schedule caste" ~ "SC",
      caste == "schedule tribe" ~ "ST",
      caste == "obc" ~ "OBC",
      TRUE ~ "Other"
    )
  ) |>
  # Two level variables for caste
  mutate(
    hh_caste_bi = if_else(caste == "none of them", "General", "SC/ST/OBC")
  ) |> 
  # Three level variable for caste
  mutate(
    hh_caste_tri = case_when(
      caste == "obc" ~ "OBC",
      caste == "none of them" ~ "General",
      TRUE ~ "SC/ST"
    ))

### Parity ---------------------------
df_merge <- df_merge |> 
  mutate(mat_parity_bi = ifelse(mat_parity == 1, "Primiparous", "Multiparous")) 

### Other variables from anna's data
tabyl(df_merge, residence)
tabyl(df_merge, age_gr5)
tabyl(df_merge, fertAge)
tabyl(df_merge, edu)


colnames(df_merge)

# Save the dataset
write.fst(df_merge, here("2-data", "2.2-processed-data", "7.4-dhs-5-IR-merged-processed.fst"))

# Check the data ----

### Check whether the OG variables add up
#### for trimester-1
df_test <- df_merge |> select(Tmax_below_5_trim1, Tmax_5_10_trim1, Tmax_10_15_trim1, 
                                Tmax_15_20_trim1, Tmax_20_23_trim1, 
                                Tmax_23_25_trim1, Tmax_25_28_trim1,
                                Tmax_28_30_trim1, Tmax_30_33_trim1,
                                Tmax_above_33_trim1) |>
                        mutate(count_trim1 = rowSums(across(starts_with("Tmax")))) |> 
                        filter(count_trim1 < 80)

                      
nrow(df_test)
# View(df_test)


tabyl(df_test, count_trim1) |> adorn_percentages("col") |> adorn_pct_formatting(digits = 2)

#### for trimester-2
df_test <- df_merge |> select(Tmax_below_5_trim2, Tmax_5_10_trim2, Tmax_10_15_trim2, 
                                Tmax_15_20_trim2, Tmax_20_23_trim2, 
                                Tmax_23_25_trim2, Tmax_25_28_trim2,
                                Tmax_28_30_trim2, Tmax_30_33_trim2,
                                Tmax_above_33_trim2) |>
                        mutate(count_trim2 = rowSums(across(starts_with("Tmax")))) |> 
                        filter(count_trim2 < 80)
nrow(df_test)

### Check whether the new variables are created properly
df_test2 <- df_merge |> select(Tmax_5_10_trim1, Tmax_10_15_trim1, Tmax_below_15_t1, 
                                Tmax_5_10_trim2, Tmax_10_15_trim2, Tmax_below_15_t2,
                                Tmax_5_10_trim3, Tmax_10_15_trim3, Tmax_below_15_t3,
                                Tmax_below_15_full) |> 
                        filter(Tmax_below_15_full != 0)

# View(df_test2)


## Explore histograms for Tmax variables
df_merge |> select(ends_with("full")) |> gather() |> ggplot(aes(value)) + geom_histogram(bins = 20) + facet_wrap(~key, scales = "free")
## Explore summaries for Tmax variables
df_merge |> select(ends_with("full")) |> summary()
## Explore frequency tables for Tmax variables
df_merge |> select(ends_with("full")) |> gather() |> count(key, value) |> ggplot(aes(value, n)) + geom_point() + facet_wrap(~key, scales = "free")
## Explore number of non-zero values for Tmax variables on average per region
df_merge |> select(ends_with("full"), region) |> group_by(region) |> summarise(across(ends_with("full"), ~sum(. != 0)/n())) |> gather() |> ggplot(aes(region, value, fill = key)) + geom_col(position = "dodge")

nrow(df_merge)
nrow(df_merge |> filter(Tmax_above_30_full == 0))/nrow(df_merge)

nrow(df_merge) - nrow(df_model_breech)
nrow(df_merge) - nrow(df_model_prolonged)
nrow(df_merge) - nrow(df_model_excessive)

tabyl(df_model_breech, fertMo)
colnames(df_model_breech)

