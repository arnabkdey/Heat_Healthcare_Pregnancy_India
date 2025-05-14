## Create variable labels 
### All predictors
var_labels_love_plots <- c(
  "hh_wealth_quintile_ru_og_middle" = "Household wealth: Middle quintile",
  "hh_wealth_quintile_ru_og_poorer" = "Household wealth: Poorer quintile",
  "hh_wealth_quintile_ru_og_poorest" = "Household wealth: Poorest quintile",
  "hh_wealth_quintile_ru_og_richer" = "Household wealth: Richer quintile",
  "hh_wealth_quintile_ru_og_richest" = "Household wealth: Richest quintile",
  "mat_age_at_int_cat_15-24" = "Mother's age at interview: 15-24 years",
  "mat_age_at_int_cat_25-34" = "Mother's age at interview: 25-34 years",
  "mat_age_at_int_cat_35-49" = "Mother's age at interview: 35-49 years",
  "mat_edu_level_higher" = "Mother's education: Higher",
  "mat_edu_level_no education" = "Mother's education: No education",
  "mat_edu_level_primary" = "Mother's education: Primary",
  "mat_edu_level_secondary" = "Mother's education: Secondary",
  "mat_media_exp_any_yes" = "Mother exposed to media: Yes",
  "mat_parity_bi_Multiparous" = "Mother parity: Multiparous",
  "meta_rural_rural" = "Rural residence",
  "ses_access_issue_distance_not-a-big-prob" = "Distance not a big problem for healthcare access",
  "ses_caste_2_General" = "Caste: General",
  "ses_religion_2_hindu_Not-Hindu" = "Religion: Not Hindu"  
)

### all exposures
exposure_labels_love_plots <- c(
  "exp_cumu_5_scaled10" = "Total number of days with Tmin < 5th percentile",
  "exp_cumu_10_scaled10" = "Total number of days with Tmin < 10th percentile",
  "exp_cumu_15_scaled10" = "Total number of days with Tmin < 15th percentile",
  "exp_cumu_85_scaled10" = "Total number of days with Tmax > 85th percentile",
  "exp_cumu_90_scaled10" = "Total number of days with Tmax > 90th percentile",
  "exp_cumu_95_scaled10" = "Total number of days with Tmax > 95th percentile"
)
