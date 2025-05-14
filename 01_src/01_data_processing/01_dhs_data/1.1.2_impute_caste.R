# load libraries ----
pacman::p_load(tidyverse, janitor, data.table, fst, here, mice)

# set paths ----
source(here("paths.R"))

# load data ----
df_IR_long <- read_fst(path = here(path_processed, 
  "1.1.1_DHS_IR_filtered_4mo.fst"))
nrow(df_IR_long)
length(unique(df_IR_long$caseid))

# Impute caste ----
## Convert to factor
df_IR_long$hh_caste <- as.factor(df_IR_long$hh_caste)

#### Check the number of missing values
sum(is.na(df_IR_long$hh_caste))
tabyl(df_IR_long$hh_caste)

#### Convert don't know to NA while retaining the factor levels
df_IR_long$hh_caste <- fct_recode(df_IR_long$hh_caste, NULL = "don't know")
tabyl(df_IR_long$hh_caste) 
dim(df_IR_long)

## Select variables for imputation
df_IR_long_sel <- df_IR_long |>  
    select(caseid, hh_caste, 
      meta_rural, meta_dist_name, meta_state_name,
      mat_num_living_children, mat_parity, mat_edu_level,
      hh_religion, hh_wealth_score,
        v467d, # distance is a problem in accessing healthcare
        v157, v158, v159, v171a # access to mass media
    ) 

### Run the imputation model
set.seed(123)
imp <- mice(df_IR_long_sel, m = 2, maxit = 5, method = "rf")

### Extract one of the completed datasets
df_IR_long_imputed <- complete(imp, 1)
dim(df_IR_long_imputed)

### Check the distribution of the imputed values in the first dataset
tabyl(df_IR_long_imputed$hh_caste) |>
  adorn_totals()

### Compare original vs imputed values
df_IR_long |> 
  filter(!is.na(hh_caste)) |>
  tabyl(hh_caste) |>
  adorn_totals() 

# save the imputed dataset
df_IR_long_imputed |> saveRDS(here(path_processed, 
  "1.1.2_DHS_IR_imputed_4mo.rds"))

