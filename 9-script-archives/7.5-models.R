rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, openxlsx, googledrive, here, beepr, Hmisc)
library(doParallel)

# Load the merged dataset ----
df_merge <- read.fst(here("2-data", "2.2-processed-data", "7.4-dhs-5-IR-merged-processed.fst"), as.data.table = T)
nrow(df_merge)
df_merge$fertAge <- as.numeric(df_merge$fertAge)
# df_merge <- df_merge[(fertMo == 1 | fertMo == 2 | fertMo == 3) & (region == "IA_goa" | region == "IA_lakshadweep")]
# tabyl(df_merge, fertMo, region)
# unique(df_merge$region)
# unique(df_merge$fertMo)
# nrow(df_merge)



# Create data subsets for the three complications ---
df_model_breech <- df_merge |> filter(comp_breech != "don't know")
df_model_prolonged <- df_merge |> filter(comp_prolonged != "don't know")
df_model_excessive <- df_merge |> filter(comp_excessive != "don't know")


# Create formula for models --- 
## Varlist for exposure ----
varlist_exposure <- c("Tmax_15_20", "Tmax_20_25", "Tmax_25_28", "Tmax_28_30", "Tmax_above_30")

## Varlist for covariates ----
varlist_cov <- c("hh_caste_tri", "hh_religion_tri", "hh_wealth_bi", "mat_parity", "residence", "edu", "fertAge")

# varlist_cov <- c("hh_caste_tri", "hh_religion_tri")

tabyl(df_model_breech, hh_caste_tri)

## Varlist for fixed effects ----
varlist_fixed <- c("fertMo", "region")

## Varlist for trimester ----
varlist_trimester <- c("t1", "t2", "t3", "full")

## Depvarlist ----
depvarlist <- c("comp_breech_bi", "comp_prolonged_bi", "comp_excessive_bi")


## Run loop to create formulas and store in a list ----
list_fmlas <- list()
for (i in depvarlist) {         
    for (k in varlist_trimester) {
            formula_name <- paste0("fmla_", i, "_", k)
            formula_value <- as.formula(paste(i, "~", 
                                              paste(paste0(varlist_exposure, "_", k), collapse = " + "), "+", 
                                              paste(varlist_cov, collapse = " + "), "+", 
                                              paste(varlist_fixed, collapse = " * ")))
            assign(formula_name, formula_value)
            list_fmlas[[i]] <- c(list_fmlas[[i]], list(formula_value))
        }
    }

list_fmlas[2]
length(list_fmlas)
str(list_fmlas)
names(list_fmlas)

# Run models ---
## Create a list of datasets for each outcome ----
datasets <- list(
  comp_breech_bi = df_model_breech,
  comp_prolonged_bi = df_model_prolonged,
  comp_excessive_bi = df_model_excessive
)


## Run the models using for loop ----
print("starting models")
print(Sys.time())

results <- list()

for (dep_var in names(list_fmlas)) {
  current_data <- datasets[[dep_var]]
  
  results[[dep_var]] <- lapply(list_fmlas[[dep_var]], function(formula) {
    # Print the formula being used
    cat("Running model with formula:\n")
    print(formula)
    cat("\n")
    print(Sys.time())    

    tryCatch({
      glm(formula, data = current_data, family = binomial())  
    }, error = function(e) {
      message("Error fitting model for ", dep_var, ": ", as.character(formula))
      message("Error message: ", e$message)
      return(NULL)
    })
  })
}


# class(results)
# # str(results)
# length(results)
# names(results)[1]
# model <- results[["comp_breech_bi"]][2]


# Save all objects starting with "model" as an RDS file ----
saveRDS(results, here("2-data", "2.2-processed-data", "7.5-models.rds"))
