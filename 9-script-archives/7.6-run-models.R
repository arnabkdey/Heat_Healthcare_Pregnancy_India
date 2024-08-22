rm(list = setdiff(ls(), c("path_processed_data", "path_raw_data")))
pacman::p_load(tidyverse, data.table, janitor, fst, openxlsx, googledrive, here, beepr, Hmisc)
library(doParallel)
library(merDeriv)
source(here(".Rprofile"))

# Function to calculate confidence intervals for coefficients ----
coefcis <- function(coef, se, exponentiate = TRUE) {
  lci = coef - 1.96*se
  uci = coef + 1.96*se
  res <- cbind(coef, lci, uci)
  res_exp = exp(res)
  if (exponentiate == TRUE) {
    res_full <- exp(res)
  } else {
    res_full <- res
  }
  return(res_full)
}

# Load dataset and formulas ----
df_merge <- read.fst(here(path_processed_data, "7.4-dhs-5-IR-merged-processed.fst"), as.data.table = T)
load(here(path_processed_data, "7.5-fmlas.RData"))

colnames(df_merge)
# df_merge <- df_merge[(fertMo == 1 | fertMo == 2 | fertMo == 3) & (region == "IA_goa" | region == "IA_lakshadweep")]
# tabyl(df_merge, fertMo, region)
# unique(df_merge$region)
# unique(df_merge$fertMo)
# nrow(df_merge)


# Create data subsets for the three complications ---
df_model_breech <- df_merge |> filter(comp_breech != "don't know")
df_model_prolonged <- df_merge |> filter(comp_prolonged != "don't know")
df_model_excessive <- df_merge |> filter(comp_excessive != "don't know")


# Models spelled out ----

## GLM - Absolute -----
model_glm_excessive_t1_abs <- glm(comp_excessive_bi ~ 
                                iv_abs_t1_Tmax_below_15 + iv_abs_t1_Tmax_15_20 + 
                                iv_abs_t1_Tmax_25_30 + iv_abs_t1_Tmax_above_30 +
                                fertAge + 
                                season_birth +
                                hh_caste_tri + hh_religion_tri + hh_wealth_bi +
                                residence + 
                                edu +
                                clim_zone, 
                              data = df_model_excessive, 
                              family = poisson(link = "log"))

summary(model_glm_excessive_t1_abs)
broom::tidy(model_glm_excessive_t1_abs, exp = TRUE) |> head()

## GLM - Percentile ----
model_glm_excessive_t1_pc <- glm(comp_excessive_bi ~ 
                                iv_pc_t1_Tmax_0_10 + iv_pc_t1_Tmax_10_40 + 
                                iv_pc_t1_Tmax_60_90 + iv_pc_t1_Tmax_90_100 +
                                fertAge + 
                                season_birth +
                                hh_caste_tri + hh_religion_tri + hh_wealth_bi +
                                residence + 
                                edu +
                                clim_zone, 
                              data = df_model_excessive, 
                              family = poisson(link = "log"))

summary(model_glm_excessive_t1_pc)
broom::tidy(model_glm_excessive_t1_pc, exp = TRUE) |> head()
m_vcov <- vcov(model_glm_prolonged_t3)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glm_prolonged_t3$coefficients
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

## GLMER - Absolute -----
model_glmer_excessive_t1_abs <- lme4::glmer(comp_excessive_bi ~ 
                                iv_abs_t1_Tmax_below_15 + iv_abs_t1_Tmax_15_20 + 
                                iv_abs_t1_Tmax_25_30 + iv_abs_t1_Tmax_above_30 +
                                fertAge + 
                                season_birth +
                                # hh_caste_tri + hh_religion_tri + hh_wealth_bi +
                                # residence + 
                                # edu +
                                (1 | region), 
                              data = df_model_excessive, 
                              family = poisson(link = "log"))

m_vcov <- vcov.glmerMod(model_glmer_excessive_t1_abs)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glmer_excessive_t1_abs@beta
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

## GLMER - Percentile ----
df_model_excessive$fertAge_scale <- scale(df_model_excessive$fertAge)
model_glmer_excessive_t1_pc <- lme4::glmer(comp_excessive_bi ~ 
                                iv_pc_t1_Tmax_0_10 + iv_pc_t1_Tmax_10_40 + 
                                iv_pc_t1_Tmax_60_90 + iv_pc_t1_Tmax_90_100 +
                                fertAge + 
                                season_birth +
                                # hh_caste_tri + hh_religion_tri + hh_wealth_bi +
                                # residence + 
                                # edu +
                                (1 | clim_zone), 
                              data = df_model_excessive, 
                              family = poisson(link = "log"))

m_vcov <- vcov.glmerMod(model_glmer_excessive_t1_pc)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glmer_excessive_t1_pc@beta
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()


# Run models in a loop ---
## Create a list of datasets for each outcome ----
datasets <- list(
  comp_breech_bi = df_model_breech,
  comp_prolonged_bi = df_model_prolonged,
  comp_excessive_bi = df_model_excessive,
  anc = df_merge
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
saveRDS(results, here(path_processed_data, "7.5-models.rds"))
