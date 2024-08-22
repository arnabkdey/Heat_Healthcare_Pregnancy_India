rm(list = setdiff(ls(), c("path_processed_data", "path_raw_data")))
pacman::p_load(tidyverse, data.table, janitor, fst, openxlsx, googledrive, here, beepr, Hmisc)
library(merDeriv)
source(here(".Rprofile"))
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


# Load the merged dataset ----
df_merge <- read.fst(here(path_processed_data, "7.4-dhs-5-IR-merged-processed.fst"), as.data.table = T)
# Combine clus and SurveyID to create unique identifier ---
df_merge <- df_merge |> mutate(psu = paste0(clust, "_", SurveyId))

nrow(df_merge)

# df_merge <- df_merge[(fertMo == 1 | fertMo == 2 | fertMo == 3) & (region == "IA_goa" | region == "IA_lakshadweep")]
# tabyl(df_merge, fertMo, region)
# unique(df_merge$region)
# unique(df_merge$fertMo)
# nrow(df_merge)

sapply(df_merge |> select(comp_breech, comp_prolonged, comp_excessive), function(x) sum(is.na(x)))


# Create data subsets for the three complications ---
df_model_breech <- df_merge |> filter(comp_breech != "don't know")
df_model_prolonged <- df_merge |> filter(comp_prolonged != "don't know")
df_model_excessive <- df_merge |> filter(comp_excessive != "don't know")


# Create formula for models --- 
## Varlist for exposure ----
grep("iv_pc", names(df_model_breech), value = T)
varlist_exp_abs_base <- c("Tmax_below_15", "Tmax_15_20", "Tmax_25_30", "Tmax_above_30")
varlist_exp_pc_base <- c("Tmax_0_10", "Tmax_10_40", "Tmax_60_90", "Tmax_90_100")

## Varlist for covariates ----
varlist_cov <- c("hh_caste_tri", "hh_religion_tri", "hh_wealth_bi", "residence", "edu", "fertAge")

## Varlist for fixed effects ----
# varlist_fixed <- c("season_birth", "region")
varlist_fixed <- c("season_birth")

## Varlist for trimester ----
varlist_trimester_abs <- c("iv_abs_t1", "iv_abs_t2", "iv_abs_t3", "iv_abs_full")
varlist_trimester_pc <- c("iv_pc_t1", "iv_pc_t2", "iv_pc_t3", "iv_pc_full")

## Depvarlist ----
depvarlist <- c("comp_breech_bi", "comp_prolonged_bi", "comp_excessive_bi", "no_four_anc_bi")


## Create Formulas ----
### GLM 
#### Absolute ----
list_fmlas_glm_abs <- list()
for (i in depvarlist) {
    for (k in varlist_trimester_abs) {
            formula_name <- paste0("fmla_", i, "_", k)
            formula_value <- as.formula(paste(i, "~", 
                                              paste(paste0(k, "_", varlist_exp_abs_base), collapse = " + "), "+", 
                                              paste(varlist_cov, collapse = " + "), "+", 
                                              paste(varlist_fixed, collapse = " + ")))
            assign(formula_name, formula_value)
            list_fmlas_glm_abs[[i]] <- c(list_fmlas_glm_abs[[i]], list(formula_value))
        }
}
#### Percentile ----
list_fmlas_glm_pc <- list()
for (i in depvarlist) {
    for (k in varlist_trimester_pc) {
            formula_name <- paste0("fmla_", i, "_", k)
            formula_value <- as.formula(paste(i, "~", 
                                              paste(paste0(k, "_", varlist_exp_pc_base), collapse = " + "), "+", 
                                              paste(varlist_cov, collapse = " + "), "+", 
                                              paste(varlist_fixed, collapse = " + "), " + clim_zone"))
            assign(formula_name, formula_value)
            list_fmlas_glm_pc[[i]] <- c(list_fmlas_glm_pc[[i]], list(formula_value))
        }
}


### GLMER
#### Absolute ----
list_fmlas_glmer_abs <- list()

for (i in depvarlist) {
    list_fmlas_glmer_abs[[i]] <- list()  # Initialize the nested list
    for (k in varlist_trimester_abs) {
        formula_name <- paste0("fmla_", i, "_", k)
        formula_value <- as.formula(paste(i, "~", 
                                          paste(paste0(k, "_", varlist_exp_abs_base), collapse = " + "), "+", 
                                          paste(varlist_cov, collapse = " + "), "+", 
                                          paste(varlist_fixed, collapse = " + "), " + (1 | psu)"))
        list_fmlas_glmer_abs[[i]][[formula_name]] <- formula_value
    }
}

# list_fmlas_glmer_abs[["comp_breech_bi"]][[1]]

#### Percentile ----
list_fmlas_glmer_pc <- list()

for (i in depvarlist) {
    list_fmlas_glmer_pc[[i]] <- list()  # Initialize the nested list
    for (k in varlist_trimester_pc) {
        formula_name <- paste0("fmla_", i, "_", k)
        formula_value <- as.formula(paste(i, "~", 
                                          paste(paste0(k, "_", varlist_exp_pc_base), collapse = " + "), "+", 
                                          paste(varlist_cov, collapse = " + "), "+", 
                                          paste(varlist_fixed, collapse = " + "), " + (1 | psu)"))
        list_fmlas_glmer_pc[[i]][[formula_name]] <- formula_value
    }
}

# GLM models ----
## Percentile ----
### Breech Presentation ----
#### Trim 1 ----
fmla_glm_breech_t1 <- list_fmlas_glm_pc[["comp_breech_bi"]][[1]]
model_glm_breech_t1 <- glm(fmla_glm_breech_t1, data = df_model_breech, family = binomial(link = "logit"))
broom::tidy(model_glm_breech_t1, exp = TRUE)[1:6,]

#### Trim 2 ----
fmla_glm_breech_t2 <- list_fmlas_glm_pc[["comp_breech_bi"]][[2]]
model_glm_breech_t2 <- glm(fmla_glm_breech_t2, data = df_model_breech, family = binomial(link = "logit"))
broom::tidy(model_glm_breech_t2, exp = TRUE)[1:6,]

#### Trim 3 ----
fmla_glm_breech_t3 <- list_fmlas_glm_pc[["comp_breech_bi"]][[3]]
model_glm_breech_t3 <- glm(fmla_glm_breech_t3, data = df_model_breech, family = binomial(link = "logit"))
broom::tidy(model_glm_breech_t3, exp = TRUE)[1:6,]

#### Full ----
fmla_glm_breech_full <- list_fmlas_glm_pc[["comp_breech_bi"]][[4]]
model_glm_breech_full <- glm(fmla_glm_breech_full, data = df_model_breech, family = binomial(link = "logit"))
broom::tidy(model_glm_breech_full, exp = TRUE)[1:6,]

### Excessive Bleeding ----

#### Trim 1 ----
fmla_glm_excessive_t1 <- list_fmlas_glm_pc[["comp_excessive_bi"]][[1]]
model_glm_excessive_t1 <- glm(fmla_glm_excessive_t1, data = df_model_excessive, family = poisson(link = "log"))
# broom::tidy(model_glm_excessive_t1, exp = TRUE)[1:6,]
m_vcov <- vcov(model_glm_excessive_t1)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glm_excessive_t1$coefficients
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

#### Trim 2 ----
fmla_glm_excessive_t2 <- list_fmlas_glm_pc[["comp_excessive_bi"]][[2]]
model_glm_excessive_t2 <- glm(fmla_glm_excessive_t2, data = df_model_excessive, family = poisson(link = "log"))
# broom::tidy(model_glm_excessive_t2, exp = TRUE)[1:6,]
m_vcov <- vcov(model_glm_excessive_t2)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glm_excessive_t2$coefficients
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

#### Trim 3 ----
fmla_glm_excessive_t3 <- list_fmlas_glm_pc[["comp_excessive_bi"]][[3]]
model_glm_excessive_t3 <- glm(fmla_glm_excessive_t3, data = df_model_excessive, family = poisson(link = "log"))
# broom::tidy(model_glm_excessive_t3, exp = TRUE)[1:6,]
m_vcov <- vcov(model_glm_excessive_t3)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glm_excessive_t3$coefficients
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

#### Full ----
fmla_glm_excessive_full <- list_fmlas_glm_pc[["comp_excessive_bi"]][[4]]
model_glm_excessive_full <- glm(fmla_glm_excessive_full, data = df_model_excessive, family = poisson(link = "log"))
# broom::tidy(model_glm_excessive_full, exp = TRUE)[1:6,]
m_vcov <- vcov(model_glm_excessive_full)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glm_excessive_t1$coefficients
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

### Prolonged Labor ----
#### Trim 1 -----
fmla_glm_prolonged_t1 <- list_fmlas_glm_pc[["comp_prolonged_bi"]][[1]]
model_glm_prolonged_t1 <- glm(fmla_glm_prolonged_t1, data = df_model_prolonged, family = poisson(link = "log"))
# broom::tidy(model_glm_prolonged_t1, exp = TRUE)[1:6,]
m_vcov <- vcov(model_glm_prolonged_t1)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glm_prolonged_t1$coefficients
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

#### Trim 2 -----
fmla_glm_prolonged_t2 <- list_fmlas_glm_pc[["comp_prolonged_bi"]][[2]]
model_glm_prolonged_t2 <- glm(fmla_glm_prolonged_t2, data = df_model_prolonged, family = poisson(link = "log"))
# broom::tidy(model_glm_prolonged_t2, exp = TRUE)[1:6,]
m_vcov <- vcov(model_glm_prolonged_t2)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glm_prolonged_t2$coefficients
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()
<- |> 
#### Trim 3 -----
fmla_glm_prolonged_t3 <- list_fmlas_glm_pc[["comp_prolonged_bi"]][[3]]
model_glm_prolonged_t3 <- glm(fmla_glm_prolonged_t3, data = df_model_prolonged, family = poisson(link = "log"))
# broom::tidy(model_glm_prolonged_t3, exp = TRUE)[1:6,]
m_vcov <- vcov(model_glm_prolonged_t3)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glm_prolonged_t3$coefficients
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

#### Full ----
fmla_glm_prolonged_full <- list_fmlas_glm_pc[["comp_prolonged_bi"]][[4]]
model_glm_prolonged_full <- glm(fmla_glm_prolonged_full, data = df_model_prolonged, family = poisson(link = "log"))
# broom::tidy(model_glm_prolonged_full, exp = TRUE)[1:6,]
m_vcov <- vcov(model_glm_prolonged_full)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glm_prolonged_full$coefficients
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

### ANC ----
#### Full Pregnancy ---- 
fmla_glm_anc_full <- list_fmlas_glm_pc[["no_four_anc_bi"]][[4]]
model_glm_anc_full <- glm(fmla_glm_anc_full, data = df_merge, family = poisson(link = "log"))
m_vcov <- vcov(model_glm_anc_full)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glm_anc_full$coefficients
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

## Absolute ----
### Breech Presentation ----
#### Trim 1 ----
fmla_glm_breech_t1 <- list_fmlas_glm_abs[["comp_breech_bi"]][[1]]
model_glm_breech_t1 <- glm(fmla_glm_breech_t1, data = df_model_breech, family = binomial(link = "logit"))
broom::tidy(model_glm_breech_t1, exp = TRUE)[1:6,]

#### Trim 2 ----
fmla_glm_breech_t2 <- list_fmlas_glm_abs[["comp_breech_bi"]][[2]]
model_glm_breech_t2 <- glm(fmla_glm_breech_t2, data = df_model_breech, family = binomial(link = "logit"))
broom::tidy(model_glm_breech_t2, exp = TRUE)[1:6,]

#### Trim 3 ----
fmla_glm_breech_t3 <- list_fmlas_glm_abs[["comp_breech_bi"]][[3]]
model_glm_breech_t3 <- glm(fmla_glm_breech_t3, data = df_model_breech, family = binomial(link = "logit"))
broom::tidy(model_glm_breech_t3, exp = TRUE)[1:6,]

#### Full ----
fmla_glm_breech_full <- list_fmlas_glm_abs[["comp_breech_bi"]][[4]]
model_glm_breech_full <- glm(fmla_glm_breech_full, data = df_model_breech, family = binomial(link = "logit"))
broom::tidy(model_glm_breech_full, exp = TRUE)[1:6,]

### Excessive Bleeding ----

#### Trim 1 ----
fmla_glm_excessive_t1 <- list_fmlas_glm_abs[["comp_excessive_bi"]][[1]]
model_glm_excessive_t1 <- glm(fmla_glm_excessive_t1, data = df_model_excessive, family = poisson(link = "log"))
# broom::tidy(model_glm_excessive_t1, exp = TRUE)[1:6,]
m_vcov <- vcov(model_glm_excessive_t1)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glm_excessive_t1$coefficients
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

#### Trim 2 ----
fmla_glm_excessive_t2 <- list_fmlas_glm_abs[["comp_excessive_bi"]][[2]]
model_glm_excessive_t2 <- glm(fmla_glm_excessive_t2, data = df_model_excessive, family = poisson(link = "log"))
# broom::tidy(model_glm_excessive_t2, exp = TRUE)[1:6,]
m_vcov <- vcov(model_glm_excessive_t2)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glm_excessive_t2$coefficients
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

#### Trim 3 ----
fmla_glm_excessive_t3 <- list_fmlas_glm_abs[["comp_excessive_bi"]][[3]]
model_glm_excessive_t3 <- glm(fmla_glm_excessive_t3, data = df_model_excessive, family = poisson(link = "log"))
# broom::tidy(model_glm_excessive_t3, exp = TRUE)[1:6,]
m_vcov <- vcov(model_glm_excessive_t3)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glm_excessive_t3$coefficients
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

#### Full ----
fmla_glm_excessive_full <- list_fmlas_glm_abs[["comp_excessive_bi"]][[4]]
model_glm_excessive_full <- glm(fmla_glm_excessive_full, data = df_model_excessive, family = poisson(link = "log"))
# broom::tidy(model_glm_excessive_full, exp = TRUE)[1:6,]
m_vcov <- vcov(model_glm_excessive_full)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glm_excessive_full$coefficients
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

### Prolonged Labor ----
#### Trim 1 ----
fmla_glm_prolonged_t1 <- list_fmlas_glm_abs[["comp_prolonged_bi"]][[1]]
model_glm_prolonged_t1 <- glm(fmla_glm_prolonged_t1, data = df_model_prolonged, family = poisson(link = "log"))
# broom::tidy(model_glm_prolonged_t1, exp = TRUE)[1:6,]
m_vcov <- vcov(model_glm_prolonged_t1)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glm_prolonged_t1$coefficients
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

#### Trim 2 ----
fmla_glm_prolonged_t2 <- list_fmlas_glm_abs[["comp_prolonged_bi"]][[2]]
model_glm_prolonged_t2 <- glm(fmla_glm_prolonged_t2, data = df_model_prolonged, family = poisson(link = "log"))
# broom::tidy(model_glm_prolonged_t2, exp = TRUE)[1:6,]
m_vcov <- vcov(model_glm_prolonged_t2)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glm_prolonged_t2$coefficients
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

#### Trim 3 ----
fmla_glm_prolonged_t3 <- list_fmlas_glm_abs[["comp_prolonged_bi"]][[3]]
model_glm_prolonged_t3 <- glm(fmla_glm_prolonged_t3, data = df_model_prolonged, family = poisson(link = "log"))
# broom::tidy(model_glm_prolonged_t3, exp = TRUE)[1:6,]
m_vcov <- vcov(model_glm_prolonged_t3)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glm_prolonged_t3$coefficients
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

tabyl(df_model_prolonged, clim_zone) |> adorn_percentages("col") |> adorn_pct_formatting(digits = 1) |> print()

#### Full ----
fmla_glm_prolonged_full <- list_fmlas_glm_abs[["comp_prolonged_bi"]][[4]]
model_glm_prolonged_full <- glm(fmla_glm_prolonged_full, data = df_model_prolonged, family = poisson(link = "log"))
# broom::tidy(model_glm_prolonged_full, exp = TRUE)[1:6,]
m_vcov <- vcov(model_glm_prolonged_full)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glm_prolonged_full$coefficients
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

### ANC ----
#### Full Pregnancy ----
fmla_glm_anc_full <- list_fmlas_glm_abs[["no_four_anc_bi"]][[4]]
model_glm_anc_full <- glm(fmla_glm_anc_full, data = df_merge, family = poisson(link = "log"))
m_vcov <- vcov(model_glm_anc_full)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glm_anc_full$coefficients
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()


# GLMER Models ----
## Percentile ----
### Breech Presentation ----
#### Trim 1 ----
fmla_glmer_breech_t1 <- list_fmlas_glmer_pc[["comp_breech_bi"]][[1]]
model_glmer_breech_t1 <- glmer(fmla_glmer_breech_t1, data = df_model_breech, family = binomial(link = "logit"))
View(broom.mixed::tidy(model_glmer_breech_t1, exponentiate = TRUE)[1:6,])

#### Trim 2 ----
fmla_glmer_breech_t2 <- list_fmlas_glmer_pc[["comp_breech_bi"]][[2]]
model_glmer_breech_t2 <- glmer(fmla_glmer_breech_t2, data = df_model_breech, family = binomial(link = "logit"))
broom::tidy(model_glmer_breech_t2, exp = TRUE)[1:6,]

#### Trim 3 ----
fmla_glmer_breech_t3 <- list_fmlas_glmer_pc[["comp_breech_bi"]][[3]]
model_glmer_breech_t3 <- glmer(fmla_glmer_breech_t3, data = df_model_breech, family = binomial(link = "logit"))
broom::tidy(model_glmer_breech_t3, exp = TRUE)[1:6,]

#### Full ----
fmla_glmer_breech_full <- list_fmlas_glmer_pc[["comp_breech_bi"]][[4]]
model_glmer_breech_full <- glmer(fmla_glmer_breech_full, data = df_model_breech, family = binomial(link = "logit"))
broom::tidy(model_glmer_breech_full, exp = TRUE)[1:6,]

### Excessive Bleeding ----
#### Trim 1 ----
fmla_glmer_excessive_t1 <- list_fmlas_glmer_pc[["comp_excessive_bi"]][[1]]
model_glmer_excessive_t1 <- glmer(fmla_glmer_excessive_t1, data = df_model_excessive, family = poisson(link = "log"))
m_vcov <- vcov.glmerMod(model_glmer_excessive_t1)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glmer_excessive_t1@beta
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

#### Trim 2 ----
fmla_glmer_excessive_t2 <- list_fmlas_glmer_pc[["comp_excessive_bi"]][[2]]
model_glmer_excessive_t2 <- glmer(fmla_glmer_excessive_t2, data = df_model_excessive, family = poisson(link = "log"))
m_vcov <- vcov.glmerMod(model_glmer_excessive_t2)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glmer_excessive_t2@beta
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

#### Trim 3 ----
fmla_glmer_excessive_t3 <- list_fmlas_glmer_pc[["comp_excessive_bi"]][[3]]
model_glmer_excessive_t3 <- glmer(fmla_glmer_excessive_t3, data = df_model_excessive, family = poisson(link = "log"))
m_vcov <- vcov.glmerMod(model_glmer_excessive_t3)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glmer_excessive_t3@beta
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

#### Full ----
fmla_glmer_excessive_full <- list_fmlas_glmer_pc[["comp_excessive_bi"]][[4]]
model_glmer_excessive_full <- glmer(fmla_glmer_excessive_full, data = df_model_excessive, family = poisson(link = "log"))
m_vcov <- vcov.glmerMod(model_glmer_excessive_full)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glmer_excessive_t1@beta
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

### Prolonged Labor ----
#### Trim 1 ---- 
fmla_glmer_prolonged_t1 <- list_fmlas_glmer_pc[["comp_prolonged_bi"]][[1]]
model_glmer_prolonged_t1 <- glmer(fmla_glmer_prolonged_t1, data = df_model_prolonged, family = poisson(link = "log"))
m_vcov <- vcov.glmerMod(model_glmer_prolonged_t1)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glmer_prolonged_t1@beta
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

#### Trim 2 ----
fmla_glmer_prolonged_t2 <- list_fmlas_glmer_pc[["comp_prolonged_bi"]][[2]]
model_glmer_prolonged_t2 <- glmer(fmla_glmer_prolonged_t2, data = df_model_prolonged, family = poisson(link = "log"))
m_vcov <- vcov.glmerMod(model_glmer_prolonged_t2)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glmer_prolonged_t2@beta
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

#### Trim 3 ----
fmla_glmer_prolonged_t3 <- list_fmlas_glmer_pc[["comp_prolonged_bi"]][[3]]
model_glmer_prolonged_t3 <- glmer(fmla_glmer_prolonged_t3, data = df_model_prolonged, family = poisson(link = "log"))
m_vcov <- vcov.glmerMod(model_glmer_prolonged_t3)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glmer_prolonged_t3@beta
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

#### Full ----
fmla_glmer_prolonged_full <- list_fmlas_glmer_pc[["comp_prolonged_bi"]][[4]]
model_glmer_prolonged_full <- glmer(fmla_glmer_prolonged_full, data = df_model_prolonged, family = poisson(link = "log"))
m_vcov <- vcov.glmerMod(model_glmer_prolonged_full)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glmer_prolonged_full@beta
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

### ANC ----
#### Full Pregnancy ----
fmla_glmer_anc_full <- list_fmlas_glmer_pc[["no_four_anc_bi"]][[4]]
model_glmer_anc_full <- glmer(fmla_glmer_anc_full, data = df_merge, family = poisson(link = "log"))
m_vcov <- vcov.glmerMod(model_glmer_anc_full)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glmer_anc_full@beta
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

## Absolute ----
### Breech Presentation ----
#### Trim 1 ----
fmla_glmer_breech_t1 <- list_fmlas_glmer_abs[["comp_breech_bi"]][[1]]
model_glmer_breech_t1 <- glmer(fmla_glmer_breech_t1, data = df_model_breech, family = binomial(link = "logit"))
broom::tidy(model_glmer_breech_t1, exp = TRUE)[1:6,]

#### Trim 2 ----
fmla_glmer_breech_t2 <- list_fmlas_glmer_abs[["comp_breech_bi"]][[2]]
model_glmer_breech_t2 <- glmer(fmla_glmer_breech_t2, data = df_model_breech, family = binomial(link = "logit"))
broom::tidy(model_glmer_breech_t2, exp = TRUE)[1:6,]

#### Trim 3 ----
fmla_glmer_breech_t3 <- list_fmlas_glmer_abs[["comp_breech_bi"]][[3]]
model_glmer_breech_t3 <- glmer(fmla_glmer_breech_t3, data = df_model_breech, family = binomial(link = "logit"))
broom::tidy(model_glmer_breech_t3, exp = TRUE)[1:6,]

#### Full ----
fmla_glmer_breech_full <- list_fmlas_glmer_abs[["comp_breech_bi"]][[4]]
model_glmer_breech_full <- glmer(fmla_glmer_breech_full, data = df_model_breech, family = binomial(link = "logit"))
broom::tidy(model_glmer_breech_full, exp = TRUE)[1:6,]

### Excessive Bleeding ----
#### Trim 1 ----
mla_glmer_excessive_t1 <- list_fmlas_glmer_abs[["comp_excessive_bi"]][[1]]
model_glmer_excessive_t1 <- glmer(fmla_glmer_excessive_t1, data = df_model_excessive, family = poisson(link = "log"))
m_vcov <- vcov.glmerMod(model_glmer_excessive_t1)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glmer_excessive_t1@beta
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

#### Trim 2 ----
fmla_glmer_excessive_t2 <- list_fmlas_glmer_abs[["comp_excessive_bi"]][[2]]
model_glmer_excessive_t2 <- glmer(fmla_glmer_excessive_t2, data = df_model_excessive, family = poisson(link = "log"))
m_vcov <- vcov.glmerMod(model_glmer_excessive_t2)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glmer_excessive_t2@beta
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

#### Trim 3 ----
fmla_glmer_excessive_t3 <- list_fmlas_glmer_abs[["comp_excessive_bi"]][[3]]
model_glmer_excessive_t3 <- glmer(fmla_glmer_excessive_t3, data = df_model_excessive, family = poisson(link = "log"))
m_vcov <- vcov.glmerMod(model_glmer_excessive_t3)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glmer_excessive_t3@beta
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

#### Full ----
fmla_glmer_excessive_full <- list_fmlas_glmer_abs[["comp_excessive_bi"]][[4]]
model_glmer_excessive_full <- glmer(fmla_glmer_excessive_full, data = df_model_excessive, family = poisson(link = "log"))
m_vcov <- vcov.glmerMod(model_glmer_excessive_full)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glmer_excessive_t1@beta
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

### Prolonged Labor ----
#### Trim 1 ----
fmla_glmer_prolonged_t1 <- list_fmlas_glmer_abs[["comp_prolonged_bi"]][[1]]
model_glmer_prolonged_t1 <- glmer(fmla_glmer_prolonged_t1, data = df_model_prolonged, family = poisson(link = "log"))
m_vcov <- vcov.glmerMod(model_glmer_prolonged_t1)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glmer_prolonged_t1@beta
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

#### Trim 2 ----
fmla_glmer_prolonged_t2 <- list_fmlas_glmer_abs[["comp_prolonged_bi"]][[2]]
model_glmer_prolonged_t2 <- glmer(fmla_glmer_prolonged_t2, data = df_model_prolonged, family = poisson(link = "log"))
m_vcov <- vcov.glmerMod(model_glmer_prolonged_t2)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glmer_prolonged_t2@beta
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

#### Trim 3 ----
fmla_glmer_prolonged_t3 <- list_fmlas_glmer_abs[["comp_prolonged_bi"]][[3]]
model_glmer_prolonged_t3 <- glmer(fmla_glmer_prolonged_t3, data = df_model_prolonged, family = poisson(link = "log"))
m_vcov <- vcov.glmerMod(model_glmer_prolonged_t3)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glmer_prolonged_t3@beta
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

#### Full ----
fmla_glmer_prolonged_full <- list_fmlas_glmer_abs[["comp_prolonged_bi"]][[4]]
model_glmer_prolonged_full <- glmer(fmla_glmer_prolonged_full, data = df_model_prolonged, family = poisson(link = "log"))
m_vcov <- vcov.glmerMod(model_glmer_prolonged_full)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glmer_prolonged_full@beta
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()

### ANC ----
#### Full Pregnancy ----
fmla_glmer_anc_full <- list_fmlas_glmer_abs[["no_four_anc_bi"]][[4]]
model_glmer_anc_full <- glmer(fmla_glmer_anc_full, data = df_merge, family = poisson(link = "log"))
m_vcov <- vcov.glmerMod(model_glmer_anc_full)
m_se <- sqrt(diag(m_vcov))
m_coef <- model_glmer_anc_full@beta
coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column() |> head()


# Save the models ----
## Create a vecotr of all object names with the prefix "model_" as RDS ----
model_names <- ls(pattern = "^model_")

## Create a list of all models ----
list_models <- lapply(model_names, get)
# class(list_models)
# list_models[[2]]

## Save the list of models ----
saveRDS(list_models, file = here(path_processed_data, "7.7-models.rds"))


