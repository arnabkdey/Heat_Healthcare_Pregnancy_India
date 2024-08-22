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
broom::tidy(model_glmer_breech_t1, exp = TRUE)[1:6,]

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
## Create a list all objects with the prefix "model_" as RDS ----
list_models <- ls(pattern = "^model_")

## Remove the objects that are not models ----
rm(list = setdiff(ls(), list_models))
ls()
