pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive, here)
library(multcomp)

rm(list = ls())

# Create a folder for the outputs ----
path_out <- here("3-outputs", "models", "models-with-interaction")
if (!dir.exists(path_out)) {
  # Create the directory if it does not exist
  dir.create(path_out, showWarnings = TRUE, recursive = TRUE)
}

# Load models ----
path_processed <- here("2-data", "2.2-processed-data")
model_int_all <- readRDS(here(path_processed, "2.3-models-interactions.rds"))
print("finished loading models")
model_names <- names(model_int_all)
length(model_int_all)

# Varlist of interaction terms -----
varlist_interaction <- c("mat_age_grp", "hh_wealth_bi", "hh_caste_bi", "hh_religion_tri")

# Get the index of the models that contain the interaction tems ----
model_names
index_age <- model_names[c(1,5)]
index_wealth <- model_names[c(2,6)]
index_caste <- model_names[c(3,7)]
index_religion <- model_names[c(4,8)]


# Extract multcomp objects for each Effect Modifier -----

## For Maternal Age ----

### Assess the number of times zero has to be repeated ----
#### First, get the number of coefficients in the model 
sum_model_cur <- summary(model_int_all[[index_age[1]]])
nrow <- nrow(sum_model_cur$coefficients)
print(nrow) # 14

# a = num of coefficients = 14
# b = number of levels of the effect modifier = 3 (15-24/25-34/35-49)
# c = number of trailing elements after rep = b - 1 = 2
# Total number of times zero has to be repeated = a - c - 2 = 10
rep_zero <- 10

#### Run loop to extract multcomp objects 
list_age <- list()
# i <- "preg_abs_age"
for (i in index_age) {
    # Assign Model 
    model_cur <- model_int_all[[i]]
    name_cur <- names(model_int_all[i])
    # Levels- Rural vs Urban
    contrast_cur <- rbind("15-24" = c(0, 1, rep(0, rep_zero), 0, 0),
                    "25-34" = c(0, 1, rep(0, rep_zero), 1, 0),
                    "35-49" = c(0, 1, rep(0, rep_zero), 0, 1),    
                    "15-24 vs 25-34" = c(0, 0, rep(0, rep_zero), 1, 0),
                    "15-24 vs 25-34" = c(0, 0, rep(0, rep_zero), 0, 1))
    # names(model_cur) <- names(model_int_all[[i]])
    # print(names(model_cur))
    glht_cur <- glht(model_cur, linfct = contrast_cur)
    tab_cur <- tidy(glht_cur, conf.int = TRUE, exponentiate = FALSE)
    list_age[[name_cur]] <- tab_cur
}

#### Save to excel 
sheet_names_age <- names(model_int_all[index_age])
write.xlsx(list_age, here(path_out, "multcomp-cis-age.xlsx"), sheetName = sheet_names_age)

## For Wealth ----

### Assess the number of times zero has to be repeated ----
#### First, get the number of coefficients in the model
sum_model_cur <- summary(model_int_all[[index_wealth[1]]])
nrow <- nrow(sum_model_cur$coefficients)
print(nrow) # 13

# a = num of coefficients = 13
# b = number of levels of the effect modifier = 2 (poorer/richer)
# c = number of trailing elements after rep = b - 1 = 1
# Total number of times zero has to be repeated = a - c - 2 = 10
rep_zero <- 10

#### Run loop to extract multcomp objects ----
list_wealth <- list()

for (i in index_wealth) {
    # Assign Model 
    model_cur <- model_int_all[[i]]
    name_cur <- names(model_int_all[i])
    # Levels- Rural vs Urban
    contrast_cur <- rbind("Poor" = c(0, 1, rep(0, rep_zero), 0),
                    "Rich" = c(0, 1, rep(0, rep_zero), 1),    
                    "Rich vs Poor" = c(0, 0, rep(0, rep_zero), 1))
    # names(model_cur) <- names(model_int_all[[i]])
    # print(names(model_cur))
    glht_cur <- glht(model_cur, linfct = contrast_cur)
    tab_cur <- tidy(glht_cur, conf.int = TRUE, exponentiate = FALSE)
    list_wealth[[name_cur]] <- tab_cur
}



#### Save to excel 
sheet_names_wealth <- names(model_int_all[index_wealth])
write.xlsx(list_wealth, here(path_out, "multcomp-cis-wealth.xlsx"), sheetName = sheet_names_wealth)


## For caste ----

### Assess the number of times zero has to be repeated ----
#### First, get the number of coefficients in the model
sum_model_cur <- summary(model_int_all[[index_caste[1]]])
nrow <- nrow(sum_model_cur$coefficients)
print(nrow) # 13

# a = num of coefficients = 13
# b = number of levels of the effect modifier = 2 (Gen vs SC/ST/OBC)
# c = number of trailing elements after rep = b - 1 = 1
# Total number of times zero has to be repeated = a - c - 2 = 10

rep_zero <- 10

#### Run loop to extract multcomp objects ----
list_caste <- list()

for (i in index_caste) {
    # Assign Model 
    model_cur <- model_int_all[[i]]
    name_cur <- names(model_int_all[i])
    # Levels- Rural vs Urban
    contrast_cur <- rbind("Gen" = c(0, 1, rep(0, rep_zero), 0),
                    "SC/ST/OBC" = c(0, 1, rep(0, rep_zero), 1),    
                    "SC/ST/OBC vs Gen" = c(0, 0, rep(0, rep_zero), 1))
    # names(model_cur) <- names(model_int_all[[i]])
    # print(names(model_cur))
    glht_cur <- glht(model_cur, linfct = contrast_cur)
    tab_cur <- tidy(glht_cur, conf.int = TRUE, exponentiate = FALSE)
    list_caste[[name_cur]] <- tab_cur
}

#### Save to excel ----
sheet_names_caste <- names(model_int_all[index_caste])
write.xlsx(list_caste, here(path_out, "multcomp-cis-caste.xlsx"), sheetName = sheet_names_caste)


## For religion ----

### Assess the number of times zero has to be repeated ----
#### First, get the number of coefficients in the model
sum_model_cur <- summary(model_int_all[[index_religion[1]]])
nrow <- nrow(sum_model_cur$coefficients)
print(nrow) # 14

# a = num of coefficients = 14
# b = number of levels of the effect modifier = 3 (Hindu/Muslim/Other)
# c = number of trailing elements after rep = b - 1 = 2
# Total number of times zero has to be repeated = a - c - 2 = 10

rep_zero <- 10

#### Run loop to extract multcomp objects ----
list_religion <- list()

for (i in index_religion) {
    # Assign Model 
    model_cur <- model_int_all[[i]]
    name_cur <- names(model_int_all[i])
    # Levels- Rural vs Urban
    contrast_cur <- rbind("Hindu" = c(0, 1, rep(0, rep_zero), 0, 0),
                    "Muslim" = c(0, 1, rep(0, rep_zero), 1, 0),
                    "Other" = c(0, 1, rep(0, rep_zero), 0, 1),    
                    "Muslim vs Hindu" = c(0, 0, rep(0, rep_zero), 1, 0),
                    "Other vs Hindu" = c(0, 0, rep(0, rep_zero), 0, 1))
    # names(model_cur) <- names(model_int_all[[i]])
    # print(names(model_cur))
    glht_cur <- glht(model_cur, linfct = contrast_cur)
    tab_cur <- tidy(glht_cur, conf.int = TRUE, exponentiate = FALSE)
    list_religion[[name_cur]] <- tab_cur
}


#### Save to excel ----
sheet_names_religion <- names(model_int_all[index_religion])
write.xlsx(list_religion, here(path_out, "multcomp-cis-religion.xlsx"), sheetName = sheet_names_religion)



# Save models arranged by exposure definition ----
path_out_consolidated <- here("3-outputs", "models", "models-with-interaction", "consolidated")
!dir.exists(path_out_consolidated) && dir.create(path_out_consolidated, showWarnings = TRUE, recursive = TRUE)

## For pregnancy complications ----
names(list_age)
list_preg <- c(list_age[1], list_wealth[1])
sheet_names_preg <- c(names(list_age[1]), names(list_wealth[1]))
write.xlsx(list_preg, here(path_out_consolidated, "multcomp-preg.xlsx"), sheetName = sheet_names_preg)

## For delivery complications ----
list_del <- c(list_age[2], list_wealth[2])
sheet_names_age <- c(names(list_age[2]), names(list_wealth[2]))
write.xlsx(list_del, here(path_out_consolidated, "multcomp-del.xlsx"), sheetName = sheet_names_age)


## Check the results ----
# Run individual model
# model <- lme4::glmer(any_del_comp_sum ~ sum_mag_abs_30_scale_iqr + rural + mat_edu_level + hh_wealth_bi + hh_caste_bi + hh_religion_tri + (1 | clim_zone_short), 
#                                     data = df_paper_final, 
#                                     subset = mat_age_grp == "25-34",
#                                     family = poisson(link = "log"))
# summary(model)
