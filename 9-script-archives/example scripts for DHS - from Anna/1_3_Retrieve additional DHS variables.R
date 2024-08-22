########################################################
########################################################
#######                                          ####### 
#######      PROJECT: Heatwaves & fertility      ####### 
#######                                          ####### 
#######  CODE: Retrieve additional DHS variables #######
#######                                          ####### 
########################################################
########################################################

rm(list =ls())
#install.packages("tidyverse")
library(tidyverse)
#install.packages("devtools")
#devtools::install_github("ropensci/rdhs", ref = "issue33_path", force = TRUE)
library(rdhs)         
library(data.table)
#install.packages("ggplot2")
library(ggplot2)
library(foreign)
#install.packages("zoo")
library(zoo)
#install.packages("MAPLES")
library(MAPLES)
options(scipen=999)
options(digits=5)

memory.limit()
gc()



#setwd("D:/Anna/Dropbox/Projects/2021_Wilde/Heatwaves and fertility/1_Data/")
setwd("C:/Users/annak/Dropbox/Projects/2021_Wilde/Heatwaves and fertility/1_Data/")



## Set up your DHS credentials [password: barabani122!]
set_rdhs_config(email = "anna.k.dimitrova@gmail.com",
                project = "Climate shocks and childhood health")


set_rdhs_config(email = "anna.k.dimitrova@gmail.com",
                project = "Climate shocks and childhood health",
                config_path = "rdhs.json",
                cache_path = "dhs",
                global = FALSE)

## Make a list of eligible surveys and download them
surveys <- dhs_datasets()
str(surveys)

surveys <- surveys %>% 
  #dplyr::filter(SurveyType == "DHS") %>% 
  dplyr::filter(SurveyYear >= 2000) %>% 
  dplyr::filter(FileFormat == "Stata dataset (.dta)") %>% 
  dplyr::filter(FileType == "Births Recode") %>% 
  dplyr::filter(SurveyId != 'IA2006DHS')

years <- sort(as.numeric(unique(surveys$SurveyYear)))
years
unique(surveys$CountryName)
unique(surveys$SurveyId)


downloads <- get_datasets(surveys$FileName, reformat=TRUE, clear_cache = TRUE)
#print(downloads)

## Select relevant variables
vars = c("caseid", "bidx", "midx", "bord", "v001", "v002", "v003", 
         #distance to health facility
         "v467d", 
         #caste and religion
         "v130", "s116"
         
)

questions <- search_variables(surveys$FileName, variables = vars,  reformat=TRUE)

extract <- extract_dhs(questions, add_geo = F)

df0 <- rbindlist(extract, use.names=TRUE, fill=TRUE)


df0_add <- df0 %>% 
  mutate(caste = ifelse(SurveyId == "IA2015DHS" | SurveyId == "IA2020DHS", s116, NA)) %>%
  mutate(health_distance = v467d) %>% 
  mutate(religion = v130) %>% 
  dplyr::select(-c(v467d, v130, s116)) %>% 
  mutate(caste = ifelse(caste=="don't know", NA, caste)) %>% 
  mutate(health_distance_problem = ifelse(health_distance=="big problem" | health_distance=="a big problem" | health_distance=="Big problem"  | health_distance=="A big problem", 1,
                                  ifelse(health_distance=="9", NA, 0))) %>% 
  dplyr::select(-c(health_distance)) %>% 
  dplyr::rename(birth_id       = bidx,
                clust          = v001,
                hhl_id         = v002,
                line_id        = v003) 

unique(df0_add$health_distance_problem)
unique(df0_add$caste)

rm(list=setdiff(ls(), c("df0_add")))

load('DHS_data_harmonized.RData')

df1 <- df1 %>% 
  left_join(df0_add)


rm(list=setdiff(ls(), c("df1", "admin")))

save.image(file='DHS_data_harmonized.RData')





