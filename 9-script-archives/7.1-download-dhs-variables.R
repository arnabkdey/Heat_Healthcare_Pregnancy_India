pacman::p_load(tidyverse, data.table, janitor, fst, openxlsx, googledrive, here, beepr, Hmisc)
library(rdhs)         
library(foreign)
library(zoo)
# install.packages("MAPLES")
# library(MAPLES)
options(scipen=999)
options(digits=5)



## Set up your DHS credentials [password: barabani122!]
set_rdhs_config(email = "adey@ucsd.edu",
                project = "Global analysis of the associations between extreme temperatures and intimate partner violence",
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
  dplyr::filter(CountryName == "India") %>%
  dplyr::filter(SurveyId != 'IA2006DHS')

years <- sort(as.numeric(unique(surveys$SurveyYear)))
years
unique(surveys$CountryName)
unique(surveys$SurveyId)


downloads <- get_datasets(surveys$FileName, reformat=TRUE, clear_cache = TRUE)
#print(downloads)

## Select relevant variables
vars = c("caseid", "bidx", "midx", "bord", "v001", "v002", "v003", "v135", "m15", "v201",
        "m14",
        # delivery complications
         "s439", "s440", "s441", "s442", "s443"
)

questions <- search_variables(surveys$FileName, variables = vars,  reformat=TRUE)

extract <- extract_dhs(questions, add_geo = F)

df0 <- rbindlist(extract, use.names=TRUE, fill=TRUE)
head(df0)

df0_add <- df0 %>% 
    mutate(comp_breech = ifelse(SurveyId == "IA2015DHS", s439, s441)) %>% 
    mutate(comp_prolonged = ifelse(SurveyId == "IA2015DHS", s440, s442)) %>% 
    mutate(comp_excessive = ifelse(SurveyId == "IA2015DHS", s441, s443)) %>% 
    dplyr::select(-c(s439, s440, s441, s442, s443)) %>%
    dplyr::rename(birth_id = bidx,
                clust = v001,
                hhl_id = v002,
                line_id = v003,
                mat_parity = v201)

setDT(df0_add)
nrow(df0_add)

# Save the dataset
write.fst(df0_add, here(path_processed_data, "7.1-dhs-4-5-BR-new-vars.fst"))