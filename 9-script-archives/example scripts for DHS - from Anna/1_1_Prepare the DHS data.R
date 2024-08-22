########################################################
########################################################
#######                                          ####### 
#######      PROJECT: Heatwaves & fertility      ####### 
#######                                          ####### 
#######       CODE: Retrieve the DHS data        #######
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


## Contents:
## 1. Retrieve the survey data
## 2. Harmonize and clean the survey data
## 3. Harmonize the wealth index



setwd("D:/Anna/Dropbox/Projects/2021_Wilde/Heatwaves and fertility/1_Data/")
setwd("C:/Users/annak/Dropbox/Projects/2021_Wilde/Heatwaves and fertility/1_Data/")


## Set up your DHS credentials [password: barabani122!]
set_rdhs_config(email = "anna.k.dimitrova@gmail.com",
                project = "Climate shocks and childhood health")


set_rdhs_config(email = "anna.k.dimitrova@gmail.com",
                project = "Climate shocks and childhood health",
                config_path = "rdhs.json",
                cache_path = "dhs",
                global = FALSE)

################################################################################
## 1. Retrieve the survey data
################################################################################
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
vars = c("caseid", "bidx", "midx", "bord", "v001", "v002", "v003", "v005", "v006", "v007", "v008", "v009", "v010", "v011", "v012", "v013", "v024", "v025", "v104", "v106", "v135", "v190", "v191", "b1", "b2", "b3", "b4",
         #variables for recalculating the wealth index
         "v113", "v115", "v116","v127", "v128", "v129", "v161", "v119", "v120", "v121", "v122", "v123", "v124", "v125", "v153", "v136", "v745b",
         #additional variables
         "m2n", "m13", "m14", "v445", "b19", "v213", "v455", "v456",
         #distance to health facility
         "v467d", 
         #antenatal care
         "m2a", "m2b", "m2c", "m2d", "m2e", "m2f", "m2g", "m2h", "m2i", "m2j", "m2k", "m2l", "m2m", "m2n",
         #caste and religion
         "v130", "s116"
         
  )
  
questions <- search_variables(surveys$FileName, variables = vars,  reformat=TRUE)
   
## Extract the data (adding geographical covariates: add_geo = TRUE)

## too many surveys, the code crashes, extract stepwise
extract <- extract_dhs(questions, add_geo = T)

df0 <- rbindlist(extract, use.names=TRUE, fill=TRUE)

CountryName <- surveys %>%
  dplyr::select(SurveyId, SurveyYear, CountryName)
 

## Filter out observations without valid GPS coordinates
df0 <- df0 %>% 
  filter(!is.na(LATNUM)) %>% 
  filter(!is.na(LONGNUM)) %>%  
  filter(LATNUM!=0 | LONGNUM!=0) %>%                  #LAT=0 and LONG=0 are missing coordinates  
  filter(LATNUM <= -0.00005 | LATNUM >= 0.00005) %>%  #missing obs. - remove
  mutate(DHSCC = substr(SurveyId, 1, 2)) %>%          #add country code identifier 
  left_join(CountryName)                              #add country names   
  


unique(df0$SurveyId)

rm(list=setdiff(ls(), c("df0", "questions")))


save.image(file='DHS_data_extract.RData')  
  

rm(list =ls())



################################################################################
## 2. Harmonize and clean the survey data
################################################################################
load('DHS_data_extract.RData')

  
## Clean the data 
## NOT RELEVANT! subset to births in the past 10 years to reduce misreporting bias 
## and to women who gave birth between age 15 and 44 
df1 <- df0 %>% 
  dplyr::rename(birth_id       = bidx,
                clust          = v001,
                hhl_id         = v002,
                line_id        = v003,
                wt             = v005,
                intMo          = v006,
                intYr          = v007,
                age            = v012,  #women's age 
                age_gr5        = v013,  #5-year age groups
                region         = v024,
                residence      = v025,
                edu            = v106,
                wealth_old         = v190,
                wealth_score_old   = v191,
                #dobYr          = v010,  #year of birth of the mother
                #dobMo          = v009,  #month of birth of the mother
                dobcmc         = v011,  #date of birth of the mother (cmc)
                age            = v012,
                fertYr         = b2,    #year of giving birth  
                fertMo         = b1,
                fertcmc        = b3,
                intcmc         = v008,
                gender         = b4
                #dur_preg      = b20
              )  %>% 
  mutate(wt = wt/1000000) %>% 
  mutate(fertAge = as.integer((fertcmc - dobcmc)/12)) %>%    #age at giving birth
  subset(fertAge >= 15 & fertAge <= 44) %>%      #restrict to women in the age group 15 to 44 at giving birth  
  #filter((intcmc - fertcmc)/12 <= 10) %>%        #keep only births that happened in the past 10 years from the interview date
  #mutate(fertDate=as.yearmon(paste(fertYr, fertMo), "%Y %m")) %>% 
  mutate(birth = 1) %>%                          #indicator for giving birth
  ## correct cmc dates for Ethiopia and Nepal
  mutate(intcmc = ifelse(DHSCC == "ET", intcmc+92, intcmc)) %>% 
  mutate(fertcmc = ifelse(DHSCC == "ET", fertcmc+92, fertcmc)) %>% 
  mutate(dobcmc = ifelse(DHSCC == "ET", dobcmc+92, dobcmc)) %>% 
  mutate(intcmc = ifelse(DHSCC == "NP", intcmc-681, intcmc)) %>% 
  mutate(fertcmc = ifelse(DHSCC == "NP", fertcmc-681, fertcmc)) %>% 
  mutate(dobcmc = ifelse(DHSCC == "NP", dobcmc-681, dobcmc)) %>% 
  #generate year and month variables based on the corrected cmc dates
  #for using cmc to determine year and month in DHS see: https://dhsprogram.com/data/Guide-to-DHS-Statistics/index.cfm
  mutate(fertYr = as.integer((fertcmc - 1)/12)+1900) %>% 
  mutate(fertMo = fertcmc - ((fertYr - 1900)*12)) %>% 
  mutate(intYr = as.integer((intcmc - 1)/12)+1900) %>% 
  mutate(intMo = intcmc - ((intYr - 1900)*12)) 



## Years lived at the current place of residence = v104 --> multiple surveys with missing observations
aggregate(v104 ~ SurveyId, data=df1, function(x) {sum(is.na(x))}, na.action = NULL)
unique(df1$v104)

df1 <- df1 %>% 
  mutate(v104_num = as.numeric(v104)) %>% 
  mutate(years_at_place = v104_num) %>% 
  mutate(years_at_place = ifelse(v104 == "always", 50, years_at_place)) %>% 
  mutate(years_at_place = ifelse(v104 == "Always", 50, years_at_place)) %>% 
  mutate(years_at_place = ifelse(v104 == "Always", 50, years_at_place)) %>% 
  mutate(years_at_place = ifelse(v104 == "less than one year", 1, years_at_place)) %>%
  mutate(years_at_place = ifelse(v104 == "inconsistent: time > age of respondent (v012)", NA, years_at_place)) %>% 
  mutate(years_at_place = ifelse(v104 == "99", NA, years_at_place)) %>% 
  mutate(years_at_place = ifelse(v104 == "Inconsistent", NA, years_at_place)) %>% 
  mutate(years_at_place = ifelse(v104 == "visitor", NA, years_at_place)) %>% 
  mutate(years_at_place = ifelse(v104 == "Visitor", NA, years_at_place)) 
  
unique(df1$years_at_place)

## Indicator for respondents who are usual residents at the place of interview = v135
aggregate(v135 ~ SurveyId, data=df1, function(x) {sum(is.na(x))}, na.action = NULL)
unique(df1$v135)

df1$visitor[df1$v135 == "visitor"] <- 1
df1$visitor[df1$v135 == "Visitor"] <- 1
df1$visitor[df1$v135 == "usual resident"] <- 0
df1$visitor[df1$v135 == "Usual resident"] <- 0

## Keep only women who are permanent residents at the place of interview
df1 <- df1 %>% 
  filter(visitor == 0)

################################################################################
## 3. Harmonize the wealth index
################################################################################
# Harmonize the variables 
# Perform PCA
# Generate new wealth categories


#removing upper case letters for easier harmonization
obj <- c("v113", "v115", "v116","v127", "v128", "v129", "v161", "v119", "v120", "v121", "v122", "v123", "v124", "v125", "v153", "v136", "v745b")
obj
df1 <- df1 %>%
  mutate_at(vars(obj), funs(tolower(.)))


df1$v127 <- str_remove(df1$v127, "\\.")
df1$v127 <- str_remove(df1$v127, "\\.")
df1$v127 <- str_remove(df1$v127, "\\.")
df1$v127 <- str_remove(df1$v127, "\\.")
df1$v127 <- str_remove(df1$v127, "\\?")

unique(df1$v127)

df1 <- df1 %>% 
  #Floor material 
  mutate(floor = v127) %>% 
  mutate(floor = ifelse(v127=="mud/clay/earth" 
                        | v127=="mud / clay / earth"
                        | v127=="clay / sand"
                        | v127=="argile, banco"                                
                        | v127=="earth"                                       
                        | v127=="sand"                                        
                        | v127=="dung"                                        
                        | v127=="dirt"                                       
                        | v127=="earth/sand"                                 
                        | v127=="earth /sand"                                
                        | v127=="Earth/ sand"                                
                        | v127=="terre/sable"                                # earth/sand
                        | v127=="natural floor - earth/sand"                 
                        | v127=="natural - earth / sand"                     
                        | v127=="natural floor mud/earth"                    
                        | v127=="natural - dung"                             
                        | v127=="earth, sand"                                                              
                        | v127=="Earth, sand"                                
                        | v127=="dirt/sand"                                                               
                        | v127=="earth/sand/clay"                             
                        | v127=="earth/mud/dung"                             
                        | v127=="earth/sand/mud"                             
                        | v127=="earth / sand"                                 
                        | v127=="earth (\"terra batida\")/sand"              
                        | v127=="earth (terra batida)"                        
                        | v127=="earth (terra n?o batida)" 
                        | v127=="earth (terra n?o batida)"
                        | v127=="earth/sand/clay"                             
                        | v127=="earth, sand, mud"                           
                        | v127=="earth, sand, dung"                          
                        | v127=="earth, sand/dung"                           
                        | v127=="earth/Sand/Mud"                             
                        | v127=="mud/earth/dung"                             
                        | v127=="earth and dung"                             
                        | v127=="animal dung"                                
                        | v127=="dung"                                       
                        | v127=="mud stones"
                        | v127=="dirt/earth"
                        | v127=="earth, mud, dung"
                        | v127=="earth/ sand"
                        | v127=="earth/bamboo"
                        | v127=="earth/bamboo (katcha)"
                        | v127=="earth/clay"
                        | v127=="manure"
                        | v127=="mud mixed with dung"
                        | v127=="natural floor"
                        | v127=="earth (terra batida)"
                        | v127=="earth (terra n?o batida)"
                        | v127=="earth, sand, dung"
                        | v127=="mud plasterwork"
                        | v127=="tierra"
                        | v127=="stone"                                      # not sure
                        | v127=="stones"                                     # not sure 
                        | v127=="11"                                         # natural
                        | v127=="12"                                         # natural 
                        | v127=="13"     
                        | v127=="nothing/unpaved earth" # natural 
                        | v127=="earth/sand", 1, floor)) %>%
  mutate(floor  = ifelse(v127=="palm / bamboo" 
                         | v127=="palm, bamboo"                              
                         | v127=="bamboo"                                    
                         | v127=="palm/bambou"                               
                         | v127=="palm / bamboo"                             
                         | v127=="palm/bamboo"                               
                         | v127=="Wood planks, bamboo, plam"                  # not sure if bamboo or wood planks category
                         | v127=="palm, bamboo, leeds"                       
                         | v127=="palm/bamboo/leeds"                         
                         | v127=="palm/bamboo/leaves"
                         | v127=="palm/ bamboo"
                         | v127=="reed, bamboo"                              
                         | v127=="cane"    
                         | v127=="palms"    
                         | v127=="reed / bamboo"                             
                         | v127== "palm/bamboo", 2, floor)) %>% 
  mutate(floor  = ifelse(v127=="raw wood planks" 
                         | v127=="floor wood planks"                         
                         | v127=="bare wood planks"                          
                         | v127=="wood"                                      
                         | v127=="wood bats (\"tacos de madeira\")"          
                         | v127=="planks"   
                         | v127=="rudimentary floor - wood planks"
                         | v127=="wood planks, palm, bamboo"
                         | v127=="wood/palm/bamboo"
                         | v127=="wood planks, bamboo, plam"                  # not sure 
                         | v127=="rudimentary floor wood planks"             
                         | v127=="rudimentary wood planks"                   
                         | v127=="rudimentary - wood planks"                 
                         | v127=="bois/autres végétaux"                     # wood/other plants
                         | v127=="wood planks"   
                         | v127=="wood plank"
                         | v127=="wood planks/timber"
                         | v127=="wood planks, bamboo, plam"
                         | v127=="bois/autres v?g?taux" 
                         | v127=="house boat"
                         | v127=="tablets / wood planks"
                         | v127=="makeshift/salvaged/improvised materials"
                         | v127=="21"                                        
                         | v127=="22", 2, floor)) %>% 
  mutate(floor  = ifelse(v127=="brick"                                      
                         | v127=="cement / bricks"                            # not sure if cement or brick category
                         | v127=="bricks without cement"                     
                         | v127=="broken bricks"  
                         | v127=="tiles/ bricks"
                         | v127=="bricks"                                    
                         | v127=="brick/concrete" 
                         |  v127=="adobe (paved earth)"  
                         | v127=="adobe", 3, floor)) %>% 
  mutate(floor  = ifelse(v127=="cement"                                      
                         | v127=="ciment"   
                         | v127=="cement, concrete"
                         | v127=="cement, tiles"
                         | v127=="cement/ tile"
                         | v127=="cement/brick"
                         | v127=="cement/gravel"
                         | v127=="stone, brick"
                         | v127=="cement / bricks"                            # not sure 
                         | v127=="cement screed"                             
                         | v127=="cement tiles"                              
                         | v127=="cement tiles (mosaic)"                     
                         | v127=="cement/concrete"                           
                         | v127=="concrete/cement"                           
                         | v127=="concrete, cement"                          
                         | v127=="cement screed"                             
                         | v127=="concrete cement"    
                         | v127=="ceramic tiles/terazzo"
                         | v127=="cement tiles/mosaic"
                         | v127=="cements"
                         | v127=="granite"
                         | v127=="concrete"                                  
                         | v127=="finished - cement", 3, floor)) %>%
  mutate(floor = ifelse(v127=="vinyl or asphalt strips"                     
                        | v127=="asphlat tiles/vynil"                        
                        | v127=="Asphlat tiles/vynil"                        
                        | v127=="vinyl or aspalt strips"                     
                        | v127=="vinyl or asphalt"                           
                        | v127=="vinyl,asphalt strips"                       
                        | v127=="Vinyl, asphalt strips"                      
                        | v127=="vinyl /asphalt strips"                      
                        | v127=="vinyl/ asphalt strips"
                        | v127=="vinyl"  
                        | v127=="vinyl / asphalt"
                        | v127=="vinyl or linoleum"                          
                        | v127=="vinyl, linoleum"                            
                        | v127=="vinyl tile/vinyl carpet"                    
                        | v127=="vinyl, asphalt strips"                      
                        | v127=="floor mat, linoleum, vinyl"                 
                        | v127=="floor mat/linoleum/vinyl"                   
                        | v127=="vinyl(pvc) or asphalt strips"               
                        | v127=="vinyl (pvc) or asphalt strips"              
                        | v127=="finished - vinyl or asphalt strips"         
                        | v127=="floor mat, linoleum, vinyl"                 
                        | v127=="vinyl sheets/tiles"                         # not sure if vinyl or tiles cat
                        | v127=="vinyl tile/vinyl carpet" 
                        | v127=="vinyl / asphalt strips"
                        | v127=="vinyl or alphalt strips"
                        | v127=="vinyl,  linoleum"
                        | v127=="vinyl, linoleum, ceramic"
                        | v127=="vinyl/asphalt strips"
                        | v127=="vynil or asphalt strips"
                        | v127=="lino/gerflex"
                        | v127=="linoleum, carpet"
                        | v127=="linoleum"                                   
                        | v127=="linoleum/rubber carpet", 4, floor)) %>% 
  mutate(floor  = ifelse(v127=="laminated or polished wood"                 
                         | v127=="parquet or polished wood"                  
                         | v127=="Parquet or polished wood"                  
                         | v127=="parquet, polished wood"                   
                         | v127=="parquet,polished wd"                       
                         | v127=="parquet, polished wd"                      
                         | v127==" parquet, polished wood"                   
                         | v127=="parquet /polish wood"                      
                         | v127=="Parquet/ polished wood"                    
                         | v127=="Parquet, polished wood"                    
                         | v127=="finished floor parquet or polished wood"   
                         | v127=="finished - parquet or polished wood"   
                         | v127=="finished floor - parquet or polished wood or laminate"
                         | v127=="finished floor - vinyl or linoleum"
                         | v127=="parket"
                         | v127=='"machimbre" / parquet'
                         | v127=="parquet / polished wood"
                         | v127=="parquet/ polished wood"
                         | v127=="parquet/polished wood"
                         | v127=="polished wood, parquet"
                         | v127=="polished wood/parquet"
                         | v127=="coconut lumber"
                         | v127=="31"                                         # finished
                         | v127=="32"                                         # finished
                         | v127=="33"                                         # finsihed 
                         | v127=="34"                                         # finsihed 
                         | v127=="35"                                         # finished 
                         | v127== "polished wood", 5, floor)) %>% 
  mutate(floor  = ifelse(v127=="ceramic mosaics"                             
                         | v127=="mosaic or tiles" 
                         | v127=="mosaic/tile" 
                         | v127=="ceramic tiles" 
                         | v127=="ceramic tiles, terrazzo"
                         | v127=="Ceramic tiles"                             
                         | v127=="Ceramic tiles/Terazzo"                     
                         | v127=="ceramic tiles/terrazo"                     
                         | v127=="ceramic/terrazzo tiles"                    
                         | v127=="ceramic tiles/ terrazo tiles"              
                         | v127=="ceramic tiles, terazzo"                   
                         | v127=="ceramic/marble/porcelain tiles / terrazo" 
                         | v127=="chips/terrazzo"         
                         | v127=="ceramic / mosaic / tiles"
                         | v127=="mosaic/ceramic"                            
                         | v127=="tile"                                      
                         | v127=="tiles"                                     
                         | v127=="cement tiles/brick"                         # not sure
                         | v127=="ceramic tiles/coastal brick"               
                         | v127== "brick tiles"                               # not sure if tiles or brick cat
                         | v127=="finished - ceramic tiles"                  
                         | v127=="carrelage"                                  # floor tile
                         | v127=="ceramic"
                         | v127=="ceramic tiles, marble chips"
                         | v127=="ceramic tiles, vinyl, bricks"
                         | v127=="ceramic tyles"
                         | v127=="ceramic/ marble tile"
                         | v127=="ceramic/ marmol"
                         | v127=="ceramic/marble tiles"
                         | v127=="ceramic/marble tiles tiles"
                         | v127=="ceramic/marble/granite"
                         | v127=="ceramics tiles"
                         | v127=="granite / marble / ceramic"
                         | v127=="granite, ceramic tiles"
                         | v127=="marble, ceramic tile"
                         | v127=="finished floor - carpeted"
                         | v127=="finished floor - cement"
                         | v127=="finished floor - ceramic or marble tiles"
                         | v127=="chips/terrazo"
                         | v127=="tile, ceramic"
                         | v127=="tile/ cement"
                         | v127=="terrazo"
                         | v127=="terrazzo"
                         | v127=="Tiles/ bricks", 5, floor)) %>%
  mutate(floor = ifelse(v127=="marble"                                       
                        | v127=="marble/ceramic tiles"                       
                        | v127=="marble/granite"    
                        | v127=="polished stone / marble / granite"
                        | v127=="ceramic/marble/porcelain/tiles/terrazo"
                        | v127=="polished stone/marble/granite", 5, floor)) %>%
  mutate(floor = ifelse(v127=="other"                                        
                        | v127=="autre"                                      
                        | v127=="oTHER"                                      
                        | v127=="other finished"                             
                        | v127=="autres matériaux moderne"                  # other modern materials
                        | v127=="floating house", 5, floor)) %>% 
  mutate(floor  = ifelse(v127=="carpeted"                                     
                         | v127=="carpet"                                    
                         | v127=="carpet/ rug"
                         | v127=="wall to wall carpet"
                         | v127=="rug, carpet"
                         | v127=="woolen carpets/synthetic carpets"
                         | v127=="woolen carpet/synthetic carpet"
                         | v127=="mats"                                       
                         | v127=="mat"                                       
                         | v127=="finished - carpet"                         
                         | v127=="woolen carpets/ synthetic carpet"          
                         | v127=="autres mat?riaux moderne", 5, floor)) %>% 
  mutate(floor = ifelse(v127=="not a de jure resident"                       
                        | v127=="not a de jure resident"                     
                        | v127=="not a dejure resident"                      
                        | v127=="not de-jure resident"                       
                        | v127=="not de jure"                                
                        | v127=="not de jure resident"                       
                        | v127=="not dejure resident"                        
                        | v127=="not a de jure member"
                        | v127=="not dejure member"
                        | v127=="97"                                         
                        | v127=="99", NA, floor))


aggregate(floor ~ SurveyId, data=df1, function(x) {sum(is.na(x))}, na.action = NULL)

unique(df1$floor)


df1 <- df1 %>%  
  #cooking fuel
  mutate(fuel = v161) %>% 
  mutate(fuel = ifelse(v161=="coal, lignite"                 
                       | v161=="Coal, lignite"               
                       | v161=="coal/lignite"                
                       | v161=="briquette"                    #solid, biofuel subst for coal/charcoal
                       | v161=="charcoal"                    
                       | v161=="cardboard/paper"             
                       | v161=="straw/shrubs/grass"          
                       | v161=="straw / shrubs / grass"      
                       | v161=="maize or other crop waste"   
                       | v161=="agricultural crop"           
                       | v161=="crop waste"                  
                       | v161=="saw dust"                    
                       | v161=="sawdust/wood chips"          
                       | v161=="firewood/straw"              
                       | v161=="firewood, straw"             
                       | v161=="wood chips"                  
                       | v161=="animal dung"   
                       | v161=="animal dung/waste"                  
                       | v161=="garbage/plastic"
                       | v161=="dung"
                       | v161=="improved smokeless chulo"
                       | v161=="coal, wood"
                       | v161=="firewood, charcoal"
                       | v161=="crop residue/grass"
                       | v161=="mineral coal"
                       | v161=="sawdust"
                       | v161=="sawdust / wood chips"
                       | v161=="traditional firewood/charcoal/dung"
                       | v161=="wood"
                       | v161== "processed biomass (pellets) or woodchips"
                       | v161=="firewood"
                       | v161=="9"                            # Straw/shrubs/grass
                       | v161=="17"
                       | v161=="15"
                       | v161=="wood", 0, fuel)) %>% 
  mutate(fuel = ifelse(v161=="biogas" 
                       | v161=="biogaz" 
                       | v161=="bottled gas" 
                       | v161=="piped natural gas"
                       | v161=="natural gas"                 
                       | v161=="natural gas/biogas"
                       | v161=="gasoline/diesel"  
                       | v161=="cylinder gas"
                       | v161=="alcohol/ethanol"
                       | v161=="gasoline"
                       | v161=="propane gas"
                       | v161=="kerosene/paraffin/petroleum"
                       | v161=="kerosene, oil, cocinol, diesel, gasoline, alcohol" , 1, fuel)) %>% 
  mutate(fuel = ifelse(v161=="electricity"                   
                       | v161=="electricity from generator"  
                       | v161=="electricity from other source", 1, fuel)) %>% 
  mutate(fuel = ifelse(v161=="kerosene"                      
                       | v161=="paraffin"  
                       | v161=="paraffin/kerosene"
                       | v161=="liquefied petroleum gas (lpg)/cooking gas"
                       | v161=="oil/paraffin/kerosene"
                       | v161=="kerosene/paraffin"           
                       | v161=="paraffin/kerosine"              
                       | v161=="parafin/ kerosene"           
                       | v161=="jelly"                        # not sure
                       | v161=="petroleum/kerosene", 1, fuel)) %>%  
  mutate(fuel = ifelse(v161=="lpg"                           
                       | v161=="lpg/natural gas" 
                       | v161=="lpg / natural gas"           
                       | v161=="lpg, natural gas"            
                       | v161=="lpg/cylinder gas", 1, fuel)) %>% 
  mutate(fuel = ifelse(v161=="solar energy"                  
                       | v161=="solar power"                 
                       | v161=="solar", 1, fuel)) %>%    
  mutate(fuel = ifelse(v161=="other", 1, fuel)) %>% 
  mutate(fuel = ifelse(v161=="no food cooked in house" 
                       | v161=="no cocina"
                       | v161=="no food cooked in hh"        
                       | v161=="no food cooked in household" 
                       | v161=="do not cook"                 
                       | v161=="does not cook" 
                       | v161=="don't cook"  
                       | v161=="no food cooked in house", 1, fuel)) %>% 
  mutate(fuel = ifelse(v161=="97"                            
                       | v161=="99", NA, fuel)) %>% 
  mutate(fuel = ifelse(v161=="not a de jure resident"        
                       | v161=="not a dejure resident"       
                       | v161=="not de-jure resident"        
                       | v161=="not dejure resident"         
                       | v161=="not dejure member", NA, fuel))



aggregate(v161 ~ SurveyId, data=df1, function(x) {sum(is.na(x))}, na.action = NULL)

unique(df1$fuel)

unique(df1$v125)

## Ownership of select assets
## NOTE: some missing obs - can we impute them? for now missing ons are assumed to be 0!
df1 <- df1 %>% 
  #Assets - electricity, radio, TV, fridge, bicycle, motorbike, car
  mutate(electricity = ifelse(v119=="yes", 1, 0))  %>% 
  mutate(radio = ifelse(v120=="yes", 1, 0))  %>% 
  mutate(tv = ifelse(v121=="yes", 1, 0))  %>% 
  mutate(fridge = ifelse(v122=="yes", 1, 0))  %>% 
  mutate(bike = ifelse(v123=="yes", 1, 0))  %>% 
  mutate(motorbike = ifelse(v124=="yes", 1, 0)) %>%  
  mutate(car = ifelse(v125=="yes", 1, 0)) 

aggregate(v119 ~ SurveyId, data=df1, function(x) {sum(is.na(x))}, na.action = NULL)

df1 <- df1 %>% 
  #filter(!is.na(electricity) & !is.na(radio) & !is.na(tv) & !is.na(fridge) & !is.na(bike)& !is.na(motorbike) & !is.na(car) & !is.na(floor) & !is.na(fuel)) %>% 
  mutate(floor = as.numeric(floor)) %>% 
  mutate(fuel = as.numeric(fuel))


## Convert floor material to series of dummies
#install.packages("fastDummies")
library('fastDummies')
df1 <- dummy_cols(df1, select_columns = 'floor')
  

## Perform PCA
#install.packages("factoextra")
library(factoextra)
library(tidyverse)  

pca.df <- df1 %>% 
  dplyr::select(floor_1, floor_2, floor_3, floor_4, floor_5, fuel, electricity, radio, tv, fridge, bike, motorbike, car) %>% 
  na.omit()

str(pca.df) #all variables need to be numeric

pca.model <- prcomp(pca.df, scale = TRUE)
fviz_eig(pca.model)
pca.pred <- predict(pca.model, newdata = df1)
pca.pred.df <- as.data.frame(pca.pred) %>% 
  dplyr::select(PC1) %>% 
  rename(wealth_score_new = PC1)

df1 <- df1 %>% #add the new scores to the main data
  cbind(pca.pred.df) 


df1 <- df1 %>% 
  mutate(wealth_score_old = wealth_score_old/1000000) %>% 
  mutate(wealth_score = -wealth_score_new)

ggplot(df1, aes(x=wealth_score_old, y=wealth_score)) + geom_point()
hist(df1$wealth_score)

## Generate 5 equally sized wealth groups - 1 (poorest) to 5 (wealthiest)
#install.packages("Hmisc")
library(Hmisc) # cut2
df1$wealth_new <- as.numeric(cut2(df1$wealth_score, g=5))

unique(df1$wealth_new)
df1$wealth <- NA
df1$wealth[df1$wealth_new == 1] <- "poorest"
df1$wealth[df1$wealth_new == 2] <- "poorer"
df1$wealth[df1$wealth_new == 3] <- "middle"
df1$wealth[df1$wealth_new == 4] <- "richer"
df1$wealth[df1$wealth_new == 5] <- "richest"


## Harmonize education. residence and wealth_old
unique(df1$wealth_old)
df1$wealth_old[df1$wealth_old == "Poorest"] <- "poorest"
df1$wealth_old[df1$wealth_old == "Poorer"] <- "poorer"
df1$wealth_old[df1$wealth_old == "Richer"] <- "richer"
df1$wealth_old[df1$wealth_old == "Richest"] <- "richest"
df1$wealth_old[df1$wealth_old == "Middle"] <- "middle"
df1$wealth_old[df1$wealth_old == "lowest"] <- "poorest"
df1$wealth_old[df1$wealth_old == "second"] <- "poorer"
df1$wealth_old[df1$wealth_old == "fourth"] <- "richest"
df1$wealth_old[df1$wealth_old == "highest"] <- "middle"

unique(df1$edu)
df1$edu[df1$edu == "Secondary"] <- "secondary"
df1$edu[df1$edu == "Primary"] <- "primary"
df1$edu[df1$edu == "basic"] <- "primary"
df1$edu[df1$edu == "Higher"] <- "higher"
df1$edu[df1$edu == "No education"] <- "no education"
df1$edu[df1$edu == "others"] <- NA
df1$edu[df1$edu ==  "don't know" ] <- NA
df1$edu[df1$edu == "9"] <- NA

unique(df1$residence)
df1$residence[df1$residence == "Urban"] <- "urban"
df1$residence[df1$residence == "Rural"] <- "rural"

unique(df1$gender)
df1$gender[df1$gender == "Female"] <- "female"
df1$gender[df1$gender == "Male"] <- "male"

psus <- df1 %>% 
  dplyr::select(SurveyId, SurveyYear, CountryName, LATNUM, LONGNUM, DHSCC, clust, DHSREGNA, ADM1NAME, region) %>% 
  unique()

write.csv(psus, "psu_locations.csv")



## Save the data for further analysis
df1 <- df1 %>% 
  dplyr::select(SurveyId, SurveyYear, CountryName, LATNUM, LONGNUM, DHSCC, clust,
                hhl_id, line_id, birth_id, midx, caseid, wt,
                visitor, years_at_place, bord,
                intMo, intYr, intcmc, dobcmc, age, age_gr5, gender,
                residence, edu, wealth_score, wealth,
                fertMo, fertYr, fertcmc, fertAge, birth,
                m13, m14, v445, b19, v213, v455, v456, 
                m2a, m2b, m2c, m2d, m2e, m2f, m2g, m2h, m2i, m2j, m2k, m2l, m2m, m2n
                )



rm(list=setdiff(ls(), c("df1")))

save.image(file='DHS_data_harmonized.RData')

sort(unique(df1$fertYr))



