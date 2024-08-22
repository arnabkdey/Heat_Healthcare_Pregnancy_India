###############################################################
###############################################################
#######                                                 ####### 
#######          PROJECT: Heatwaves & fertility         ####### 
#######                                                 ####### 
#######          CODE: Add spatial covariates           #######
#######                                                 ####### 
###############################################################
###############################################################
#options(scipen=999)
#options(digits=5)
library(tidyverse)
library(tmap)
library(sf)
library(units)
library(rgeos)
library(smoothr)
library(tiff)
library(sp)
library(raster)
library(rasterVis)
library(tmap)
library(tidyverse)
library(data.table)
library(rdhs)


rm(list =ls())

## Contents:
## 1. Retrieve and harmonise the admin level 1 regions for all countries
## 2. Harmonise the regional and district data for India
## 3. Combine the matched data for all countries
## 4. Add the harmonized admin information to the DHS data



################################################################################
## 1. Retrieve and harmonise the admin level 1 regions for all countries
################################################################################
# load the DHS PSU coordinates
setwd("D:/Anna/Dropbox/Projects/2021_Wilde/Heatwaves and fertility/1_Data/")

psu <- read.csv("psu_locations.csv")[-1]
psu <- psu %>%
  mutate_at(vars(DHSREGNA), funs(tolower(.))) %>%
  mutate_at(vars(ADM1NAME), funs(tolower(.))) %>%
  mutate_at(vars(region), funs(tolower(.))) %>% 
  unique()

unique(psu$CountryName)

psu$CountryName[psu$CountryName == "Congo Democratic Republic"] = "Democratic Republic of the Congo"
psu$CountryName[psu$CountryName == "Cote d'Ivoire"] = "C?te d'Ivoire"
psu$CountryName[psu$CountryName == "Eswatini"] = "Swaziland"
ctry_names <- unique(psu$CountryName)

# convert the PSUs to spatial data
sp <- psu
coordinates(sp) <- cbind(sp$LONGNUM , sp$LATNUM)
proj4string(sp) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
plot(sp)


# retrieve the latest DHS spatial boundaries for each country using the most recent survey
IDs <- psu %>% 
  mutate(SurveyYr = substr(SurveyId, 3, 6)) %>%      #add survey year
  dplyr::select(CountryName, SurveyId, SurveyYr) %>% 
  unique() %>% 
  group_by(CountryName) %>%
  filter(SurveyYr == max(SurveyYr)) %>% 
  subset(SurveyId != "DR2013SPE")
IDs <-  IDs$SurveyId
IDs



bord_list_tmp <- download_boundaries(surveyId = "AL2017DHS", method = "sf")
bord_sf_tmp <- do.call(rbind.data.frame, bord_list_tmp) #convert to special feature dataframe
bord_sf <- bord_sf_tmp                                  #bind the spatial data together

IDs <- IDs[-(1)]
IDs

for (a in IDs){
  bord_list_tmp <- download_boundaries(surveyId = a, method = "sf")
  bord_sf_tmp <- do.call(rbind.data.frame, bord_list_tmp) #convert to special feature dataframe
  bord_sf <- rbind(bord_sf, bord_sf_tmp)                  #bind the spatial data together
}

unique(bord_sf$DHSCC)
plot(bord_sf[1])


bord_sp <- sf:::as_Spatial(bord_sf)

save.image(file='DHS_spatial_data.RData')

rm(list =ls())


load("DHS_spatial_data.RData")

#plot(bord_sp)
#plot(sp, add=T)

df1 <- over(sp, bord_sp)
df2 <- cbind(psu, df1)

df3 <- df2 %>%
  dplyr::select(SurveyId, CountryName, clust, DHSREGNA, ADM1NAME, region, CNTRYNAMEE, DHSREGEN, REGNAME) %>% 
  mutate_at(vars(DHSREGEN), funs(tolower(.))) %>%
  mutate_at(vars(REGNAME), funs(tolower(.))) %>% 
  mutate(CNTRYNAMEE = ifelse(CNTRYNAMEE == "Congo Democratic Republic", "Democratic Republic of the Congo", CNTRYNAMEE)) %>% 
  mutate(CNTRYNAMEE = ifelse(CNTRYNAMEE == "Cote d'Ivoire", "C?te d'Ivoire", CNTRYNAMEE)) %>% 
  filter(CountryName!="India") 
  

## Correct mismatched areas
df3 <- df3 %>% 
  mutate(REGNAME = ifelse(CountryName=="Bolivia" & is.na(REGNAME), "tarija", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Burundi" & REGNAME == "western", "cankuzo", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Burundi" & REGNAME == "east", "muyinga", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Cambodia" & is.na(REGNAME) & region=="kandal", "kandal", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Cambodia" & is.na(REGNAME) & region=="banteay mean chey", "banteay meanchey", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Cambodia" & is.na(REGNAME) & region=="takeo", "takeo", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Cambodia" & is.na(REGNAME) & region=="kampot & krong kep", "kampot", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Cambodia" & is.na(REGNAME) & region=="kandaal", "kandal", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Cambodia" & is.na(REGNAME) & region=="krong preah sihanouk & kaoh kong", "koh kong", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Cambodia" & is.na(REGNAME) & region=="preah vihear & steung treng", "stung treng", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Cambodia" & is.na(REGNAME) & region=="svay rieng", "svay rieng", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Cameroon" & REGNAME=="hadjer-lamis", "extreme-nord", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Cameroon" & REGNAME=="mayo kebbi est", "extreme-nord", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Cameroon" & REGNAME=="mayo kebbi ouest", "nord", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Cameroon" & REGNAME=="n'djam?na", "extreme-nord", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Cameroon" & REGNAME=="north east", "extreme-nord", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Cameroon" & is.na(REGNAME), "sud", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="C?te d'Ivoire" & is.na(REGNAME) & region=="sud sans abidjan", "lagunes", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Dominican Republic" & (is.na(REGNAME) | REGNAME =="centre" | REGNAME=="nord-est" | REGNAME=="sud-est" ) & DHSREGNA=="0", "region 0", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Dominican Republic" & (is.na(REGNAME) | REGNAME =="centre" | REGNAME=="nord-est" | REGNAME=="sud-est" ) & DHSREGNA=="i", "region i", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Dominican Republic" & (is.na(REGNAME) | REGNAME =="centre" | REGNAME=="nord-est" | REGNAME=="sud-est" ) & DHSREGNA=="ii", "region ii", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Dominican Republic" & (is.na(REGNAME) | REGNAME =="centre" | REGNAME=="nord-est" | REGNAME=="sud-est" ) & DHSREGNA=="iii", "region iii", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Dominican Republic" & (is.na(REGNAME) | REGNAME =="centre" | REGNAME=="nord-est" | REGNAME=="sud-est" ) & DHSREGNA=="iv", "region iv", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Dominican Republic" & (is.na(REGNAME) | REGNAME =="centre" | REGNAME=="nord-est" | REGNAME=="sud-est" ) & DHSREGNA=="v", "region v", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Dominican Republic" & (is.na(REGNAME) | REGNAME =="centre" | REGNAME=="nord-est" | REGNAME=="sud-est" ) & DHSREGNA=="vi", "region vi", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Dominican Republic" & (is.na(REGNAME) | REGNAME =="centre" | REGNAME=="nord-est" | REGNAME=="sud-est" ) & DHSREGNA=="vii", "region vii", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Dominican Republic" & (is.na(REGNAME) | REGNAME =="centre" | REGNAME=="nord-est" | REGNAME=="sud-est" ) & DHSREGNA=="viii", "region viii", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Egypt" & (is.na(REGNAME) | REGNAME =="null") & DHSREGNA=="frontier governorates", "frontier governorates", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Egypt" & (is.na(REGNAME) | REGNAME =="null") & DHSREGNA=="urban governorates", "urban governorates", REGNAME))  %>% 
  mutate(REGNAME = ifelse(CountryName=="Swaziland" & is.na(REGNAME), "shiselweni", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Swaziland" & REGNAME=="maputo provincia\r\n", "lubombo", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Swaziland" & REGNAME=="mpumalanga", "manzini", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Ethiopia" & is.na(REGNAME), "benishangul-gumuz", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Gabon" & is.na(REGNAME), "haut-ogooue", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Guinea" & (is.na(REGNAME) | REGNAME=="northern"), DHSREGNA, REGNAME))  %>% 
  mutate(REGNAME = ifelse(CountryName=="Liberia" & (is.na(REGNAME) | REGNAME=="eastern" | REGNAME=="bas sassandra"), region, REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Liberia" & REGNAME=="monrovia", "south central" , REGNAME))  %>% 
  mutate(REGNAME = ifelse(CountryName=="Malawi" & (is.na(REGNAME) | REGNAME=="niassa" | REGNAME=="south west highlands" | REGNAME=="tete\r\n" | REGNAME=="zambezia\r\n"), paste(region,"region", sep=" "), REGNAME))  %>% 
  mutate(REGNAME = ifelse(CountryName=="Mali" & (is.na(REGNAME) | REGNAME=="sud"), region, REGNAME))  %>% 
  mutate(REGNAME = ifelse(CountryName=="Mozambique" & (is.na(REGNAME)), region, REGNAME))  %>% 
  mutate(REGNAME = ifelse(CountryName=="Nepal" & (is.na(REGNAME) | REGNAME=="bihar"), "terai", REGNAME))  %>% 
  mutate(REGNAME = ifelse(CountryName=="Nigeria" & (is.na(REGNAME)), region, REGNAME))  %>% 
  mutate(REGNAME = ifelse(CountryName=="Philippines" & (is.na(REGNAME) & (DHSREGNA=="armm" )), "barmm - bangsamoro autonomous region in muslim mindanao", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Philippines" & (is.na(REGNAME) & (DHSREGNA=="bicol" | DHSREGNA=="v - bicol" | DHSREGNA=="v - bicol region")), "region v - bicol", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Philippines" & (is.na(REGNAME) & (DHSREGNA=="calabarzon" | DHSREGNA=="iva - calabarzon")), "region iva - calabarzon", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Philippines" & (is.na(REGNAME) & (DHSREGNA=="caraga" | DHSREGNA=="xiii - caraga")), "caraga", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Philippines" & (is.na(REGNAME) & (DHSREGNA=="central luzon" | DHSREGNA=="iii - central luzon" )), "region iii - central luzon", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Philippines" & (is.na(REGNAME) & (DHSREGNA=="central visayas" | DHSREGNA=="vii - central visayas")), "region vii - central visayas", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Philippines" & (is.na(REGNAME) & (DHSREGNA=="i - ilocos" | DHSREGNA=="ilocos" | DHSREGNA=="i - ilocos region")), "region i - ilocos", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Philippines" & (is.na(REGNAME) & (DHSREGNA=="ivb - mimaropa" | DHSREGNA=="mimaropa" )), "mimaropa", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Philippines" & (is.na(REGNAME) & (DHSREGNA=="ix - zamboanga peninsula" | DHSREGNA=="zamboanga peninsula")), "region ix - zamboanga peninsula", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Philippines" & (is.na(REGNAME) & (DHSREGNA=="national capital region" )), "national capital region", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Philippines" & (is.na(REGNAME) & (DHSREGNA=="northern mindanao" | DHSREGNA=="x - northern mindanao")), "region x - northern mindanao", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Philippines" & (is.na(REGNAME) & (DHSREGNA=="vi - western visayas" | DHSREGNA=="western visayas")), "region vi - western visayas", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Philippines" & (is.na(REGNAME) & (DHSREGNA=="viii - eastern visayas" |DHSREGNA=="eastern visayas")), "region viii - eastern visayas", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Philippines" & (is.na(REGNAME) & (DHSREGNA=="xi - davao" | DHSREGNA=="xi - davao peninsula")), "region xi - davao", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Philippines" & (is.na(REGNAME) & (DHSREGNA=="xii - soccsksargen" )), "region xii - soccsksargen", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Senegal" & is.na(REGNAME) & DHSREGNA=="saint-louis", "nord", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Senegal" & is.na(REGNAME) & DHSREGNA=="dakar", "ouest", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Senegal" & is.na(REGNAME) & DHSREGNA=="kolda", "sud", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Senegal" & is.na(REGNAME) & DHSREGNA=="tambacounda", "sud", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Senegal" & is.na(REGNAME) & DHSREGNA=="thies", "ouest", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Senegal" & REGNAME=="kayes", "sud", REGNAME))  %>% 
  mutate(REGNAME = ifelse(CountryName=="Sierra Leone" & is.na(REGNAME), region, REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="South Africa" & is.na(REGNAME), region, REGNAME))  %>% 
  mutate(REGNAME = ifelse(CountryName=="Tanzania" & REGNAME=="kajiado", "northern", REGNAME)) %>% 
  mutate(REGNAME = ifelse(CountryName=="Zambia" & (REGNAME=="central region" | REGNAME=="south west highlands"), region, REGNAME))  %>% 
  mutate(REGNAME = ifelse(CountryName=="Zimbabwe" & (REGNAME=="tete\r\n"), region, REGNAME))  %>% 
  mutate(REGNAME = (ifelse(CountryName == "Tajikistan", region, REGNAME))) %>% 
  mutate(REGNAME = (ifelse(CountryName == "India" & is.na(REGNAME), region, REGNAME)))


## Hard to determine few regions in Kenya - drop 7 PSUs from DHS 2008 wave
df3 <- df3 %>% 
  mutate(REGNAME = ifelse(CountryName=="Kenya" & (is.na(REGNAME) | REGNAME=="bukedi"), NA, REGNAME)) 

check <- df3 %>% 
  filter(CountryName!=CNTRYNAMEE)

check <- df3 %>% 
  filter(CountryName=="Kenya")

df3 <- df3 %>% 
  dplyr::select(SurveyId, CountryName, clust, REGNAME) %>% 
  rename(region   = REGNAME)

df3 <- df3 %>% 
  filter(CountryName!="India")


################################################################################
## 2. Harmonise the regional and district data for India
################################################################################

psu_India <- psu %>% 
  filter(CountryName=="India")
sp_India <- psu_India 
coordinates(sp_India) <- cbind(sp_India$LONGNUM , sp_India$LATNUM)
proj4string(sp_India) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
plot(sp_India)

bord_sf_India <- bord_sf %>% 
  #dplyr::select(SurveyId, CountryName, clust, DHSREGNA, ADM1NAME, region, CNTRYNAMEE, DHSREGEN, REGNAME,) %>% 
  filter(CNTRYNAMEE=="India" ) %>% 
  filter(REGVAR=="shdist")

bord_sp_India <- sf:::as_Spatial(bord_sf_India)

df1_India <- over(sp_India, bord_sp_India)
df2_India <- cbind(psu_India, df1_India)
unique(df2_India$REGNAME)

df3_India <- df2_India %>%
  dplyr::select(SurveyId, CountryName, clust, DHSREGNA, ADM1NAME, region, CNTRYNAMEE, DHSREGEN, REGNAME) %>% 
  mutate_at(vars(DHSREGEN), funs(tolower(.))) %>%
  mutate_at(vars(REGNAME), funs(tolower(.)))

check <- df3_India %>% 
  filter(is.na(REGNAME))

df3_India <- df3_India %>% 
  mutate(ADM1NAME = ifelse(CountryName =="India" & (ADM1NAME=="dadra & nagar haveli" | ADM1NAME=="daman & diu"), "dadra & nagar haveli and daman & diu", ADM1NAME))

df3_India <- df3_India %>% 
  mutate(REGNAME=ifelse(is.na(REGNAME), region, REGNAME))

df3_India <- df3_India %>% 
  mutate(DHSREGNA = ifelse(DHSREGNA == "papumpare", "papum pare", DHSREGNA)) %>% 
  #mutate(DHSREGNA = ifelse(DHSREGNA == "buxar", "buxer", DHSREGNA)) %>% 
  mutate(DHSREGNA = ifelse(DHSREGNA == "korea (koriya)", "koriya", DHSREGNA)) %>% 
  mutate(DHSREGNA = ifelse(DHSREGNA == "kabirdham", "kabeerdham", DHSREGNA)) %>% 
  mutate(DHSREGNA = ifelse(DHSREGNA == "dakshin bastar dantewada", "dantewada", DHSREGNA)) %>% 
  mutate(DHSREGNA = ifelse(DHSREGNA == "janjgir - champa", "janjgir-champa", DHSREGNA)) %>% 
  mutate(DHSREGNA = ifelse(DHSREGNA == "banaskantha", "banas kantha", DHSREGNA)) %>% 
  mutate(DHSREGNA = ifelse(DHSREGNA == "sabarkantha", "sabar kantha", DHSREGNA)) %>% 
  mutate(DHSREGNA = ifelse(DHSREGNA == "panchmahal", "panch mahals", DHSREGNA)) %>% 
  mutate(DHSREGNA = ifelse(DHSREGNA == "lahul and spiti", "lahul & spiti", DHSREGNA)) %>% 
  mutate(DHSREGNA = ifelse(DHSREGNA == "kargil", "kargil", DHSREGNA)) %>% 
  mutate(DHSREGNA = ifelse(DHSREGNA == "leh", "leh", DHSREGNA)) %>% 
  mutate(DHSREGNA = ifelse(DHSREGNA == "saraikela kharsawan", "saraikela-kharsawan", DHSREGNA)) %>% 
  mutate(DHSREGNA = ifelse(DHSREGNA == "senapati (excluding 3 sub-divisions)", "senapati", DHSREGNA)) %>% 
  mutate(DHSREGNA = ifelse(DHSREGNA == "jaintia hills", "west jaintia hills", DHSREGNA)) %>% 
  mutate(DHSREGNA = ifelse(DHSREGNA == "siddharth nagar", "siddharthnagar", DHSREGNA)) %>% 
  mutate(DHSREGNA = ifelse(DHSREGNA == "sant ravidas nagar (bhadohi)", "sant ravidas nagar", DHSREGNA)) %>% 
  mutate(DHSREGNA = ifelse(DHSREGNA == "mahrajganj", "maharajganj", DHSREGNA)) %>% 
  mutate(DHSREGNA = ifelse(DHSREGNA == "mahbubnagar", "mahabubnagar", DHSREGNA)) %>% 
  mutate(DHSREGNA = ifelse(DHSREGNA == "rangareddy", "ranga reddy", DHSREGNA)) 

df3_India <- df3_India %>% 
  mutate(ADM1NAME = ifelse(DHSREGNA=="kargil", "jammu & kashmir", ADM1NAME)) %>% 
  mutate(ADM1NAME = ifelse(DHSREGNA=="leh", "jammu & kashmir", ADM1NAME)) %>% 
  mutate(DHSREGNA = ifelse(DHSREGNA=="warangal rural" | DHSREGNA=="warangal urban", "warangal", DHSREGNA)) %>% 
  mutate(DHSREGNA = ifelse(DHSREGNA=="paschim barddhaman" | DHSREGNA=="purba barddhaman", "barddhaman", DHSREGNA)) 


admin_list <- df3_India %>% 
  group_by(CountryName, SurveyId, ADM1NAME, DHSREGNA) %>% 
  summarise(n=n())

admin_check <- admin_list %>% 
  spread(key = SurveyId, value = n)


df3_India <- df3_India %>%
  dplyr::select(SurveyId, CountryName, clust, ADM1NAME, DHSREGNA) %>% 
  rename(region   = ADM1NAME,
         district = DHSREGNA)

unique(df3_India$region)


admin_list <- df3_India %>% 
  group_by(CountryName, SurveyId, region, district) %>% 
  summarise(n=n())

admin_check <- admin_list %>% 
  spread(key = SurveyId, value = n)


################################################################################
## 3. Combine the matched data for all countries
################################################################################

df3 <- bind_rows(df3, df3_India)
  
unique(df3$CountryName)

### Check again all countries

admin_check <- df3 %>% 
  filter(CountryName == "Bangladesh") %>% 
  group_by(CountryName, SurveyId, region, district) %>% 
  summarise(n=n()) %>% 
  spread(key = SurveyId, value = n)

admin <- df3

#admin$region <- paste(admin$CountryName,"_",admin$region, sep="")


rm(list=setdiff(ls(), c("admin")))
save.image(file='DHS_admin_data.RData')




################################################################################
## 4. Add the harmonized admin information to the DHS data
################################################################################

rm(list =ls())

load('DHS_data_harmonized.RData')
load("DHS_admin_data.RData") ## See codes 2_R_ADD SPATIAL COVARIATES   

df1$CountryName[df1$CountryName == "Congo Democratic Republic"] = "Democratic Republic of the Congo"
df1$CountryName[df1$CountryName == "Cote d'Ivoire"] = "C?te d'Ivoire"
df1$CountryName[df1$CountryName == "Eswatini"] = "Swaziland"

sort(unique(admin$CountryName))
sort(unique(df1$CountryName))


#df1 <- df1 %>% 
#  dplyr::select(-region)

df1 <- df1 %>% 
  left_join(admin)


unique(df1$visitor)

df1 <- df1 %>% 
  dplyr::select(-visitor)

# generate combination of country code and region identifier
#df1 <- df1 %>% 
#  unite(region, c("DHSCC", "region"))

df1$region <- paste(df1$DHSCC,"_",df1$region, sep="")

df1 <- df1 %>% 
  mutate(region = ifelse(region=="KE_NA", NA, region))

df1$district <- paste(df1$region,"_",df1$district, sep="")

df1 <- df1 %>% 
  mutate(district = ifelse(CountryName!="India", NA, district))

unique(df1$district)

df1 <- df1 %>% 
  mutate(migrated = ifelse((intYr - fertYr) > years_at_place, 1, 0))


save.image(file='DHS_data_harmonized.RData')


df1 <- df1 %>% 
  group_by(migrated) %>% 
  summarise(obs = n())























