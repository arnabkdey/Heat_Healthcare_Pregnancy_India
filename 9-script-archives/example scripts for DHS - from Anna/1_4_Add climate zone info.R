########################################################
########################################################
#######                                          ####### 
#######      PROJECT: Heatwaves & fertility      ####### 
#######                                          ####### 
#######         CODE: Add climate zones          #######
#######                                          ####### 
########################################################
########################################################

rm(list =ls())
library(ncdf4.helpers)
library(ncdf4)
#library(here)
require(sf)
library(sp)
require(tidyverse)
require(raster)
#install.packages("cruts")
library(cruts)
library(rworldmap)
options(scipen=999)
options(digits=5)

memory.limit()
gc()

setwd("D:/Anna/Dropbox/Projects/2021_Wilde/Heatwaves and fertility/1_Data/")

## Import the PSUs with LAT and LONG coordinates
psu <- read.csv("psu_locations.csv")[-1]
unique(psu$SurveyId)

psu <- psu %>%
  dplyr::select(SurveyId, CountryName, clust, LATNUM, LONGNUM) %>% 
  unique() 


sp <- psu
## Convert to spatial points
coordinates(sp) <- c("LONGNUM", "LATNUM")
## Assign CRS
proj4string(sp) <- CRS("+proj=longlat +datum=WGS84")
plot(sp)


## Present KG climate map
file <-'D:/Anna/Dropbox/Data/Climate classification maps/Beck_KG/Beck_KG_V1_present_0p083.tif' 
plot(raster(file))
# Convert to raster object
KG.rd = raster(file)
plot(KG.rd)
projection(KG.rd)
coltab <- colortable(KG.rd)[2:31]
#head(rasterToPoints(KG.rd))



## Extract the climate zone info for each PSU location
clim.zones <- raster::extract(KG.rd,      # raster layer 
                              sp,         # SPDF with centroids for buffer
                              #buffer = 0,
                              df=TRUE)    # return a dataframe 


## Add the PSU information
clim.zones <- cbind(sp@data, clim.zones)

clim.zones <- clim.zones %>% 
  dplyr::select(-ID) %>% 
  rename(clim_zone = Beck_KG_V1_present_0p083)

sort(unique(clim.zones$clim_zone))    

clim.zones$clim_zone[clim.zones$clim_zone == 0] <- NA
clim.zones$clim_zone[clim.zones$clim_zone == 1] <- "Af: Tropical, rainforest"
clim.zones$clim_zone[clim.zones$clim_zone == 2] <- "Am: Tropical, monsoon "
clim.zones$clim_zone[clim.zones$clim_zone == 3] <- "Aw: Tropical, savannah"
clim.zones$clim_zone[clim.zones$clim_zone == 4] <- "BWh: Arid, desert, hot"
clim.zones$clim_zone[clim.zones$clim_zone == 5] <- "BWk: Arid, desert, cold"
clim.zones$clim_zone[clim.zones$clim_zone == 6] <- "BSh: Arid, steppe, hot"
clim.zones$clim_zone[clim.zones$clim_zone == 7] <- "BSk: Arid, steppe, cold"
clim.zones$clim_zone[clim.zones$clim_zone == 8] <- "Csa: Temperate, dry summer, hot summer"
clim.zones$clim_zone[clim.zones$clim_zone == 9] <- "Csb: Temperate, dry summer, warm summer"
clim.zones$clim_zone[clim.zones$clim_zone == 10] <- "Csc: Temperate, dry summer, cold summer"
clim.zones$clim_zone[clim.zones$clim_zone == 11] <- "Cwa: Temperate, dry winter, hot summer"
clim.zones$clim_zone[clim.zones$clim_zone == 12] <- "Cwb: Temperate, dry winter, warm summer"
clim.zones$clim_zone[clim.zones$clim_zone == 13] <- "Cwc: Temperate, dry winter, cold summer"
clim.zones$clim_zone[clim.zones$clim_zone == 14] <- "Cfa: Temperate, no dry season, hot summer"
clim.zones$clim_zone[clim.zones$clim_zone == 15] <- "Cfb: Temperate, no dry season, warm summer"
clim.zones$clim_zone[clim.zones$clim_zone == 16] <- "Cfc: Temperate, no dry season, cold summer"
clim.zones$clim_zone[clim.zones$clim_zone == 17] <- "Dsa: Cold, dry summer, hot summer"
clim.zones$clim_zone[clim.zones$clim_zone == 18] <- "Dsb: Cold, dry summer, warm summer"
clim.zones$clim_zone[clim.zones$clim_zone == 19] <- "Dsc: Cold, dry summer, cold summer"
clim.zones$clim_zone[clim.zones$clim_zone == 20] <- "Dsd: Cold, dry summer, very cold winter"
clim.zones$clim_zone[clim.zones$clim_zone == 21] <- "Dwa: Cold, dry winter, hot summer"
clim.zones$clim_zone[clim.zones$clim_zone == 22] <- "Dwb: Cold, dry winter, warm summer"
clim.zones$clim_zone[clim.zones$clim_zone == 23] <- "Dwc: Cold, dry winter, cold summer"
clim.zones$clim_zone[clim.zones$clim_zone == 24] <- "Dwd: Cold, dry winter, very cold winter"
clim.zones$clim_zone[clim.zones$clim_zone == 25] <- "Dfa: Cold, no dry season, hot summer"
clim.zones$clim_zone[clim.zones$clim_zone == 26] <- "Dfb: Cold, no dry season, warm summer"
clim.zones$clim_zone[clim.zones$clim_zone == 27] <- "Dfc: Cold, no dry season, cold summer"
clim.zones$clim_zone[clim.zones$clim_zone == 28] <- "Dfd: Cold, no dry season, very cold winter"
clim.zones$clim_zone[clim.zones$clim_zone == 29] <- "ET: Polar, tundra"
clim.zones$clim_zone[clim.zones$clim_zone == 30] <- "EF: Polar, frost"


check <- clim.zones %>% 
  group_by(CountryName, clim_zone) %>% 
  summarise(n=n())

rm(list=setdiff(ls(), c("clim.zones")))

load('./DHS_data_harmonized.RData')


regions <- df1 %>% 
  dplyr::select(CountryName, SurveyId, clust, region, district) %>% 
  unique() %>% 
  mutate(admin = ifelse(CountryName == "India", district, region)) %>% 
  dplyr::select(-c(region, district))


clim.zones <- clim.zones %>% left_join(regions)


### Determine the most comon climatic zone in each admin area
library(plyr)
clim.zones.sum <- ddply(clim.zones, .(CountryName, admin),summarize,clim_zone={
  tt <- table(clim_zone)
  names(tt)[which.max(tt)]
})

unique(clim.zones.sum$clim_zone)

clim.zones.final <- clim.zones %>% 
  dplyr::select(-c(clim_zone)) %>% 
  left_join(clim.zones.sum) %>% 
  dplyr::select(-c(admin))


df1 <- df1 %>% 
  left_join(clim.zones.final)


rm(list=setdiff(ls(), c("df1")))

save.image(file='DHS_data_harmonized.RData')

