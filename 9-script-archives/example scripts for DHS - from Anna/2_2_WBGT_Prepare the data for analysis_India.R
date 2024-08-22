########################################################
########################################################
#######                                          ####### 
#######   PROJECT: Temperature & sex ratio       ####### 
#######                                          #######
########################################################
########################################################
rm(list =ls())
library(foreign)
library(tidyverse)
library(lubridate)
library(dplyr)
#install.packages("MAPLES")
library(MAPLES)
#library(data.table)
#install.packages("devtools")
#devtools::install_github("ropensci/rdhs", ref = "issue33_path", force = TRUE)
library(rdhs)   
library(rworldmap)
library(raster)
library(cruts)
library(sp)
library(sf)
library(ncdf4.helpers)
library(ncdf4)
library(xts)


setwd("D:/Anna/Dropbox/Projects/2021_Wilde/Temperature & sex ratio/Data/")


##########################################################################
## 1. Prepare the DHS data
##########################################################################

load('D:/Anna/Dropbox/Projects/2021_Wilde/Heatwaves and fertility/1_Data/DHS_data_harmonized.RData')
sort(unique(df1$SurveyYear))

df1 <- df1 %>% dplyr::select(-c(dobcmc, birth))

# generate variable for length of time living in the current place of residence and drop out observations for which women have lived shorter time in place of residence than time of conception 
# this can be used in robustness check since some surveys are missing this information (about a third of surveys) 
df1 <- df1 %>% 
  mutate(concmc = fertcmc-9) %>%                  #generate a variable for approximate month of conception (date of birth - 9 months)
  mutate(concmc = fertcmc-9) %>%                  #generate a variable for approximate month of conception (date of birth - 9 months)
  mutate(rescmc = intcmc-(years_at_place*12)) %>% #generate a variable for start of residence at place of interview
  #generate variable for migration since conception - remove from the analysis as a robustness check. 
  #Note: number of surveys are missing migration information
  mutate(migrated = ifelse(concmc<rescmc, 1, 0))  


unique(df1$CountryName)

## Add world region and filter to Sub-Saharan Africa and India only
library(countrycode)
df1$WorldRegion <- countrycode(df1$CountryName, 'country.name', 'region')  
unique(df1$WorldRegion)

df1 %>% dplyr::select(CountryName, WorldRegion) %>% unique()

df1 <- df1 %>% 
  filter(WorldRegion == "Sub-Saharan Africa" | CountryName == "India")

unique(df1$CountryName)

df1 <- df1 %>% 
  dplyr::select(-WorldRegion)

rm(list=setdiff(ls(), c("df1")))


##########################################################################
## 2. Import the monthly precipitation data
##########################################################################

#install.packages("cruts")
library(cruts)
pre_nc <- cruts2raster(ncfile = "D:/Anna/Dropbox/Data/cru_ts_4/cru_ts4.07.1901.2022.pre.dat.nc", timeRange = c("1900-01-01", "2022-12-31"))



##########################################################################
## 3. Add the lagged temperature data
##########################################################################

unique(df1$CountryName)

## Import the world map 
bound0 <- getMap(resolution = "low")
bound0$NAME <- as.character(bound0$NAME)
bound0$NAME
## Correct mismatched country names
bound0@data$NAME[bound0@data$NAME == "East Timor"] <- "Timor-Leste"
bound0@data$NAME[bound0@data$NAME == "Congo (Kinshasa)"] <- "Democratic Republic of the Congo"
bound0@data$NAME[bound0@data$NAME == "Ivory Coast"] <- "Côte d'Ivoire" 
bound0@data$NAME[bound0@data$NAME == "Dominican Rep."] <- "Dominican Republic"
#bound0@data$NAME[bound0@data$NAME == "Swaziland"] <- "Eswatini"
bound0@data$NAME[bound0@data$NAME == "Kyrgyzstan"] <- "Kyrgyz Republic"
## Restrict to the selected countries
bound1 <- bound0[bound0@data$NAME %in% unique(df1$CountryName), ]
plot(bound1)

sort(unique(df1$CountryName))
sort(unique(bound1$NAME))


# keep only surveys for India
df1 <- df1 %>% 
  filter(SurveyId %in% c("IA2015DHS", "IA2020DHS")) 

df1$ID <- paste(df1$SurveyId,"_",df1$region, sep="")
unique(df1$ID)


IDs <- unique(df1$ID)
IDs

IDs <- IDs[-c(1:46)]
IDs


a = IDs[1]
a

for (a in IDs) {
  
  psu <- df1 %>% 
    filter(ID == a) %>% 
    dplyr::select(CountryName, SurveyId, clust, LATNUM, LONGNUM, intYr, fertYr) %>% 
    unique()
  
  
  #start = unique(max(psu$intYr))
  start = 2021 
  #end = unique(min(psu$fertYr))
  #end = end - 2
  #end = ifelse(end<1979, 1979, end)
  end = 1979
  year  = c(start:end)
  year
  
  ## convert the psus to spatial points data frame  
  sp <- psu %>% 
    dplyr::select(SurveyId, clust, LATNUM, LONGNUM) %>% 
    unique()
  
  sp <- as.data.frame(sp)
  
  coordinates(sp) <- c("LONGNUM","LATNUM")
  plot(sp)
  a
  
  ## Restrict the spatial boundaries to the selected country
  bound1 <- bound0[bound0@data$NAME %in% unique(psu$CountryName), ]
  #plot(bound1)
  ## Add a buffer zone around the country boundary
  bound2 <- buffer(bound1, width=2) 
  #plot(bound2, border="red")
  #plot(bound1, add=T)
  
  
  ### Prepare the TMAX data
  tmax = NULL
  
  for (y in year){
    ### Import the gridded TMP data 
    
    rd1 <- brick(paste("D:/Anna/Dropbox/Data/wbgt_daily/wbgtmax/wbgtmax_",y,"_daily_ERA5.nc", sep="")) 
    #plot(rd1,1)
    #plot(bound0, add=T)
    
    ## Restrict the spatial data to the country boundaries 
    cd0 <- crop(x = rd1, y = bound2)
    cd1 <- rasterize(x = bound2, y = cd0)
    cd2 <- mask(x = cd0, mask = cd1)
    #plot(cd2, 2)
    
    # Extract the climate data for each PSU location
    df3 <- raster::extract(cd2,        # raster layer cropped to the country boundary
                           sp,         # SPDF with centroids for buffer
                           df=TRUE)    # return a dataframe 
    
    # Add the PSU information
    df3 <- cbind(sp@data, df3)
    
    # Convert to long format
    df4 <- df3 %>% 
      dplyr::select(-c(ID)) %>% 
      gather(key = date, value = tmax, -SurveyId, -clust) %>%
      mutate(date = as.Date(substring(date, 2), format = "%Y.%m.%d"),
             year  = format(date, format = "%Y"),
             month = format(date, format = "%m"),
             day   =  format(date, format = "%d"))
    
    tmax <- bind_rows(tmax, df4)
  }
  
  
  ### Prepare the TMIN data
  tmean = NULL
  
  for (y in year){
    ### Import the gridded TMP data 
    
    rd1 <- brick(paste("D:/Anna/Dropbox/Data/wbgt_daily/wbgtmean/wbgtmean_",y,"_daily_ERA5.nc", sep="")) 
    plot(rd1, 1)
    
    ## Restrict the spatial data to the country boundaries 
    cd0 <- crop(x = rd1, y = bound2)
    cd1 <- rasterize(x = bound2, y = cd0)
    cd2 <- mask(x = cd0, mask = cd1)
    plot(cd2, 2)
    
    # Extract the climate data for each PSU location
    df3 <- raster::extract(cd2,        # raster layer cropped to the country boundary
                           sp,         # SPDF with centroids for buffer
                           df=TRUE)    # return a dataframe 
    
    # Add the PSU information
    df3 <- cbind(sp@data, df3)
    
    # Convert to long format
    df4 <- df3 %>% 
      dplyr::select(-c(ID)) %>% 
      gather(key = date, value = tmean, -SurveyId, -clust) %>%
      mutate(date = as.Date(substring(date, 2), format = "%Y.%m.%d"),
             year  = format(date, format = "%Y"),
             month = format(date, format = "%m"),
             day   =  format(date, format = "%d"))
    
    tmean <- bind_rows(tmean, df4)
  }
  
  
  tmax <- tmax %>% filter(!is.na(tmax))
  tmean <- tmean %>% filter(!is.na(tmean))
  
  ### Join the Tmax and Tmin data and calculate Tmean 
  temp <- tmax %>% 
    left_join(tmean) %>% 
    arrange(clust, year, month, day)  %>% 
    filter(!is.na(tmax) & !is.na(tmean)) %>% 
    dplyr::select(SurveyId, clust, date, year, month, day, tmax, tmean)
  
  
  
  rm(tmax, tmean)
  
  ### Add temperature percentiles
  temp <- temp %>% 
    group_by(SurveyId, clust) %>% 
    mutate(tmean_pct = ntile(tmean, 100)) %>% 
    mutate(tmax_pct = ntile(tmax, 100)) %>% 
    ungroup()
  
  ### Find the absolute temperature thresholds for each percentile
  pct_thresholds_tmean <- temp %>% 
    group_by(SurveyId, clust, tmean_pct) %>% 
    summarise(pct_max = max(tmean),
              pct_min= min(tmean)) %>% 
    ungroup()
  
  pct_thresholds_tmax <- temp %>% 
    group_by(SurveyId, clust, tmax_pct) %>% 
    summarise(pct_max = max(tmean),
              pct_min= min(tmean)) %>% 
    ungroup() 

  write.csv(pct_thresholds_tmean, paste("./WBGT_pct_thresholds/",a,"_pct_thresholds_tmean.csv", sep=""))
  write.csv(pct_thresholds_tmax, paste("./WBGT_pct_thresholds/",a,"_pct_thresholds_tmax.csv", sep=""))
  
  
  ### Generate temperature bins
  temp <- temp %>% 
    ## Tmax absolute bins
    mutate(Tmax_below_5  = ifelse(tmax <5, 1, 0)) %>% 
    mutate(Tmax_5_10     = ifelse(tmax >=5  & tmax <10, 1, 0)) %>% 
    mutate(Tmax_10_15    = ifelse(tmax >=10 & tmax <15, 1, 0)) %>% 
    mutate(Tmax_15_20    = ifelse(tmax >=15 & tmax <20, 1, 0)) %>% 
    mutate(Tmax_20_23    = ifelse(tmax >=20 & tmax <23, 1, 0)) %>% 
    mutate(Tmax_23_25    = ifelse(tmax >=23 & tmax <25, 1, 0)) %>% 
    mutate(Tmax_25_28    = ifelse(tmax >=25 & tmax <28, 1, 0)) %>% 
    mutate(Tmax_28_30    = ifelse(tmax >=28 & tmax <30, 1, 0)) %>% 
    mutate(Tmax_30_33    = ifelse(tmax >=30 & tmax <33, 1, 0)) %>% 
    mutate(Tmax_above_33 = ifelse(tmax >=33, 1, 0)) %>% 
    ## Tmean absolute bins
    mutate(Tmean_below_5  = ifelse(tmean <5, 1, 0)) %>% 
    mutate(Tmean_5_10     = ifelse(tmean >=5  & tmean <10, 1, 0)) %>% 
    mutate(Tmean_10_15    = ifelse(tmean >=10 & tmean <15, 1, 0)) %>% 
    mutate(Tmean_15_20    = ifelse(tmean >=15 & tmean <20, 1, 0)) %>% 
    mutate(Tmean_20_23    = ifelse(tmean >=20 & tmean <23, 1, 0)) %>% 
    mutate(Tmean_23_25    = ifelse(tmean >=23 & tmean <25, 1, 0)) %>% 
    mutate(Tmean_25_28    = ifelse(tmean >=25 & tmean <28, 1, 0)) %>% 
    mutate(Tmean_28_30    = ifelse(tmean >=28 & tmean <30, 1, 0)) %>% 
    mutate(Tmean_30_33    = ifelse(tmean >=30 & tmean <33, 1, 0)) %>% 
    mutate(Tmean_above_33 = ifelse(tmean >=33, 1, 0)) %>% 
    ## Tmax percentile bins
    mutate(Tmax_pc_0_10   = ifelse(tmax_pct <10, 1, 0)) %>% 
    mutate(Tmax_pc_10_20  = ifelse(tmax_pct >=10 & tmax_pct <20, 1, 0)) %>% 
    mutate(Tmax_pc_20_30  = ifelse(tmax_pct >=20 & tmax_pct <30, 1, 0)) %>% 
    mutate(Tmax_pc_30_40  = ifelse(tmax_pct >=30 & tmax_pct <40, 1, 0)) %>% 
    mutate(Tmax_pc_40_50  = ifelse(tmax_pct >=40 & tmax_pct <50, 1, 0)) %>% 
    mutate(Tmax_pc_50_60  = ifelse(tmax_pct >=50 & tmax_pct <60, 1, 0)) %>% 
    mutate(Tmax_pc_60_70  = ifelse(tmax_pct >=60 & tmax_pct <70, 1, 0)) %>% 
    mutate(Tmax_pc_70_80  = ifelse(tmax_pct >=70 & tmax_pct <80, 1, 0)) %>% 
    mutate(Tmax_pc_80_90  = ifelse(tmax_pct >=80 & tmax_pct <90, 1, 0)) %>% 
    mutate(Tmax_pc_90_100 = ifelse(tmax_pct >=90, 1, 0)) %>% 
    ## Tmean percentile bins
    mutate(Tmean_pc_0_10   = ifelse(tmean_pct <10, 1, 0)) %>% 
    mutate(Tmean_pc_10_20  = ifelse(tmean_pct >=10 & tmean_pct <20, 1, 0)) %>% 
    mutate(Tmean_pc_20_30  = ifelse(tmean_pct >=20 & tmean_pct <30, 1, 0)) %>% 
    mutate(Tmean_pc_30_40  = ifelse(tmean_pct >=30 & tmean_pct <40, 1, 0)) %>% 
    mutate(Tmean_pc_40_50  = ifelse(tmean_pct >=40 & tmean_pct <50, 1, 0)) %>% 
    mutate(Tmean_pc_50_60  = ifelse(tmean_pct >=50 & tmean_pct <60, 1, 0)) %>% 
    mutate(Tmean_pc_60_70  = ifelse(tmean_pct >=60 & tmean_pct <70, 1, 0)) %>% 
    mutate(Tmean_pc_70_80  = ifelse(tmean_pct >=70 & tmean_pct <80, 1, 0)) %>% 
    mutate(Tmean_pc_80_90  = ifelse(tmean_pct >=80 & tmean_pct <90, 1, 0)) %>% 
    mutate(Tmean_pc_90_100 = ifelse(tmean_pct >=90, 1, 0)) %>%    
    ## Tmax above specific percentile
    mutate(Tmax_above_pc75 = ifelse(tmax_pct > 75, 1, 0)) %>% 
    mutate(Tmax_above_pc90 = ifelse(tmax_pct > 90, 1, 0)) %>% 
    mutate(Tmax_above_pc95 = ifelse(tmax_pct > 95, 1, 0)) %>% 
    mutate(Tmax_above_pc98 = ifelse(tmax_pct > 98, 1, 0)) %>% 
    ## Tmean above specific percentile
    mutate(Tmean_above_pc75 = ifelse(tmean_pct > 75, 1, 0)) %>% 
    mutate(Tmean_above_pc90 = ifelse(tmean_pct > 90, 1, 0)) %>% 
    mutate(Tmean_above_pc95 = ifelse(tmean_pct > 95, 1, 0)) %>% 
    mutate(Tmean_above_pc98 = ifelse(tmean_pct > 98, 1, 0)) 
  
  ### Aggregate by year and month and calculate the number of days in each bin
  tbins <- temp %>% 
    group_by(SurveyId, clust, year, month) %>% 
    dplyr::summarise(
      ## Absolute
      ## Tmax
      Tmax_below_5  = sum(Tmax_below_5),  
      Tmax_5_10     = sum(Tmax_5_10),  
      Tmax_10_15    = sum(Tmax_10_15),
      Tmax_15_20    = sum(Tmax_15_20),
      Tmax_20_23    = sum(Tmax_20_23),
      Tmax_23_25    = sum(Tmax_23_25),
      Tmax_25_28    = sum(Tmax_25_28),
      Tmax_28_30    = sum(Tmax_28_30),
      Tmax_30_33    = sum(Tmax_30_33),
      Tmax_above_33 = sum(Tmax_above_33),
      ## Tmean
      Tmean_below_5  = sum(Tmean_below_5),  
      Tmean_5_10     = sum(Tmean_5_10),  
      Tmean_10_15    = sum(Tmean_10_15),
      Tmean_15_20    = sum(Tmean_15_20),
      Tmean_20_23    = sum(Tmean_20_23),
      Tmean_23_25    = sum(Tmean_23_25),
      Tmean_25_28    = sum(Tmean_25_28),
      Tmean_28_30    = sum(Tmean_28_30),
      Tmean_30_33    = sum(Tmean_30_33),
      Tmean_above_33 = sum(Tmean_above_33),
      tmax = mean(tmax),                   #add average monthly TMAX
      tmean = mean(tmean),                 #add average monthly TMEAN
      ## Percentiles
      ## Tmax
      Tmax_pc_0_10   = sum(Tmax_pc_0_10),
      Tmax_pc_10_20  = sum(Tmax_pc_10_20), 
      Tmax_pc_20_30  = sum(Tmax_pc_20_30), 
      Tmax_pc_30_40  = sum(Tmax_pc_30_40), 
      Tmax_pc_40_50  = sum(Tmax_pc_40_50), 
      Tmax_pc_50_60  = sum(Tmax_pc_50_60), 
      Tmax_pc_60_70  = sum(Tmax_pc_60_70), 
      Tmax_pc_70_80  = sum(Tmax_pc_70_80), 
      Tmax_pc_80_90  = sum(Tmax_pc_80_90), 
      Tmax_pc_90_100 = sum(Tmax_pc_90_100), 
      ## Tmean
      Tmean_pc_0_10   = sum(Tmean_pc_0_10),
      Tmean_pc_10_20  = sum(Tmean_pc_10_20), 
      Tmean_pc_20_30  = sum(Tmean_pc_20_30), 
      Tmean_pc_30_40  = sum(Tmean_pc_30_40), 
      Tmean_pc_40_50  = sum(Tmean_pc_40_50), 
      Tmean_pc_50_60  = sum(Tmean_pc_50_60), 
      Tmean_pc_60_70  = sum(Tmean_pc_60_70), 
      Tmean_pc_70_80  = sum(Tmean_pc_70_80), 
      Tmean_pc_80_90  = sum(Tmean_pc_80_90), 
      Tmean_pc_90_100 = sum(Tmean_pc_90_100),                     
      ## 75th, 90th, 95th, 98th percentiles
      ## Tmax
      Tmax_above_pc75   = sum(Tmax_above_pc75),
      Tmax_above_pc90   = sum(Tmax_above_pc90),
      Tmax_above_pc95   = sum(Tmax_above_pc95),
      Tmax_above_pc98   = sum(Tmax_above_pc98),
      ## Tmean
      Tmean_above_pc75   = sum(Tmean_above_pc75),
      Tmean_above_pc90   = sum(Tmean_above_pc90),
      Tmean_above_pc95   = sum(Tmean_above_pc95),
      Tmean_above_pc98   = sum(Tmean_above_pc98)) %>%             
    ungroup() 
  
  
  tbins <- tbins %>% 
    mutate(year = as.numeric(year)) %>% 
    mutate(month = as.numeric(month)) %>% 
    #mutate(cmc = mkdate(year, month, cmc = TRUE)) %>% 
    mutate(cmc = (year-1900)*12+month)  %>% 
    dplyr::select(!c(year, month)) 
  
  
  
  ## Extract the precipitation data for each PSU location
  pre <- raster::extract(pre_nc,      # raster layer cropped to the country boundary
                         sp,         # SPDF with centroids for buffer
                         df=TRUE)    # return a dataframe 
  
  ## Add the PSU information
  pre <- cbind(sp@data, pre)
  
  pre <- pre %>% 
    dplyr::select(-c(ID)) %>% 
    gather(key = date, value = pre, -SurveyId, -clust) %>%
    mutate(date = as.Date(substring(date, 2), format = "%Y.%m.%d"),
           year  = format(date, format = "%Y"),
           month = format(date, format = "%m"),
           day   =  format(date, format = "%d"))
  
  pre <- pre %>% 
    dplyr::select(-c(date, day)) %>% 
    filter(year>=1979 & year<=2021) 
  
  pre <- pre %>% 
    mutate(year = as.numeric(year)) %>% 
    mutate(month = as.numeric(month)) %>%
    mutate(cmc = (year-1900)*12+month)  %>% 
    dplyr::select(!c(year, month)) 
  
  
  ## Join the temperature bins with the monthly pre data
  tbins <- tbins %>% 
    left_join(pre)
  
  rm(pre)
  
  ## Join the climate data with the DHS survey data
  df5 <- df1 %>% 
    filter(ID == a) %>%  
    dplyr::select(-ID)
  
  ## Generate unique women ID
  df5 <- df5 %>% 
    mutate(woman_id = group_indices(., SurveyId, clust, hhl_id, line_id))
  
  
  # Add the lagged temperature data using the cluster ID, survey ID and cmc at the time of exposure
  df5 <- df5 %>% 
    mutate(fertcmc_lag0 = fertcmc,
           fertcmc_lag1 = fertcmc - 1,
           fertcmc_lag2 = fertcmc - 2,
           fertcmc_lag3 = fertcmc - 3,
           fertcmc_lag4 = fertcmc - 4,
           fertcmc_lag5 = fertcmc - 5,
           fertcmc_lag6 = fertcmc - 6,
           fertcmc_lag7 = fertcmc - 7,
           fertcmc_lag8 = fertcmc - 8,
           fertcmc_lag9 = fertcmc - 9,
           fertcmc_lag10 = fertcmc - 10,
           fertcmc_lag11 = fertcmc - 11,
           fertcmc_lag12 = fertcmc - 12,
           fertcmc_lag13 = fertcmc - 13,
           fertcmc_lag14 = fertcmc - 14,
           fertcmc_lag15 = fertcmc - 15
    )  
  
  df5 <- df5 %>%  
    ### Lag 0
    left_join(tbins, by = c("clust" = "clust", "fertcmc_lag0" = "cmc", "SurveyId" = "SurveyId")) %>% 
    dplyr::rename(Tmax_below_5_lag0 = Tmax_below_5,   
                  Tmax_5_10_lag0 = Tmax_5_10, 
                  Tmax_10_15_lag0 = Tmax_10_15,
                  Tmax_15_20_lag0 = Tmax_15_20,
                  Tmax_20_23_lag0 = Tmax_20_23,
                  Tmax_23_25_lag0 = Tmax_23_25,
                  Tmax_25_28_lag0 = Tmax_25_28,
                  Tmax_28_30_lag0 = Tmax_28_30,
                  Tmax_30_33_lag0 = Tmax_30_33,
                  Tmax_above_33_lag0 = Tmax_above_33,
                  ## Absolute Tmean
                  Tmean_below_5_lag0 = Tmean_below_5,   
                  Tmean_5_10_lag0 = Tmean_5_10, 
                  Tmean_10_15_lag0 = Tmean_10_15,
                  Tmean_15_20_lag0 = Tmean_15_20,
                  Tmean_20_23_lag0 = Tmean_20_23,
                  Tmean_23_25_lag0 = Tmean_23_25,
                  Tmean_25_28_lag0 = Tmean_25_28,
                  Tmean_28_30_lag0 = Tmean_28_30,
                  Tmean_30_33_lag0 = Tmean_30_33,
                  Tmean_above_33_lag0 = Tmean_above_33,
                  tmax_lag0 = tmax,
                  tmean_lag0 = tmean,
                  ## Tmax prc
                  Tmax_pc_0_10_lag0  = Tmax_pc_0_10,
                  Tmax_pc_10_20_lag0 = Tmax_pc_10_20,
                  Tmax_pc_20_30_lag0 = Tmax_pc_20_30,
                  Tmax_pc_30_40_lag0 = Tmax_pc_30_40,
                  Tmax_pc_40_50_lag0 = Tmax_pc_40_50,   
                  Tmax_pc_50_60_lag0 = Tmax_pc_50_60, 
                  Tmax_pc_60_70_lag0 = Tmax_pc_60_70,  
                  Tmax_pc_70_80_lag0 = Tmax_pc_70_80,  
                  Tmax_pc_80_90_lag0 = Tmax_pc_80_90,  
                  Tmax_pc_90_100_lag0 = Tmax_pc_90_100, 
                  ## Tmean prc
                  Tmean_pc_0_10_lag0  = Tmean_pc_0_10,  
                  Tmean_pc_10_20_lag0 = Tmean_pc_10_20,
                  Tmean_pc_20_30_lag0 = Tmean_pc_20_30,
                  Tmean_pc_30_40_lag0 = Tmean_pc_30_40,
                  Tmean_pc_40_50_lag0 = Tmean_pc_40_50,
                  Tmean_pc_50_60_lag0 = Tmean_pc_50_60,
                  Tmean_pc_60_70_lag0 = Tmean_pc_60_70,
                  Tmean_pc_70_80_lag0 = Tmean_pc_70_80,
                  Tmean_pc_80_90_lag0 = Tmean_pc_80_90,
                  Tmean_pc_90_100_lag0 = Tmean_pc_90_100,
                  ## Tmax pct thresholds
                  Tmax_above_pc75_lag0 = Tmax_above_pc75,
                  Tmax_above_pc90_lag0 = Tmax_above_pc90,
                  Tmax_above_pc95_lag0 = Tmax_above_pc95,
                  Tmax_above_pc98_lag0 = Tmax_above_pc98,
                  ## Tmean pct thresholds
                  Tmean_above_pc75_lag0 = Tmean_above_pc75,
                  Tmean_above_pc90_lag0 = Tmean_above_pc90,
                  Tmean_above_pc95_lag0 = Tmean_above_pc95,
                  Tmean_above_pc98_lag0 = Tmean_above_pc98,
                  ## Pre
                  pre_lag0 = pre
    ) %>% 
    ### Lag 1
    left_join(tbins, by = c("clust" = "clust", "fertcmc_lag1" = "cmc", "SurveyId" = "SurveyId")) %>% 
    dplyr::rename(Tmax_below_5_lag1 = Tmax_below_5,   
                  Tmax_5_10_lag1 = Tmax_5_10, 
                  Tmax_10_15_lag1 = Tmax_10_15,
                  Tmax_15_20_lag1 = Tmax_15_20,
                  Tmax_20_23_lag1 = Tmax_20_23,
                  Tmax_23_25_lag1 = Tmax_23_25,
                  Tmax_25_28_lag1 = Tmax_25_28,
                  Tmax_28_30_lag1 = Tmax_28_30,
                  Tmax_30_33_lag1 = Tmax_30_33,
                  Tmax_above_33_lag1 = Tmax_above_33,
                  ## Absolute Tmean
                  Tmean_below_5_lag1 = Tmean_below_5,   
                  Tmean_5_10_lag1 = Tmean_5_10, 
                  Tmean_10_15_lag1 = Tmean_10_15,
                  Tmean_15_20_lag1 = Tmean_15_20,
                  Tmean_20_23_lag1 = Tmean_20_23,
                  Tmean_23_25_lag1 = Tmean_23_25,
                  Tmean_25_28_lag1 = Tmean_25_28,
                  Tmean_28_30_lag1 = Tmean_28_30,
                  Tmean_30_33_lag1 = Tmean_30_33,
                  Tmean_above_33_lag1 = Tmean_above_33,
                  tmax_lag1 = tmax,
                  tmean_lag1 = tmean,
                  ## Tmax prc
                  Tmax_pc_0_10_lag1  = Tmax_pc_0_10,
                  Tmax_pc_10_20_lag1 = Tmax_pc_10_20,
                  Tmax_pc_20_30_lag1 = Tmax_pc_20_30,
                  Tmax_pc_30_40_lag1 = Tmax_pc_30_40,
                  Tmax_pc_40_50_lag1 = Tmax_pc_40_50,   
                  Tmax_pc_50_60_lag1 = Tmax_pc_50_60, 
                  Tmax_pc_60_70_lag1 = Tmax_pc_60_70,  
                  Tmax_pc_70_80_lag1 = Tmax_pc_70_80,  
                  Tmax_pc_80_90_lag1 = Tmax_pc_80_90,  
                  Tmax_pc_90_100_lag1 = Tmax_pc_90_100, 
                  ## Tmean prc
                  Tmean_pc_0_10_lag1  = Tmean_pc_0_10,  
                  Tmean_pc_10_20_lag1 = Tmean_pc_10_20,
                  Tmean_pc_20_30_lag1 = Tmean_pc_20_30,
                  Tmean_pc_30_40_lag1 = Tmean_pc_30_40,
                  Tmean_pc_40_50_lag1 = Tmean_pc_40_50,
                  Tmean_pc_50_60_lag1 = Tmean_pc_50_60,
                  Tmean_pc_60_70_lag1 = Tmean_pc_60_70,
                  Tmean_pc_70_80_lag1 = Tmean_pc_70_80,
                  Tmean_pc_80_90_lag1 = Tmean_pc_80_90,
                  Tmean_pc_90_100_lag1 = Tmean_pc_90_100,
                  ## Tmax pct thresholds
                  Tmax_above_pc75_lag1 = Tmax_above_pc75,
                  Tmax_above_pc90_lag1 = Tmax_above_pc90,
                  Tmax_above_pc95_lag1 = Tmax_above_pc95,
                  Tmax_above_pc98_lag1 = Tmax_above_pc98,
                  ## Tmean pct thresholds
                  Tmean_above_pc75_lag1 = Tmean_above_pc75,
                  Tmean_above_pc90_lag1 = Tmean_above_pc90,
                  Tmean_above_pc95_lag1 = Tmean_above_pc95,
                  Tmean_above_pc98_lag1 = Tmean_above_pc98,
                  ## Pre
                  pre_lag1 = pre
    )   %>% 
    ### Lag 2
    left_join(tbins, by = c("clust" = "clust", "fertcmc_lag2" = "cmc", "SurveyId" = "SurveyId")) %>% 
    dplyr::rename(Tmax_below_5_lag2 = Tmax_below_5,   
                  Tmax_5_10_lag2 = Tmax_5_10, 
                  Tmax_10_15_lag2 = Tmax_10_15,
                  Tmax_15_20_lag2 = Tmax_15_20,
                  Tmax_20_23_lag2 = Tmax_20_23,
                  Tmax_23_25_lag2 = Tmax_23_25,
                  Tmax_25_28_lag2 = Tmax_25_28,
                  Tmax_28_30_lag2 = Tmax_28_30,
                  Tmax_30_33_lag2 = Tmax_30_33,
                  Tmax_above_33_lag2 = Tmax_above_33,
                  ## Absolute Tmean
                  Tmean_below_5_lag2 = Tmean_below_5,   
                  Tmean_5_10_lag2 = Tmean_5_10, 
                  Tmean_10_15_lag2 = Tmean_10_15,
                  Tmean_15_20_lag2 = Tmean_15_20,
                  Tmean_20_23_lag2 = Tmean_20_23,
                  Tmean_23_25_lag2 = Tmean_23_25,
                  Tmean_25_28_lag2 = Tmean_25_28,
                  Tmean_28_30_lag2 = Tmean_28_30,
                  Tmean_30_33_lag2 = Tmean_30_33,
                  Tmean_above_33_lag2 = Tmean_above_33,
                  tmax_lag2 = tmax,
                  tmean_lag2 = tmean,
                  ## Tmax prc
                  Tmax_pc_0_10_lag2  = Tmax_pc_0_10,
                  Tmax_pc_10_20_lag2 = Tmax_pc_10_20,
                  Tmax_pc_20_30_lag2 = Tmax_pc_20_30,
                  Tmax_pc_30_40_lag2 = Tmax_pc_30_40,
                  Tmax_pc_40_50_lag2 = Tmax_pc_40_50,   
                  Tmax_pc_50_60_lag2 = Tmax_pc_50_60, 
                  Tmax_pc_60_70_lag2 = Tmax_pc_60_70,  
                  Tmax_pc_70_80_lag2 = Tmax_pc_70_80,  
                  Tmax_pc_80_90_lag2 = Tmax_pc_80_90,  
                  Tmax_pc_90_100_lag2 = Tmax_pc_90_100, 
                  ## Tmean prc
                  Tmean_pc_0_10_lag2  = Tmean_pc_0_10,  
                  Tmean_pc_10_20_lag2 = Tmean_pc_10_20,
                  Tmean_pc_20_30_lag2 = Tmean_pc_20_30,
                  Tmean_pc_30_40_lag2 = Tmean_pc_30_40,
                  Tmean_pc_40_50_lag2 = Tmean_pc_40_50,
                  Tmean_pc_50_60_lag2 = Tmean_pc_50_60,
                  Tmean_pc_60_70_lag2 = Tmean_pc_60_70,
                  Tmean_pc_70_80_lag2 = Tmean_pc_70_80,
                  Tmean_pc_80_90_lag2 = Tmean_pc_80_90,
                  Tmean_pc_90_100_lag2 = Tmean_pc_90_100,
                  ## Tmax pct thresholds
                  Tmax_above_pc75_lag2 = Tmax_above_pc75,
                  Tmax_above_pc90_lag2 = Tmax_above_pc90,
                  Tmax_above_pc95_lag2 = Tmax_above_pc95,
                  Tmax_above_pc98_lag2 = Tmax_above_pc98,
                  ## Tmean pct thresholds
                  Tmean_above_pc75_lag2 = Tmean_above_pc75,
                  Tmean_above_pc90_lag2 = Tmean_above_pc90,
                  Tmean_above_pc95_lag2 = Tmean_above_pc95,
                  Tmean_above_pc98_lag2 = Tmean_above_pc98,
                  ## Pre
                  pre_lag2 = pre
    ) %>% 
    ### Lag 3
    left_join(tbins, by = c("clust" = "clust", "fertcmc_lag3" = "cmc", "SurveyId" = "SurveyId")) %>% 
    dplyr::rename(Tmax_below_5_lag3 = Tmax_below_5,   
                  Tmax_5_10_lag3 = Tmax_5_10, 
                  Tmax_10_15_lag3 = Tmax_10_15,
                  Tmax_15_20_lag3 = Tmax_15_20,
                  Tmax_20_23_lag3 = Tmax_20_23,
                  Tmax_23_25_lag3 = Tmax_23_25,
                  Tmax_25_28_lag3 = Tmax_25_28,
                  Tmax_28_30_lag3 = Tmax_28_30,
                  Tmax_30_33_lag3 = Tmax_30_33,
                  Tmax_above_33_lag3 = Tmax_above_33,
                  ## Absolute Tmean
                  Tmean_below_5_lag3 = Tmean_below_5,   
                  Tmean_5_10_lag3 = Tmean_5_10, 
                  Tmean_10_15_lag3 = Tmean_10_15,
                  Tmean_15_20_lag3 = Tmean_15_20,
                  Tmean_20_23_lag3 = Tmean_20_23,
                  Tmean_23_25_lag3 = Tmean_23_25,
                  Tmean_25_28_lag3 = Tmean_25_28,
                  Tmean_28_30_lag3 = Tmean_28_30,
                  Tmean_30_33_lag3 = Tmean_30_33,
                  Tmean_above_33_lag3 = Tmean_above_33,
                  tmax_lag3 = tmax,
                  tmean_lag3 = tmean,
                  ## Tmax prc
                  Tmax_pc_0_10_lag3  = Tmax_pc_0_10,
                  Tmax_pc_10_20_lag3 = Tmax_pc_10_20,
                  Tmax_pc_20_30_lag3 = Tmax_pc_20_30,
                  Tmax_pc_30_40_lag3 = Tmax_pc_30_40,
                  Tmax_pc_40_50_lag3 = Tmax_pc_40_50,   
                  Tmax_pc_50_60_lag3 = Tmax_pc_50_60, 
                  Tmax_pc_60_70_lag3 = Tmax_pc_60_70,  
                  Tmax_pc_70_80_lag3 = Tmax_pc_70_80,  
                  Tmax_pc_80_90_lag3 = Tmax_pc_80_90,  
                  Tmax_pc_90_100_lag3 = Tmax_pc_90_100, 
                  ## Tmean prc
                  Tmean_pc_0_10_lag3  = Tmean_pc_0_10,  
                  Tmean_pc_10_20_lag3 = Tmean_pc_10_20,
                  Tmean_pc_20_30_lag3 = Tmean_pc_20_30,
                  Tmean_pc_30_40_lag3 = Tmean_pc_30_40,
                  Tmean_pc_40_50_lag3 = Tmean_pc_40_50,
                  Tmean_pc_50_60_lag3 = Tmean_pc_50_60,
                  Tmean_pc_60_70_lag3 = Tmean_pc_60_70,
                  Tmean_pc_70_80_lag3 = Tmean_pc_70_80,
                  Tmean_pc_80_90_lag3 = Tmean_pc_80_90,
                  Tmean_pc_90_100_lag3 = Tmean_pc_90_100,
                  ## Tmax pct thresholds
                  Tmax_above_pc75_lag3 = Tmax_above_pc75,
                  Tmax_above_pc90_lag3 = Tmax_above_pc90,
                  Tmax_above_pc95_lag3 = Tmax_above_pc95,
                  Tmax_above_pc98_lag3 = Tmax_above_pc98,
                  ## Tmean pct thresholds
                  Tmean_above_pc75_lag3 = Tmean_above_pc75,
                  Tmean_above_pc90_lag3 = Tmean_above_pc90,
                  Tmean_above_pc95_lag3 = Tmean_above_pc95,
                  Tmean_above_pc98_lag3 = Tmean_above_pc98,
                  ## Pre
                  pre_lag3 = pre
    ) %>% 
    ### Lag 4
    left_join(tbins, by = c("clust" = "clust", "fertcmc_lag4" = "cmc", "SurveyId" = "SurveyId")) %>% 
    dplyr::rename(Tmax_below_5_lag4 = Tmax_below_5,   
                  Tmax_5_10_lag4 = Tmax_5_10, 
                  Tmax_10_15_lag4 = Tmax_10_15,
                  Tmax_15_20_lag4 = Tmax_15_20,
                  Tmax_20_23_lag4 = Tmax_20_23,
                  Tmax_23_25_lag4 = Tmax_23_25,
                  Tmax_25_28_lag4 = Tmax_25_28,
                  Tmax_28_30_lag4 = Tmax_28_30,
                  Tmax_30_33_lag4 = Tmax_30_33,
                  Tmax_above_33_lag4 = Tmax_above_33,
                  ## Absolute Tmean
                  Tmean_below_5_lag4 = Tmean_below_5,   
                  Tmean_5_10_lag4 = Tmean_5_10, 
                  Tmean_10_15_lag4 = Tmean_10_15,
                  Tmean_15_20_lag4 = Tmean_15_20,
                  Tmean_20_23_lag4 = Tmean_20_23,
                  Tmean_23_25_lag4 = Tmean_23_25,
                  Tmean_25_28_lag4 = Tmean_25_28,
                  Tmean_28_30_lag4 = Tmean_28_30,
                  Tmean_30_33_lag4 = Tmean_30_33,
                  Tmean_above_33_lag4 = Tmean_above_33,
                  tmax_lag4 = tmax,
                  tmean_lag4 = tmean,
                  ## Tmax prc
                  Tmax_pc_0_10_lag4  = Tmax_pc_0_10,
                  Tmax_pc_10_20_lag4 = Tmax_pc_10_20,
                  Tmax_pc_20_30_lag4 = Tmax_pc_20_30,
                  Tmax_pc_30_40_lag4 = Tmax_pc_30_40,
                  Tmax_pc_40_50_lag4 = Tmax_pc_40_50,   
                  Tmax_pc_50_60_lag4 = Tmax_pc_50_60, 
                  Tmax_pc_60_70_lag4 = Tmax_pc_60_70,  
                  Tmax_pc_70_80_lag4 = Tmax_pc_70_80,  
                  Tmax_pc_80_90_lag4 = Tmax_pc_80_90,  
                  Tmax_pc_90_100_lag4 = Tmax_pc_90_100, 
                  ## Tmean prc
                  Tmean_pc_0_10_lag4  = Tmean_pc_0_10,  
                  Tmean_pc_10_20_lag4 = Tmean_pc_10_20,
                  Tmean_pc_20_30_lag4 = Tmean_pc_20_30,
                  Tmean_pc_30_40_lag4 = Tmean_pc_30_40,
                  Tmean_pc_40_50_lag4 = Tmean_pc_40_50,
                  Tmean_pc_50_60_lag4 = Tmean_pc_50_60,
                  Tmean_pc_60_70_lag4 = Tmean_pc_60_70,
                  Tmean_pc_70_80_lag4 = Tmean_pc_70_80,
                  Tmean_pc_80_90_lag4 = Tmean_pc_80_90,
                  Tmean_pc_90_100_lag4 = Tmean_pc_90_100,
                  ## Tmax pct thresholds
                  Tmax_above_pc75_lag4 = Tmax_above_pc75,
                  Tmax_above_pc90_lag4 = Tmax_above_pc90,
                  Tmax_above_pc95_lag4 = Tmax_above_pc95,
                  Tmax_above_pc98_lag4 = Tmax_above_pc98,
                  ## Tmean pct thresholds
                  Tmean_above_pc75_lag4 = Tmean_above_pc75,
                  Tmean_above_pc90_lag4 = Tmean_above_pc90,
                  Tmean_above_pc95_lag4 = Tmean_above_pc95,
                  Tmean_above_pc98_lag4 = Tmean_above_pc98,
                  ## Pre
                  pre_lag4 = pre
    ) %>% 
    ### Lag 5
    left_join(tbins, by = c("clust" = "clust", "fertcmc_lag5" = "cmc", "SurveyId" = "SurveyId")) %>% 
    dplyr::rename(Tmax_below_5_lag5 = Tmax_below_5,   
                  Tmax_5_10_lag5 = Tmax_5_10, 
                  Tmax_10_15_lag5 = Tmax_10_15,
                  Tmax_15_20_lag5 = Tmax_15_20,
                  Tmax_20_23_lag5 = Tmax_20_23,
                  Tmax_23_25_lag5 = Tmax_23_25,
                  Tmax_25_28_lag5 = Tmax_25_28,
                  Tmax_28_30_lag5 = Tmax_28_30,
                  Tmax_30_33_lag5 = Tmax_30_33,
                  Tmax_above_33_lag5 = Tmax_above_33,
                  ## Absolute Tmean
                  Tmean_below_5_lag5 = Tmean_below_5,   
                  Tmean_5_10_lag5 = Tmean_5_10, 
                  Tmean_10_15_lag5 = Tmean_10_15,
                  Tmean_15_20_lag5 = Tmean_15_20,
                  Tmean_20_23_lag5 = Tmean_20_23,
                  Tmean_23_25_lag5 = Tmean_23_25,
                  Tmean_25_28_lag5 = Tmean_25_28,
                  Tmean_28_30_lag5 = Tmean_28_30,
                  Tmean_30_33_lag5 = Tmean_30_33,
                  Tmean_above_33_lag5 = Tmean_above_33,
                  tmax_lag5 = tmax,
                  tmean_lag5 = tmean,
                  ## Tmax prc
                  Tmax_pc_0_10_lag5  = Tmax_pc_0_10,
                  Tmax_pc_10_20_lag5 = Tmax_pc_10_20,
                  Tmax_pc_20_30_lag5 = Tmax_pc_20_30,
                  Tmax_pc_30_40_lag5 = Tmax_pc_30_40,
                  Tmax_pc_40_50_lag5 = Tmax_pc_40_50,   
                  Tmax_pc_50_60_lag5 = Tmax_pc_50_60, 
                  Tmax_pc_60_70_lag5 = Tmax_pc_60_70,  
                  Tmax_pc_70_80_lag5 = Tmax_pc_70_80,  
                  Tmax_pc_80_90_lag5 = Tmax_pc_80_90,  
                  Tmax_pc_90_100_lag5 = Tmax_pc_90_100, 
                  ## Tmean prc
                  Tmean_pc_0_10_lag5  = Tmean_pc_0_10,  
                  Tmean_pc_10_20_lag5 = Tmean_pc_10_20,
                  Tmean_pc_20_30_lag5 = Tmean_pc_20_30,
                  Tmean_pc_30_40_lag5 = Tmean_pc_30_40,
                  Tmean_pc_40_50_lag5 = Tmean_pc_40_50,
                  Tmean_pc_50_60_lag5 = Tmean_pc_50_60,
                  Tmean_pc_60_70_lag5 = Tmean_pc_60_70,
                  Tmean_pc_70_80_lag5 = Tmean_pc_70_80,
                  Tmean_pc_80_90_lag5 = Tmean_pc_80_90,
                  Tmean_pc_90_100_lag5 = Tmean_pc_90_100,
                  ## Tmax pct thresholds
                  Tmax_above_pc75_lag5 = Tmax_above_pc75,
                  Tmax_above_pc90_lag5 = Tmax_above_pc90,
                  Tmax_above_pc95_lag5 = Tmax_above_pc95,
                  Tmax_above_pc98_lag5 = Tmax_above_pc98,
                  ## Tmean pct thresholds
                  Tmean_above_pc75_lag5 = Tmean_above_pc75,
                  Tmean_above_pc90_lag5 = Tmean_above_pc90,
                  Tmean_above_pc95_lag5 = Tmean_above_pc95,
                  Tmean_above_pc98_lag5 = Tmean_above_pc98,
                  ## Pre
                  pre_lag5 = pre
    ) %>% 
    ### Lag 6
    left_join(tbins, by = c("clust" = "clust", "fertcmc_lag6" = "cmc", "SurveyId" = "SurveyId")) %>% 
    dplyr::rename(Tmax_below_5_lag6 = Tmax_below_5,   
                  Tmax_5_10_lag6 = Tmax_5_10, 
                  Tmax_10_15_lag6 = Tmax_10_15,
                  Tmax_15_20_lag6 = Tmax_15_20,
                  Tmax_20_23_lag6 = Tmax_20_23,
                  Tmax_23_25_lag6 = Tmax_23_25,
                  Tmax_25_28_lag6 = Tmax_25_28,
                  Tmax_28_30_lag6 = Tmax_28_30,
                  Tmax_30_33_lag6 = Tmax_30_33,
                  Tmax_above_33_lag6 = Tmax_above_33,
                  ## Absolute Tmean
                  Tmean_below_5_lag6 = Tmean_below_5,   
                  Tmean_5_10_lag6 = Tmean_5_10, 
                  Tmean_10_15_lag6 = Tmean_10_15,
                  Tmean_15_20_lag6 = Tmean_15_20,
                  Tmean_20_23_lag6 = Tmean_20_23,
                  Tmean_23_25_lag6 = Tmean_23_25,
                  Tmean_25_28_lag6 = Tmean_25_28,
                  Tmean_28_30_lag6 = Tmean_28_30,
                  Tmean_30_33_lag6 = Tmean_30_33,
                  Tmean_above_33_lag6 = Tmean_above_33,
                  tmax_lag6 = tmax,
                  tmean_lag6 = tmean,
                  ## Tmax prc
                  Tmax_pc_0_10_lag6  = Tmax_pc_0_10,
                  Tmax_pc_10_20_lag6 = Tmax_pc_10_20,
                  Tmax_pc_20_30_lag6 = Tmax_pc_20_30,
                  Tmax_pc_30_40_lag6 = Tmax_pc_30_40,
                  Tmax_pc_40_50_lag6 = Tmax_pc_40_50,   
                  Tmax_pc_50_60_lag6 = Tmax_pc_50_60, 
                  Tmax_pc_60_70_lag6 = Tmax_pc_60_70,  
                  Tmax_pc_70_80_lag6 = Tmax_pc_70_80,  
                  Tmax_pc_80_90_lag6 = Tmax_pc_80_90,  
                  Tmax_pc_90_100_lag6 = Tmax_pc_90_100, 
                  ## Tmean prc
                  Tmean_pc_0_10_lag6  = Tmean_pc_0_10,  
                  Tmean_pc_10_20_lag6 = Tmean_pc_10_20,
                  Tmean_pc_20_30_lag6 = Tmean_pc_20_30,
                  Tmean_pc_30_40_lag6 = Tmean_pc_30_40,
                  Tmean_pc_40_50_lag6 = Tmean_pc_40_50,
                  Tmean_pc_50_60_lag6 = Tmean_pc_50_60,
                  Tmean_pc_60_70_lag6 = Tmean_pc_60_70,
                  Tmean_pc_70_80_lag6 = Tmean_pc_70_80,
                  Tmean_pc_80_90_lag6 = Tmean_pc_80_90,
                  Tmean_pc_90_100_lag6 = Tmean_pc_90_100,
                  ## Tmax pct thresholds
                  Tmax_above_pc75_lag6 = Tmax_above_pc75,
                  Tmax_above_pc90_lag6 = Tmax_above_pc90,
                  Tmax_above_pc95_lag6 = Tmax_above_pc95,
                  Tmax_above_pc98_lag6 = Tmax_above_pc98,
                  ## Tmean pct thresholds
                  Tmean_above_pc75_lag6 = Tmean_above_pc75,
                  Tmean_above_pc90_lag6 = Tmean_above_pc90,
                  Tmean_above_pc95_lag6 = Tmean_above_pc95,
                  Tmean_above_pc98_lag6 = Tmean_above_pc98,
                  ## Pre
                  pre_lag6 = pre
    ) %>% 
    ### Lag 7
    left_join(tbins, by = c("clust" = "clust", "fertcmc_lag7" = "cmc", "SurveyId" = "SurveyId")) %>% 
    dplyr::rename(Tmax_below_5_lag7 = Tmax_below_5,   
                  Tmax_5_10_lag7 = Tmax_5_10, 
                  Tmax_10_15_lag7 = Tmax_10_15,
                  Tmax_15_20_lag7 = Tmax_15_20,
                  Tmax_20_23_lag7 = Tmax_20_23,
                  Tmax_23_25_lag7 = Tmax_23_25,
                  Tmax_25_28_lag7 = Tmax_25_28,
                  Tmax_28_30_lag7 = Tmax_28_30,
                  Tmax_30_33_lag7 = Tmax_30_33,
                  Tmax_above_33_lag7 = Tmax_above_33,
                  ## Absolute Tmean
                  Tmean_below_5_lag7 = Tmean_below_5,   
                  Tmean_5_10_lag7 = Tmean_5_10, 
                  Tmean_10_15_lag7 = Tmean_10_15,
                  Tmean_15_20_lag7 = Tmean_15_20,
                  Tmean_20_23_lag7 = Tmean_20_23,
                  Tmean_23_25_lag7 = Tmean_23_25,
                  Tmean_25_28_lag7 = Tmean_25_28,
                  Tmean_28_30_lag7 = Tmean_28_30,
                  Tmean_30_33_lag7 = Tmean_30_33,
                  Tmean_above_33_lag7 = Tmean_above_33,
                  tmax_lag7 = tmax,
                  tmean_lag7 = tmean,
                  ## Tmax prc
                  Tmax_pc_0_10_lag7  = Tmax_pc_0_10,
                  Tmax_pc_10_20_lag7 = Tmax_pc_10_20,
                  Tmax_pc_20_30_lag7 = Tmax_pc_20_30,
                  Tmax_pc_30_40_lag7 = Tmax_pc_30_40,
                  Tmax_pc_40_50_lag7 = Tmax_pc_40_50,   
                  Tmax_pc_50_60_lag7 = Tmax_pc_50_60, 
                  Tmax_pc_60_70_lag7 = Tmax_pc_60_70,  
                  Tmax_pc_70_80_lag7 = Tmax_pc_70_80,  
                  Tmax_pc_80_90_lag7 = Tmax_pc_80_90,  
                  Tmax_pc_90_100_lag7 = Tmax_pc_90_100, 
                  ## Tmean prc
                  Tmean_pc_0_10_lag7  = Tmean_pc_0_10,  
                  Tmean_pc_10_20_lag7 = Tmean_pc_10_20,
                  Tmean_pc_20_30_lag7 = Tmean_pc_20_30,
                  Tmean_pc_30_40_lag7 = Tmean_pc_30_40,
                  Tmean_pc_40_50_lag7 = Tmean_pc_40_50,
                  Tmean_pc_50_60_lag7 = Tmean_pc_50_60,
                  Tmean_pc_60_70_lag7 = Tmean_pc_60_70,
                  Tmean_pc_70_80_lag7 = Tmean_pc_70_80,
                  Tmean_pc_80_90_lag7 = Tmean_pc_80_90,
                  Tmean_pc_90_100_lag7 = Tmean_pc_90_100,
                  ## Tmax pct thresholds
                  Tmax_above_pc75_lag7 = Tmax_above_pc75,
                  Tmax_above_pc90_lag7 = Tmax_above_pc90,
                  Tmax_above_pc95_lag7 = Tmax_above_pc95,
                  Tmax_above_pc98_lag7 = Tmax_above_pc98,
                  ## Tmean pct thresholds
                  Tmean_above_pc75_lag7 = Tmean_above_pc75,
                  Tmean_above_pc90_lag7 = Tmean_above_pc90,
                  Tmean_above_pc95_lag7 = Tmean_above_pc95,
                  Tmean_above_pc98_lag7 = Tmean_above_pc98,
                  ## Pre
                  pre_lag7 = pre
    ) %>% 
    ### Lag 8
    left_join(tbins, by = c("clust" = "clust", "fertcmc_lag8" = "cmc", "SurveyId" = "SurveyId")) %>% 
    dplyr::rename(Tmax_below_5_lag8 = Tmax_below_5,   
                  Tmax_5_10_lag8 = Tmax_5_10, 
                  Tmax_10_15_lag8 = Tmax_10_15,
                  Tmax_15_20_lag8 = Tmax_15_20,
                  Tmax_20_23_lag8 = Tmax_20_23,
                  Tmax_23_25_lag8 = Tmax_23_25,
                  Tmax_25_28_lag8 = Tmax_25_28,
                  Tmax_28_30_lag8 = Tmax_28_30,
                  Tmax_30_33_lag8 = Tmax_30_33,
                  Tmax_above_33_lag8 = Tmax_above_33,
                  ## Absolute Tmean
                  Tmean_below_5_lag8 = Tmean_below_5,   
                  Tmean_5_10_lag8 = Tmean_5_10, 
                  Tmean_10_15_lag8 = Tmean_10_15,
                  Tmean_15_20_lag8 = Tmean_15_20,
                  Tmean_20_23_lag8 = Tmean_20_23,
                  Tmean_23_25_lag8 = Tmean_23_25,
                  Tmean_25_28_lag8 = Tmean_25_28,
                  Tmean_28_30_lag8 = Tmean_28_30,
                  Tmean_30_33_lag8 = Tmean_30_33,
                  Tmean_above_33_lag8 = Tmean_above_33,
                  tmax_lag8 = tmax,
                  tmean_lag8 = tmean,
                  ## Tmax prc
                  Tmax_pc_0_10_lag8  = Tmax_pc_0_10,
                  Tmax_pc_10_20_lag8 = Tmax_pc_10_20,
                  Tmax_pc_20_30_lag8 = Tmax_pc_20_30,
                  Tmax_pc_30_40_lag8 = Tmax_pc_30_40,
                  Tmax_pc_40_50_lag8 = Tmax_pc_40_50,   
                  Tmax_pc_50_60_lag8 = Tmax_pc_50_60, 
                  Tmax_pc_60_70_lag8 = Tmax_pc_60_70,  
                  Tmax_pc_70_80_lag8 = Tmax_pc_70_80,  
                  Tmax_pc_80_90_lag8 = Tmax_pc_80_90,  
                  Tmax_pc_90_100_lag8 = Tmax_pc_90_100, 
                  ## Tmean prc
                  Tmean_pc_0_10_lag8  = Tmean_pc_0_10,  
                  Tmean_pc_10_20_lag8 = Tmean_pc_10_20,
                  Tmean_pc_20_30_lag8 = Tmean_pc_20_30,
                  Tmean_pc_30_40_lag8 = Tmean_pc_30_40,
                  Tmean_pc_40_50_lag8 = Tmean_pc_40_50,
                  Tmean_pc_50_60_lag8 = Tmean_pc_50_60,
                  Tmean_pc_60_70_lag8 = Tmean_pc_60_70,
                  Tmean_pc_70_80_lag8 = Tmean_pc_70_80,
                  Tmean_pc_80_90_lag8 = Tmean_pc_80_90,
                  Tmean_pc_90_100_lag8 = Tmean_pc_90_100,
                  ## Tmax pct thresholds
                  Tmax_above_pc75_lag8 = Tmax_above_pc75,
                  Tmax_above_pc90_lag8 = Tmax_above_pc90,
                  Tmax_above_pc95_lag8 = Tmax_above_pc95,
                  Tmax_above_pc98_lag8 = Tmax_above_pc98,
                  ## Tmean pct thresholds
                  Tmean_above_pc75_lag8 = Tmean_above_pc75,
                  Tmean_above_pc90_lag8 = Tmean_above_pc90,
                  Tmean_above_pc95_lag8 = Tmean_above_pc95,
                  Tmean_above_pc98_lag8 = Tmean_above_pc98,
                  ## Pre
                  pre_lag8 = pre
    ) %>% 
    ### Lag 9
    left_join(tbins, by = c("clust" = "clust", "fertcmc_lag9" = "cmc", "SurveyId" = "SurveyId")) %>% 
    dplyr::rename(Tmax_below_5_lag9 = Tmax_below_5,   
                  Tmax_5_10_lag9 = Tmax_5_10, 
                  Tmax_10_15_lag9 = Tmax_10_15,
                  Tmax_15_20_lag9 = Tmax_15_20,
                  Tmax_20_23_lag9 = Tmax_20_23,
                  Tmax_23_25_lag9 = Tmax_23_25,
                  Tmax_25_28_lag9 = Tmax_25_28,
                  Tmax_28_30_lag9 = Tmax_28_30,
                  Tmax_30_33_lag9 = Tmax_30_33,
                  Tmax_above_33_lag9 = Tmax_above_33,
                  ## Absolute Tmean
                  Tmean_below_5_lag9 = Tmean_below_5,   
                  Tmean_5_10_lag9 = Tmean_5_10, 
                  Tmean_10_15_lag9 = Tmean_10_15,
                  Tmean_15_20_lag9 = Tmean_15_20,
                  Tmean_20_23_lag9 = Tmean_20_23,
                  Tmean_23_25_lag9 = Tmean_23_25,
                  Tmean_25_28_lag9 = Tmean_25_28,
                  Tmean_28_30_lag9 = Tmean_28_30,
                  Tmean_30_33_lag9 = Tmean_30_33,
                  Tmean_above_33_lag9 = Tmean_above_33,
                  tmax_lag9 = tmax,
                  tmean_lag9 = tmean,
                  ## Tmax prc
                  Tmax_pc_0_10_lag9  = Tmax_pc_0_10,
                  Tmax_pc_10_20_lag9 = Tmax_pc_10_20,
                  Tmax_pc_20_30_lag9 = Tmax_pc_20_30,
                  Tmax_pc_30_40_lag9 = Tmax_pc_30_40,
                  Tmax_pc_40_50_lag9 = Tmax_pc_40_50,   
                  Tmax_pc_50_60_lag9 = Tmax_pc_50_60, 
                  Tmax_pc_60_70_lag9 = Tmax_pc_60_70,  
                  Tmax_pc_70_80_lag9 = Tmax_pc_70_80,  
                  Tmax_pc_80_90_lag9 = Tmax_pc_80_90,  
                  Tmax_pc_90_100_lag9 = Tmax_pc_90_100, 
                  ## Tmean prc
                  Tmean_pc_0_10_lag9  = Tmean_pc_0_10,  
                  Tmean_pc_10_20_lag9 = Tmean_pc_10_20,
                  Tmean_pc_20_30_lag9 = Tmean_pc_20_30,
                  Tmean_pc_30_40_lag9 = Tmean_pc_30_40,
                  Tmean_pc_40_50_lag9 = Tmean_pc_40_50,
                  Tmean_pc_50_60_lag9 = Tmean_pc_50_60,
                  Tmean_pc_60_70_lag9 = Tmean_pc_60_70,
                  Tmean_pc_70_80_lag9 = Tmean_pc_70_80,
                  Tmean_pc_80_90_lag9 = Tmean_pc_80_90,
                  Tmean_pc_90_100_lag9 = Tmean_pc_90_100,
                  ## Tmax pct thresholds
                  Tmax_above_pc75_lag9 = Tmax_above_pc75,
                  Tmax_above_pc90_lag9 = Tmax_above_pc90,
                  Tmax_above_pc95_lag9 = Tmax_above_pc95,
                  Tmax_above_pc98_lag9 = Tmax_above_pc98,
                  ## Tmean pct thresholds
                  Tmean_above_pc75_lag9 = Tmean_above_pc75,
                  Tmean_above_pc90_lag9 = Tmean_above_pc90,
                  Tmean_above_pc95_lag9 = Tmean_above_pc95,
                  Tmean_above_pc98_lag9 = Tmean_above_pc98,
                  ## Pre
                  pre_lag9 = pre
    ) %>% 
    ### Lag 10
    left_join(tbins, by = c("clust" = "clust", "fertcmc_lag10" = "cmc", "SurveyId" = "SurveyId")) %>% 
    dplyr::rename(Tmax_below_5_lag10 = Tmax_below_5,   
                  Tmax_5_10_lag10 = Tmax_5_10, 
                  Tmax_10_15_lag10 = Tmax_10_15,
                  Tmax_15_20_lag10 = Tmax_15_20,
                  Tmax_20_23_lag10 = Tmax_20_23,
                  Tmax_23_25_lag10 = Tmax_23_25,
                  Tmax_25_28_lag10 = Tmax_25_28,
                  Tmax_28_30_lag10 = Tmax_28_30,
                  Tmax_30_33_lag10 = Tmax_30_33,
                  Tmax_above_33_lag10 = Tmax_above_33,
                  ## Absolute Tmean
                  Tmean_below_5_lag10 = Tmean_below_5,   
                  Tmean_5_10_lag10 = Tmean_5_10, 
                  Tmean_10_15_lag10 = Tmean_10_15,
                  Tmean_15_20_lag10 = Tmean_15_20,
                  Tmean_20_23_lag10 = Tmean_20_23,
                  Tmean_23_25_lag10 = Tmean_23_25,
                  Tmean_25_28_lag10 = Tmean_25_28,
                  Tmean_28_30_lag10 = Tmean_28_30,
                  Tmean_30_33_lag10 = Tmean_30_33,
                  Tmean_above_33_lag10 = Tmean_above_33,
                  tmax_lag10 = tmax,
                  tmean_lag10 = tmean,
                  ## Tmax prc
                  Tmax_pc_0_10_lag10  = Tmax_pc_0_10,
                  Tmax_pc_10_20_lag10 = Tmax_pc_10_20,
                  Tmax_pc_20_30_lag10 = Tmax_pc_20_30,
                  Tmax_pc_30_40_lag10 = Tmax_pc_30_40,
                  Tmax_pc_40_50_lag10 = Tmax_pc_40_50,   
                  Tmax_pc_50_60_lag10 = Tmax_pc_50_60, 
                  Tmax_pc_60_70_lag10 = Tmax_pc_60_70,  
                  Tmax_pc_70_80_lag10 = Tmax_pc_70_80,  
                  Tmax_pc_80_90_lag10 = Tmax_pc_80_90,  
                  Tmax_pc_90_100_lag10 = Tmax_pc_90_100, 
                  ## Tmean prc
                  Tmean_pc_0_10_lag10  = Tmean_pc_0_10,  
                  Tmean_pc_10_20_lag10 = Tmean_pc_10_20,
                  Tmean_pc_20_30_lag10 = Tmean_pc_20_30,
                  Tmean_pc_30_40_lag10 = Tmean_pc_30_40,
                  Tmean_pc_40_50_lag10 = Tmean_pc_40_50,
                  Tmean_pc_50_60_lag10 = Tmean_pc_50_60,
                  Tmean_pc_60_70_lag10 = Tmean_pc_60_70,
                  Tmean_pc_70_80_lag10 = Tmean_pc_70_80,
                  Tmean_pc_80_90_lag10 = Tmean_pc_80_90,
                  Tmean_pc_90_100_lag10 = Tmean_pc_90_100,
                  ## Tmax pct thresholds
                  Tmax_above_pc75_lag10 = Tmax_above_pc75,
                  Tmax_above_pc90_lag10 = Tmax_above_pc90,
                  Tmax_above_pc95_lag10 = Tmax_above_pc95,
                  Tmax_above_pc98_lag10 = Tmax_above_pc98,
                  ## Tmean pct thresholds
                  Tmean_above_pc75_lag10 = Tmean_above_pc75,
                  Tmean_above_pc90_lag10 = Tmean_above_pc90,
                  Tmean_above_pc95_lag10 = Tmean_above_pc95,
                  Tmean_above_pc98_lag10 = Tmean_above_pc98,
                  ## Pre
                  pre_lag10 = pre
    ) %>% 
    ### Lag 11
    left_join(tbins, by = c("clust" = "clust", "fertcmc_lag11" = "cmc", "SurveyId" = "SurveyId")) %>% 
    dplyr::rename(Tmax_below_5_lag11 = Tmax_below_5,   
                  Tmax_5_10_lag11 = Tmax_5_10, 
                  Tmax_10_15_lag11 = Tmax_10_15,
                  Tmax_15_20_lag11 = Tmax_15_20,
                  Tmax_20_23_lag11 = Tmax_20_23,
                  Tmax_23_25_lag11 = Tmax_23_25,
                  Tmax_25_28_lag11 = Tmax_25_28,
                  Tmax_28_30_lag11 = Tmax_28_30,
                  Tmax_30_33_lag11 = Tmax_30_33,
                  Tmax_above_33_lag11 = Tmax_above_33,
                  ## Absolute Tmean
                  Tmean_below_5_lag11 = Tmean_below_5,   
                  Tmean_5_10_lag11 = Tmean_5_10, 
                  Tmean_10_15_lag11 = Tmean_10_15,
                  Tmean_15_20_lag11 = Tmean_15_20,
                  Tmean_20_23_lag11 = Tmean_20_23,
                  Tmean_23_25_lag11 = Tmean_23_25,
                  Tmean_25_28_lag11 = Tmean_25_28,
                  Tmean_28_30_lag11 = Tmean_28_30,
                  Tmean_30_33_lag11 = Tmean_30_33,
                  Tmean_above_33_lag11 = Tmean_above_33,
                  tmax_lag11 = tmax,
                  tmean_lag11 = tmean,
                  ## Tmax prc
                  Tmax_pc_0_10_lag11  = Tmax_pc_0_10,
                  Tmax_pc_10_20_lag11 = Tmax_pc_10_20,
                  Tmax_pc_20_30_lag11 = Tmax_pc_20_30,
                  Tmax_pc_30_40_lag11 = Tmax_pc_30_40,
                  Tmax_pc_40_50_lag11 = Tmax_pc_40_50,   
                  Tmax_pc_50_60_lag11 = Tmax_pc_50_60, 
                  Tmax_pc_60_70_lag11 = Tmax_pc_60_70,  
                  Tmax_pc_70_80_lag11 = Tmax_pc_70_80,  
                  Tmax_pc_80_90_lag11 = Tmax_pc_80_90,  
                  Tmax_pc_90_100_lag11 = Tmax_pc_90_100, 
                  ## Tmean prc
                  Tmean_pc_0_10_lag11  = Tmean_pc_0_10,  
                  Tmean_pc_10_20_lag11 = Tmean_pc_10_20,
                  Tmean_pc_20_30_lag11 = Tmean_pc_20_30,
                  Tmean_pc_30_40_lag11 = Tmean_pc_30_40,
                  Tmean_pc_40_50_lag11 = Tmean_pc_40_50,
                  Tmean_pc_50_60_lag11 = Tmean_pc_50_60,
                  Tmean_pc_60_70_lag11 = Tmean_pc_60_70,
                  Tmean_pc_70_80_lag11 = Tmean_pc_70_80,
                  Tmean_pc_80_90_lag11 = Tmean_pc_80_90,
                  Tmean_pc_90_100_lag11 = Tmean_pc_90_100,
                  ## Tmax pct thresholds
                  Tmax_above_pc75_lag11 = Tmax_above_pc75,
                  Tmax_above_pc90_lag11 = Tmax_above_pc90,
                  Tmax_above_pc95_lag11 = Tmax_above_pc95,
                  Tmax_above_pc98_lag11 = Tmax_above_pc98,
                  ## Tmean pct thresholds
                  Tmean_above_pc75_lag11 = Tmean_above_pc75,
                  Tmean_above_pc90_lag11 = Tmean_above_pc90,
                  Tmean_above_pc95_lag11 = Tmean_above_pc95,
                  Tmean_above_pc98_lag11 = Tmean_above_pc98,
                  ## Pre
                  pre_lag11 = pre
    ) %>% 
    ### Lag 12
    left_join(tbins, by = c("clust" = "clust", "fertcmc_lag12" = "cmc", "SurveyId" = "SurveyId")) %>% 
    dplyr::rename(Tmax_below_5_lag12 = Tmax_below_5,   
                  Tmax_5_10_lag12 = Tmax_5_10, 
                  Tmax_10_15_lag12 = Tmax_10_15,
                  Tmax_15_20_lag12 = Tmax_15_20,
                  Tmax_20_23_lag12 = Tmax_20_23,
                  Tmax_23_25_lag12 = Tmax_23_25,
                  Tmax_25_28_lag12 = Tmax_25_28,
                  Tmax_28_30_lag12 = Tmax_28_30,
                  Tmax_30_33_lag12 = Tmax_30_33,
                  Tmax_above_33_lag12 = Tmax_above_33,
                  ## Absolute Tmean
                  Tmean_below_5_lag12 = Tmean_below_5,   
                  Tmean_5_10_lag12 = Tmean_5_10, 
                  Tmean_10_15_lag12 = Tmean_10_15,
                  Tmean_15_20_lag12 = Tmean_15_20,
                  Tmean_20_23_lag12 = Tmean_20_23,
                  Tmean_23_25_lag12 = Tmean_23_25,
                  Tmean_25_28_lag12 = Tmean_25_28,
                  Tmean_28_30_lag12 = Tmean_28_30,
                  Tmean_30_33_lag12 = Tmean_30_33,
                  Tmean_above_33_lag12 = Tmean_above_33,
                  tmax_lag12 = tmax,
                  tmean_lag12 = tmean,
                  ## Tmax prc
                  Tmax_pc_0_10_lag12  = Tmax_pc_0_10,
                  Tmax_pc_10_20_lag12 = Tmax_pc_10_20,
                  Tmax_pc_20_30_lag12 = Tmax_pc_20_30,
                  Tmax_pc_30_40_lag12 = Tmax_pc_30_40,
                  Tmax_pc_40_50_lag12 = Tmax_pc_40_50,   
                  Tmax_pc_50_60_lag12 = Tmax_pc_50_60, 
                  Tmax_pc_60_70_lag12 = Tmax_pc_60_70,  
                  Tmax_pc_70_80_lag12 = Tmax_pc_70_80,  
                  Tmax_pc_80_90_lag12 = Tmax_pc_80_90,  
                  Tmax_pc_90_100_lag12 = Tmax_pc_90_100, 
                  ## Tmean prc
                  Tmean_pc_0_10_lag12  = Tmean_pc_0_10,  
                  Tmean_pc_10_20_lag12 = Tmean_pc_10_20,
                  Tmean_pc_20_30_lag12 = Tmean_pc_20_30,
                  Tmean_pc_30_40_lag12 = Tmean_pc_30_40,
                  Tmean_pc_40_50_lag12 = Tmean_pc_40_50,
                  Tmean_pc_50_60_lag12 = Tmean_pc_50_60,
                  Tmean_pc_60_70_lag12 = Tmean_pc_60_70,
                  Tmean_pc_70_80_lag12 = Tmean_pc_70_80,
                  Tmean_pc_80_90_lag12 = Tmean_pc_80_90,
                  Tmean_pc_90_100_lag12 = Tmean_pc_90_100,
                  ## Tmax pct thresholds
                  Tmax_above_pc75_lag12 = Tmax_above_pc75,
                  Tmax_above_pc90_lag12 = Tmax_above_pc90,
                  Tmax_above_pc95_lag12 = Tmax_above_pc95,
                  Tmax_above_pc98_lag12 = Tmax_above_pc98,
                  ## Tmean pct thresholds
                  Tmean_above_pc75_lag12 = Tmean_above_pc75,
                  Tmean_above_pc90_lag12 = Tmean_above_pc90,
                  Tmean_above_pc95_lag12 = Tmean_above_pc95,
                  Tmean_above_pc98_lag12 = Tmean_above_pc98,
                  ## Pre
                  pre_lag12 = pre
    )  %>%
    ### Lag 13
    left_join(tbins, by = c("clust" = "clust", "fertcmc_lag13" = "cmc", "SurveyId" = "SurveyId")) %>% 
    dplyr::rename(Tmax_below_5_lag13 = Tmax_below_5,   
                  Tmax_5_10_lag13 = Tmax_5_10, 
                  Tmax_10_15_lag13 = Tmax_10_15,
                  Tmax_15_20_lag13 = Tmax_15_20,
                  Tmax_20_23_lag13 = Tmax_20_23,
                  Tmax_23_25_lag13 = Tmax_23_25,
                  Tmax_25_28_lag13 = Tmax_25_28,
                  Tmax_28_30_lag13 = Tmax_28_30,
                  Tmax_30_33_lag13 = Tmax_30_33,
                  Tmax_above_33_lag13 = Tmax_above_33,
                  ## Absolute Tmean
                  Tmean_below_5_lag13 = Tmean_below_5,   
                  Tmean_5_10_lag13 = Tmean_5_10, 
                  Tmean_10_15_lag13 = Tmean_10_15,
                  Tmean_15_20_lag13 = Tmean_15_20,
                  Tmean_20_23_lag13 = Tmean_20_23,
                  Tmean_23_25_lag13 = Tmean_23_25,
                  Tmean_25_28_lag13 = Tmean_25_28,
                  Tmean_28_30_lag13 = Tmean_28_30,
                  Tmean_30_33_lag13 = Tmean_30_33,
                  Tmean_above_33_lag13 = Tmean_above_33,
                  tmax_lag13 = tmax,
                  tmean_lag13 = tmean,
                  ## Tmax prc
                  Tmax_pc_0_10_lag13  = Tmax_pc_0_10,
                  Tmax_pc_10_20_lag13 = Tmax_pc_10_20,
                  Tmax_pc_20_30_lag13 = Tmax_pc_20_30,
                  Tmax_pc_30_40_lag13 = Tmax_pc_30_40,
                  Tmax_pc_40_50_lag13 = Tmax_pc_40_50,   
                  Tmax_pc_50_60_lag13 = Tmax_pc_50_60, 
                  Tmax_pc_60_70_lag13 = Tmax_pc_60_70,  
                  Tmax_pc_70_80_lag13 = Tmax_pc_70_80,  
                  Tmax_pc_80_90_lag13 = Tmax_pc_80_90,  
                  Tmax_pc_90_100_lag13 = Tmax_pc_90_100, 
                  ## Tmean prc
                  Tmean_pc_0_10_lag13  = Tmean_pc_0_10,  
                  Tmean_pc_10_20_lag13 = Tmean_pc_10_20,
                  Tmean_pc_20_30_lag13 = Tmean_pc_20_30,
                  Tmean_pc_30_40_lag13 = Tmean_pc_30_40,
                  Tmean_pc_40_50_lag13 = Tmean_pc_40_50,
                  Tmean_pc_50_60_lag13 = Tmean_pc_50_60,
                  Tmean_pc_60_70_lag13 = Tmean_pc_60_70,
                  Tmean_pc_70_80_lag13 = Tmean_pc_70_80,
                  Tmean_pc_80_90_lag13 = Tmean_pc_80_90,
                  Tmean_pc_90_100_lag13 = Tmean_pc_90_100,
                  ## Tmax pct thresholds
                  Tmax_above_pc75_lag13 = Tmax_above_pc75,
                  Tmax_above_pc90_lag13 = Tmax_above_pc90,
                  Tmax_above_pc95_lag13 = Tmax_above_pc95,
                  Tmax_above_pc98_lag13 = Tmax_above_pc98,
                  ## Tmean pct thresholds
                  Tmean_above_pc75_lag13 = Tmean_above_pc75,
                  Tmean_above_pc90_lag13 = Tmean_above_pc90,
                  Tmean_above_pc95_lag13 = Tmean_above_pc95,
                  Tmean_above_pc98_lag13 = Tmean_above_pc98,
                  ## Pre
                  pre_lag13 = pre
    )  %>%
    ### Lag 14
    left_join(tbins, by = c("clust" = "clust", "fertcmc_lag14" = "cmc", "SurveyId" = "SurveyId")) %>% 
    dplyr::rename(Tmax_below_5_lag14 = Tmax_below_5,   
                  Tmax_5_10_lag14 = Tmax_5_10, 
                  Tmax_10_15_lag14 = Tmax_10_15,
                  Tmax_15_20_lag14 = Tmax_15_20,
                  Tmax_20_23_lag14 = Tmax_20_23,
                  Tmax_23_25_lag14 = Tmax_23_25,
                  Tmax_25_28_lag14 = Tmax_25_28,
                  Tmax_28_30_lag14 = Tmax_28_30,
                  Tmax_30_33_lag14 = Tmax_30_33,
                  Tmax_above_33_lag14 = Tmax_above_33,
                  ## Absolute Tmean
                  Tmean_below_5_lag14 = Tmean_below_5,   
                  Tmean_5_10_lag14 = Tmean_5_10, 
                  Tmean_10_15_lag14 = Tmean_10_15,
                  Tmean_15_20_lag14 = Tmean_15_20,
                  Tmean_20_23_lag14 = Tmean_20_23,
                  Tmean_23_25_lag14 = Tmean_23_25,
                  Tmean_25_28_lag14 = Tmean_25_28,
                  Tmean_28_30_lag14 = Tmean_28_30,
                  Tmean_30_33_lag14 = Tmean_30_33,
                  Tmean_above_33_lag14 = Tmean_above_33,
                  tmax_lag14 = tmax,
                  tmean_lag14 = tmean,
                  ## Tmax prc
                  Tmax_pc_0_10_lag14  = Tmax_pc_0_10,
                  Tmax_pc_10_20_lag14 = Tmax_pc_10_20,
                  Tmax_pc_20_30_lag14 = Tmax_pc_20_30,
                  Tmax_pc_30_40_lag14 = Tmax_pc_30_40,
                  Tmax_pc_40_50_lag14 = Tmax_pc_40_50,   
                  Tmax_pc_50_60_lag14 = Tmax_pc_50_60, 
                  Tmax_pc_60_70_lag14 = Tmax_pc_60_70,  
                  Tmax_pc_70_80_lag14 = Tmax_pc_70_80,  
                  Tmax_pc_80_90_lag14 = Tmax_pc_80_90,  
                  Tmax_pc_90_100_lag14 = Tmax_pc_90_100, 
                  ## Tmean prc
                  Tmean_pc_0_10_lag14  = Tmean_pc_0_10,  
                  Tmean_pc_10_20_lag14 = Tmean_pc_10_20,
                  Tmean_pc_20_30_lag14 = Tmean_pc_20_30,
                  Tmean_pc_30_40_lag14 = Tmean_pc_30_40,
                  Tmean_pc_40_50_lag14 = Tmean_pc_40_50,
                  Tmean_pc_50_60_lag14 = Tmean_pc_50_60,
                  Tmean_pc_60_70_lag14 = Tmean_pc_60_70,
                  Tmean_pc_70_80_lag14 = Tmean_pc_70_80,
                  Tmean_pc_80_90_lag14 = Tmean_pc_80_90,
                  Tmean_pc_90_100_lag14 = Tmean_pc_90_100,
                  ## Tmax pct thresholds
                  Tmax_above_pc75_lag14 = Tmax_above_pc75,
                  Tmax_above_pc90_lag14 = Tmax_above_pc90,
                  Tmax_above_pc95_lag14 = Tmax_above_pc95,
                  Tmax_above_pc98_lag14 = Tmax_above_pc98,
                  ## Tmean pct thresholds
                  Tmean_above_pc75_lag14 = Tmean_above_pc75,
                  Tmean_above_pc90_lag14 = Tmean_above_pc90,
                  Tmean_above_pc95_lag14 = Tmean_above_pc95,
                  Tmean_above_pc98_lag14 = Tmean_above_pc98,
                  ## Pre
                  pre_lag14 = pre
    )  %>%
    ### Lag 15
    left_join(tbins, by = c("clust" = "clust", "fertcmc_lag15" = "cmc", "SurveyId" = "SurveyId")) %>% 
    dplyr::rename(Tmax_below_5_lag15 = Tmax_below_5,   
                  Tmax_5_10_lag15 = Tmax_5_10, 
                  Tmax_10_15_lag15 = Tmax_10_15,
                  Tmax_15_20_lag15 = Tmax_15_20,
                  Tmax_20_23_lag15 = Tmax_20_23,
                  Tmax_23_25_lag15 = Tmax_23_25,
                  Tmax_25_28_lag15 = Tmax_25_28,
                  Tmax_28_30_lag15 = Tmax_28_30,
                  Tmax_30_33_lag15 = Tmax_30_33,
                  Tmax_above_33_lag15 = Tmax_above_33,
                  ## Absolute Tmean
                  Tmean_below_5_lag15 = Tmean_below_5,   
                  Tmean_5_10_lag15 = Tmean_5_10, 
                  Tmean_10_15_lag15 = Tmean_10_15,
                  Tmean_15_20_lag15 = Tmean_15_20,
                  Tmean_20_23_lag15 = Tmean_20_23,
                  Tmean_23_25_lag15 = Tmean_23_25,
                  Tmean_25_28_lag15 = Tmean_25_28,
                  Tmean_28_30_lag15 = Tmean_28_30,
                  Tmean_30_33_lag15 = Tmean_30_33,
                  Tmean_above_33_lag15 = Tmean_above_33,
                  tmax_lag15 = tmax,
                  tmean_lag15 = tmean,
                  ## Tmax prc
                  Tmax_pc_0_10_lag15  = Tmax_pc_0_10,
                  Tmax_pc_10_20_lag15 = Tmax_pc_10_20,
                  Tmax_pc_20_30_lag15 = Tmax_pc_20_30,
                  Tmax_pc_30_40_lag15 = Tmax_pc_30_40,
                  Tmax_pc_40_50_lag15 = Tmax_pc_40_50,   
                  Tmax_pc_50_60_lag15 = Tmax_pc_50_60, 
                  Tmax_pc_60_70_lag15 = Tmax_pc_60_70,  
                  Tmax_pc_70_80_lag15 = Tmax_pc_70_80,  
                  Tmax_pc_80_90_lag15 = Tmax_pc_80_90,  
                  Tmax_pc_90_100_lag15 = Tmax_pc_90_100, 
                  ## Tmean prc
                  Tmean_pc_0_10_lag15  = Tmean_pc_0_10,  
                  Tmean_pc_10_20_lag15 = Tmean_pc_10_20,
                  Tmean_pc_20_30_lag15 = Tmean_pc_20_30,
                  Tmean_pc_30_40_lag15 = Tmean_pc_30_40,
                  Tmean_pc_40_50_lag15 = Tmean_pc_40_50,
                  Tmean_pc_50_60_lag15 = Tmean_pc_50_60,
                  Tmean_pc_60_70_lag15 = Tmean_pc_60_70,
                  Tmean_pc_70_80_lag15 = Tmean_pc_70_80,
                  Tmean_pc_80_90_lag15 = Tmean_pc_80_90,
                  Tmean_pc_90_100_lag15 = Tmean_pc_90_100,
                  ## Tmax pct thresholds
                  Tmax_above_pc75_lag15 = Tmax_above_pc75,
                  Tmax_above_pc90_lag15 = Tmax_above_pc90,
                  Tmax_above_pc95_lag15 = Tmax_above_pc95,
                  Tmax_above_pc98_lag15 = Tmax_above_pc98,
                  ## Tmean pct thresholds
                  Tmean_above_pc75_lag15 = Tmean_above_pc75,
                  Tmean_above_pc90_lag15 = Tmean_above_pc90,
                  Tmean_above_pc95_lag15 = Tmean_above_pc95,
                  Tmean_above_pc98_lag15 = Tmean_above_pc98,
                  ## Pre
                  pre_lag15 = pre
    ) 
  
  
  
  ## Generate TMAX bins for each trimester
  df5 <- df5 %>% 
    # TMAX below 5
    mutate(Tmax_below_5_trim1 = Tmax_below_5_lag6 + Tmax_below_5_lag7 + Tmax_below_5_lag8,
           Tmax_below_5_trim2 = Tmax_below_5_lag3 + Tmax_below_5_lag4 + Tmax_below_5_lag5,
           Tmax_below_5_trim3 = Tmax_below_5_lag0 + Tmax_below_5_lag1 + Tmax_below_5_lag2,
           Tmax_below_5_trim_minus1 = Tmax_below_5_lag9 + Tmax_below_5_lag10 + Tmax_below_5_lag11,
           Tmax_below_5_trim_minus2 = Tmax_below_5_lag12 + Tmax_below_5_lag13 + Tmax_below_5_lag14
    ) %>% 
    # TMAX 5 to 10
    mutate(Tmax_5_10_trim1 = Tmax_5_10_lag6 + Tmax_5_10_lag7 + Tmax_5_10_lag8,
           Tmax_5_10_trim2 = Tmax_5_10_lag3 + Tmax_5_10_lag4 + Tmax_5_10_lag5,
           Tmax_5_10_trim3 = Tmax_5_10_lag0 + Tmax_5_10_lag1 + Tmax_5_10_lag2,
           Tmax_5_10_trim_minus1 = Tmax_5_10_lag9 + Tmax_5_10_lag10 + Tmax_5_10_lag11,
           Tmax_5_10_trim_minus2 = Tmax_5_10_lag12 + Tmax_5_10_lag13 + Tmax_5_10_lag14
    ) %>% 
    # TMAX 10 to 15
    mutate(Tmax_10_15_trim1 = Tmax_10_15_lag6 + Tmax_10_15_lag7 + Tmax_10_15_lag8,
           Tmax_10_15_trim2 = Tmax_10_15_lag3 + Tmax_10_15_lag4 + Tmax_10_15_lag5,
           Tmax_10_15_trim3 = Tmax_10_15_lag0 + Tmax_10_15_lag1 + Tmax_10_15_lag2,
           Tmax_10_15_trim_minus1 = Tmax_10_15_lag9 + Tmax_10_15_lag10 + Tmax_10_15_lag11,
           Tmax_10_15_trim_minus2 = Tmax_10_15_lag12 + Tmax_10_15_lag13 + Tmax_10_15_lag14
    ) %>% 
    # TMAX 15 to 20
    mutate(Tmax_15_20_trim1 = Tmax_15_20_lag6 + Tmax_15_20_lag7 + Tmax_15_20_lag8,
           Tmax_15_20_trim2 = Tmax_15_20_lag3 + Tmax_15_20_lag4 + Tmax_15_20_lag5,
           Tmax_15_20_trim3 = Tmax_15_20_lag0 + Tmax_15_20_lag1 + Tmax_15_20_lag2,
           Tmax_15_20_trim_minus1 = Tmax_15_20_lag9 + Tmax_15_20_lag10 + Tmax_15_20_lag11,
           Tmax_15_20_trim_minus2 = Tmax_15_20_lag12 + Tmax_15_20_lag13 + Tmax_15_20_lag14
    ) %>% 
    # TMAX 20 to 23
    mutate(Tmax_20_23_trim1 = Tmax_20_23_lag6 + Tmax_20_23_lag7 + Tmax_20_23_lag8,
           Tmax_20_23_trim2 = Tmax_20_23_lag3 + Tmax_20_23_lag4 + Tmax_20_23_lag5,
           Tmax_20_23_trim3 = Tmax_20_23_lag0 + Tmax_20_23_lag1 + Tmax_20_23_lag2,
           Tmax_20_23_trim_minus1 = Tmax_20_23_lag9 + Tmax_20_23_lag10 + Tmax_20_23_lag11,
           Tmax_20_23_trim_minus2 = Tmax_20_23_lag12 + Tmax_20_23_lag13 + Tmax_20_23_lag14
    ) %>% 
    # TMAX 23 to 25
    mutate(Tmax_23_25_trim1 = Tmax_23_25_lag6 + Tmax_23_25_lag7 + Tmax_23_25_lag8,
           Tmax_23_25_trim2 = Tmax_23_25_lag3 + Tmax_23_25_lag4 + Tmax_23_25_lag5,
           Tmax_23_25_trim3 = Tmax_23_25_lag0 + Tmax_23_25_lag1 + Tmax_23_25_lag2,
           Tmax_23_25_trim_minus1 = Tmax_23_25_lag9 + Tmax_23_25_lag10 + Tmax_23_25_lag11,
           Tmax_23_25_trim_minus2 = Tmax_23_25_lag12 + Tmax_23_25_lag13 + Tmax_23_25_lag14
    ) %>% 
    # TMAX 25 to 28
    mutate(Tmax_25_28_trim1 = Tmax_25_28_lag6 + Tmax_25_28_lag7 + Tmax_25_28_lag8,
           Tmax_25_28_trim2 = Tmax_25_28_lag3 + Tmax_25_28_lag4 + Tmax_25_28_lag5,
           Tmax_25_28_trim3 = Tmax_25_28_lag0 + Tmax_25_28_lag1 + Tmax_25_28_lag2,
           Tmax_25_28_trim_minus1 = Tmax_25_28_lag9 + Tmax_25_28_lag10 + Tmax_25_28_lag11,
           Tmax_25_28_trim_minus2 = Tmax_25_28_lag12 + Tmax_25_28_lag13 + Tmax_25_28_lag14
    ) %>%
    # TMAX 28 to 30
    mutate(Tmax_28_30_trim1 = Tmax_28_30_lag6 + Tmax_28_30_lag7 + Tmax_28_30_lag8,
           Tmax_28_30_trim2 = Tmax_28_30_lag3 + Tmax_28_30_lag4 + Tmax_28_30_lag5,
           Tmax_28_30_trim3 = Tmax_28_30_lag0 + Tmax_28_30_lag1 + Tmax_28_30_lag2,
           Tmax_28_30_trim_minus1 = Tmax_28_30_lag9 + Tmax_28_30_lag10 + Tmax_28_30_lag11,
           Tmax_28_30_trim_minus2 = Tmax_28_30_lag12 + Tmax_28_30_lag13 + Tmax_28_30_lag14
    ) %>%
    # TMAX 30 to 33
    mutate(Tmax_30_33_trim1 = Tmax_30_33_lag6 + Tmax_30_33_lag7 + Tmax_30_33_lag8,
           Tmax_30_33_trim2 = Tmax_30_33_lag3 + Tmax_30_33_lag4 + Tmax_30_33_lag5,
           Tmax_30_33_trim3 = Tmax_30_33_lag0 + Tmax_30_33_lag1 + Tmax_30_33_lag2,
           Tmax_30_33_trim_minus1 = Tmax_30_33_lag9 + Tmax_30_33_lag10 + Tmax_30_33_lag11,
           Tmax_30_33_trim_minus2 = Tmax_30_33_lag12 + Tmax_30_33_lag13 + Tmax_30_33_lag14
    ) %>% 
    # TMAX above 33
    mutate(Tmax_above_33_trim1 = Tmax_above_33_lag6 + Tmax_above_33_lag7 + Tmax_above_33_lag8,
           Tmax_above_33_trim2 = Tmax_above_33_lag3 + Tmax_above_33_lag4 + Tmax_above_33_lag5,
           Tmax_above_33_trim3 = Tmax_above_33_lag0 + Tmax_above_33_lag1 + Tmax_above_33_lag2,
           Tmax_above_33_trim_minus1 = Tmax_above_33_lag9 + Tmax_above_33_lag10 + Tmax_above_33_lag11,
           Tmax_above_33_trim_minus2 = Tmax_above_33_lag12 + Tmax_above_33_lag13 + Tmax_above_33_lag14
    ) 
  
  
  
  
  ## Generate TMEAN bins for each trimester
  df5 <- df5 %>% 
    # Tmean below 5
    mutate(Tmean_below_5_trim1 = Tmean_below_5_lag6 + Tmean_below_5_lag7 + Tmean_below_5_lag8,
           Tmean_below_5_trim2 = Tmean_below_5_lag3 + Tmean_below_5_lag4 + Tmean_below_5_lag5,
           Tmean_below_5_trim3 = Tmean_below_5_lag0 + Tmean_below_5_lag1 + Tmean_below_5_lag2,
           Tmean_below_5_trim_minus1 = Tmean_below_5_lag9 + Tmean_below_5_lag10 + Tmean_below_5_lag11,
           Tmean_below_5_trim_minus2 = Tmean_below_5_lag12 + Tmean_below_5_lag13 + Tmean_below_5_lag14
    ) %>% 
    # Tmean 5 to 10
    mutate(Tmean_5_10_trim1 = Tmean_5_10_lag6 + Tmean_5_10_lag7 + Tmean_5_10_lag8,
           Tmean_5_10_trim2 = Tmean_5_10_lag3 + Tmean_5_10_lag4 + Tmean_5_10_lag5,
           Tmean_5_10_trim3 = Tmean_5_10_lag0 + Tmean_5_10_lag1 + Tmean_5_10_lag2,
           Tmean_5_10_trim_minus1 = Tmean_5_10_lag9 + Tmean_5_10_lag10 + Tmean_5_10_lag11,
           Tmean_5_10_trim_minus2 = Tmean_5_10_lag12 + Tmean_5_10_lag13 + Tmean_5_10_lag14
    ) %>% 
    # Tmean 10 to 15
    mutate(Tmean_10_15_trim1 = Tmean_10_15_lag6 + Tmean_10_15_lag7 + Tmean_10_15_lag8,
           Tmean_10_15_trim2 = Tmean_10_15_lag3 + Tmean_10_15_lag4 + Tmean_10_15_lag5,
           Tmean_10_15_trim3 = Tmean_10_15_lag0 + Tmean_10_15_lag1 + Tmean_10_15_lag2,
           Tmean_10_15_trim_minus1 = Tmean_10_15_lag9 + Tmean_10_15_lag10 + Tmean_10_15_lag11,
           Tmean_10_15_trim_minus2 = Tmean_10_15_lag12 + Tmean_10_15_lag13 + Tmean_10_15_lag14
    ) %>% 
    # Tmean 15 to 20
    mutate(Tmean_15_20_trim1 = Tmean_15_20_lag6 + Tmean_15_20_lag7 + Tmean_15_20_lag8,
           Tmean_15_20_trim2 = Tmean_15_20_lag3 + Tmean_15_20_lag4 + Tmean_15_20_lag5,
           Tmean_15_20_trim3 = Tmean_15_20_lag0 + Tmean_15_20_lag1 + Tmean_15_20_lag2,
           Tmean_15_20_trim_minus1 = Tmean_15_20_lag9 + Tmean_15_20_lag10 + Tmean_15_20_lag11,
           Tmean_15_20_trim_minus2 = Tmean_15_20_lag12 + Tmean_15_20_lag13 + Tmean_15_20_lag14
    ) %>% 
    # Tmean 20 to 23
    mutate(Tmean_20_23_trim1 = Tmean_20_23_lag6 + Tmean_20_23_lag7 + Tmean_20_23_lag8,
           Tmean_20_23_trim2 = Tmean_20_23_lag3 + Tmean_20_23_lag4 + Tmean_20_23_lag5,
           Tmean_20_23_trim3 = Tmean_20_23_lag0 + Tmean_20_23_lag1 + Tmean_20_23_lag2,
           Tmean_20_23_trim_minus1 = Tmean_20_23_lag9 + Tmean_20_23_lag10 + Tmean_20_23_lag11,
           Tmean_20_23_trim_minus2 = Tmean_20_23_lag12 + Tmean_20_23_lag13 + Tmean_20_23_lag14
    ) %>% 
    # Tmean 23 to 25
    mutate(Tmean_23_25_trim1 = Tmean_23_25_lag6 + Tmean_23_25_lag7 + Tmean_23_25_lag8,
           Tmean_23_25_trim2 = Tmean_23_25_lag3 + Tmean_23_25_lag4 + Tmean_23_25_lag5,
           Tmean_23_25_trim3 = Tmean_23_25_lag0 + Tmean_23_25_lag1 + Tmean_23_25_lag2,
           Tmean_23_25_trim_minus1 = Tmean_23_25_lag9 + Tmean_23_25_lag10 + Tmean_23_25_lag11,
           Tmean_23_25_trim_minus2 = Tmean_23_25_lag12 + Tmean_23_25_lag13 + Tmean_23_25_lag14
    ) %>% 
    # Tmean 25 to 28
    mutate(Tmean_25_28_trim1 = Tmean_25_28_lag6 + Tmean_25_28_lag7 + Tmean_25_28_lag8,
           Tmean_25_28_trim2 = Tmean_25_28_lag3 + Tmean_25_28_lag4 + Tmean_25_28_lag5,
           Tmean_25_28_trim3 = Tmean_25_28_lag0 + Tmean_25_28_lag1 + Tmean_25_28_lag2,
           Tmean_25_28_trim_minus1 = Tmean_25_28_lag9 + Tmean_25_28_lag10 + Tmean_25_28_lag11,
           Tmean_25_28_trim_minus2 = Tmean_25_28_lag12 + Tmean_25_28_lag13 + Tmean_25_28_lag14
    ) %>%
    # Tmean 28 to 30
    mutate(Tmean_28_30_trim1 = Tmean_28_30_lag6 + Tmean_28_30_lag7 + Tmean_28_30_lag8,
           Tmean_28_30_trim2 = Tmean_28_30_lag3 + Tmean_28_30_lag4 + Tmean_28_30_lag5,
           Tmean_28_30_trim3 = Tmean_28_30_lag0 + Tmean_28_30_lag1 + Tmean_28_30_lag2,
           Tmean_28_30_trim_minus1 = Tmean_28_30_lag9 + Tmean_28_30_lag10 + Tmean_28_30_lag11,
           Tmean_28_30_trim_minus2 = Tmean_28_30_lag12 + Tmean_28_30_lag13 + Tmean_28_30_lag14
    ) %>%
    # Tmean 30 to 33
    mutate(Tmean_30_33_trim1 = Tmean_30_33_lag6 + Tmean_30_33_lag7 + Tmean_30_33_lag8,
           Tmean_30_33_trim2 = Tmean_30_33_lag3 + Tmean_30_33_lag4 + Tmean_30_33_lag5,
           Tmean_30_33_trim3 = Tmean_30_33_lag0 + Tmean_30_33_lag1 + Tmean_30_33_lag2,
           Tmean_30_33_trim_minus1 = Tmean_30_33_lag9 + Tmean_30_33_lag10 + Tmean_30_33_lag11,
           Tmean_30_33_trim_minus2 = Tmean_30_33_lag12 + Tmean_30_33_lag13 + Tmean_30_33_lag14
    ) %>% 
    # Tmean above 33
    mutate(Tmean_above_33_trim1 = Tmean_above_33_lag6 + Tmean_above_33_lag7 + Tmean_above_33_lag8,
           Tmean_above_33_trim2 = Tmean_above_33_lag3 + Tmean_above_33_lag4 + Tmean_above_33_lag5,
           Tmean_above_33_trim3 = Tmean_above_33_lag0 + Tmean_above_33_lag1 + Tmean_above_33_lag2,
           Tmean_above_33_trim_minus1 = Tmean_above_33_lag9 + Tmean_above_33_lag10 + Tmean_above_33_lag11,
           Tmean_above_33_trim_minus2 = Tmean_above_33_lag12 + Tmean_above_33_lag13 + Tmean_above_33_lag14
    ) 
  
  
  
  ## Generate Tmax percentile bins for each trimester
  df5 <- df5 %>% 
    # TMAX pct 0 to 10
    mutate(Tmax_pc_0_10_trim1 = Tmax_pc_0_10_lag6 + Tmax_pc_0_10_lag7 + Tmax_pc_0_10_lag8,
           Tmax_pc_0_10_trim2 = Tmax_pc_0_10_lag3 + Tmax_pc_0_10_lag4 + Tmax_pc_0_10_lag5,
           Tmax_pc_0_10_trim3 = Tmax_pc_0_10_lag0 + Tmax_pc_0_10_lag1 + Tmax_pc_0_10_lag2,
           Tmax_pc_0_10_trim_minus1 = Tmax_pc_0_10_lag9 + Tmax_pc_0_10_lag10 + Tmax_pc_0_10_lag11,
           Tmax_pc_0_10_trim_minus2 = Tmax_pc_0_10_lag12 + Tmax_pc_0_10_lag13 + Tmax_pc_0_10_lag14
    ) %>% 
    # TMAX pct 10 to 20
    mutate(Tmax_pc_10_20_trim1 = Tmax_pc_10_20_lag6 + Tmax_pc_10_20_lag7 + Tmax_pc_10_20_lag8,
           Tmax_pc_10_20_trim2 = Tmax_pc_10_20_lag3 + Tmax_pc_10_20_lag4 + Tmax_pc_10_20_lag5,
           Tmax_pc_10_20_trim3 = Tmax_pc_10_20_lag0 + Tmax_pc_10_20_lag1 + Tmax_pc_10_20_lag2,
           Tmax_pc_10_20_trim_minus1 = Tmax_pc_10_20_lag9 + Tmax_pc_10_20_lag10 + Tmax_pc_10_20_lag11,
           Tmax_pc_10_20_trim_minus2 = Tmax_pc_10_20_lag12 + Tmax_pc_10_20_lag13 + Tmax_pc_10_20_lag14
    ) %>% 
    # TMAX pct 20 to 30
    mutate(Tmax_pc_20_30_trim1 = Tmax_pc_20_30_lag6 + Tmax_pc_20_30_lag7 + Tmax_pc_20_30_lag8,
           Tmax_pc_20_30_trim2 = Tmax_pc_20_30_lag3 + Tmax_pc_20_30_lag4 + Tmax_pc_20_30_lag5,
           Tmax_pc_20_30_trim3 = Tmax_pc_20_30_lag0 + Tmax_pc_20_30_lag1 + Tmax_pc_20_30_lag2,
           Tmax_pc_20_30_trim_minus1 = Tmax_pc_20_30_lag9 + Tmax_pc_20_30_lag10 + Tmax_pc_20_30_lag11,
           Tmax_pc_20_30_trim_minus2 = Tmax_pc_20_30_lag12 + Tmax_pc_20_30_lag13 + Tmax_pc_20_30_lag14
    ) %>% 
    # TMAX pct 30 to 40
    mutate(Tmax_pc_30_40_trim1 = Tmax_pc_30_40_lag6 + Tmax_pc_30_40_lag7 + Tmax_pc_30_40_lag8,
           Tmax_pc_30_40_trim2 = Tmax_pc_30_40_lag3 + Tmax_pc_30_40_lag4 + Tmax_pc_30_40_lag5,
           Tmax_pc_30_40_trim3 = Tmax_pc_30_40_lag0 + Tmax_pc_30_40_lag1 + Tmax_pc_30_40_lag2,
           Tmax_pc_30_40_trim_minus1 = Tmax_pc_30_40_lag9 + Tmax_pc_30_40_lag10 + Tmax_pc_30_40_lag11,
           Tmax_pc_30_40_trim_minus2 = Tmax_pc_30_40_lag12 + Tmax_pc_30_40_lag13 + Tmax_pc_30_40_lag14
    ) %>% 
    # TMAX pct 40 to 50
    mutate(Tmax_pc_40_50_trim1 = Tmax_pc_40_50_lag6 + Tmax_pc_40_50_lag7 + Tmax_pc_40_50_lag8,
           Tmax_pc_40_50_trim2 = Tmax_pc_40_50_lag3 + Tmax_pc_40_50_lag4 + Tmax_pc_40_50_lag5,
           Tmax_pc_40_50_trim3 = Tmax_pc_40_50_lag0 + Tmax_pc_40_50_lag1 + Tmax_pc_40_50_lag2,
           Tmax_pc_40_50_trim_minus1 = Tmax_pc_40_50_lag9 + Tmax_pc_40_50_lag10 + Tmax_pc_40_50_lag11,
           Tmax_pc_40_50_trim_minus2 = Tmax_pc_40_50_lag12 + Tmax_pc_40_50_lag13 + Tmax_pc_40_50_lag14
    ) %>% 
    # TMAX pct 50 to 60
    mutate(Tmax_pc_50_60_trim1 = Tmax_pc_50_60_lag6 + Tmax_pc_50_60_lag7 + Tmax_pc_50_60_lag8,
           Tmax_pc_50_60_trim2 = Tmax_pc_50_60_lag3 + Tmax_pc_50_60_lag4 + Tmax_pc_50_60_lag5,
           Tmax_pc_50_60_trim3 = Tmax_pc_50_60_lag0 + Tmax_pc_50_60_lag1 + Tmax_pc_50_60_lag2,
           Tmax_pc_50_60_trim_minus1 = Tmax_pc_50_60_lag9 + Tmax_pc_50_60_lag10 +  Tmax_pc_50_60_lag11,
           Tmax_pc_50_60_trim_minus2 = Tmax_pc_50_60_lag12 + Tmax_pc_50_60_lag13 +  Tmax_pc_50_60_lag14
    ) %>% 
    # TMAX pct 60 to 70
    mutate(Tmax_pc_60_70_trim1 = Tmax_pc_60_70_lag6 + Tmax_pc_60_70_lag7 + Tmax_pc_60_70_lag8,
           Tmax_pc_60_70_trim2 = Tmax_pc_60_70_lag3 + Tmax_pc_60_70_lag4 + Tmax_pc_60_70_lag5,
           Tmax_pc_60_70_trim3 = Tmax_pc_60_70_lag0 + Tmax_pc_60_70_lag1 + Tmax_pc_60_70_lag2,
           Tmax_pc_60_70_trim_minus1 = Tmax_pc_60_70_lag9 + Tmax_pc_60_70_lag10 + Tmax_pc_60_70_lag11,
           Tmax_pc_60_70_trim_minus2 = Tmax_pc_60_70_lag12 + Tmax_pc_60_70_lag13 + Tmax_pc_60_70_lag14
    ) %>% 
    # TMAX pct 70 to 80
    mutate(Tmax_pc_70_80_trim1 = Tmax_pc_70_80_lag6 + Tmax_pc_70_80_lag7 + Tmax_pc_70_80_lag8,
           Tmax_pc_70_80_trim2 = Tmax_pc_70_80_lag3 + Tmax_pc_70_80_lag4 + Tmax_pc_70_80_lag5,
           Tmax_pc_70_80_trim3 = Tmax_pc_70_80_lag0 + Tmax_pc_70_80_lag1 + Tmax_pc_70_80_lag2,
           Tmax_pc_70_80_trim_minus1 = Tmax_pc_70_80_lag9 + Tmax_pc_70_80_lag10 + Tmax_pc_70_80_lag11,
           Tmax_pc_70_80_trim_minus2 = Tmax_pc_70_80_lag12 + Tmax_pc_70_80_lag13 + Tmax_pc_70_80_lag14
    ) %>% 
    # TMAX pct 80 to 90
    mutate(Tmax_pc_80_90_trim1 = Tmax_pc_80_90_lag6 + Tmax_pc_80_90_lag7 + Tmax_pc_80_90_lag8,
           Tmax_pc_80_90_trim2 = Tmax_pc_80_90_lag3 + Tmax_pc_80_90_lag4 + Tmax_pc_80_90_lag5,
           Tmax_pc_80_90_trim3 = Tmax_pc_80_90_lag0 + Tmax_pc_80_90_lag1 + Tmax_pc_80_90_lag2,
           Tmax_pc_80_90_trim_minus1 = Tmax_pc_80_90_lag9 + Tmax_pc_80_90_lag10 + Tmax_pc_80_90_lag11,
           Tmax_pc_80_90_trim_minus2 = Tmax_pc_80_90_lag12 + Tmax_pc_80_90_lag13 + Tmax_pc_80_90_lag14
    ) %>%     
    # TMAX pct 90 to 100
    mutate(Tmax_pc_90_100_trim1 = Tmax_pc_90_100_lag6 + Tmax_pc_90_100_lag7 + Tmax_pc_90_100_lag8,
           Tmax_pc_90_100_trim2 = Tmax_pc_90_100_lag3 + Tmax_pc_90_100_lag4 + Tmax_pc_90_100_lag5,
           Tmax_pc_90_100_trim3 = Tmax_pc_90_100_lag0 + Tmax_pc_90_100_lag1 + Tmax_pc_90_100_lag2,
           Tmax_pc_90_100_trim_minus1 = Tmax_pc_90_100_lag9 + Tmax_pc_90_100_lag10 + Tmax_pc_90_100_lag11,
           Tmax_pc_90_100_trim_minus2 = Tmax_pc_90_100_lag12 + Tmax_pc_90_100_lag13 + Tmax_pc_90_100_lag14
    ) 
  
  
  
  ## Generate Tmean percentile bins for each trimester
  df5 <- df5 %>% 
    # Tmean pct 0 to 10
    mutate(Tmean_pc_0_10_trim1 = Tmean_pc_0_10_lag6 + Tmean_pc_0_10_lag7 + Tmean_pc_0_10_lag8,
           Tmean_pc_0_10_trim2 = Tmean_pc_0_10_lag3 + Tmean_pc_0_10_lag4 + Tmean_pc_0_10_lag5,
           Tmean_pc_0_10_trim3 = Tmean_pc_0_10_lag0 + Tmean_pc_0_10_lag1 + Tmean_pc_0_10_lag2,
           Tmean_pc_0_10_trim_minus1 = Tmean_pc_0_10_lag9 + Tmean_pc_0_10_lag10 + Tmean_pc_0_10_lag11,
           Tmean_pc_0_10_trim_minus2 = Tmean_pc_0_10_lag12 + Tmean_pc_0_10_lag13 + Tmean_pc_0_10_lag14
    ) %>% 
    # Tmean pct 10 to 20
    mutate(Tmean_pc_10_20_trim1 = Tmean_pc_10_20_lag6 + Tmean_pc_10_20_lag7 + Tmean_pc_10_20_lag8,
           Tmean_pc_10_20_trim2 = Tmean_pc_10_20_lag3 + Tmean_pc_10_20_lag4 + Tmean_pc_10_20_lag5,
           Tmean_pc_10_20_trim3 = Tmean_pc_10_20_lag0 + Tmean_pc_10_20_lag1 + Tmean_pc_10_20_lag2,
           Tmean_pc_10_20_trim_minus1 = Tmean_pc_10_20_lag9 + Tmean_pc_10_20_lag10 + Tmean_pc_10_20_lag11,
           Tmean_pc_10_20_trim_minus2 = Tmean_pc_10_20_lag12 + Tmean_pc_10_20_lag13 + Tmean_pc_10_20_lag14
    ) %>% 
    # Tmean pct 20 to 30
    mutate(Tmean_pc_20_30_trim1 = Tmean_pc_20_30_lag6 + Tmean_pc_20_30_lag7 + Tmean_pc_20_30_lag8,
           Tmean_pc_20_30_trim2 = Tmean_pc_20_30_lag3 + Tmean_pc_20_30_lag4 + Tmean_pc_20_30_lag5,
           Tmean_pc_20_30_trim3 = Tmean_pc_20_30_lag0 + Tmean_pc_20_30_lag1 + Tmean_pc_20_30_lag2,
           Tmean_pc_20_30_trim_minus1 = Tmean_pc_20_30_lag9 + Tmean_pc_20_30_lag10 + Tmean_pc_20_30_lag11,
           Tmean_pc_20_30_trim_minus2 = Tmean_pc_20_30_lag12 + Tmean_pc_20_30_lag13 + Tmean_pc_20_30_lag14
    ) %>% 
    # Tmean pct 30 to 40
    mutate(Tmean_pc_30_40_trim1 = Tmean_pc_30_40_lag6 + Tmean_pc_30_40_lag7 + Tmean_pc_30_40_lag8,
           Tmean_pc_30_40_trim2 = Tmean_pc_30_40_lag3 + Tmean_pc_30_40_lag4 + Tmean_pc_30_40_lag5,
           Tmean_pc_30_40_trim3 = Tmean_pc_30_40_lag0 + Tmean_pc_30_40_lag1 + Tmean_pc_30_40_lag2,
           Tmean_pc_30_40_trim_minus1 = Tmean_pc_30_40_lag9 + Tmean_pc_30_40_lag10 + Tmean_pc_30_40_lag11,
           Tmean_pc_30_40_trim_minus2 = Tmean_pc_30_40_lag12 + Tmean_pc_30_40_lag13 + Tmean_pc_30_40_lag14
    ) %>% 
    # Tmean pct 40 to 50
    mutate(Tmean_pc_40_50_trim1 = Tmean_pc_40_50_lag6 + Tmean_pc_40_50_lag7 + Tmean_pc_40_50_lag8,
           Tmean_pc_40_50_trim2 = Tmean_pc_40_50_lag3 + Tmean_pc_40_50_lag4 + Tmean_pc_40_50_lag5,
           Tmean_pc_40_50_trim3 = Tmean_pc_40_50_lag0 + Tmean_pc_40_50_lag1 + Tmean_pc_40_50_lag2,
           Tmean_pc_40_50_trim_minus1 = Tmean_pc_40_50_lag9 + Tmean_pc_40_50_lag10 + Tmean_pc_40_50_lag11,
           Tmean_pc_40_50_trim_minus2 = Tmean_pc_40_50_lag12 + Tmean_pc_40_50_lag13 + Tmean_pc_40_50_lag14
    ) %>% 
    # Tmean pct 50 to 60
    mutate(Tmean_pc_50_60_trim1 = Tmean_pc_50_60_lag6 + Tmean_pc_50_60_lag7 + Tmean_pc_50_60_lag8,
           Tmean_pc_50_60_trim2 = Tmean_pc_50_60_lag3 + Tmean_pc_50_60_lag4 + Tmean_pc_50_60_lag5,
           Tmean_pc_50_60_trim3 = Tmean_pc_50_60_lag0 + Tmean_pc_50_60_lag1 + Tmean_pc_50_60_lag2,
           Tmean_pc_50_60_trim_minus1 = Tmean_pc_50_60_lag9 + Tmean_pc_50_60_lag10 +  Tmean_pc_50_60_lag11,
           Tmean_pc_50_60_trim_minus2 = Tmean_pc_50_60_lag12 + Tmean_pc_50_60_lag13 +  Tmean_pc_50_60_lag14
    ) %>% 
    # Tmean pct 60 to 70
    mutate(Tmean_pc_60_70_trim1 = Tmean_pc_60_70_lag6 + Tmean_pc_60_70_lag7 + Tmean_pc_60_70_lag8,
           Tmean_pc_60_70_trim2 = Tmean_pc_60_70_lag3 + Tmean_pc_60_70_lag4 + Tmean_pc_60_70_lag5,
           Tmean_pc_60_70_trim3 = Tmean_pc_60_70_lag0 + Tmean_pc_60_70_lag1 + Tmean_pc_60_70_lag2,
           Tmean_pc_60_70_trim_minus1 = Tmean_pc_60_70_lag9 + Tmean_pc_60_70_lag10 + Tmean_pc_60_70_lag11,
           Tmean_pc_60_70_trim_minus2 = Tmean_pc_60_70_lag12 + Tmean_pc_60_70_lag13 + Tmean_pc_60_70_lag14
    ) %>% 
    # Tmean pct 70 to 80
    mutate(Tmean_pc_70_80_trim1 = Tmean_pc_70_80_lag6 + Tmean_pc_70_80_lag7 + Tmean_pc_70_80_lag8,
           Tmean_pc_70_80_trim2 = Tmean_pc_70_80_lag3 + Tmean_pc_70_80_lag4 + Tmean_pc_70_80_lag5,
           Tmean_pc_70_80_trim3 = Tmean_pc_70_80_lag0 + Tmean_pc_70_80_lag1 + Tmean_pc_70_80_lag2,
           Tmean_pc_70_80_trim_minus1 = Tmean_pc_70_80_lag9 + Tmean_pc_70_80_lag10 + Tmean_pc_70_80_lag11,
           Tmean_pc_70_80_trim_minus2 = Tmean_pc_70_80_lag12 + Tmean_pc_70_80_lag13 + Tmean_pc_70_80_lag14
    ) %>% 
    # Tmean pct 80 to 90
    mutate(Tmean_pc_80_90_trim1 = Tmean_pc_80_90_lag6 + Tmean_pc_80_90_lag7 + Tmean_pc_80_90_lag8,
           Tmean_pc_80_90_trim2 = Tmean_pc_80_90_lag3 + Tmean_pc_80_90_lag4 + Tmean_pc_80_90_lag5,
           Tmean_pc_80_90_trim3 = Tmean_pc_80_90_lag0 + Tmean_pc_80_90_lag1 + Tmean_pc_80_90_lag2,
           Tmean_pc_80_90_trim_minus1 = Tmean_pc_80_90_lag9 + Tmean_pc_80_90_lag10 + Tmean_pc_80_90_lag11,
           Tmean_pc_80_90_trim_minus2 = Tmean_pc_80_90_lag12 + Tmean_pc_80_90_lag13 + Tmean_pc_80_90_lag14
    ) %>%     
    # Tmean pct 90 to 100
    mutate(Tmean_pc_90_100_trim1 = Tmean_pc_90_100_lag6 + Tmean_pc_90_100_lag7 + Tmean_pc_90_100_lag8,
           Tmean_pc_90_100_trim2 = Tmean_pc_90_100_lag3 + Tmean_pc_90_100_lag4 + Tmean_pc_90_100_lag5,
           Tmean_pc_90_100_trim3 = Tmean_pc_90_100_lag0 + Tmean_pc_90_100_lag1 + Tmean_pc_90_100_lag2,
           Tmean_pc_90_100_trim_minus1 = Tmean_pc_90_100_lag9 + Tmean_pc_90_100_lag10 + Tmean_pc_90_100_lag11,
           Tmean_pc_90_100_trim_minus2 = Tmean_pc_90_100_lag12 + Tmean_pc_90_100_lag13 + Tmean_pc_90_100_lag14
    ) 
  
  
  ## Generate Tmax percentile thresholds bins for each trimester
  df5 <- df5 %>% 
    # Tmax pct above 75
    mutate(Tmax_above_pc75_trim1 = Tmax_above_pc75_lag6 + Tmax_above_pc75_lag7 + Tmax_above_pc75_lag8,
           Tmax_above_pc75_trim2 = Tmax_above_pc75_lag3 + Tmax_above_pc75_lag4 + Tmax_above_pc75_lag5,
           Tmax_above_pc75_trim3 = Tmax_above_pc75_lag0 + Tmax_above_pc75_lag1 + Tmax_above_pc75_lag2,
           Tmax_above_pc75_trim_minus1 = Tmax_above_pc75_lag9 + Tmax_above_pc75_lag10 + Tmax_above_pc75_lag11,
           Tmax_above_pc75_trim_minus2 = Tmax_above_pc75_lag12 + Tmax_above_pc75_lag13 + Tmax_above_pc75_lag14
    ) %>% 
    # Tmax pct above 90
    mutate(Tmax_above_pc90_trim1 = Tmax_above_pc90_lag6 + Tmax_above_pc90_lag7 + Tmax_above_pc90_lag8,
           Tmax_above_pc90_trim2 = Tmax_above_pc90_lag3 + Tmax_above_pc90_lag4 + Tmax_above_pc90_lag5,
           Tmax_above_pc90_trim3 = Tmax_above_pc90_lag0 + Tmax_above_pc90_lag1 + Tmax_above_pc90_lag2,
           Tmax_above_pc90_trim_minus1 = Tmax_above_pc90_lag9 + Tmax_above_pc90_lag10 + Tmax_above_pc90_lag11,
           Tmax_above_pc90_trim_minus2 = Tmax_above_pc90_lag12 + Tmax_above_pc90_lag13 + Tmax_above_pc90_lag14
    ) %>% 
    # Tmax pct above 95
    mutate(Tmax_above_pc95_trim1 = Tmax_above_pc95_lag6 + Tmax_above_pc95_lag7 + Tmax_above_pc95_lag8,
           Tmax_above_pc95_trim2 = Tmax_above_pc95_lag3 + Tmax_above_pc95_lag4 + Tmax_above_pc95_lag5,
           Tmax_above_pc95_trim3 = Tmax_above_pc95_lag0 + Tmax_above_pc95_lag1 + Tmax_above_pc95_lag2,
           Tmax_above_pc95_trim_minus1 = Tmax_above_pc95_lag9 + Tmax_above_pc95_lag10 + Tmax_above_pc95_lag11,
           Tmax_above_pc95_trim_minus2 = Tmax_above_pc95_lag12 + Tmax_above_pc95_lag13 + Tmax_above_pc95_lag14
    ) %>% 
    # Tmax pct above 98
    mutate(Tmax_above_pc98_trim1 = Tmax_above_pc98_lag6 + Tmax_above_pc98_lag7 + Tmax_above_pc98_lag8,
           Tmax_above_pc98_trim2 = Tmax_above_pc98_lag3 + Tmax_above_pc98_lag4 + Tmax_above_pc98_lag5,
           Tmax_above_pc98_trim3 = Tmax_above_pc98_lag0 + Tmax_above_pc98_lag1 + Tmax_above_pc98_lag2,
           Tmax_above_pc98_trim_minus1 = Tmax_above_pc98_lag9 + Tmax_above_pc98_lag10 + Tmax_above_pc98_lag11,
           Tmax_above_pc98_trim_minus2 = Tmax_above_pc98_lag12 + Tmax_above_pc98_lag13 + Tmax_above_pc98_lag14
    )
  
  
  ## Generate Tmean percentile thresholds bins for each trimester
  df5 <- df5 %>% 
    # Tmean pct above 75
    mutate(Tmean_above_pc75_trim1 = Tmean_above_pc75_lag6 + Tmean_above_pc75_lag7 + Tmean_above_pc75_lag8,
           Tmean_above_pc75_trim2 = Tmean_above_pc75_lag3 + Tmean_above_pc75_lag4 + Tmean_above_pc75_lag5,
           Tmean_above_pc75_trim3 = Tmean_above_pc75_lag0 + Tmean_above_pc75_lag1 + Tmean_above_pc75_lag2,
           Tmean_above_pc75_trim_minus1 = Tmean_above_pc75_lag9 + Tmean_above_pc75_lag10 + Tmean_above_pc75_lag11,
           Tmean_above_pc75_trim_minus2 = Tmean_above_pc75_lag12 + Tmean_above_pc75_lag13 + Tmean_above_pc75_lag14
    ) %>% 
    # Tmean pct above 90
    mutate(Tmean_above_pc90_trim1 = Tmean_above_pc90_lag6 + Tmean_above_pc90_lag7 + Tmean_above_pc90_lag8,
           Tmean_above_pc90_trim2 = Tmean_above_pc90_lag3 + Tmean_above_pc90_lag4 + Tmean_above_pc90_lag5,
           Tmean_above_pc90_trim3 = Tmean_above_pc90_lag0 + Tmean_above_pc90_lag1 + Tmean_above_pc90_lag2,
           Tmean_above_pc90_trim_minus1 = Tmean_above_pc90_lag9 + Tmean_above_pc90_lag10 + Tmean_above_pc90_lag11,
           Tmean_above_pc90_trim_minus2 = Tmean_above_pc90_lag12 + Tmean_above_pc90_lag13 + Tmean_above_pc90_lag14
    ) %>% 
    # Tmean pct above 95
    mutate(Tmean_above_pc95_trim1 = Tmean_above_pc95_lag6 + Tmean_above_pc95_lag7 + Tmean_above_pc95_lag8,
           Tmean_above_pc95_trim2 = Tmean_above_pc95_lag3 + Tmean_above_pc95_lag4 + Tmean_above_pc95_lag5,
           Tmean_above_pc95_trim3 = Tmean_above_pc95_lag0 + Tmean_above_pc95_lag1 + Tmean_above_pc95_lag2,
           Tmean_above_pc95_trim_minus1 = Tmean_above_pc95_lag9 + Tmean_above_pc95_lag10 + Tmean_above_pc95_lag11,
           Tmean_above_pc95_trim_minus2 = Tmean_above_pc95_lag12 + Tmean_above_pc95_lag13 + Tmean_above_pc95_lag14
    ) %>% 
    # Tmean pct above 98
    mutate(Tmean_above_pc98_trim1 = Tmean_above_pc98_lag6 + Tmean_above_pc98_lag7 + Tmean_above_pc98_lag8,
           Tmean_above_pc98_trim2 = Tmean_above_pc98_lag3 + Tmean_above_pc98_lag4 + Tmean_above_pc98_lag5,
           Tmean_above_pc98_trim3 = Tmean_above_pc98_lag0 + Tmean_above_pc98_lag1 + Tmean_above_pc98_lag2,
           Tmean_above_pc98_trim_minus1 = Tmean_above_pc98_lag9 + Tmean_above_pc98_lag10 + Tmean_above_pc98_lag11,
           Tmean_above_pc98_trim_minus2 = Tmean_above_pc98_lag12 + Tmean_above_pc98_lag13 + Tmean_above_pc98_lag14
    )
  
  
  ## Generate pre for each trimester
  df5 <- df5 %>% 
    mutate(pre_trim1 = pre_lag6 + pre_lag7 + pre_lag8,
           pre_trim2 = pre_lag3 + pre_lag4 + pre_lag5,
           pre_trim3 = pre_lag0 + pre_lag1 + pre_lag2,
           pre_trim_minus1 = pre_lag9 + pre_lag10 + pre_lag11,
           pre_trim_minus2 = pre_lag12 + pre_lag13 + pre_lag14
    ) 
  
  
  
  
  #df5 <- df5 %>% filter_at(vars(c(Tmean_below_5_lag0:pre_lag15)), all_vars(!is.na(.)))
  
  write.csv(df5, paste("./WBGT_data_for_analysis_India/", a, ".csv", sep=""))  
}
