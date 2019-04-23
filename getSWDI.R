# trying out RNOAA getting severe weather reports...
# MAC 4/8/2019

library(rnoaa)
library(ggmap)

# API key
source('APIkey.R')

# GET ACIS Stations ----
# get map and bounding box
where<-geocode("tucson", source = "dsk")
TucsonMap <- qmap(location = c(lon = where[1,1], lat = where[1,2]), zoom = 9,
                  color = "bw") # zoom-10 for Tucson
# RCC ACIS bbox - Bounding box specified in decimal degrees (W,S,E,N) (e.g. [-90, 40, -88, 41])
bbox<-paste0(min(TucsonMap$data$lon),',',min(TucsonMap$data$lat),',',max(TucsonMap$data$lon),',',max(TucsonMap$data$lat))


# Get all 'plsr' within the bounding box (-91,30,-90,31)
test<-swdi(dataset='plsr', startdate='20060505', enddate='20060510',
     bbox=c(min(TucsonMap$data$lon),min(TucsonMap$data$lat),max(TucsonMap$data$lon),max(TucsonMap$data$lat)))

test<-swdi(dataset='plsr', startdate='20060701', enddate='20061001', limit=100,
     bbox=c(min(TucsonMap$data$lon),min(TucsonMap$data$lat),max(TucsonMap$data$lon),max(TucsonMap$data$lat)))

# use API directly...https://www.ncdc.noaa.gov/swdi/#Intro
#https://www.ncdc.noaa.gov/swdiws/csv/plsr/20070101:20071231?bbox=-111,31,-110,32