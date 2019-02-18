# Get stats of gauges in Rainlog Network
# MAC 2/13/19
# adapted from ggPlotRainlog.R

#library(plyr)
library(RCurl)
library(jsonlite)
#library(spacetime)
library(ggplot2)
library(ggmap)
library(scales)

# API key
source('APIkey.R')

# set date ranges
dateRangeStart="1980-01-01"
dateRangeEnd="2019-02-12"
allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)

# specify center and radius for search area
# get gauges
# while statement to loop through gauges
limit<-1000
i<-0
done<-0

while (done==0) {
  jsonQuery=paste0('{"dateRangeStart":"',dateRangeStart,'","dateRangeEnd":"',dateRangeEnd,'","pagination":{"offset":',i,',"limit":',limit,'}}')
  out<-postForm("https://rainlog.org/api/1.0/GaugeRevision/getFiltered", 
                .opts = list(postfields = jsonQuery, 
                             httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
  out<-fromJSON(out)
  if(exists("out")==TRUE){
    if (i==0){
      gaugeStackRev<-flatten(out)
    }else{
      gaugeStackRev<-rbind(gaugeStackRev, flatten(out))
    }
  }else{
    break
    done<-1
  }
  
  i <-i+limit
  rm(out)
  print(i)
}

# get original gauges
limit<-1000
i<-0
done<-0

while (done==0) {
  jsonQuery=paste0('{"dateRangeStart":"',dateRangeStart,'","dateRangeEnd":"',dateRangeEnd,'","pagination":{"offset":',i,',"limit":',limit,'}}')
  out<-postForm("https://rainlog.org/api/1.0/Gauge/getFiltered", 
                .opts = list(postfields = jsonQuery, 
                             httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
  out<-fromJSON(out)
  if(exists("out")==TRUE){
    if (i==0){
      gaugeStack<-flatten(out)
    }else{
      gaugeStack<-rbind(gaugeStack, flatten(out))
    }
  }else{
    break
    done<-1
  }
  
  i <-i+limit
  rm(out)
  print(i)
}

# join two tables
mergedGauges<-merge(gaugeStackRev, gaugeStack, by="gaugeId")


# map out gauge locations
#myLocation <- c(-125, 30, -100, 50)
myLocation <- c(-180, -90, 180, 90)
myMap <- get_map(location=myLocation,
                 source="google", crop=FALSE)
ggmap(myMap)

ggmap(myMap)+
  geom_point(aes(x = position.lng, y = position.lat), data = mergedGauges,
             alpha = .5, color="darkred", size = 0.5)

# look for unique gauges
length(unique(mergedGauges$gaugeRevisionId))
length(unique(mergedGauges$gaugeId))

# # reverse geocode
# library(revgeo)
# test<-gaugeStack[1:20,]
# location<-revgeo(test$position.lng, test$position.lat, output = 'frame')


