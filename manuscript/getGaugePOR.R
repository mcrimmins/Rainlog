# get stats for each gauge (por, missing....)
# MAC 02/14/2019

# load libaries
library(RCurl)
library(jsonlite)

# set date ranges
dateRangeStart="1950-01-01" #1950-01-01
dateRangeEnd="2018-12-31"   #2018-12-31
allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)

# get gauges
# while statement to loop through gauges
limit<-1000
i<-0
done<-0

while (done==0) {
  jsonQuery=paste0('{"dateRangeStart":"',dateRangeStart,'","dateRangeEnd":"',dateRangeEnd,'","region":{"type":"Circle","center":{"lat":39.828165,"lng":-98.579480},"radius":2000.0},"pagination":{"offset":',i,',"limit":',limit,'}}')
  out<-postForm("https://rainlog.org/api/1.0/GaugeRevision/getFiltered", 
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

# get rid of problem gauges
gaugeStack <- gaugeStack[-which(gaugeStack$gaugeRevisionId==7480), ]

#### --- end get gauges


# GET DATA for Gauge
# loop through k that points to gaugesIDs
#k=500
for (k in 1:nrow(gaugeStack)) {
    # while statement to loop through pages
    limit<-1000
    i<-0
    j<-1
    done<-0
    dataStack = list()
    
    while (done==0) {
       jsonQuery=paste0('{"dateRangeStart":"',dateRangeStart,'","dateRangeEnd":"',dateRangeEnd,'","gaugeRevisionIds":[',
                       gaugeStack$gaugeRevisionId[k],'],"pagination":{"offset":',i,',"limit":',limit,'}}')
       out<-postForm("https://rainlog.org/api/1.0/Reading/getFiltered", 
                    .opts = list(postfields = jsonQuery, 
                                 httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
      out<-fromJSON(out)
      if(exists("out")==TRUE){
        if (length(out)==0){
          done<-1
          break
        }else{}    
        dataStack[[j]] <- out # add it to your list
      }else{
        break
        done<-1
      }
      
      i <-i+limit
      j <-j+1
      rm(out)
      #print(i)
    }

  # calculate gauge stats  
  gaugeData = do.call(rbind, dataStack)
  
  if(length(gaugeData)==0){
    gaugeStack$minDate[k]<-NA
    gaugeStack$maxDate[k]<-NA
    gaugeStack$numObs[k]<-NA
    gaugeStack$numZeros[k]<-NA
    gaugeStack$traceObs[k]<-NA
    gaugeStack$maxRain[k]<-NA
    gaugeStack$numLTtenth[k]<-NA
    gaugeStack$snowDepthObs[k]<-NA
    gaugeStack$snowAccumObs[k]<-NA
    gaugeStack$numRemarks[k]<-NA
    gaugeStack$qualGood[k]<-NA
    gaugeStack$qualTrace[k]<-NA
    gaugeStack$qualPoor[k]<-NA
    gaugeStack$qualSnow[k]<-NA
    gaugeStack$qualLost[k]<-NA
    gaugeStack$qualAbsent[k]<-NA
    gaugeStack$maxHour[k]<-NA
    gaugeStack$minHour[k]<-NA
    gaugeStack$avgHour[k]<-NA
    gaugeStack$maxMin[k]<-NA
    gaugeStack$minMin[k]<-NA
  }
  else{
    gaugeStack$minDate[k]<-as.character(min(as.Date(gaugeData$readingDate)))
    gaugeStack$maxDate[k]<-as.character(max(as.Date(gaugeData$readingDate)))
    gaugeStack$numObs[k]<-nrow(gaugeData)
    gaugeStack$numZeros[k]<-sum(gaugeData$rainAmount==0, na.rm=TRUE)
    gaugeStack$traceObs[k]<-sum(is.na(gaugeData$rainAmount))
    gaugeStack$maxRain[k]<-max(gaugeData$rainAmount, na.rm=TRUE)
    gaugeStack$numLTtenth[k]<-sum(gaugeData$rainAmount<=0.1, na.rm=TRUE)
    gaugeStack$snowDepthObs[k]<-sum(!is.na(gaugeData$snowDepth))
    gaugeStack$snowAccumObs[k]<-sum(!is.na(gaugeData$snowAccumulation))
    gaugeStack$numRemarks[k]<-sum(!is.na(gaugeData$remarks))
    gaugeStack$qualGood[k]<-length(which(gaugeData$quality=="Good"))
    gaugeStack$qualTrace[k]<-length(which(gaugeData$quality=="Trace"))
    gaugeStack$qualPoor[k]<-length(which(gaugeData$quality=="Poor"))
    gaugeStack$qualSnow[k]<-length(which(gaugeData$quality=="Snow"))
    gaugeStack$qualLost[k]<-length(which(gaugeData$quality=="Lost"))
    gaugeStack$qualAbsent[k]<-length(which(gaugeData$quality=="Absent"))
    gaugeStack$maxHour[k]<-max(gaugeData$readingHour, na.rm=TRUE)
    gaugeStack$minHour[k]<-min(gaugeData$readingHour, na.rm=TRUE)
    gaugeStack$avgHour[k]<-mean(gaugeData$readingHour, na.rm=TRUE)
    gaugeStack$maxMin[k]<-max(gaugeData$readingMinute, na.rm=TRUE)
    gaugeStack$minMin[k]<-min(gaugeData$readingMinute, na.rm=TRUE)
  }
 #gaugeStack$minDate[k]<-min(as.Date(gaugeData$readingDate))
  # max(as.Date(gaugeData$readingDate))
  # nrow(gaugeData)
  # sum(is.na(gaugeData$rainAmount))
  # sum(!is.na(gaugeData$snowDepth))
  # sum(!is.na(gaugeData$snowAccumulation))
  # sum(!is.na(gaugeData$remarks))
  print(paste0("Gauge: ",k,", ",round((k/nrow(gaugeStack))*100),"% complete"))
}

# PushBullet
source('notifyGaugePOR.R')

rm(gaugeData,out)

## ---- location reverse geocode
load("~/RProjects/RainlogAPI/gaugeStackData.RData")

 library(revgeo)
 location<-revgeo(gaugeStack$position.lng, gaugeStack$position.lat, output = 'frame')
 source('notifyGaugePOR.R')

## -----


save.image("~/RProjects/RainlogAPI/gaugeStackData.RData")
# gaugeStack$minDate<-as.Date(gaugeStack$minDate, format="%Y-%m-%d")