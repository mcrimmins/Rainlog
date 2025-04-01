# testing out Rainlog API - using ACIS JSON code
# GET ALL GAUGES
# MAC 6/13/18

#library(plyr)
library(RCurl)
library(jsonlite)

# Get gauges
# https://app.swaggerhub.com/apis/rainlog/rainlog/1.0.0#/GaugeRevision/post_GaugeRevision_getFiltered
# jsonQuery1='{"dateRangeStart":"2016-01-01","dateRangeEnd":"2018-05-01","region":{"type":"Circle","center":{"lat":32.221551,"lng":-110.909479},"radius":20.0},"pagination":{"offset":0,"limit":1000}}'
# 
# out<-postForm("https://rainlog.org/api/1.0/GaugeRevision/getFiltered", 
#               .opts = list(postfields = jsonQuery1, 
#                            httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
# 
# out<-fromJSON(out)
# get gauges
# while statement to loop through gauges from AZ center 34.454345, -111.443652
limit<-1000
i<-0
done<-0

while (done==0) {
  jsonQuery=paste0('{"dateRangeStart":"2023-01-01","dateRangeEnd":"2024-01-01","region":{"type":"Circle","center":{"lat":34.454345,"lng":-111.443652},"radius":200.0},"pagination":{"offset":',i,',"limit":',limit,'}}')
  out<-postForm("https://rainlog.org/api/1.0/GaugeRevision/getFiltered", 
                .opts = list(postfields = jsonQuery, 
                             httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
  out<-fromJSON(out)
  if(exists("out")==TRUE){
    if (i==0){
      gaugeMeta<-flatten(out)
    }else{
      gaugeMeta<-rbind(gaugeMeta, flatten(out))
    }
  }else{
    break
    done<-1
  }
  
  i <-i+limit
  rm(out)
  print(i)
}


# get data from single gauge or several gauges

limit<-1000
i<-0
done<-0

while (done==0) {
  jsonQuery=paste0('{"dateRangeStart":"2023-01-01","dateRangeEnd":"2024-01-01","pagination":{"offset":',i,',"limit":',limit,'}}')
  out<-postForm("https://rainlog.org/api/1.0/Reading/getFiltered", 
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

write.csv2(gaugeStack, file="Rainlog1185.csv")


#####

# join two tables
mergedGauges<-merge(gaugeStack, gaugeMeta, by="gaugeId")

gauges<-mergedGauges[!duplicated(mergedGauges[c('gaugeId')]), ]

gauges<-gauges[,c(12:20)]

write.csv(gauges, file="RainlogGauges2024.csv")

