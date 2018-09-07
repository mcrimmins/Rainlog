# testing out Rainlog API - using ACIS JSON code
# MAC 4/7/18

#library(plyr)
library(RCurl)
library(jsonlite)
#library(reshape)
#library(dplyr)
#library(tidyr)
#library(seas)

# download data from Rainlog API - from
# https://app.swaggerhub.com/apis/rainlog/rainlog/1.0.0#/Gauge/post_Gauge_getFiltered
jsonQuery='{"pagination":{"offset":0,"limit":1000}}'



out<-postForm("https://rainlog.org/api/1.0/Gauge/getFiltered", 
              .opts = list(postfields = jsonQuery, 
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))

out<-fromJSON(out)

# https://app.swaggerhub.com/apis/rainlog/rainlog/1.0.0#/Reading/post_Reading_getFiltered
#jsonQuery='{"readingIds":[0],"quality":["Good"],"pagination":{"offset":0,"limit":1000},"gaugeRevisionIds":[0],"dateRangeStart":"1999-02-25","dateRangeEnd":"1999-02-25","region":{"type":"Circle","center":{"lat":32.23,"lng":-110.87},"radius":10.0},"gaugeType":["TruCheckWedge"],"gaugeIds":[0],"userIds":[0]}'
jsonQuery='{"quality":["Good"],"pagination":{"offset":0,"limit":1001},"dateRangeStart":"2016-01-01","dateRangeEnd":"2018-05-01"}'



out<-postForm("https://rainlog.org/api/1.0/Reading/getFiltered", 
              .opts = list(postfields = jsonQuery, 
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))

out<-fromJSON(out)

# https://app.swaggerhub.com/apis/rainlog/rainlog/1.0.0#/Reading/post_Reading_getFiltered
#jsonQuery='{"readingIds":[0],"quality":["Good"],"pagination":{"offset":0,"limit":1000},"gaugeRevisionIds":[0],"dateRangeStart":"1999-02-25","dateRangeEnd":"1999-02-25","region":{"type":"Circle","center":{"lat":32.23,"lng":-110.87},"radius":10.0},"gaugeType":["TruCheckWedge"],"gaugeIds":[0],"userIds":[0]}'
jsonQuery='{"quality":["Good"],"pagination":{"offset":0,"limit":1000},"dateRangeStart":"2016-01-01","dateRangeEnd":"2016-12-31","region":{"type":"Circle","center":{"lat":32.221551,"lng":-110.909479},"radius":20.0}}'



out<-postForm("https://rainlog.org/api/1.0/Reading/getFiltered", 
              .opts = list(postfields = jsonQuery, 
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))

out<-fromJSON(out)

# from Tim V
# const int LIMIT = 1000;
# int i=0;
# while (true) {
#   result = query_with_pagination(<query>, pagination: {offset:i, limit:LIMIT})
#   if (result.status != 200) {
#     //retry or fail
#   } else if (result.json.length == 0) {
#     break;
#   }
#   
#   useResult(result.json);
#   i += LIMIT;
# }


# while statement to loop through pages
limit<-1000
i<-0
done<-0

while (done==0) {
  jsonQuery=paste0('{"quality":["Good"],"pagination":{"offset":',i,',"limit":',limit,'},"dateRangeStart":"2016-01-01","dateRangeEnd":"2018-05-01","region":{"type":"Circle","center":{"lat":32.221551,"lng":-110.909479},"radius":20.0}}')
  out<-postForm("https://rainlog.org/api/1.0/Reading/getFiltered", 
                .opts = list(postfields = jsonQuery, 
                             httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
  out<-fromJSON(out)
    if(exists("out")==TRUE){
        if (i==0){
          dataStack<-out
        }else{
          dataStack<-rbind(dataStack, out)
                  }
      }else{
        break
        done<-1
     }
  
  i <-i+limit
  rm(out)
  print(i)
}

# Get gauges
# https://app.swaggerhub.com/apis/rainlog/rainlog/1.0.0#/GaugeRevision/post_GaugeRevision_getFiltered
jsonQuery1='{"dateRangeStart":"2016-01-01","dateRangeEnd":"2018-05-01","region":{"type":"Circle","center":{"lat":32.221551,"lng":-110.909479},"radius":20.0},"pagination":{"offset":0,"limit":1000}}'

out<-postForm("https://rainlog.org/api/1.0/GaugeRevision/getFiltered", 
              .opts = list(postfields = jsonQuery1, 
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))

out<-fromJSON(out)
# get gauges
# while statement to loop through gauges
limit<-1000
i<-0
done<-0

while (done==0) {
  jsonQuery=paste0('{"dateRangeStart":"2016-01-01","dateRangeEnd":"2018-05-01","region":{"type":"Circle","center":{"lat":32.221551,"lng":-110.909479},"radius":20.0},"pagination":{"offset":',i,',"limit":',limit,'}}')
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

# join data frames
mergedData <- merge(dataStack,gaugeStack,by="gaugeId")
