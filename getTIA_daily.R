# ACIS JSON Code to get TIA precipitation info
# 4/3/19

library(RCurl)
library(jsonlite)
library(reshape)
library(dplyr)
library(tidyr)
#library(seas)

#dateRangeStart="2007-01-01"
#dateRangeEnd="2018-12-31"

# download data in JSON format and convert --- TIA appears to be midnight to midnight
jsonQuery='{"sid":"028820","sdate":"2007-01-01","edate":"2018-12-31","elems":"1,2,43,4,10,11"}' # sid = station id, 029439=Winslow, arizona
out<-postForm("http://data.rcc-acis.org/StnData", 
              .opts = list(postfields = jsonQuery, 
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
out<-fromJSON(out)

# get data from list
data<-data.frame(out$data)
colnames(data)<-c("date","t_max","t_min","t_mean","precip","snow","snowD")
data$date<-as.Date(as.character(data$date))
# convert columns to numeric
unfactorize<-c("t_max","t_min","t_mean","precip","snow","snowD")
data[,unfactorize]<-lapply(unfactorize, function(x) as.numeric(as.character(data[,x])))

dataTIA<-data

save(dataTIA, file = "./manuscript/TIA_COOP.RData")
# could use SPEI package to calculate PET here or hard code in Hargreaves 

