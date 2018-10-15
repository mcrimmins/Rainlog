# Get Rainlog and ACIS gauges, get ACIS grids, difference points from grids
# MAC 09/20/2018

library(RCurl)
library(jsonlite)
library(ggplot2)
library(ggmap)
library(scales)
library(reshape2)
library(raster)

# API key
source('APIkey.R')

# Universal date range
dateRangeStart="2018-06-14"
dateRangeEnd="2018-09-30"
allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)

# GET ACIS Stations ----
# get map and bounding box
where<-geocode("tucson", source = "dsk")
TucsonMap <- qmap(location = c(lon = where[1,1], lat = where[1,2]), zoom = 10,
                  color = "bw") # or "color"
# RCC ACIS bbox - Bounding box specified in decimal degrees (W,S,E,N) (e.g. [-90, 40, -88, 41])
ACISbbox<-paste0(min(TucsonMap$data$lon),',',min(TucsonMap$data$lat),',',max(TucsonMap$data$lon),',',max(TucsonMap$data$lat))

# ACIS query
jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","elems":"pcpn","meta":"name,ll"}') # or uid
out<-postForm("http://data.rcc-acis.org/MultiStnData", 
              .opts = list(postfields = jsonQuery, 
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
out<-fromJSON(out)

# format into dataframe
ll<-data.frame(matrix(unlist(out$data$meta$ll), nrow=length(out$data$meta$ll), byrow=T))
meta<-out$data$meta
# get summary formatted
summary<- data.frame(matrix(unlist(out$data$data), nrow=nrow(out$data), byrow=T))
colnames(summary)<-allDates
summary<-cbind(ll,meta$name,summary)

# melt data
meltData<-melt(summary, id=1:3)
meltData$value<-as.numeric(meltData$value)
# change to common dataframe format
colnames(meltData)<-c("position.lng","position.lat","gaugeId","readingDate","rainAmount")
meltData$readingDate<-as.Date(meltData$readingDate)# adjusting to match Rainlog

# GET Rainlog Stations ----
# while statement to loop through pages
limit<-1000
i<-0
done<-0

# download Rainlog obs ----
while (done==0) {
  jsonQuery=paste0('{"quality":["Good"],"pagination":{"offset":',i,',"limit":',limit,'},"dateRangeStart":"',dateRangeStart,'","dateRangeEnd":"',dateRangeEnd,'","region":{"type":"Circle","center":{"lat":32.221551,"lng":-110.909479},"radius":20.0}}')
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
    if (out$readingDate[length(out$readingDate)]==dateRangeEnd){
      done<-1
      break
    }else{}
    
  }else{
    break
    done<-1
  }
  
  i <-i+limit
  rm(out)
  print(i)
}

# get Rainlog gauges ----
# while statement to loop through gauges
limit<-1000
i<-0
done<-0

while (done==0) {
  jsonQuery=paste0('{"dateRangeStart":"',dateRangeStart,'","dateRangeEnd":"',dateRangeEnd,'","region":{"type":"Circle","center":{"lat":32.221551,"lng":-110.909479},"radius":20.0},"pagination":{"offset":',i,',"limit":',limit,'}}')
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

# join Rainlog data frames
mergedData <- merge(dataStack,gaugeStack,by="gaugeId")
# fix dates
#mergedData$readingDate<-as.Date(mergedData$readingDate)-1

# add network id
mergedData$network<-'Rainlog'
meltData$network<-'ACIS'

# Combine datasets ----
rainlogObs<-mergedData[,c(20,19,1,5,9,21)]
rainlogObs$gaugeId<-as.factor(rainlogObs$gaugeId)
rainlogObs$readingDate<-as.Date(rainlogObs$readingDate)
allObs<-rbind(rainlogObs,meltData)
# blank out positive 0 reports
#allObs$rainAmount[allObs$rainAmount == 0] <- NA

# GET GRID ----
#jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","grid":"21","elems":"pcpn","meta":"ll,elev","output":"json"}') # or uid
jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","grid":"2","elems":"pcpn","meta":"ll","output":"json"}') # or uid

out<-postForm("http://data.rcc-acis.org/GridData", 
              .opts = list(postfields = jsonQuery, 
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
out<-fromJSON(out)

# convert to list of matrices, flipud with PRISM
matrixList <- vector("list",length(out$data))
for(i in 1:length(out$data)){
  matrixList[[i]]<-apply(t(out$data[[i]][[2]]),1,rev) 
}

# read into raster stack
rasterList<-lapply(matrixList, raster)
gridStack<-stack(rasterList)
gridExtent<-extent(min(out$meta$lon), max(out$meta$lon), min(out$meta$lat), max(out$meta$lat))
gridStack<-setExtent(gridStack, gridExtent, keepres=FALSE, snap=FALSE)
names(gridStack)<-allDates

# GET grid value for each day/location
for(i in 1:nrow(allObs)){
    allObs$gridPrecip[i]<-extract(gridStack[[which(allDates==allObs$readingDate[i])]], 
            cellFromXY(gridStack[[1]], c(allObs$position.lng[i],allObs$position.lat[i])))
}
# get diffs
allObs$gridPrecip[allObs$gridPrecip < 0] <- NA # missing data to NA
allObs$gridDiff<-allObs$rainAmount-allObs$gridPrecip



# PLOTTING ----- 
# plot some results
library(ggplot2)
ggplot(allObs, aes(x=gridDiff, color=network)) + 
  geom_histogram(fill="white", alpha=0.5, position="identity")+
  xlim(-3,3)+
  labs(title="Point-Grid Precipitation Observations - Monsoon Season 2018")

# only rain days in observations
subObs<-subset(allObs, rainAmount>0)
ggplot(subObs, aes(x=gridDiff, color=network)) + 
  geom_histogram(fill="white", alpha=0.5, position="identity")+
  xlim(-3,3)+
  labs(title="Point-Grid Precipitation Observations - Monsoon Season 2018")

# plot diffs
# only rain days in observations
subObs<-subset(allObs, rainAmount>0)
# with basemap
theme_set(theme_bw(16))
TucsonMap +
  geom_point(data=subObs, aes(x=position.lng ,y=position.lat, color=gridDiff, shape=network), size=1)+ # removed from AES
  #facet_wrap(~readingDate)+
  facet_wrap(~readingDate, ncol = 15, nrow = ceiling(length(allDates)/15))+ # 15 for whole season
  scale_shape_manual(values=c(15, 17))+ # outline=21, plus=3
  scale_color_gradient2(limits=c(-0.5, 0.5), mid=("white"), high="orange", low="purple", oob=squish, midpoint = 0, name="Precip Diff (in)", na.value="white")+
  labs(title="Diff in Precip Obs (point-MPE grid) - Monsoon Season 2018")

# PLOT ERRORS ----
#RMSE time series plot
# Function that returns Root Mean Squared Error
rmse <- function(error){sqrt(mean(error^2,na.rm = TRUE))}
# Function that returns Mean Absolute Error
mae <- function(error){mean(abs(error), na.rm = TRUE)}

library('dplyr')

allObs$rainAmount[allObs$rainAmount > 6] <- NA # outliers to NA
# summarize values
errorAllObs<-allObs %>% group_by(readingDate) %>% summarize(countObs = n(),
                                                            rainObsN = sum(rainAmount > 0, na.rm = TRUE),
                                                            pctRainObsN = (rainObsN/countObs)*100,
                                                            maxAmount=max(rainAmount, na.rm = TRUE),
                                                            medianAmount=median(rainAmount, na.rm = TRUE),
                                                            rmseGridDiff=rmse(gridDiff),
                                                            maeGridDiff=mae(gridDiff)
                                                            )
errorNetworkObs<-allObs %>% group_by(readingDate, network) %>% summarize(countObs = n(),
                                                                         rainObsN = sum(rainAmount > 0, na.rm = TRUE),
                                                                         pctRainObsN = (rainObsN/countObs)*100,
                                                                         maxAmount=max(rainAmount, na.rm = TRUE),
                                                                         medianAmount=median(rainAmount, na.rm = TRUE),
                                                                         rmseGridDiff=rmse(gridDiff),
                                                                         maeGridDiff=mae(gridDiff)
                                                            )

# ggplot values
errorAllObsMelt<-melt(errorAllObs, id.vars = 'readingDate')
ggplot(errorAllObsMelt, aes(x=readingDate, y=value))+
  geom_line()+
  facet_wrap(~variable, nrow = 7, scales="free_y")

errorNetworkObsMelt<-melt(errorNetworkObs, id.vars = c('readingDate','network'))
errorNetworkObsMelt$network<-as.factor(errorNetworkObsMelt$network)
ggplot(errorNetworkObsMelt, aes(x=readingDate, y=value, color=network))+
  geom_line()+
  facet_wrap(~variable, nrow = 7, scales="free_y")+
  labs(title='Tucson Monsoon Season 2018 - Network and Grid Comparisons (NOAA AHPS/MPE grid)')+
  theme_bw()

