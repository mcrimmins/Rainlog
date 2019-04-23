# Get Gridded data from ACIS
# MAC 09/19/2018

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
dateRangeStart="2007-01-02"
dateRangeEnd="2019-01-01"
allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)

# GET ACIS Stations ----
# get map and bounding box
where<-geocode("tucson", source = "dsk")
TucsonMap <- qmap(location = c(lon = where[1,1], lat = where[1,2]), zoom = 10,
                  color = "bw")
# RCC ACIS bbox - Bounding box specified in decimal degrees (W,S,E,N) (e.g. [-90, 40, -88, 41])
ACISbbox<-paste0(min(TucsonMap$data$lon),',',min(TucsonMap$data$lat),',',max(TucsonMap$data$lon),',',max(TucsonMap$data$lat))

# ACIS query 21-PRISM, 2-MPE
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

# get into data frame
gridStack2<-stack(init(gridStack[[1]], 'y'),init(gridStack[[1]], 'x'), gridStack)
gridFrame <- data.frame((values(gridStack2)))
gridFrame<-melt(gridFrame, id.vars=1:2)
colnames(gridFrame)<-c("lat","lon","date","precip")
gridFrame$date<-as.Date(gridFrame$date, format = "X%Y.%m.%d")

# save PRISM
# prismFrame<-gridFrame
# prismStack<-gridStack
# save(prismFrame,prismStack, file="~/RProjects/RainlogAPI/manuscript/prismGridData.RData")
# save MPE
mpeFrame<-gridFrame
mpeStack<-gridStack
save(mpeFrame,mpeStack, file="~/RProjects/RainlogAPI/manuscript/mpeGridData.RData")


#v$precip[v$precip == 0] <- NA
# remove 0 value cells
gridFrame<-subset(gridFrame, precip>0)

# subset to date
gridDay<-subset(gridFrame, date=="2014-09-09")

# plot a map
theme_set(theme_bw(16))
TucsonMap +
  geom_raster(data=gridDay, aes(x=lon ,y=lat, fill=precip), alpha = 0.5)+ # removed from AES
  #facet_wrap(~readingDate)+
  #facet_wrap(~date,ncol = 10, nrow = ceiling(length(allDates)/10))+ # 15 for whole season
  coord_equal()+
  scale_fill_gradient2(limits=c(0, 1), mid=("blue"), high="red", low="green", oob=squish, midpoint = 0.5, name="Precip (in)",
                        labels = c("0.00", "0.25", "0.5", "0.75", "≥ 1.00"),
                        breaks = c(0, 0.25, 0.5, 0.75, 1.0))+
  labs(title="Daily Multi-sensor Precipitation Estimates - Monsoon Season 2018")
