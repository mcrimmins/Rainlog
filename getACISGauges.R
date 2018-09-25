# RCC ACIS Stations - Tucson Rainlog comparison
# MAC 9/17/18

library(ggmap)
library(reshape2)
library(ggplot2)

# get map and bounding box
TucsonMap <- qmap("tucson", zoom = 10,
                  color = "bw")
# RCC ACIS bbox - Bounding box specified in decimal degrees (W,S,E,N) (e.g. [-90, 40, -88, 41])
ACISbbox<-paste0(min(TucsonMap$data$lon),',',min(TucsonMap$data$lat),',',max(TucsonMap$data$lon),',',max(TucsonMap$data$lat))

# date range
dateRangeStart="2018-06-15"
dateRangeEnd="2018-09-15"
allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)

# from RCC ACIS query builder
#{"bbox":"-111.413477279492,31.8494949097163,-110.534571029492,32.5930361292801","sdate":"2018-07-01","edate":"2018-07-31","elems":"pcpn","meta":"ll"}

jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","elems":"pcpn","meta":"name,ll"}') # or uid

#jsonQuery='{"state":"nm","sdate":"2018-06-15","edate":"2018-08-16","elems":[{"name":"pcpn","interval":"dly","duration":"dly","smry":{"reduce":"sum","add":"mcnt"},"smry_only":1},{"name":"pcpn","interval":"dly","duration":"dly","smry":"sum","smry_only":1,"normal":"departure"},{"name":"pcpn","interval":"dly","duration":"dly","smry":{"reduce":"max","add":"date"},"smry_only":1}]}'

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


# GGPLOT
# blank out positive 0 reports
meltData$value[meltData$value == 0] <- NA

# with basemap
theme_set(theme_bw(16))
TucsonMap +
  geom_point(data=meltData, aes(x=X1 ,y=X2, color=value), size=0.5)+ # removed from AES
  #facet_wrap(~readingDate)+
  facet_wrap(~variable, ncol = 15, nrow = ceiling(length(allDates)/15))+ # 15 for whole season
  #scale_shape_manual(values=c(21, 4), guide=FALSE)+ # outline=21, plus=3
  scale_color_gradient2(limits=c(0, 1), mid=("blue"), high="red", low="green", oob=squish, midpoint = 0.5, name="Precip (in)",
                        labels = c("0.00", "0.25", "0.5", "0.75", "≥ 1.00"),
                        breaks = c(0, 0.25, 0.5, 0.75, 1.0))+
  labs(title="Daily ACIS Precipitation Observations - Monsoon Season 2018")
