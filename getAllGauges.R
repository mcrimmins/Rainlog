# Get Rainlog and ACIS gauges
# MAC 09/18/2018

library(RCurl)
library(jsonlite)
library(ggplot2)
library(ggmap)
library(scales)
library(reshape2)

# Universal date range
dateRangeStart="2018-09-18"
dateRangeEnd="2018-09-21"
allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)

# GET ACIS Stations ----
# get map and bounding box
where<-geocode("tucson", source = "dsk")
TucsonMap <- qmap(location = c(lon = where[1,1], lat = where[1,2]), zoom = 10,
                  color = "bw")
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
meltData$readingDate<-as.Date(meltData$readingDate)-1 # adjusting to match Rainlog

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
mergedData$readingDate<-as.Date(mergedData$readingDate)-1

# add network id
mergedData$network<-'Rainlog'
meltData$network<-'ACIS'

# Combine datasets ----
rainlogObs<-mergedData[,c(20,19,1,5,9,21)]
rainlogObs$gaugeId<-as.factor(rainlogObs$gaugeId)
allObs<-rbind(rainlogObs,meltData)
# blank out positive 0 reports
allObs$rainAmount[allObs$rainAmount == 0] <- NA
#allObs<-subset(allObs, rainAmount>0)


# with basemap
theme_set(theme_bw(16))
TucsonMap +
  geom_point(data=allObs, aes(x=position.lng ,y=position.lat, color=rainAmount), size=0.5)+ # removed from AES
  #facet_wrap(~readingDate)+
  facet_wrap(~readingDate, ncol = 15, nrow = ceiling(length(allDates)/15))+ # 15 for whole season
  #scale_shape_manual(values=c(21, 4), guide=FALSE)+ # outline=21, plus=3
  scale_color_gradient2(limits=c(0, 1), mid=("blue"), high="red", low="green", oob=squish, midpoint = 0.5, name="Precip (in)",
                        labels = c("0.00", "0.25", "0.5", "0.75", "≥ 1.00"),
                        breaks = c(0, 0.25, 0.5, 0.75, 1.0))+
  labs(title="Daily ACIS & Rainlog Precipitation Observations - Monsoon Season 2018")

# subset to date
SubAllObs<-subset(allObs, readingDate == as.Date("2018-09-19") )

# define jet colormap
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

# with basemap
theme_set(theme_bw(16))
TucsonMap +
  geom_point(data=SubAllObs, aes(x=position.lng ,y=position.lat, color=rainAmount, shape=network), size=1.5)+ # removed from AES
  #facet_wrap(~readingDate)+
  facet_wrap(~readingDate, ncol = 15, nrow = ceiling(length(allDates)/15))+ # 15 for whole season
  scale_shape_manual(values=c(15, 17))+ # outline=21, plus=3
  # scale_color_gradient2(limits=c(0, 1), mid=("blue"), high="red", low="green", oob=squish, midpoint = 0.5, name="Precip (in)",
  #                       labels = c("0.00", "0.25", "0.5", "0.75", "≥ 1.00"),
  #                       breaks = c(0, 0.25, 0.5, 0.75, 1.0), na.value="white")+
  scale_color_gradientn(colors =  jet.colors(7),
    limits=c(0, 10),name="Precip (in)",
                        labels = c("0.00", "0.25", "0.5", "0.75", "≥ 1.00"),
                        breaks = c(0, 0.25, 0.5, 0.75, 1.0), na.value="grey48")+
  labs(title="Daily ACIS & Rainlog Precipitation Observations - Monsoon Season 2018")

