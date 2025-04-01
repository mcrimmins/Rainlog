# Map Rainlog data with ggplot
# MAC 9/8/18

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
dateRangeStart="2020-07-24"
dateRangeEnd="2020-07-24"
allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)

# specify center and radius for search area

# while statement to loop through pages
limit<-1000
i<-0
done<-0

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

# get gauges
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

# reverse geocode
# library(revgeo)
# test<-gaugeStack[1:20,]
# location<-revgeo(test$position.lng, test$position.lat, output = 'frame')

# join data frames
mergedData <- merge(dataStack,gaugeStack,by="gaugeId")
# fix dates
mergedData$readingDate<-as.Date(mergedData$readingDate)-1

# look for outliers
#stats<-boxplot(mergedData$rainAmount)
#which.max(mergedData$rainAmount)
#mergedData<-mergedData[-(which.max(mergedData$rainAmount)), ]



# generate summaries
library(dplyr)
summaryDays <-mergedData %>%
  group_by(readingDate) %>%
  summarize(countObs = n(), 
            max=max(rainAmount),
            rainObsN = sum(rainAmount > 0),
            pctRainObsN = (rainObsN/countObs)*100,
            gt1inchObsN= sum(rainAmount>1),
            PctGt1inchObsN = (gt1inchObsN/countObs)*100
            )
plot(summaryDays$readingDate,summaryDays$PctGt1inchObsN, main="% of Rainlog Obs >1in Monsoon 2018", type='b', ylim=c(0,45))
plot(summaryDays$readingDate,summaryDays$pctRainObsN, main="% of Rainlog Obs >0in Monsoon 2018", type='b', ylim=c(0,100))
plot(summaryDays$readingDate,summaryDays$countObs, main="# of obs/day - Monsoon 2018", type='b')
plot(summaryDays$readingDate,summaryDays$max, main="Max ob/day - Monsoon 2018", type='b')

# ggplot version of time series plots
ggplot(data=summaryDays, aes(x=readingDate, y=PctGt1inchObsN)) +
  geom_bar(stat="identity",fill="steelblue")+
  xlab("Date")+
  ylab("Number of Observations")+
  ggtitle("% of Rainlog Obs >1in Monsoon 2018")
ggplot(data=summaryDays, aes(x=readingDate, y=countObs)) +
  geom_bar(stat="identity",fill="steelblue")+
  xlab("Date")+
  ylab("Number of Observations")+
  ggtitle("# of obs/day - Monsoon 2018")


# create 0-symbol 
mergedData$zeroDay<- mergedData$rainAmount ==0
# blank out positive 0 reports
mergedData$rainAmount[mergedData$rainAmount == 0] <- NA

# ggmap basemap
#TucsonMap <- qmap("tucson", zoom = 10,
#                   color = "bw", maptype = "terrain-background", source = "stamen")
# get map and bounding box
where<-geocode("tucson", source = "dsk")
TucsonMap <- qmap(location = c(lon = where[1,1], lat = where[1,2]), zoom = 10,
                  color = "bw")


# plot faceted map
ggplot(mergedData, aes(x=position.lng ,y=position.lat, fill=rainAmount, shape=zeroDay))+
  geom_point()+
  facet_grid(~readingDate)+
  scale_shape_manual(values=c(21, 3))+
  scale_fill_gradient2(limits=c(0, 3), mid=("blue"), high="red", low="green", oob=squish, midpoint = 1.5)

# with basemap
theme_set(theme_bw(16))
TucsonMap +
  geom_point(data=mergedData, aes(x=position.lng ,y=position.lat, color=rainAmount), size=0.5)+ # removed from AES
  #facet_wrap(~readingDate)+
  facet_wrap(~readingDate, ncol = 15, nrow = ceiling(length(allDates)/15))+ # 15 for whole season
  #scale_shape_manual(values=c(21, 4), guide=FALSE)+ # outline=21, plus=3
  scale_color_gradient2(limits=c(0, 1), mid=("blue"), high="red", low="green", oob=squish, midpoint = 0.5, name="Precip (in)",
                       labels = c("0.00", "0.25", "0.5", "0.75", "≥ 1.00"),
                       breaks = c(0, 0.25, 0.5, 0.75, 1.0))+
  labs(title="Daily Rainlog.org Precipitation Observations - Monsoon Season 2018")

# JET COLOR MAP --- with basemap
# define jet colormap
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

theme_set(theme_bw(16))
TucsonMap +
  geom_point(data=mergedData, aes(x=position.lng ,y=position.lat, color=rainAmount, shape=zeroDay), size=1.5)+ # removed from AES
  facet_wrap(~readingDate,nrow = 4, ncol=4)+
  #facet_wrap(~readingDate, ncol = 15, nrow = ceiling(length(allDates)/15))+ # 15 for whole season
  scale_shape_manual(values=c(16, 4), guide=FALSE)+ # outline=21, plus=3
  scale_color_gradient2(limits=c(0, 1), mid=("yellow"), high="red", low="cyan", oob=squish, midpoint = 0.5, name="Precip (in)",
                        labels = c("0.00", "0.25", "0.5", "0.75", "≥ 1.00"),
                        breaks = c(0, 0.25, 0.5, 0.75, 1.0), na.value="black")+
  # scale_color_gradientn(colors =  jet.colors(7),
  #                       limits=c(0, 1),name="Precip (in)",
  #                       labels = c("0.00", "0.25", "0.5", "0.75", "≥ 1.00"),
  #                       breaks = c(0, 0.25, 0.5, 0.75, 1.0), na.value="grey48")+
  labs(title="Rainlog Precipitation Observations - Tucson, Early July 2018")




# contour plots
ggplot(mergedData, aes(x=position.lng ,y=position.lat, z=rainAmount))+
  stat_density2d()+
  facet_wrap(~readingDate)

TucsonMap +
  stat_density2d(data=mergedData, aes(x=position.lng ,y=position.lat, z=rainAmount))+
  facet_wrap(~readingDate)

# interpolate to grid
library(akima)
library(dplyr)
fld <- with(mergedData, interp(x = position.lng, y = position.lat, z = rainAmount))

interpDays <-mergedData %>%
  group_by(readingDate) %>%
  interp(x = mergedData$position.lng, y = mergedData$position.lat, z = mergedData$rainAmount)

# look for outliers
#stats<-boxplot(mergedData$rainAmount)
#which.max(mergedData$rainAmount)
#mergedData<-mergedData[-(which.max(mergedData$rainAmount)), ]

# use more elaborate outlier detection
# account for missing days too...
