# generate gauge stats from output of getGaugePOR.R
# MAC 02/18/2019

library(ggplot2)

load("~/RProjects/RainlogAPI/manuscript/gaugeStackData.RData")
load("~/RProjects/RainlogAPI/geocodes/geocoded_ALLGauges.RData")

# change dates to Date format
gaugeStack$minDate<-as.Date(gaugeStack$minDate, format = "%Y-%m-%d")
gaugeStack$maxDate<-as.Date(gaugeStack$maxDate, format = "%Y-%m-%d")
gaugeStack$createdDate<-as.Date(gaugeStack$createdDate, format = "%Y-%m-%d")
# add completeness of record column max-min/num obs
gaugeStack$percMissing<-100-round((gaugeStack$numObs/as.numeric(gaugeStack$maxDate-gaugeStack$minDate))*100)
   gaugeStack$percMissing[gaugeStack$percMissing<0] <- NA
gaugeStack$periodOfRecord<-gaugeStack$maxDate-gaugeStack$minDate   

# flag gauge revision
gaugeStack$changed<-ifelse(gaugeStack$gaugeRevisionId-gaugeStack$gaugeId==0,'no','yes')

# merge in geocodes
gaugeStack<-merge(gaugeStack, geocodes, by="gaugeRevisionId", all.x = TRUE)


# plot of percent missing vs num of obs
   plot(gaugeStack$numObs,gaugeStack$percMissing, main = 'Rainlog Observers: Percent Missing vs. Total Num of Obs')
# density plot
  #  sp <- ggplot(gaugeStack, aes(x=numObs, y=percMissing)) +
  #    geom_point()+
  #    xlim(0,5000)
  #  sp + geom_density_2d()
  # ggplot(gaugeStack, aes(x=numObs, y=percMissing)) +
  #  geom_bin2d()+
  #   xlim(0,5000)
      
# some histograms
# hist(gaugeStack$numObs, breaks = 500, xlim=c(0,100))

# basic project stats from gaugeStack
cat(
        "Total Number of Gauges: ",nrow(gaugeStack),'\n',
        "% of Gauges with >2 obs: ",round(length(which(gaugeStack$numObs>=2))/nrow(gaugeStack)*100),'%','\n',
        "% of Gauges with >20 obs: ",round(length(which(gaugeStack$numObs>=20))/nrow(gaugeStack)*100),'%','\n',
        "% of Gauges with >200 obs: ",round(length(which(gaugeStack$numObs>=200))/nrow(gaugeStack)*100),'%','\n',
        "% of Gauges with >2000 obs: ",round(length(which(gaugeStack$numObs>=2000))/nrow(gaugeStack)*100),'%','\n',
        "Total Number of Records in Rainlog: ",sum(gaugeStack$numObs, na.rm = TRUE)+sum(gaugeStack$traceObs, na.rm = TRUE),'\n',
        "Total Number of SnowDepth Records in Rainlog: ",sum(gaugeStack$snowDepthObs, na.rm = TRUE),'\n',
        "Total Number of SnowAccum Records in Rainlog: ",sum(gaugeStack$snowAccumObs, na.rm = TRUE),'\n',
        "Total Number of Comments Entered in Rainlog: ",sum(gaugeStack$numRemarks, na.rm = TRUE),'\n'
        
)

# observation quality flags
cat(
  "Total Number of Quality Good: ",sum(gaugeStack$qualGood, na.rm = TRUE),'\n',
  "Total Number of Quality Trace: ",sum(gaugeStack$qualTrace, na.rm = TRUE),'\n',
  "Total Number of Quality Poor: ",sum(gaugeStack$qualPoor, na.rm = TRUE),'\n',
  "Total Number of Quality Snow: ",sum(gaugeStack$qualSnow, na.rm = TRUE),'\n',
  "Total Number of Quality Lost: ",sum(gaugeStack$qualLost, na.rm = TRUE),'\n',
  "Total Number of Quality Absent: ",sum(gaugeStack$qualAbsent, na.rm = TRUE),'\n'
)

# leaflet map of locations
library(leaflet)
# Create a palette that maps factor levels to colors
#gaugeStack<-gaugeStack[which(gaugeStack$createdDate<"2005-12-31"),]
# gaugeStack<-gaugeStack[which(gaugeStack$numObs>=2),]
# 
# pal <- colorNumeric(
#   palette = "Spectral",
#   domain = gaugeStack$percMissing)
# 
# leaflet(gaugeStack) %>% addTiles() %>%
#   addCircleMarkers(lng = ~position.lng.x, lat = ~position.lat.x,
#                    radius=1.5,
#                    color = ~pal(percMissing),
#                    stroke = FALSE,
#                    fillOpacity=1
#                    )
#     
#     
#     radius = ~ifelse(type == "ship", 6, 10),
#     color = ~pal(type),
#     stroke = FALSE, fillOpacity = 0.5
#   )


# tables of gauge location geocodes - sort and screen cap
temp<-gaugeStack[which(gaugeStack$numObs>=2),]
states<-as.data.frame(table(temp$State))
cities<-as.data.frame(table(temp$City))
temp<-temp[which(temp$City=="Flagstaff"),]
  nrow(temp[which(temp$numObs>=100),])


# plot gauge creation time
ggplot(gaugeStack, aes(createdDate,as.factor(gaugeId)))+
  geom_point(size=1)+
  theme_bw()+  
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  ggtitle("Creation date for each registered Rainlog gauge")

# plot of percent missing vs num of obs
plot(gaugeStack$numObs,gaugeStack$percMissing, main = 'Rainlog Observers: Percent Missing vs. Total Num of Obs')
ggplot(gaugeStack, aes(numObs, percMissing))+
  geom_point()+
  theme_bw()+
  ggtitle("Total Num Obs vs Perc Missing: Rainlog Observers")

# hist of ratio of 'zero' reports to total obs -- 
ggplot(gaugeStack, aes(gaugeStack$numZeros/gaugeStack$numObs))+
  geom_histogram(fill = "darkblue", alpha = 0.5)+
  #geom_vline(xintercept = 7)+
  theme_bw()+
  xlab("Ratio of zero to total number of observations")+
  ggtitle("Ratio of zero-obs to total number of observations - Rainlog")
# filter on no-zero reports 
temp<-gaugeStack[which(gaugeStack$numZeros/gaugeStack$numObs==0),]
ggplot(temp, aes(as.numeric(temp$periodOfRecord)))+
  geom_histogram(fill = "purple", alpha = 0.5)+
  #geom_vline(xintercept = 7)+
  theme_bw()+
  xlab("Period of record length (days)")+
  ggtitle("Period of record length for 'no-zero' observers - Rainlog")
ggplot(temp, aes(as.numeric(temp$numObs)))+
  geom_histogram(fill = "purple", alpha = 0.5)+
  #geom_vline(xintercept = 7)+
  theme_bw()+
  xlab("# observations")+
  ggtitle("Number of observations for 'no-zero' observers - Rainlog")

# hist of observation time
ggplot(gaugeStack, aes(avgHour))+
  geom_histogram(fill = "red", alpha = 0.2)+
  geom_vline(xintercept = 7)+
  theme_bw()+
  ggtitle("Average Reporting Times - Rainlog Observers")

# histogram of num of obs
ggplot(gaugeStack, aes(numObs))+
  geom_histogram(fill = "blue", alpha = 0.2)+
  #geom_vline(xintercept = 7)+
  theme_bw()+
  ggtitle("Number of observations per gauge - Rainlog")

# percent missing obs hist
ggplot(gaugeStack, aes(percMissing))+
  geom_histogram(fill = "orange", alpha = 0.5)+
  #geom_vline(xintercept = 7)+
  theme_bw()+
  ggtitle("Percent missing per gauge - Rainlog")

# plot period of records
library(reshape2)
temp<-subset(gaugeStack, percMissing<5)
porDates <- melt(temp, measure.vars = c("minDate", "maxDate"))
porDates$gaugeId<-as.factor(porDates$gaugeId)
ggplot(porDates, aes(value,gaugeId,group=gaugeId, color=percMissing)) + 
  geom_line(size = 0.1)+
  scale_color_gradientn(colours=rev(brewer.pal(7, "Spectral")))+
  geom_vline(xintercept = as.Date("2005-01-01"))+
  xlim(c(as.Date("2005-01-01"),as.Date("2018-12-31")))+
  theme(axis.text.y = element_blank(),
             axis.ticks.y = element_blank())+
  ggtitle("Period of record length for each registered Rainlog gauge")


# get first/last year of observation -- trying to get at recruitment/retention
ggplot(gaugeStack, aes(as.numeric(format(gaugeStack$minDate, "%Y"))))+
  geom_histogram(fill = "blue", alpha = 0.2, binwidth = 1)+
  #geom_vline(xintercept = 7)+
  theme_bw()+
  scale_x_continuous(breaks=seq(2004,2019,1), limits = c(2004,2019))+
  xlab("Min Year")+
  ggtitle("Min observation year - Rainlog")
ggplot(gaugeStack, aes(as.numeric(format(gaugeStack$maxDate, "%Y"))))+
  geom_histogram(fill = "red", alpha = 0.2, binwidth = 1)+
  #geom_vline(xintercept = 7)+
  theme_bw()+
  scale_x_continuous(breaks=seq(2004,2019,1), limits = c(2004,2019))+
  xlab("Max Year")+
  ggtitle("Max observation year - Rainlog")
# observer has entry in 2018, less than 10% missing 
cat("Number of Rainloggers with observations in 2018 and <10% missing:",
  sum(ifelse(as.numeric(format(gaugeStack$maxDate, "%Y"))==2018 & gaugeStack$percMissing<10, 1, 0), na.rm=TRUE)
)
cat("Number of Rainloggers with observations in 2018, more than 2 years of data, and <10% missing:",
    sum(ifelse(as.numeric(format(gaugeStack$maxDate, "%Y"))==2018 &
               as.numeric(format(gaugeStack$minDate, "%Y"))<=2016 &
                 gaugeStack$percMissing<50, 1, 0), na.rm=TRUE)
)


# max rain
ggplot(gaugeStack, aes(maxRain))+
  geom_histogram(fill = "darkgreen", alpha = 0.8, binwidth = 0.25)+
  #geom_vline(xintercept = 7)+
  theme_bw()+
  xlab("Max Rain Observed by Gauge")+
  geom_vline(xintercept = 5)+
  ggtitle("Max observation per gauge - Rainlog")

# gauge type counts
table(gaugeStack$gaugeType)
table(gaugeStack$brand)

# number of zero's vs missing obs over time
# density of obs over time
# reading time vs total
# plot snow obs from Feb 2019

# maps of observation density, gauge type, reporting time...
library(ggmap)
library(RColorBrewer)
# API key
source('APIkey.R')

# getting the map
mapSW <- get_map(location = "Arizona", zoom = 7,
                      maptype = "terrain", scale = 2)

# heat map of observer density
ggmap(mapSW)+
  stat_density2d(data=gaugeStack, 
                     aes(x=position.lng.x, y=position.lat.x, fill=..level.., alpha=..level..), geom="polygon")+
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")))

# plotting the map with some points on it
temp<-subset(gaugeStack, percMissing<10)
temp<-subset(gaugeStack, numObs>=10)


ggmap(mapSW) +
  geom_point(data = temp, aes(x = position.lng.x, y = position.lat.x, color = percMissing, alpha = 0.1), size = 0.7, shape = 19)+ 
  #guides(fill=FALSE, alpha=FALSE, size=FALSE)
  scale_color_gradientn(colours=rev(brewer.pal(7, "Spectral")))+
  ggtitle("All Rainlog Observation Sites (n>=2) with percent missing")+
  guides(alpha=FALSE)

