# Pull and process Tucson area Rainlog obs for manuscript analysis
# MAC 3/21/19
# adapted from getALLRainlog.R

library(scales)
library(dplyr)
library(cowplot)
library(ggmap)
library(moments)
library(viridis)
library(forcats)
library(grid)

# API key
source('APIkey.R')
# get map and bounding box
where<-geocode("tucson", source = "dsk")
TucsonMap <- qmap(location = c(lon = where[1,1], lat = where[1,2]), zoom = 10,
                  color = "bw")

TucsonMap <- qmap(location = c(lon = where[1,1], lat = where[1,2]), zoom = 10,
                  source = "stamen", maptype = "terrain", color="bw")


#load("~/RProjects/RainlogAPI/manuscript/TucsonRainlogObs_2014_2018.RData")
load("~/RProjects/RainlogAPI/manuscript/TucsonRainlogObs_2007_2018_allQuality.RData")
load("~/RProjects/RainlogAPI/manuscript/TIA_COOP.RData")
load("~/RProjects/RainlogAPI/manuscript/TucsonGauges_Elevations.RData")

# add elevations to gauges
elevs<-LatLons_Elevs@data
gaugeStack<-merge(gaugeStack,elevs, by="gaugeRevisionId", all.x = TRUE)

# set date ranges 
# changed from 2007-01-01/2018-12-31 to 2007-01-02/2019-01-01
dateRangeStart="2007-01-01"
dateRangeEnd="2018-12-31"
allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)

# join data frames
mergedData <- merge(dataStack,gaugeStack,by="gaugeRevisionId", all.x = TRUE)
# fix dates
mergedData$readingDate<-as.Date(mergedData$readingDate)-1

#test<-mergedData[which(mergedData$readingDate=="2012-07-15"),]

# add in year month
mergedData$yearMonth<-as.Date(paste0(format(mergedData$readingDate, "%m"),
                                     "-","01-",format(mergedData$readingDate,"%Y")),format = "%m-%d-%Y")
mergedData$year<-as.numeric(format(mergedData$readingDate, "%Y"))
mergedData$month<-as.numeric(format(mergedData$readingDate, "%m"))
mergedData$season<-as.factor(ifelse(mergedData$month>4 & mergedData$month<11, "May-Oct", "Nov-Apr"))
preClean<-nrow(mergedData)

## ----- DATA CLEANING -----
# scrub out extremes >5.24" TIA 1000 year 24 hr total
# https://hdsc.nws.noaa.gov/hdsc/pfds/pfds_map_cont.html?bkmrk=az
mergedData <- mergedData[-which(mergedData$rainAmount>5.24),]

# eliminate Summerhaven gauge obs, 10565
mergedData<-mergedData[-which(mergedData$gaugeId.x==10565),] # Summerhaven gauge
mergedData<-mergedData[-which(mergedData$gaugeId.x==630),] # Mt Kimball gauge

# find some key words in remarks col - looking for cumulative totals
mergedData<-mergedData[-which(grepl("cumulative", mergedData$remarks, ignore.case = TRUE)==TRUE),]
mergedData<-mergedData[-which(grepl("accumulated", mergedData$remarks, ignore.case = TRUE)==TRUE),]
mergedData<-mergedData[-which(grepl("gone", mergedData$remarks, ignore.case = TRUE)==TRUE),]
mergedData<-mergedData[-which(grepl("out of town", mergedData$remarks, ignore.case = TRUE)==TRUE),]
mergedData<-mergedData[-which(grepl("vacation", mergedData$remarks, ignore.case = TRUE)==TRUE),]
mergedData<-mergedData[-which(grepl("trip", mergedData$remarks, ignore.case = TRUE)==TRUE),]
# keep only Good and Trace
mergedData<-mergedData[mergedData$quality %in% c('Good','Trace'),]
postClean<-nrow(mergedData)
cleanedRecs<-preClean-postClean
# ----- END CLEANING

# ---- Descriptive Stats of Network ----

# FIGURE 5 ----
# histogram
p<-ggplot(mergedData, aes(as.factor(month)))+
  geom_histogram(fill = "skyblue", alpha = 1, stat = "count")+
  #geom_vline(xintercept = 7)+
  theme_bw()+
  xlab("month")+
  ylab("Total Observations")+
  scale_y_continuous(limits=c(0,100000),oob = rescale_none)
  #ggtitle("Total Tucson Area Rainlog observations by month (2007-2018)")
# png("/home/crimmins/RProjects/RainlogAPI/manuscript/ObsByMonth.png",
#     width = 7, height = 3, units = "in", res = 300L)
tiff("/home/crimmins/RProjects/RainlogAPI/manuscript/figs/Fig5_ObsByMonth.tif",
    width = 7, height = 3, units = "in", res = 300L)
print(p)
dev.off()
# ----

# FIGURE 5 with average daily observation rate
monthlyDayRate<-mergedData %>%
      group_by(month) %>%
      summarize(countObs =n(),
                countNA = sum(is.na(rainAmount)))
monthlyDayRate$daysInMonth<-c(31,28,31,30,31,30,31,31,30,31,30,31)
monthlyDayRate$avgRate<-round(monthlyDayRate$countObs/(monthlyDayRate$daysInMonth*12),0)
p<-ggplot(monthlyDayRate, aes(as.factor(month),avgRate))+
  geom_bar(fill = "skyblue", alpha = 1, stat = "identity")+
  #geom_vline(xintercept = 7)+
  theme_bw()+
  xlab("month")+
  ylab("Average daily number of observations")
  #scale_y_continuous(limits=c(0,100000),oob = rescale_none)
#ggtitle("Total Tucson Area Rainlog observations by month (2007-2018)")
# png("/home/crimmins/RProjects/RainlogAPI/manuscript/ObsByMonth.png",
#     width = 7, height = 3, units = "in", res = 300L)
tiff("/home/crimmins/RProjects/RainlogAPI/manuscript/figs/Fig5_ObsByMonth.tif",
     width = 7, height = 3, units = "in", res = 300L)
print(p)
dev.off()


# generate month summaries
summaryMonths<-mergedData %>%
  group_by(yearMonth) %>%
  summarize(countObs = n(), 
            max=max(rainAmount),
            rainObsN = sum(rainAmount > 0, na.rm = TRUE),
            pctRainObsN = (rainObsN/countObs)*100,
            gt1inchObsN= sum(rainAmount>1, na.rm = TRUE),
            PctGt1inchObsN = (gt1inchObsN/countObs)*100,
            countNA = sum(is.na(rainAmount))
  )

# monthly plots
ggplot(data=summaryMonths, aes(x=yearMonth, y=countObs)) +
  geom_bar(stat="identity",fill="steelblue")+
  xlab("Date")+
  ylab("Number of Observations")+
  ggtitle("Number of Rainlog Observations/Month - 2007-2018")+
  scale_y_continuous(limits=c(4000,8000),oob = rescale_none)
#---- End of Desc stats

#---- Daily summary stats ----

# generate daily summaries
tempMerge<-mergedData
#tempMerge$rainAmount[tempMerge$rainAmount == 0] <- NA
temp<-tempMerge[tempMerge$readingDate=="2014-09-08",]
temp<-temp[temp$rainAmount>=1.8,]

summaryDays <-tempMerge %>%
  group_by(readingDate) %>%
  summarize(countObs = n(), 
            max=max(rainAmount, na.rm = TRUE),
            rainObsN = sum(rainAmount > 0, na.rm = TRUE),
            zeroObs = sum(rainAmount == 0, na.rm = TRUE),
            pctRainObsN = (rainObsN/countObs)*100,
            gt1inchObsN= sum(rainAmount>1, na.rm = TRUE),
            PctGt1inchObsN = (gt1inchObsN/countObs)*100,
            medianRain = median(rainAmount, na.rm=TRUE),
            iqrRain = IQR(rainAmount, na.rm=TRUE),
            cvIQR = IQR(rainAmount, na.rm=TRUE)/median(rainAmount, na.rm=TRUE),
            cvMAD = mad(rainAmount, na.rm = TRUE)/median(rainAmount, na.rm=TRUE),
            madRain = mad(rainAmount, na.rm = TRUE),
            skewRain = skewness(rainAmount, na.rm = TRUE),
            q90 = quantile(rainAmount, probs = 0.9, na.rm = TRUE),
            q10 = quantile(rainAmount, probs = 0.1, na.rm = TRUE),
            traceCount = length(which(quality == "Trace")),
            medianRainwo0 =median(rainAmount[rainAmount!=0], na.rm=TRUE),
            iqrRainwo0 = IQR(rainAmount[rainAmount!=0], na.rm=TRUE),
            cvwo0 = IQR(rainAmount[rainAmount!=0], na.rm=TRUE)/median(rainAmount[rainAmount!=0], na.rm=TRUE),
            meanRain = mean(rainAmount, na.rm=TRUE),
            sdRain = sd(rainAmount, na.rm=TRUE),
            cvMean = sd(rainAmount, na.rm=TRUE)/mean(rainAmount, na.rm=TRUE)
  )

summaryDays$q9010diff<-summaryDays$q90-summaryDays$q10
summaryDays$stdq90q10<-summaryDays$q9010diff/summaryDays$pctRainObsN
summaryDays$cvIQR[is.infinite(summaryDays$cvIQR)]<-NA
summaryDays$cvMAD[is.infinite(summaryDays$cvMAD)]<-NA
summaryDays$cvwo0[is.infinite(summaryDays$cvwo0)]<-NA
summaryDays$IQR_MAD_diff<-summaryDays$iqrRain-summaryDays$madRain

summaryDays$doy <- as.numeric(strftime(as.POSIXlt(summaryDays$readingDate), format = "%j"))
summaryDays$year<-as.numeric(format(summaryDays$readingDate, format="%Y"))
# add in TIA data
# NOT NECESSARY !! TIA is midnight to midnight -- dataTIA$date<-dataTIA$date-1 # adjusting dates to match
temp<-dataTIA[,c(1,5)]
summaryDays<-merge(summaryDays,temp,by.x="readingDate",by.y="date", all.x = TRUE)
  summaryDays$precip[is.na(summaryDays$precip)] <- 0
summaryDays$TIAminusRainlog<-summaryDays$precip-summaryDays$medianRain
# add season 
summaryDays$month<-as.numeric(format(summaryDays$readingDate, "%m"))
summaryDays$season<-as.factor(ifelse(summaryDays$month>4 & summaryDays$month<11, "May-Oct", "Nov-Apr"))

ggplot(summaryDays, aes(as.factor(month),iqrRainwo0))+
  geom_boxplot()

# check if there are days with no observations
# allDates<-as.data.frame(seq(as.Date("2014-01-01"), as.Date("2018-12-31"),"day"))
#   colnames(allDates)<-"readingDate"
# test<-merge(allDates, summaryDays, by="readingDate", all.x = TRUE)

# plot(summaryDays$readingDate,summaryDays$PctGt1inchObsN, main="% of Rainlog Obs >1in Monsoon 2018", type='b', ylim=c(0,45))
# plot(summaryDays$readingDate,summaryDays$pctRainObsN, main="% of Rainlog Obs >0in Monsoon 2018", type='b', ylim=c(0,100))
# plot(summaryDays$readingDate,summaryDays$countObs, main="# of obs/day - Monsoon 2018", type='b')
# plot(summaryDays$readingDate,summaryDays$max, main="Max ob/day - Monsoon 2018", type='b')
# ---- END of DAILY ----

# ----- TIME SERIES PLOTS ----

# FIGURE 3 ----
# ggplot version of time series plots
minDayInYear<-summaryDays %>%
  group_by(year) %>% 
  summarize(min_countObs=min(countObs),
            min_countObsDate= readingDate[which.min(countObs)]
            )
maxDayInYear<-summaryDays %>%
  group_by(year) %>% 
  summarize(max_countObs=max(countObs),
            max_countObsDate= readingDate[which.max(countObs)]
  )

p<-ggplot(data=summaryDays, aes(x=readingDate, y=countObs)) +
  #geom_bar(stat="identity",fill="steelblue", size=3)+
  geom_line(color="steelblue")+
  ylim(0,400)+
  xlab("Date")+
  ylab("Observations/Day")+
  #ggtitle("Number of Observations/day - Tucson Area Rainlog, 2007-2018")+
  #geom_hline(yintercept = mean(summaryDays$countObs))+
  geom_point(data=minDayInYear, aes(x=min_countObsDate, y=min_countObs), color="darkred")+
  geom_point(data=maxDayInYear, aes(x=max_countObsDate, y=max_countObs), color="darkblue")
#png("/home/crimmins/RProjects/RainlogAPI/manuscript/ObsPerDay.png",
#    width = 7, height = 3, units = "in", res = 300L)
tiff("/home/crimmins/RProjects/RainlogAPI/manuscript/figs/Fig3_ObsPerDay.tif",
    width = 7, height = 3, units = "in", res = 300L)
print(p)
dev.off()
# ----

# relationship between reports and event size
ggplot(data=summaryDays, aes(x=countObs, y=medianRain, color=pctRainObsN)) +
  geom_point()+
  xlab("Number of daily observations")+
  ylab("Median daily rain event size")+
  ggtitle("Number of obs vs. rain event size - Tucson Area Rainlog, 2007-2018")+
  scale_color_viridis()

# FIGURE 4 ----
# X-Y switch - relationship between reports and event size
p<-ggplot(data=summaryDays, aes(x=medianRain*25.4, y=countObs, color=pctRainObsN)) +
  geom_point()+
  ylab("Observations/day")+
  xlab("Median daily rainfall (mm)")+
  #ggtitle("Number of obs vs. rain event size - Tucson Area Rainlog, 2007-2018")+
  scale_color_viridis(name="% non-zero")
#png("/home/crimmins/RProjects/RainlogAPI/manuscript/RainEventSize.png",
#    width = 7, height = 4, units = "in", res = 300L)
tiff("/home/crimmins/RProjects/RainlogAPI/manuscript/figs/Fig4_RainEventSize.tif",
    width = 7, height = 4, units = "in", res = 300L)
print(p)
dev.off()

# ----

# X-Y switch - relationship between reports and event size
ggplot(data=summaryDays, aes(x=medianRain, y=countObs, color=as.factor(year))) +
  geom_point()+
  ylab("Number of daily observations")+
  xlab("Median daily rain event size")+
  ggtitle("Rain event size vs. Number of obs - Tucson Area Rainlog, 2007-2018")+
  scale_color_brewer(name="Year", type='seq')

# X-Y switch - relationship between reports and event size
temp<-summaryDays[summaryDays$pctRainObsN!=0,]
ggplot(data=temp, aes(x=q90, y=countObs, color=as.factor(year))) +
  geom_point()+
  ylab("Number of daily observations")+
  xlab("90th %tile precip amount (in.)")+
  ggtitle("Rain event size vs. Number of obs - Tucson Area Rainlog, 2007-2018")
  #scale_color_viridis(name="Year")

# 2018 example
temp <- subset(summaryDays, year==2018)
ggplot(data=temp, aes(x=medianRain, y=countObs)) +
  geom_point(color="blue")+
  ylab("Number of daily observations")+
  xlab("Median daily rain event size")+
  ggtitle("Rain event size vs. Number of obs - Tucson Area Rainlog, 2018")


# median event size observed   
ggplot(data=summaryDays, aes(x=readingDate, y=medianRain)) +
  #geom_bar(stat="identity",fill="darkgreen")+
  geom_line(color="darkgreen")+
  xlab("Date")+
  ylab("precip (in)")+
  ggtitle("Median daily total precip - Tucson Area Rainlog, 2014-2018")

ggplot(data=summaryDays, aes(x=countObs, y=pctRainObsN, color=as.factor(year)))+
  geom_point(size=1)+
  scale_color_brewer(palette = "Paired", direction = -1)
  #scale_color_viridis()

ggplot(data=summaryDays, aes(x=readingDate, y=pctRainObsN, color=as.factor(year)))+
  geom_line(size=1)

# ----- END OF TIME SERIES PLOTS ----

# ---- GAUGE SUMMARY STATS ----
# generate observer stats
summaryGauges<-mergedData %>%
  group_by(gaugeRevisionId) %>%
            summarize(countObs = n(),
            max=max(rainAmount, na.rm = TRUE),
            min=min(rainAmount, na.rm = TRUE),
            rainObsN = sum(rainAmount > 0, na.rm = TRUE),
            pctRainObsN = (rainObsN/countObs)*100,
            gt1inchObsN= sum(rainAmount>1, na.rm = TRUE),
            PctGt1inchObsN = (gt1inchObsN/countObs)*100,
            countNA = sum(is.na(rainAmount)),
            lat = max(position.lat, na.rm=TRUE),
            lon = max(position.lng, na.rm = TRUE),
            elevs = max(elevation, na.rm = TRUE)
            )
summaryGauges$percMiss<-(1-(summaryGauges$countObs/length(allDates)))*100
# correct Inf elevs
summaryGauges$elevs[is.infinite(summaryGauges$elevs)] <- NA
# elev stats
mean(summaryGauges$elevs, na.rm = TRUE)
sd(summaryGauges$elevs, na.rm = TRUE)
# gauge types
gauges<-distinct(mergedData,gaugeRevisionId, .keep_all= TRUE)
table(gauges$gaugeType)/nrow(gauges)


#  FIGURE 2 ----
# map of missing 
summaryGauges$percMissFactor<-as.factor(ifelse(summaryGauges$percMiss<=10, "<=10%",">10%"))
temp<-summaryGauges[summaryGauges$percMiss<=10,]

theme_set(theme_bw(16))
p<-TucsonMap +
  geom_point(data=summaryGauges, aes(x=lon ,y=lat, color=percMissFactor, size=percMissFactor))+ # removed from AES
  #scale_color_viridis(limits=c(3900, 5000), name="# obs")+
  scale_colour_manual(values = c("#253494","#66c2a4"), name="% Missing")+
  scale_size_manual(values=c(1.5,0.5), guide='none')+
  #labs(title="Gauges in Tucson Rainlog Network (2007-2018)")+
  theme(legend.position="right",plot.title = element_text(size = 8, face = "bold"))+
  geom_point(aes(x=-110.9552, y=32.1313), shape=8, color="red")+
  geom_text(aes(x=-110.93, y=32.115), label="TIA", size=3)+
  geom_point(data = temp, aes(x=lon, y=lat), size=1.5, color="#253494", show.legend = FALSE)
# make inset
states <- map_data("state")
world<-map_data("world")
insetmap<-ggplot() + 
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill="white", color="grey")  + # get the state border back on top
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill="lightgrey", color="grey")  + # get the state border back on top
  coord_fixed(xlim=c(-125, -95), ylim=c(25, 50), ratio = 1.3) +
  geom_point(aes(x=-110.9552, y=32.1313), shape=8, color="red")+
  theme_bw(base_size=5)
vp <- viewport(width = 0.28, height = 0.28, x = 0.85, y = 0.15)
#Just draw the plot twice
#png("/home/crimmins/RProjects/RainlogAPI/manuscript/gaugeMap.png",
#    width = 7, height = 5, units = "in", res = 300L)
tiff("/home/crimmins/RProjects/RainlogAPI/manuscript/figs/Fig2_gaugeMap.tif",
    width = 7, height = 5, units = "in", res = 300L)
print(p)
print(insetmap, vp = vp)
dev.off()
# ----

# less than 10% missing
temp<-summaryGauges[summaryGauges$percMiss<=10,]
TucsonMap +
  geom_point(data=temp, aes(x=lon ,y=lat, color=percMiss), size=2)+ # removed from AES
  scale_color_viridis(limits=c(0, 10), name="% missing")+
  labs(title="Rainlog Network Gauges with <10% missing (2007-2018)")+
  theme(legend.position="right",plot.title = element_text(size = 8, face = "bold"))+
  geom_point(aes(x=-110.9552, y=32.1313), shape=8)

# hist of percent missing 
ggplot(summaryGauges, aes(percMiss))+
  geom_histogram(fill = "darkblue", alpha = 0.5)+
  #geom_vline(xintercept = 7)+
  theme_bw()+
  xlab("percent missing obs")+
  ggtitle("Percent missing days - Tucson Area Rainlog, 2007-15")
# ---- END OF GAUGE SUMMARIES ----


# ---- heat maps of daily stats ----
ggplot(summaryDays, aes(doy,year, fill=medianRain))+
  geom_tile()+
  #scale_fill_gradientn(colours = rev(terrain.colors(10)))+
  scale_fill_distiller(palette = "YlGnBu", direction = 1)+
  geom_vline(xintercept = 166, linetype="solid", 
             color = "blue", size=0.5)+
  geom_vline(xintercept = 273, linetype="solid", 
             color = "blue", size=0.5)+
  ggtitle("Median of Daily Obs - Tucson Rainloggers")

ggplot(summaryDays, aes(doy,year, fill=TIAminusRainlog))+
  geom_tile()+
  scale_fill_gradient2(low = "purple",mid = "white",high = "orange", midpoint = 0)+
  geom_vline(xintercept = 166, linetype="solid", 
             color = "blue", size=0.5)+
  geom_vline(xintercept = 273, linetype="solid", 
             color = "blue", size=0.5)+
  ggtitle("Diff between TIA and median Rainlog Obs - Tucson Rainloggers")
# ---- end of heat maps ----

# ---- time series plot ----   
ggplot(data=summaryDays, aes(x=readingDate, y=iqrRain)) +
  #geom_bar(stat="identity",fill="darkgreen")+
  geom_line(color="darkgreen")+
  xlab("Date")+
  ylab("precip (in)")+
  ggtitle("IQR of daily obs - Tucson Area Rainlog, 2007-2018")
   
ggplot(data=summaryDays, aes(x=readingDate, y=cv)) +
  #geom_bar(stat="identity",fill="darkgreen")+
  geom_line(color="darkgreen")+
  xlab("Date")+
  ylab("precip (in)")+
  ggtitle("IQR/Median of daily obs - Tucson Area Rainlog, 2007-2018")

ggplot(data=summaryDays, aes(x=readingDate, y=cvwo0)) +
  #geom_bar(stat="identity",fill="darkgreen")+
  geom_line(color="darkgreen")+
  xlab("Date")+
  ylab("precip (in)")+
  ggtitle("IQR/Median of daily obs (w/o 0's) - Tucson Area Rainlog, 2007-2018")

ggplot(data=summaryDays, aes(x=readingDate, y=madRain)) +
  #geom_bar(stat="identity",fill="darkgreen")+
  geom_line(color="darkgreen")+
  xlab("Date")+
  ylab("precip (in)")+
  ggtitle("Mean Absolute Deviation of daily obs - Tucson Area Rainlog, 2007-2018")

ggplot(data=summaryDays, aes(x=readingDate, y=IQR_MAD_diff)) +
  #geom_bar(stat="identity",fill="darkgreen")+
  geom_line(color="darkgreen")+
  xlab("Date")+
  ylab("precip (in)")+
  ggtitle("Diff in IQR-MAD of daily obs - Tucson Area Rainlog, 2007-2018")

ggplot(data=summaryDays, aes(x=readingDate, y=stdq90q10)) +
  #geom_bar(stat="identity",fill="darkgreen")+
  geom_line(color="darkgreen")+
  xlab("Date")+
  ylab("IDR/% Rain Obs")+
  ggtitle("Interdecile Range/%rain daily obs - Tucson Area Rainlog, 2007-2018")

ggplot(data=summaryDays, aes(x=readingDate, y=meanRain-medianRain)) +
  #geom_bar(stat="identity",fill="darkgreen")+
  geom_line(color="darkgreen")+
  xlab("Date")+
  ylab("IDR/% Rain Obs")+
  ggtitle("Interdecile Range/%rain daily obs - Tucson Area Rainlog, 2007-2018")
# ---- end of time series ----

# ---- scatter plots ----   
ggplot(data=summaryDays, aes(x=medianRain, y=iqrRain, color=TIAminusRainlog)) +
  #geom_bar(stat="identity",fill="darkgreen")+
  geom_point(size=0.5)+
  scale_color_gradient2(low="purple",mid="grey",high="orange", midpoint = 0, limits=c(-1, 1), oob=squish)+
  xlab("median Precip(in)")+
  ylab("IQR Precip (in)")+
  ggtitle("median vs IQR daily precip - Tucson Area Rainlog, 2007-2018")

ggplot(data=summaryDays, aes(x=PctGt1inchObsN, y=iqrRain, color=pctRainObsN)) +
  #geom_bar(stat="identity",fill="darkgreen")+
  geom_point(size=0.5)+
  scale_color_gradient2(low="purple",mid="grey",high="orange", midpoint = 50, limits=c(0, 100), oob=squish)+
  xlab("Pct obs >1in (%)")+
  ylab("IQR Precip (in)")+
  ggtitle("median vs IQR daily precip - Tucson Area Rainlog, 2007-2018")

ggplot(data=summaryDays, aes(x=medianRain, y=precip, color=pctRainObsN)) +
  #geom_bar(stat="identity",fill="darkgreen")+
  geom_point(size=0.75)+
  scale_color_gradient2(low="darkblue",mid="yellow",high="darkred", midpoint = 50, limits=c(0, 100), oob=squish)+
  xlab("Rainlog Median Precip (in)")+
  ylab("TIA Precip (in)")+
  ggtitle("Rainlog Median Precip vs. TIA Precip (in) - Tucson Area Rainlog, 2007-2018")
# ---- end of scatters ----

# ---- box and bar plots ----
# # daily boxplot
temp<-mergedData[mergedData$rainAmount!=0,]
temp$seasonGroup<-paste(temp$season,temp$year)
ggplot(data=temp, aes(x=year, y=rainAmount, group=seasonGroup, color=season))+
  geom_boxplot(varwidth = TRUE, outlier.shape = NA)+
  ylim(0,1.2)+
  scale_color_manual(values=c("#1f78b4","#ff7f00"))# outlier.shape = NA

temp<-mergedData[mergedData$rainAmount!=0,]
temp$seasonGroup<-paste(temp$season,temp$year)
ggplot(data=temp, aes(y=rainAmount, group=season, color=season))+
  geom_boxplot(varwidth = TRUE)+
  ylim(0,5)+
  scale_color_manual(values=c("#1f78b4","#ff7f00"))# outlier.shape = NA


# bar plot
topPoints<-summaryDays[sort(summaryDays$medianRain,decreasing=TRUE, index.return=TRUE)$ix[1:6],]
ggplot(data=summaryDays, aes(x=readingDate, y=medianRain, color=season, fill=season))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c("#ff7f00","#1f78b4"))+
  scale_color_manual(values=c("#ff7f00","#1f78b4"))+
  geom_point(data=topPoints, aes(x=readingDate, y=medianRain), colour="blue")
aggregate(summaryDays$medianRainwo0, list(summaryDays$season), median, na.rm=TRUE) # get stats by season

topPoints<-summaryDays[sort(summaryDays$iqrRain,decreasing=TRUE, index.return=TRUE)$ix[1:6],]
ggplot(data=summaryDays, aes(x=readingDate, y=iqrRain, color=season, fill=season))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c("#ff7f00","#1f78b4"))+
  scale_color_manual(values=c("#ff7f00","#1f78b4"))+
  geom_point(data=topPoints, aes(x=readingDate, y=iqrRain), colour="blue")  

topPoints<-summaryDays[sort(summaryDays$q9010diff,decreasing=TRUE, index.return=TRUE)$ix[1:6],]
ggplot(data=summaryDays, aes(x=readingDate, y=q9010diff, color=season, fill=season))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c("#ff7f00","#1f78b4"))+
  scale_color_manual(values=c("#ff7f00","#1f78b4"))+
  geom_point(data=topPoints, aes(x=readingDate, y=q9010diff), colour="blue")  

topPoints<-summaryDays[sort(summaryDays$cvIQR,decreasing=TRUE, index.return=TRUE,na.last = TRUE)$ix[1:6],]
ggplot(data=summaryDays, aes(x=readingDate, y=cvIQR, color=season, fill=season))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c("#ff7f00","#1f78b4"))+
  scale_color_manual(values=c("#ff7f00","#1f78b4"))+
  geom_point(data=topPoints, aes(x=readingDate, y=cvIQR), colour="blue")

topPoints<-summaryDays[sort(summaryDays$q9010diff/summaryDays$pctRainObsN,decreasing=TRUE, index.return=TRUE,na.last = TRUE)$ix[1:6],]
topPoints$var<-topPoints$q9010diff/topPoints$pctRainObsN
ggplot(data=summaryDays, aes(x=readingDate, y=summaryDays$q9010diff/summaryDays$pctRainObsN, color=season, fill=season))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c("#ff7f00","#1f78b4"))+
  scale_color_manual(values=c("#ff7f00","#1f78b4"))+
  geom_point(data=topPoints, aes(x=readingDate, y=var), colour="blue")

ggplot(data=summaryDays, aes(x=readingDate, y=max, color=season, fill=season))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c("#ff7f00","#1f78b4"))+
  scale_color_manual(values=c("#ff7f00","#1f78b4"))+
  geom_point(data=topPoints, aes(x=readingDate, y=cvIQR), colour="blue")


# ---- end of plots ----

# ---- combined bar plots ----
# library(zoo)
# p1<-ggplot(data=summaryDays, aes(x=readingDate, y=medianRain, color=season, fill=season))+
#   geom_bar(stat = "identity")+
#   scale_fill_manual(values=c("#ff7f00","#1f78b4"))+
#   scale_color_manual(values=c("#ff7f00","#1f78b4"))+
#   geom_step(aes(y=rollmean(medianRain, 30, na.pad=TRUE, align = "center"),x=readingDate), color="black")

# FIGURE 7 ----
#topPoints<-summaryDays[sort(summaryDays$medianRain,decreasing=TRUE, index.return=TRUE,na.last = TRUE)$ix[1:2],]
# select key observations
topPoints<-summaryDays[summaryDays$readingDate == "2007-07-28" | summaryDays$readingDate == "2013-11-22" | summaryDays$readingDate == "2014-09-08",]

p1<-ggplot(data=summaryDays, aes(x=readingDate, y=medianRain*25.4, color=season, fill=season))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c("#ff7f00","#1f78b4"))+
  scale_color_manual(values=c("#ff7f00","#1f78b4"))+
  geom_point(data=topPoints, aes(x=readingDate, y=medianRain*25.4), colour="blue")+
  xlab("Date")+
  ylab("DMR (mm)")

  #geom_point(data=topPoints, aes(x=readingDate, y=medianRain), colour="blue")
  #aggregate(summaryDays$medianRainwo0, list(summaryDays$season), median, na.rm=TRUE) # get stats by season
p2<-ggplot(data=summaryDays, aes(x=readingDate, y=iqrRain*25.4, color=season, fill=season))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c("#ff7f00","#1f78b4"))+
  scale_color_manual(values=c("#ff7f00","#1f78b4"))+
  geom_point(data=topPoints, aes(x=readingDate, y=iqrRain*25.4), colour="blue")+
  xlab("Date")+
  ylab("IQR (mm)")
# ----
# p3<-ggplot(data=summaryDays, aes(x=readingDate, y=cvIQR, color=season, fill=season))+
#   geom_bar(stat = "identity")+
#   scale_fill_manual(values=c("#ff7f00","#1f78b4"))+
#   scale_color_manual(values=c("#ff7f00","#1f78b4"))+
#   ylim(0,20)
p<-plot_grid(p1,p2, labels = c("a","b"),ncol = 1, align = 'v')
# write out high res map
#png("/home/crimmins/RProjects/RainlogAPI/manuscript/TopDaysTS.png", width = 9, height = 5, units = "in", res = 300L)
tiff("/home/crimmins/RProjects/RainlogAPI/manuscript/figs/Fig7_TopDaysTS.tif",
    width = 9, height = 5, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()

# ---- end of combined plots -----

# ---- table of extreme days ----
temp<-summaryDays 
temp<-temp[order( temp[,9], temp[,10]),]

# ---- end of table ----


# ---- MAPPING OF EXTREME DAYS ----
# get top 10 days based on IQR
#dateSort<-summaryDays$readingDate[sort(summaryDays$iqrRain,decreasing=TRUE, index.return=TRUE)$ix][1:6]
#dateSort<-summaryDays$readingDate[sort(summaryDays$medianRain,decreasing=TRUE, index.return=TRUE)$ix][1:6]
#dateSort<-summaryDays$readingDate[sort(summaryDays$PctGt1inchObsN,decreasing=TRUE, index.return=TRUE)$ix][1:12]
#dateSort<-summaryDays$readingDate[sort(summaryDays$pctRainObsN,decreasing=TRUE, index.return=TRUE)$ix][1:12]
#dateSort<-summaryDays$readingDate[sort(summaryDays$madRain,decreasing=TRUE, index.return=TRUE)$ix][1:12]
#dateSort<-summaryDays$readingDate[sort(summaryDays$skewRain,decreasing=FALSE, index.return=TRUE, na.last = TRUE)$ix][1:12]
#dateSort<-summaryDays$readingDate[sort(summaryDays$max,decreasing=TRUE, index.return=TRUE)$ix][1:4]
#dateSort<-summaryDays$readingDate[sort(summaryDays$traceCount,decreasing=TRUE, index.return=TRUE)$ix][1:12]
#dateSort<-summaryDays$readingDate[sort(summaryDays$cv,decreasing=TRUE, index.return=TRUE,na.last = TRUE)$ix][1:12]
#dateSort<-summaryDays$readingDate[sort(summaryDays$cvwo0,decreasing=TRUE, index.return=TRUE,na.last = TRUE)$ix][1:12]
#dateSort<-summaryDays$readingDate[sort(summaryDays$IQR_MAD_diff,decreasing=TRUE, index.return=TRUE,na.last = TRUE)$ix][1:12]
#dateSort<-summaryDays$readingDate[sort(summaryDays$stdq90q10,decreasing=TRUE, index.return=TRUE,na.last = TRUE)$ix][1:12]
dateSort<-summaryDays$readingDate[sort(summaryDays$q9010diff/summaryDays$pctRainObsN,decreasing=TRUE, index.return=TRUE,na.last = TRUE)$ix][1:6]
#dateSort<-summaryDays$readingDate[sort(summaryDays$q9010diff,decreasing=TRUE, index.return=TRUE,na.last = TRUE)$ix][1:6]
#dateSort<-summaryDays$readingDate[sort(summaryDays$q9010diff/summaryDays$medianRain,decreasing=TRUE, index.return=TRUE,na.last = TRUE)$ix][1:6]
#dateSort<-summaryDays$readingDate[sort(summaryDays$cvIQR,decreasing=TRUE, index.return=TRUE,na.last = TRUE)$ix][1:6]

# or selected toppoints
dateSort<-topPoints$readingDate

# get subset of summaryDays for facet stats
summaryDaysSubset <- summaryDays[summaryDays$readingDate %in% dateSort, ]
summaryDaysSubset$readingDate<-factor(as.character(summaryDaysSubset$readingDate),levels=as.character(rev(dateSort)))

# manual date sort
#dateSort<-c(as.Date("2011-09-15"), as.Date("2014-09-08"))

# create subset
mergedDataSubset <- mergedData[mergedData$readingDate %in% dateSort, ]
mergedDataSubset$readingDate<-factor(as.character(mergedDataSubset$readingDate),levels=as.character(rev(dateSort)))

# clean up the dataset -- PROBLEM HERE, eliminating good obs
#mergedDataSubset<-mergedDataSubset[mergedDataSubset$quality %in% c('Good','Trace'),]

# create 0-symbol 
#mergedDataSubset$zeroDay<- mergedDataSubset$rainAmount ==0
# control symbols, rain, trace, zero
mergedDataSubset$symbol <- ifelse(mergedDataSubset$quality=="Trace", 1, ifelse(mergedDataSubset$rainAmount==0, 2, 0) ) 
# blank out positive 0 reports
mergedDataSubset$rainAmount[mergedDataSubset$rainAmount == 0] <- NA
# ---

# get TIA data for subset days
subTIAdata <- dataTIA[dataTIA$date %in% dateSort, ]
colnames(subTIAdata)[1]<-"readingDate"
subTIAdata$readingDate<-factor(as.character(subTIAdata$readingDate),levels=as.character(rev(dateSort)))


# Top Points table...
topPointsTable<-topPoints[,c(1,2,3,5,7,9,10,17,29)]
topPointsTable

# FIGURE 8 ----
# mapping stuff
theme_set(theme_bw(16))
p<-TucsonMap +
  geom_point(data=mergedDataSubset, aes(x=position.lng ,y=position.lat, color=rainAmount*25.4, shape=as.factor(symbol)), size=1.0)+ # removed from AES
  facet_wrap(~readingDate,nrow = 1, ncol=3)+
  #facet_wrap(~readingDate, ncol = 15, nrow = ceiling(length(allDates)/15))+ # 15 for whole season
  scale_shape_manual(values=c(16,utf8ToInt("T"),utf8ToInt("0")), guide=FALSE)+ # outline=21, plus=3
  # scale_color_gradient2(limits=c(0, 1), mid=("yellow"), high="red", low="cyan", oob=squish, midpoint = 0.5, name="Precip (in)",
  #                       labels = c("0.00", "0.25", "0.5", "0.75", "≥ 1.00"),
  #                       breaks = c(0, 0.25, 0.5, 0.75, 1.0), na.value="black")+
  # scale_color_gradient2(limits=c(0, 3), mid=("#41b6c4"), high="#081d58", low="#edf8b1", oob=squish, midpoint = 1.5, name="Precip (in)",
  #                       labels = c("0.01", "1.00", "2.00", "≥ 3.00"),
  #                       breaks = c(0.01, 1.00, 2.00, 3.00),
  #                       na.value="darkred")+
  # scale_color_viridis(limits=c(0, 3), oob=squish, name="Precip (in)",
  #                       labels = c("0.01", "1.00", "2.00", "≥ 3.00"),
  #                       breaks = c(0.01, 1.00, 2.00, 3.00),
  #                       na.value="darkred", direction=-1)+
  scale_color_viridis(limits=c(0, 75), oob=squish, name="Precip (mm)",
                      labels = c("1", "25", "50", "≥ 75"),
                      breaks = c(1, 25, 50, 75),
                      na.value="darkred", direction=-1)+
    #labs(title="Top Extreme Days - Tucson Rainlog Network (2007-2018)")+
  theme(legend.position="bottom",plot.title = element_text(size = 8, face = "bold"))+
  theme(legend.text = element_text(colour="black", size=10))+
  theme(legend.title = element_text(colour="black", size=10))+
  theme(strip.background = element_rect(fill="white"))+
# add stats to facets
annotate("rect", xmin = -111.35, xmax = -111.15, ymin = 31.88, ymax = 32.01,
         alpha = 1, color="black", fill="white")+
  geom_point(aes(x=-110.9552, y=32.1313), shape=8)+
  geom_text(data=subTIAdata, aes(x=-110.9, y=32.1, label=round(precip*25.4,1)), size=3)

labs<-summaryDaysSubset[,c("readingDate","countObs", "max","medianRain","iqrRain")]
labs$xNum<--111.345; labs$yNum<-31.98
labs$xMax<--111.345; labs$yMax<-31.95
labs$xMed<--111.345; labs$yMed<-31.92
labs$xIQR<--111.345; labs$yIQR<-31.89

p<-p + geom_text(data=labs, aes(x=xNum, y=yNum, 
                                        label=paste0("n Obs: ", countObs)), color="black",  size=2,
                    vjust=0,hjust=0, parse=FALSE)+
        geom_text(data=labs, aes(x=xMax, y=yMax, 
                           label=paste0("Max: ", round(max*25.4,1))), color="black",  size=2,
            vjust=0,hjust=0, parse=FALSE)+
         geom_text(data=labs, aes(x=xMed, y=yMed, 
                           label=paste0("Median: ", round(medianRain*25.4,1))), color="black",  size=2,
            vjust=0,hjust=0, parse=FALSE)+
        geom_text(data=labs, aes(x=xIQR, y=yIQR,
                           label=paste0("IQR: ",round(iqrRain*25.4,1))), color="black",  size=2,
            vjust=0,hjust=0, parse=FALSE)

# write out high res map
# png("/home/crimmins/RProjects/RainlogAPI/manuscript/top3map.png",
#     width = 7, height = 4, units = "in", res = 300L)
tiff("/home/crimmins/RProjects/RainlogAPI/manuscript/figs/Fig8_top3map.tif",
    width = 7, height = 4, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()


# map of number of observations for Tucson area
# get map and bounding box
where<-geocode("tucson", source = "dsk")
TucsonMap <- qmap(location = c(lon = where[1,1], lat = where[1,2]), zoom = 10,
                  color = "bw")

TucsonMap <- qmap(location = c(lon = where[1,1], lat = where[1,2]), zoom = 10,
                  source = "google", maptype = "roadmap", color="bw")


# compare with grids
library(raster)
load("~/RProjects/RainlogAPI/manuscript/prismGridData.RData")

# GET grid value for each day/location
for(i in 1:nrow(mergedDataSubset)){
  mergedDataSubset$gridPrecip[i]<-extract(prismStack[[which(allDates==as.Date(mergedDataSubset$readingDate[i]))]], 
                                cellFromXY(prismStack[[1]], c(mergedDataSubset$position.lng[i],mergedDataSubset$position.lat[i])))
  print(i)
}
# get diffs
#allObs$gridPrecip[allObs$gridPrecip < 0] <- NA # missing data to NA
#allObs$gridDiff<-allObs$rainAmount-allObs$gridPrecip





# # leaflet map of days for data checking
# mergedDataSubsetLeaflet<-mergedDataSubset[which(mergedDataSubset$readingDate=="2012-07-15"),]
# library(leaflet)
# # Create a palette that maps factor levels to colors
# pal <- colorNumeric(
#   palette = "Spectral",
#   domain = mergedDataSubsetLeaflet$rainAmount)
# 
# leaflet(mergedDataSubsetLeaflet) %>% addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
#   addCircleMarkers(lng = ~position.lng, lat = ~position.lat,
#                    radius=3,
#                    color = ~pal(rainAmount),
#                    stroke = FALSE,
#                    fillOpacity=1,
#                    popup = (as.character(mergedDataSubsetLeaflet$rainAmount))
#   )
#test<-dataStack[which(dataStack$gaugeId==5523),]

hist(mergedData[which(mergedData$readingDate=="2015-09-20"),])

# point density calculations ----
library(pointdensityP)


temp<-mergedData[c("readingDate","position.lat","position.lng")]
temp<-temp[complete.cases(temp), ]

reading_density <- pointdensity(df = temp, lat_col = "position.lat", lon_col = "position.lng",
                               date_col = "readingDate", grid_size = 1, radius = 5)

# API key
source('APIkey.R')
# get map and bounding box
where<-geocode("tucson", source = "dsk")
TucsonMap <- qmap(location = c(lon = where[1,1], lat = where[1,2]), zoom = 10,
                  source = "google", maptype = "roadmap", color="bw")

TucsonMap + geom_point(aes(x = lon, y = lat, colour = count), shape = 16, size = 2,
                      data = reading_density) + scale_colour_gradient(low = "green", high = "red")

# end point density ----

# Less than 10% missing gauge stats ----

# get list of gauges with <10% missing
temp<-summaryGauges[summaryGauges$percMiss<=10,]
mergedDataHQ<- mergedData[mergedData$gaugeRevisionId %in% temp$gaugeRevisionId,]

# remove 2007 half season for full period stats
mergedDataHQ2<-mergedDataHQ
mergedDataHQ2$rainAmount[which(mergedDataHQ2$season=="Nov-Apr" & mergedDataHQ2$year=="2007")] <- NA
#mergedDataHQ2$rainAmount[which(mergedDataHQ2$readingDate<as.Date("2007-05-01"))] <- NA
#mergedDataHQ2<-subset(mergedDataHQ2, readingDate>=as.Date("2007-05-01"))

# cool/warm season stats
seasonalMean<-mergedDataHQ2 %>%
  group_by(gaugeRevisionId,season) %>%
  summarize(countObs = n(),
            meanP = sum(rainAmount, na.rm = TRUE)/12,
            lat = max(position.lat, na.rm=TRUE),
            lon = max(position.lng, na.rm = TRUE),
            elevs = max(elevation, na.rm = TRUE)
            )
# TIA summary
# remove 2007 half season for full period stats
summaryDays2<-summaryDays
summaryDays2$precip[which(summaryDays2$season=="Nov-Apr" & summaryDays2$year=="2007")] <- NA
# TIA data
TIAmean<-summaryDays2 %>%
  group_by(season) %>%
  summarize(countObs = n(),
            meanP = sum(precip, na.rm = TRUE)/12)
#levels(TIAmean$season)[levels(TIAmean$season)=="May-Oct"] <- "Warm Season"
#levels(TIAmean$season)[levels(TIAmean$season)=="Nov-Apr"] <- "Cool Season"

#levels(seasonalMean$season)<-c("Nov-Apr","May-Oct")
#levels(TIAmean$season)<-c("Nov-Apr","May-Oct")
seasonalMean$season <- factor(seasonalMean$season,
                       levels = c("Nov-Apr","May-Oct"),ordered = TRUE)
TIAmean$season <- factor(TIAmean$season,
                              levels = c("Nov-Apr","May-Oct"),ordered = TRUE)

# boxplot - seasonal means
p<-ggplot(data=seasonalMean, aes(y=meanP*25.4, x=season, group=(season), color=(season)))+
  geom_boxplot(varwidth = TRUE, fill="grey85")+
  ylim(0,400)+
  scale_color_manual(values=c("#1f78b4","#ff7f00"))+
  theme(legend.position="none")+
  #ggtitle("  ")+
  ylab("mm")+
  geom_hline(yintercept = 4.34*25.4, color="#1f78b4")+
  geom_hline(yintercept = 7.25*25.4, color="#ff7f00")# outlier.shape = NA

p1<- p+geom_point(data=TIAmean, aes(y=meanP*25.4, x=season), shape=8, size=3) # color='black'

              
# cool/warm season stats - BY YEAR
seasonalMeanYear<-mergedDataHQ %>%
  group_by(gaugeRevisionId,season, year) %>%
  summarize(countObs = n(),
            meanP = sum(rainAmount, na.rm = TRUE),
            lat = max(position.lat, na.rm=TRUE),
            lon = max(position.lng, na.rm = TRUE),
            elevs = max(elevation, na.rm = TRUE)
  )
# get rid of stations with missing vals <10%
seasonalMeanYear<-seasonalMeanYear[seasonalMeanYear$countObs>=164,]

# TIA summary
TIAmeanYearly<-summaryDays %>%
  group_by(season, year) %>%
  summarize(countObs = n(),
            meanP = sum(precip, na.rm = TRUE))
#levels(TIAmeanYearly$season)[levels(TIAmeanYearly$season)=="May-Oct"] <- "Warm Season"
#levels(TIAmeanYearly$season)[levels(TIAmeanYearly$season)=="Nov-Apr"] <- "Cool Season"

# boxplot - seasonal means and orders
seasonalMeanYear$seasonGroup<-as.factor(paste(seasonalMeanYear$season,seasonalMeanYear$year))
TIAmeanYearly$seasonGroup<-as.factor(paste(TIAmeanYearly$season, TIAmeanYearly$year))

seasonalMeanYear$season <- factor(seasonalMeanYear$season,
                              levels = c("Nov-Apr","May-Oct"),ordered = TRUE)
TIAmeanYearly$season <- factor(TIAmeanYearly$season,
                         levels = c("Nov-Apr","May-Oct"),ordered = TRUE)

seasonalMeanYear$seasonGroup<-factor(seasonalMeanYear$seasonGroup,levels = rev(levels(seasonalMeanYear$seasonGroup)),ordered = TRUE)
TIAmeanYearly$seasonGroup<-factor(TIAmeanYearly$seasonGroup,levels = rev(levels(TIAmeanYearly$seasonGroup)),ordered = TRUE)

# censor out Nov-Apr 2007 - incomplete season
seasonalMeanYear$meanP[which(seasonalMeanYear$season=="Nov-Apr" & seasonalMeanYear$year=="2007")] <- -99
TIAmeanYearly$meanP[which(TIAmeanYearly$season=="Nov-Apr" & TIAmeanYearly$year=="2007")] <- -99


# add long-term mean for TIA from 1981-2010 http://drought.rcc-acis.org/
# cool season 4.34
# warm season 7.25

# FIGURE 6 
p2<-ggplot(data=seasonalMeanYear, aes(y=meanP*25.4,x=year,group=seasonGroup, color=season))+
  geom_boxplot(varwidth = FALSE,fill="grey85")+ # position_dodge single
  #ylim(0,10*25.4)+
  scale_x_continuous(breaks=seq(2008,2018,2))+
  scale_color_manual(values=c("#1f78b4","#ff7f00"))+
  ylab("mm")+
  #ggtitle("Near-complete Rainlog Stations (2007-18)")+# outlier.shape = NA
geom_point(data=TIAmeanYearly, aes(y=meanP*25.4,x=year,group=seasonGroup, color=season), shape=8, size=3,position=position_dodge(width=0.75))+
geom_hline(yintercept = 4.34*25.4, color="#1f78b4")+
  geom_hline(yintercept = 7.25*25.4, color="#ff7f00")+
  coord_cartesian(ylim = c(0, 400))
  
#p+geom_bar(data=TIAmeanYearly, aes(y=meanP,x=year,fill=season, group=seasonGroup),stat="identity")
p<-plot_grid(p1,p2, labels = c("a","b"),rel_widths = c(1, 4))

# write out high res map
# png("/home/crimmins/RProjects/RainlogAPI/manuscript/seasonalHist.png", 
#     width = 12, height = 4, units = "in", res = 300L)
tiff("/home/crimmins/RProjects/RainlogAPI/manuscript/figs/Fig6_seasonalHist.tif", 
    width = 12, height = 4, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()


# Figure 9 ----
# ----- case study map from 2018

subMergedData<-mergedData[mergedData$readingDate >= "2018-07-07" & mergedData$readingDate <= "2018-07-10",]

# control symbols, rain, trace, zero
subMergedData$symbol <- ifelse(subMergedData$quality=="Trace", 1, ifelse(subMergedData$rainAmount==0, 2, 0) ) 
# blank out positive 0 reports
subMergedData$rainAmount[subMergedData$rainAmount == 0] <- NA

# get TIA data
subTIAdata <- dataTIA[dataTIA$date %in% unique(subMergedData$readingDate), ]
colnames(subTIAdata)[1]<-"readingDate"
  subTIAdata$precip<-round(subTIAdata$precip*25.4,1)
subTIAdata[is.na(subTIAdata)] <- "T"

theme_set(theme_bw(16))
p<-TucsonMap +
  geom_point(data=subMergedData, aes(x=position.lng ,y=position.lat, color=rainAmount*25.4, shape=as.factor(symbol)), size=1.5)+ # removed from AES
  facet_wrap(~readingDate,nrow = 2, ncol=2)+
  #facet_wrap(~readingDate, ncol = 15, nrow = ceiling(length(allDates)/15))+ # 15 for whole season
  scale_shape_manual(values=c(16,utf8ToInt("T"),utf8ToInt("0")), guide=FALSE)+ # outline=21, plus=3
  # scale_color_viridis(limits=c(0, 3), oob=squish, name="Precip (in)",
  #                     labels = c("0.01", "1.00", "2.00", "≥ 3.00"),
  #                     breaks = c(0.01, 1.00, 2.00, 3.00),
  #                     na.value="darkred", direction=-1)+
  scale_color_viridis(limits=c(0, 75), oob=squish, name="Precip (mm)",
                      labels = c("1", "25", "50", "≥ 75"),
                      breaks = c(1, 25, 50, 75),
                      na.value="darkred", direction=-1)+
  # scale_color_gradientn(colors =  jet.colors(7),
  #                       limits=c(0, 1),name="Precip (in)",
  #                       labels = c("0.00", "0.25", "0.5", "0.75", "≥ 1.00"),
  #                       breaks = c(0, 0.25, 0.5, 0.75, 1.0), na.value="grey48")+
  #labs(title="Rainlog Precipitation Observations - Tucson, Early July 2018")+
  theme(strip.background = element_rect(fill="white"))+
  geom_point(aes(x=-110.9552, y=32.1313), shape=8)+
  geom_text(data=subTIAdata, aes(x=-110.9, y=32.1, label=precip), size=3)

# write out high res map
# png("/home/crimmins/RProjects/RainlogAPI/manuscript/July18_caseStudy.png",
#     width = 8, height = 7, units = "in", res = 300L)
# write out high res map
tiff("/home/crimmins/RProjects/RainlogAPI/manuscript/figs/Fig9_July18_caseStudy.tif",
    width = 8, height = 7, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()
