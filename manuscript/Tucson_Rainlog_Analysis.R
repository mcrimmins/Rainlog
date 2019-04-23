# Pull and process Tucson area Rainlog obs for manuscript analysis
# MAC 3/21/19
# adapted from getALLRainlog.R

library(scales)
library(dplyr)
library(cowplot)
library(ggmap)
library(moments)
library(viridis)

# API key
source('APIkey.R')
# get map and bounding box
where<-geocode("tucson", source = "dsk")
TucsonMap <- qmap(location = c(lon = where[1,1], lat = where[1,2]), zoom = 10,
                  color = "bw")

TucsonMap <- qmap(location = c(lon = where[1,1], lat = where[1,2]), zoom = 10,
                  source = "google", maptype = "roadmap", color="bw")


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
mergedData$season<-as.factor(ifelse(mergedData$month>4 & mergedData$month<11, "Warm Season", "Cool Season"))
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
# histogram
ggplot(mergedData, aes(as.factor(month)))+
  geom_histogram(fill = "skyblue", alpha = 1, stat = "count")+
  #geom_vline(xintercept = 7)+
  theme_bw()+
  xlab("month")+
  scale_y_continuous(limits=c(0,100000),oob = rescale_none)+
  ggtitle("Total Tucson Area Rainlog observations by month (2007-2018)")

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

ggplot(data=summaryDays, aes(x=readingDate, y=countObs)) +
  #geom_bar(stat="identity",fill="steelblue", size=3)+
  geom_line(color="steelblue")+
  ylim(0,400)+
  xlab("Date")+
  ylab("Number of Observations")+
  ggtitle("Number of Observations/day - Tucson Area Rainlog, 2007-2018")+
  #geom_hline(yintercept = mean(summaryDays$countObs))+
  geom_point(data=minDayInYear, aes(x=min_countObsDate, y=min_countObs), color="darkred")+
  geom_point(data=maxDayInYear, aes(x=max_countObsDate, y=max_countObs), color="darkblue")


# relationship between reports and event size
ggplot(data=summaryDays, aes(x=countObs, y=medianRain, color=pctRainObsN)) +
  geom_point()+
  xlab("Number of daily observations")+
  ylab("Median daily rain event size")+
  ggtitle("Number of obs vs. rain event size - Tucson Area Rainlog, 2007-2018")+
  scale_color_viridis()
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

# map of missing 
theme_set(theme_bw(16))
TucsonMap +
  geom_point(data=summaryGauges, aes(x=lon ,y=lat, color=countObs), size=0.5)+ # removed from AES
  scale_color_viridis(limits=c(2, 5000), name="# obs")+
  labs(title="Number of Observations at each gauge in Tucson Rainlog Network (2007-2018)")+
  theme(legend.position="right",plot.title = element_text(size = 8, face = "bold"))


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
ggplot(data=temp, aes(x=year, y=rainAmount, group=year))+
  geom_boxplot() # outlier.shape = NA

# bar plot
topPoints<-summaryDays[sort(summaryDays$medianRain,decreasing=TRUE, index.return=TRUE)$ix[1:6],]
ggplot(data=summaryDays, aes(x=readingDate, y=medianRain, color=season, fill=season))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c("#ff7f00","#1f78b4"))+
  scale_color_manual(values=c("#ff7f00","#1f78b4"))+
  geom_point(data=topPoints, aes(x=readingDate, y=medianRain), colour="blue")

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
# ---- end of plots ----


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


# get subset of summaryDays for facet stats
summaryDaysSubset <- summaryDays[summaryDays$readingDate %in% dateSort, ]
summaryDaysSubset$readingDate<-factor(as.character(summaryDaysSubset$readingDate),levels=as.character(dateSort))

# manual date sort
#dateSort<-c(as.Date("2011-09-15"), as.Date("2014-09-08"))

# create subset
mergedDataSubset <- mergedData[mergedData$readingDate %in% dateSort, ]
mergedDataSubset$readingDate<-factor(as.character(mergedDataSubset$readingDate),levels=as.character(dateSort))

# clean up the dataset -- PROBLEM HERE, eliminating good obs
#mergedDataSubset<-mergedDataSubset[mergedDataSubset$quality %in% c('Good','Trace'),]

# create 0-symbol 
#mergedDataSubset$zeroDay<- mergedDataSubset$rainAmount ==0
# control symbols, rain, trace, zero
mergedDataSubset$symbol <- ifelse(mergedDataSubset$quality=="Trace", 1, ifelse(mergedDataSubset$rainAmount==0, 2, 0) ) 
# blank out positive 0 reports
mergedDataSubset$rainAmount[mergedDataSubset$rainAmount == 0] <- NA
# ---

# mapping stuff
theme_set(theme_bw(16))
p<-TucsonMap +
  geom_point(data=mergedDataSubset, aes(x=position.lng ,y=position.lat, color=rainAmount, shape=as.factor(symbol)), size=1.0)+ # removed from AES
  facet_wrap(~readingDate,nrow = 3, ncol=2)+
  #facet_wrap(~readingDate, ncol = 15, nrow = ceiling(length(allDates)/15))+ # 15 for whole season
  scale_shape_manual(values=c(16,utf8ToInt("T"),utf8ToInt("0")), guide=FALSE)+ # outline=21, plus=3
  # scale_color_gradient2(limits=c(0, 1), mid=("yellow"), high="red", low="cyan", oob=squish, midpoint = 0.5, name="Precip (in)",
  #                       labels = c("0.00", "0.25", "0.5", "0.75", "≥ 1.00"),
  #                       breaks = c(0, 0.25, 0.5, 0.75, 1.0), na.value="black")+
  # scale_color_gradient2(limits=c(0, 3), mid=("#41b6c4"), high="#081d58", low="#edf8b1", oob=squish, midpoint = 1.5, name="Precip (in)",
  #                       labels = c("0.01", "1.00", "2.00", "≥ 3.00"),
  #                       breaks = c(0.01, 1.00, 2.00, 3.00),
  #                       na.value="darkred")+
  scale_color_viridis(limits=c(0, 3), oob=squish, name="Precip (in)",
                        labels = c("0.01", "1.00", "2.00", "≥ 3.00"),
                        breaks = c(0.01, 1.00, 2.00, 3.00),
                        na.value="darkred", direction=-1)+
    labs(title="Top 6 Days with highest ratio of Interdecile range to % precip obs in Tucson Rainlog Network (2007-2018)")+
  theme(legend.position="bottom",plot.title = element_text(size = 8, face = "bold"))+
  theme(legend.text = element_text(colour="black", size=10))+
  theme(legend.title = element_text(colour="black", size=10))+
# add stats to facets
annotate("rect", xmin = -111.35, xmax = -111.19, ymin = 31.87, ymax = 32.00,
         alpha = 1, color="black", fill="white")  

labs<-summaryDaysSubset[,c("readingDate","countObs", "max","medianRain")]
labs$xNum<--111.345; labs$yNum<-31.98
labs$xMax<--111.345; labs$yMax<-31.95
labs$xMed<--111.345; labs$yMed<-31.92

p<-p + geom_text(data=labs, aes(x=xNum, y=yNum, 
                                        label=paste0("n Obs: ", countObs)), color="black",  size=2,
                    vjust=0,hjust=0, parse=FALSE)+
        geom_text(data=labs, aes(x=xMax, y=yMax, 
                           label=paste0("Max: ", max)), color="black",  size=2,
            vjust=0,hjust=0, parse=FALSE)+
         geom_text(data=labs, aes(x=xMed, y=yMed, 
                           label=paste0("Median: ", medianRain)), color="black",  size=2,
            vjust=0,hjust=0, parse=FALSE)

# write out high res map
png("/home/crimmins/RProjects/RainlogAPI/manuscript/6map.png", width = 8.5, height = 11, units = "in", res = 300L)
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

# point density calculations
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


