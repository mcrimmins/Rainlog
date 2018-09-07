# SpaceTime and Interpolation of Rainlog data
# MAC 6/13/18

#library(plyr)
library(RCurl)
library(jsonlite)
library(spacetime)

dateRangeStart="2017-06-15"
dateRangeEnd="2017-09-30"

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

# join data frames
mergedData <- merge(dataStack,gaugeStack,by="gaugeId")

# look for outliers
#stats<-boxplot(mergedData$rainAmount)
#which.max(mergedData$rainAmount)
#mergedData<-mergedData[-(which.max(mergedData$rainAmount)), ]

# use more elaborate outlier detection
# account for missing days too...



# try out automap on date range total
library(dplyr)
library(automap)
library(RColorBrewer)
gaugeSum<-mergedData %>%
  group_by(gaugeId) %>%
  summarize(totalPrecip=sum(rainAmount), lat=first(position.lat), lon=last(position.lng))

# look for outliers
stats<-boxplot(gaugeSum$totalPrecip)
gaugeSum<-gaugeSum[-(which.max(gaugeSum$totalPrecip)), ]

coordinates(gaugeSum) =~ lon+lat
gaugeSum@proj4string<-CRS("+proj=longlat +datum=WGS84")
# reproject to UTM
gaugeSumUTM <- spTransform(gaugeSum, CRS("+proj=utm +zone=12 ellps=WGS84"))
# make grid
gaugeGrid <- makegrid(gaugeSumUTM, cellsize = 500) # cellsize in map units!
gridded(gaugeGrid) =~ x1+x2
gaugeGrid@proj4string<-CRS("+proj=utm +zone=12 ellps=WGS84")
kriging_result = autoKrige(totalPrecip~1, gaugeSumUTM, gaugeGrid)
automapPlot(kriging_result$krige_output, "var1.pred", at=seq(6,11,0.5), col.regions=brewer.pal(11, "Spectral"),
            sp.layout = list("sp.points", gaugeSumUTM))


# spaceTime data frames
mergedData$readingDate<-as.Date(mergedData$readingDate)
x = stConstruct(mergedData, c("position.lng", "position.lat"), "readingDate", SpatialObj = pts)
stplot(x[,,"rainAmount"], number=30, cuts=12)

library(ggmap)
library(sp)
#tucson<-qmap("Tucson", zoom = 12)


# http://www.nickeubank.com/wp-content/uploads/2015/10/RGIS3_MakingMaps_part1_mappingVectorData.html
# COPY AND PASTE SEGEMENT 1 Series of weird conversions to deal with
# inconsistencies in units for API.

# REPROJECT YOUR DATA TO EPSG 3857
x@sp@proj4string <- CRS("+proj=longlat +datum=WGS84")
xSp <- spTransform(x, CRS("+init=EPSG:3857"))

box <- x@sp@bbox

midpoint <- c(mean(box[1, ]), mean(box[2, ]))
left.bottom <- c(box[1, 1], box[2, 1])
top.right <- c(box[1, 2], box[2, 2])

boundaries <- SpatialPoints(rbind(left.bottom, top.right))
boundaries.latlong <- c(t(boundaries@coords))
boundaries.latlong<-boundaries.latlong[c(2,1,4,3)]
# END COPY-PASTE SEGMENT 1


# SET MAP TYPE HERE, LEAVE OTHER PARAMETERS AS THEY ARE
gmap <- get_map(boundaries.latlong, maptype = "terrain", source = "google", 
                crop = TRUE)

# COPY-PASTE SEGMENT 2 Create object that sp.layout likes.
long.center <- midpoint[1]
lat.center <- midpoint[2]
height <- box[2, 2] - box[2, 1]
width <- box[1, 2] - box[1, 1]

sp.raster <- list("grid.raster", gmap, x = long.center, y = lat.center, width = width, 
                  height = height, default.units = "native", first = TRUE)
# END COPY-PASTE SEGMENT 2

# Housecleaning and set colors
#my.palette <- c("red", "blue")
point.size <- 0.5
#spplot(to.plot.web.merc, "ethnicity", sp.layout = sp.raster, col.regions = my.palette, 
#       cex = point.size, main = "Demographic Distribution of Santa Clara County")

# Plot!
stplot(x[,,"rainAmount"], number=2, cuts=12, sp.layout = sp.raster, 
       cex = point.size, main = "Rainlog Observations July 2017", key.space="left")

