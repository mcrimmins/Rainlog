# Reverse geocoding for Rainlog sites
# MAC 03/16/19

library(revgeo)
library(dplyr)

load("~/RProjects/RainlogAPI/gaugeStackData.RData")

# all at once
#gaugeStack<-gaugeStack[1:10,]
#location<-revgeo(gaugeStack$position.lng, gaugeStack$position.lat, output = 'frame')

#gaugeStack<-gaugeStack[1:1000,] # file1
#gaugeStack<-gaugeStack[1001:2000,] # file2
#gaugeStack<-gaugeStack[2001:3000,] # file3
gaugeStack<-gaugeStack[3001:4237,] # file4



# https://towardsdatascience.com/reverse-geocoding-in-r-f7fe4b908355
# Step 1: Create a blank dataframe to store results.
data_all = data.frame()
start <- Sys.time()

# Step 2: Create a while loop to have the function running until the # dataframe with 100,000 rows is empty.
while (nrow(gaugeStack)>0) {
  # Step 3: Subset the data even further so that you are sending only # a small portion of requests to the Photon server.
  main_sub_t <-  gaugeStack[1:100,]
  # Step 4: Extracting the lat/longs from the subsetted data from
  # the previous step (Step 3).
  latlong <- main_sub_t %>% 
    select(position.lat, position.lng, gaugeRevisionId) %>% 
    #unique() %>% 
    mutate(index=row_number())
  
  
  # Step 5: Incorporate the revgeo package here. I left_joined the 
  # output with the latlong dataframe from the previous step to add 
  # the latitude/longitude information with the reverse geocoded data.
  cities <- revgeo(latlong$position.lng, latlong$position.lat, output = 'frame') %>% 
  mutate(index = row_number(),country = as.character(country)) %>%
  filter(country == 'United States of America') %>% 
  mutate(location = paste(city, state, zip, sep = ", ")) %>% 
  select(index, location) %>% 
  left_join(latlong, by="index") %>% 
  select(-index)

# Removing the latlong dataframe because I no longer need it. This 
# helps with reducing memory in my global environment.
rm(latlong)


# Step 6: Adding the information from the cities dataframe to 
# main_sub_t dataframe (from Step 3).

# data_new <- main_sub_t %>% 
#   left_join(cities, by=c("position.lat","position.lng")) # %>% 
  #select(X, text, location, position.lat, position.lng)


# Step 7: Adding data_new into the empty data_all dataframe where 
# all subsetted reverse geocoded data will be combined.

data_all <- bind_rows(data_all,cities) #%>% 
  #na.omit()


# Step 8: Remove the rows that were used in the first loop from the # main_sub frame so the next 200 rows can be read into the while # loop.

gaugeStack <- anti_join(gaugeStack, main_sub_t, by=c("gaugeRevisionId"))
print(nrow(gaugeStack))

# Remove dataframes that are not needed before the while loop closes # to free up space.
#rm(data_sub_t)
#rm(data_new)
#rm(latlong_1)
rm(cities)

# garbage collection
gc()
# pause
print('Sleeping for 10 seconds')
Sys.sleep(10)

}
# save file parts
geocodes4<-data_all

save(geocodes4, file="./geocodes/geocoded_Gauges_4.RData")
end <- Sys.time()

# combine files
load("~/RProjects/RainlogAPI/geocodes/geocoded_Gauges_1.RData")
load("~/RProjects/RainlogAPI/geocodes/geocoded_Gauges_2.RData")
load("~/RProjects/RainlogAPI/geocodes/geocoded_Gauges_3.RData")
load("~/RProjects/RainlogAPI/geocodes/geocoded_Gauges_4.RData")

geocodes<-rbind(geocodes1,geocodes2,geocodes3,geocodes4)

# seperate geocodes in cols
library(tidyr)
geocodes<-separate(geocodes, location, c("City","State","Zip"), sep=",")
# save to file
save(geocodes, file="./geocodes/geocoded_ALLGauges.RData")

