#Loading Packages
library(dplyr)
library(sf)
library(units)
library(ggplot2)
library(ggpubr)
library(plotly)
#Loading in Data
#Location of Loving trailer coordinates(32.297345/lat,-104.109386/long) 
Loving_Trailer_Coordinates <- data.frame(longitude = -104.109386,latitude = 32.297345)
Loving_Trailer_Coordinates <- st_as_sf(Loving_Trailer_Coordinates, coords = c("longitude", "latitude"), crs = 4326)

#Trailer Data (Ends June 13th 2023)
Trailer_Data <- readRDS("TrailerData/Merged_Data.rds")
Trailer_Data <- Trailer_Data %>% filter(time != "2023-06-06") #filter this out

Trailer_Data_HOUR <- readRDS("TrailerData/Merged_Data_hourly.rds")



#Flaring Data (Ends June 15th 2023)
Flaring_Data <- readRDS("FlaringData/VNF_data_final_2012-2023.rds") %>% filter(date >= "2023-04-15" & date <= "2023-06-13")
Flaring_Data$longitude <- Flaring_Data$long
Flaring_Data$latitude <- Flaring_Data$lat
Flaring_Data <- st_as_sf(Flaring_Data, coords = c("longitude", "latitude"), crs = 4326)


#---------------------------------------------Flaring distance to the Trailer -----------------------#
#Flaring Data for 10km (NOTE 32613 is in METERS)
buffer_10km <- Loving_Trailer_Coordinates %>% st_transform(crs = 32613) %>% st_buffer(dist = 10000) %>% st_transform(crs = 4326)
Flaring_Data_10km <- Flaring_Data[lengths(st_within(Flaring_Data, buffer_10km)) > 0, ]
Flaring_Data_10km <- Flaring_Data_10km %>% mutate(dist_trailer = st_distance(geometry, Loving_Trailer_Coordinates, by_element= FALSE))
Flaring_Data_10km$dist_trailer_km <- set_units(Flaring_Data_10km$dist_trailer, "km") #converting to km since dist_trailer is in "units"
test <- set_units(Flaring_Data_10km$dist_trailer, "km") #converting to km since dist_trailer is in "units"
Flaring_Data_10km_shortest_distance <- Flaring_Data_10km %>% as.data.frame() %>% group_by(date) %>% 
  summarise(nearest_10km = min(dist_trailer_km), number_of_10 = n()) %>% rename(time = date) #adding closest flaring and number of flaring


#Flaring Data for 20km
buffer_20km <- Loving_Trailer_Coordinates %>% st_transform(crs = 32613) %>% st_buffer(dist = 20000) %>% st_transform(crs = 4326)
Flaring_Data_20km <- Flaring_Data[lengths(st_within(Flaring_Data, buffer_20km)) > 0, ]
Flaring_Data_20km <- Flaring_Data_20km %>% mutate(dist_trailer = st_distance(geometry, Loving_Trailer_Coordinates)) #adding distance for each cluster to the trailer
Flaring_Data_20km$dist_trailer_km <- set_units(Flaring_Data_20km$dist_trailer, "km")
Flaring_Data_20km_shortest_distance <- Flaring_Data_20km %>% as.data.frame() %>% group_by(date) %>% summarise(nearest_20km = min(dist_trailer_km), number_of_20 = n()) %>% rename(time = date) 

#Flaring Data for 30km
buffer_30km <- Loving_Trailer_Coordinates %>% st_transform(crs = 32613) %>% st_buffer(dist = 30000) %>% st_transform(crs = 4326)
Flaring_Data_30km <- Flaring_Data[lengths(st_within(Flaring_Data, buffer_30km)) > 0, ]
Flaring_Data_30km <- Flaring_Data_30km %>% mutate(dist_trailer = st_distance(geometry, Loving_Trailer_Coordinates))
Flaring_Data_30km$dist_trailer_km <- set_units(Flaring_Data_30km$dist_trailer, "km")
Flaring_Data_30km_shortest_distance <- Flaring_Data_30km %>% as.data.frame() %>% group_by(date) %>% summarise(nearest_30km = min(dist_trailer_km), number_of_30 = n()) %>% rename(time = date)

#Flaring Data for 50km
buffer_50km <- Loving_Trailer_Coordinates %>% st_transform(crs = 32613) %>% st_buffer(dist = 50000) %>% st_transform(crs = 4326)
Flaring_Data_50km <- Flaring_Data[lengths(st_within(Flaring_Data, buffer_50km)) > 0, ]
Flaring_Data_50km <- Flaring_Data_50km %>% mutate(dist_trailer = st_distance(geometry, Loving_Trailer_Coordinates))
Flaring_Data_50km$dist_trailer_km <- set_units(Flaring_Data_50km$dist_trailer, "km")
Flaring_Data_50km_shortest_distance <- Flaring_Data_50km %>% as.data.frame() %>% group_by(date) %>% summarise(nearest_50km = min(dist_trailer_km), number_of_50 = n()) %>% rename(time = date)

#Flaring Data for 100km
buffer_100km <- Loving_Trailer_Coordinates %>% st_transform(crs = 32613) %>% st_buffer(dist = 100000) %>% st_transform(crs = 4326)
Flaring_Data_100km <- Flaring_Data[lengths(st_within(Flaring_Data, buffer_100km)) > 0, ]
Flaring_Data_100km <- Flaring_Data_100km %>% mutate(dist_trailer = st_distance(geometry, Loving_Trailer_Coordinates))
Flaring_Data_100km$dist_trailer_km <- set_units(Flaring_Data_100km$dist_trailer, "km")
Flaring_Data_100km_shortest_distance <- Flaring_Data_100km %>% as.data.frame() %>% group_by(date) %>% summarise(nearest_100km = min(dist_trailer_km), number_of_100 = n()) %>% rename(time = date)

#--------------------------------------------------------------------------------------------------#
#Now Merging the Data Above
List_of_flaring_min_distance <- list(Trailer_Data,Flaring_Data_10km_shortest_distance,Flaring_Data_20km_shortest_distance, 
                                     Flaring_Data_30km_shortest_distance, Flaring_Data_50km_shortest_distance, Flaring_Data_100km_shortest_distance)
Trailer_Data_with_closest_flaring_and_number <- Reduce(function(x, y) merge(x, y, by="time", all.x=TRUE), List_of_flaring_min_distance)

#-----------------------------------Plots-----------------------------------------------------#




# Create the plotly line chart (OZONE AND NUMBER OF FLARING)
plot_20_ozone <- plot_ly(data = Trailer_Data_with_closest_flaring_and_number, x = ~time) %>%
  add_bars(y = ~number_of_20, name = "Number of Flaring", marker = list(color = "blue"), yaxis = "y1")  %>%
  add_lines(y = ~o3, name = "Ozone Levels", line = list(color = "red"), yaxis = "y2") %>%
  layout(
    title = "Flaring Events and Ozone Levels_20KM",
    xaxis = list(title = "Day"),
    yaxis = list(title = "Number of Flaring", side = "left"),
    yaxis2 = list(title = "Ozone Levels", side = "right", overlaying = "y"),
    hovermode = "x"
  )

plot_30_ozone <- plot_ly(data = Trailer_Data_with_closest_flaring_and_number, x = ~time) %>%
  add_bars(y = ~number_of_30, name = "Number of Flaring", marker = list(color = "blue"), yaxis = "y1")  %>%
  add_lines(y = ~o3, name = "Ozone Levels", line = list(color = "red"), yaxis = "y2") %>%
  layout(
    title = "Flaring Events and Ozone Levels_30KM",
    xaxis = list(title = "Day"),
    yaxis = list(title = "Number of Flaring", side = "left"),
    yaxis2 = list(title = "Ozone Levels", side = "right", overlaying = "y"),
    hovermode = "x"
  )

plot_50_ozone <- plot_ly(data = Trailer_Data_with_closest_flaring_and_number, x = ~time) %>%
  add_bars(y = ~number_of_50, name = "Number of Flaring", marker = list(color = "blue"), yaxis = "y1")  %>%
  add_lines(y = ~o3, name = "Ozone Levels", line = list(color = "red"), yaxis = "y2") %>%
  layout(
    title = "Flaring Events and Ozone Levels_50KM",
    xaxis = list(title = "Day"),
    yaxis = list(title = "Number of Flaring", side = "left"),
    yaxis2 = list(title = "Ozone Levels", side = "right", overlaying = "y"),
    hovermode = "x"
  )

plot_100_ozone <- plot_ly(data = Trailer_Data_with_closest_flaring_and_number, x = ~time) %>%
  add_bars(y = ~number_of_100, name = "Number of Flaring", marker = list(color = "blue"), yaxis = "y1")  %>%
  add_lines(y = ~o3, name = "Ozone Levels", line = list(color = "red"), yaxis = "y2") %>%
  layout(
    title = "Flaring Events and Ozone Levels_100KM",
    xaxis = list(title = "Day"),
    yaxis = list(title = "Number of Flaring", side = "left"),
    yaxis2 = list(title = "Ozone Levels", side = "right", overlaying = "y"),
    hovermode = "x"
  )
plot_20_ozone
plot_30_ozone
plot_50_ozone
plot_100_ozone


# Create the plotly line chart (NOX AND NUMBER OF FLARING)
plot_nox20 <- plot_ly(data = Trailer_Data_with_closest_flaring_and_number, x = ~time) %>%
  add_bars(y = ~number_of_20, name = "Number of Flaring", marker = list(color = "blue"), yaxis = "y1")  %>%
  add_lines(y = ~nox, name = "Nox Levels", line = list(color = "red"), yaxis = "y2") %>%
  layout(
    title = "Flaring Events and Ozone Levels_20KM",
    xaxis = list(title = "Day"),
    yaxis = list(title = "Number of Flaring", side = "left"),
    yaxis2 = list(title = "Ozone Levels", side = "right", overlaying = "y"),
    hovermode = "x"
  )

plot_nox30 <- plot_ly(data = Trailer_Data_with_closest_flaring_and_number, x = ~time) %>%
  add_bars(y = ~number_of_30, name = "Number of Flaring", marker = list(color = "blue"), yaxis = "y1")  %>%
  add_lines(y = ~nox, name = "Nox Levels", line = list(color = "red"), yaxis = "y2") %>%
  layout(
    title = "Flaring Events and Ozone Levels_30KM",
    xaxis = list(title = "Day"),
    yaxis = list(title = "Number of Flaring", side = "left"),
    yaxis2 = list(title = "Ozone Levels", side = "right", overlaying = "y"),
    hovermode = "x"
  )

plot_nox50 <- plot_ly(data = Trailer_Data_with_closest_flaring_and_number, x = ~time) %>%
  add_bars(y = ~number_of_50, name = "Number of Flaring", marker = list(color = "blue"), yaxis = "y1")  %>%
  add_lines(y = ~nox, name = "Nox Levels", line = list(color = "red"), yaxis = "y2") %>%
  layout(
    title = "Flaring Events and Ozone Levels_50KM",
    xaxis = list(title = "Day"),
    yaxis = list(title = "Number of Flaring", side = "left"),
    yaxis2 = list(title = "Ozone Levels", side = "right", overlaying = "y"),
    hovermode = "x"
  )
plot_nox100 <- plot_ly(data = Trailer_Data_with_closest_flaring_and_number, x = ~time) %>%
  add_bars(y = ~number_of_100, name = "Number of Flaring", marker = list(color = "blue"), yaxis = "y1")  %>%
  add_lines(y = ~nox, name = "Nox Levels", line = list(color = "red"), yaxis = "y2") %>%
  layout(
    title = "Flaring Events and Ozone Levels_100KM",
    xaxis = list(title = "Day"),
    yaxis = list(title = "Number of Flaring", side = "left"),
    yaxis2 = list(title = "Ozone Levels", side = "right", overlaying = "y"),
    hovermode = "x"
  )

plot_nox20
plot_nox30
plot_nox50
plot_nox100
# Display the chart

#plots of number of flaring by distance
number_of_flaring_20 <- ggplot(Trailer_Data_with_closest_flaring_and_number, aes(x = time)) +
  geom_bar(aes( y = number_of_20), stat="identity") + ggtitle("Number of Flaring 20km radius")
number_of_flaring_30 <- ggplot(Trailer_Data_with_closest_flaring_and_number, aes(x = time)) +
  geom_bar(aes( y = number_of_30), stat="identity") + ggtitle("Number of Flaring 30km radius")
number_of_flaring_50 <- ggplot(Trailer_Data_with_closest_flaring_and_number, aes(x = time)) +
  geom_bar(aes( y = number_of_50), stat="identity") + ggtitle("Number of Flaring 50km radius")
number_of_flaring_100 <- ggplot(Trailer_Data_with_closest_flaring_and_number, aes(x = time)) +
  geom_bar(aes( y = number_of_100), stat="identity") + ggtitle("Number of Flaring 100km radius")
ggarrange(number_of_flaring_20, number_of_flaring_30, number_of_flaring_50, number_of_flaring_100)
#minimum distance to a flare
minimum_dist_flare_20 <- ggplot(Trailer_Data_with_closest_flaring_and_number, aes(x = time)) + 
  geom_bar(aes(y = nearest_20km), stat = "identity") + ggtitle("Closest Distance to Flaring 20KM")

minimum_dist_flare_30 <- ggplot(Trailer_Data_with_closest_flaring_and_number, aes(x = time)) + 
  geom_bar(aes(y = nearest_30km), stat = "identity") + ggtitle("Closest Distance to Flaring 30KM")

minimum_dist_flare_50 <- ggplot(Trailer_Data_with_closest_flaring_and_number, aes(x = time)) + 
  geom_bar(aes(y = nearest_50km), stat = "identity") + ggtitle("Closest Distance to Flaring 50KM")

minimum_dist_flare_100 <- ggplot(Trailer_Data_with_closest_flaring_and_number, aes(x = time)) + 
  geom_bar(aes(y = nearest_100km), stat = "identity") + ggtitle("Closest Distance to Flaring 100km")
ggarrange(minimum_dist_flare_20, minimum_dist_flare_30, minimum_dist_flare_50, minimum_dist_flare_100)

#how nox, temp, solar radiation, ozone, differ by hour
nox_vs_hour <- ggplot(Trailer_Data_HOUR, aes(x = hour, y = nox)) +   geom_bar(stat="identity") + ggtitle("nox vs hour")
temp_vs_hour <- ggplot(Trailer_Data_HOUR, aes(x = hour, y = temp_f)) +   geom_bar(stat="identity") + ggtitle("temp(F) vs hour")
solr_vs_hour <- ggplot(Trailer_Data_HOUR, aes(x = hour, y = solr))+   geom_bar(stat="identity")+ ggtitle("Solar Radiation vs hour")
ozone_vs_hour <- ggplot(Trailer_Data_HOUR, aes(x = hour, y = o3))+   geom_bar(stat="identity") + ggtitle("Ozone vs hour")
ggarrange(nox_vs_hour, temp_vs_hour, solr_vs_hour, ozone_vs_hour)

#needed for below
Trailer_Data_HOUR_PLOT <- Trailer_Data_HOUR
Trailer_Data_HOUR_PLOT$mode_direction_8 <- factor(Trailer_Data_HOUR$mode_direction_8, levels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"))
Trailer_Data_HOUR_PLOT$mode_direction_16 <- factor(Trailer_Data_HOUR$mode_direction_16, levels = c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", 
                                                                                                   "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"))
# Create tile plot
Wind_Direction_8 <- ggplot(Trailer_Data_HOUR_PLOT, aes(x = hour, y = mode_direction_8)) + 
  geom_tile(aes(fill = mode_direction_8), width = 1, height = 1) +
  scale_y_discrete(expand = expansion(add = c(0, 1))) +
  coord_cartesian(clip = "off") +
  theme(axis.text = element_text(size = 12), legend.title = element_blank()) +
  labs(title = "Tile plot of Hour vs Direction (8)", x = "Hour", y = "Direction")

Wind_Direction_16 <- ggplot(Trailer_Data_HOUR_PLOT, aes(x = hour, y = mode_direction_16)) + 
  geom_tile(aes(fill = mode_direction_16), width = 1, height = 1) +
  scale_y_discrete(expand = expansion(add = c(0, 1))) +
  coord_cartesian(clip = "off") +
  theme(axis.text = element_text(size = 12), legend.title = element_blank()) +
  labs(title = "Tile plot of Hour vs Direction (16)", x = "Hour", y = "Direction")
ggarrange(Wind_Direction_8, Wind_Direction_16, ncol = 1)
#--------------------------------------------------------------------------------------------------#


#-----------------------------------Regression-----------------------------------------------------#
#nox units is ppm?
model_nox <- lm(nox ~ wsp + temp_f + relh + solr + rain, data = Trailer_Data_with_closest_flaring_and_number)
summary(model_nox)
model_ozone <- lm(o3~ wsp + temp_f + relh + solr + rain, data = Trailer_Data_with_closest_flaring_and_number)
summary(model_ozone)


model_nox_with_flares <- lm(nox ~number_of_10 + wsp + temp_f + relh + solr, data = Trailer_Data_with_closest_flaring_and_number)
summary(model_nox_with_flares)

model_nox_with_flares_20 <- lm(nox ~ number_of_20 + wsp + temp_f + relh + solr + rain, data = Trailer_Data_with_closest_flaring_and_number)
model_nox_with_flares_50 <- lm(nox ~ number_of_50 + wsp + temp_f + relh + solr + rain, data = Trailer_Data_with_closest_flaring_and_number)
model_nox_with_flares_100<- lm(nox ~number_of_100 + wsp + temp_f + relh + solr + rain, data = Trailer_Data_with_closest_flaring_and_number)

summary(model_nox_with_flares_20)
summary(model_nox_with_flares_50)
summary(model_nox_with_flares_100)


model_ozone_with_flares_20 <- lm(o3 ~ number_of_20 + wsp + temp_f + relh + solr + rain, data = Trailer_Data_with_closest_flaring_and_number)
model_ozone_with_flares_30 <- lm(o3 ~ number_of_30 + wsp + temp_f + relh + solr + rain, data = Trailer_Data_with_closest_flaring_and_number)
model_ozone_with_flares_50 <- lm(o3 ~ number_of_50 + wsp + temp_f + relh + solr + rain, data = Trailer_Data_with_closest_flaring_and_number)
model_ozone_with_flares_100 <- lm(o3 ~ number_of_100 + wsp + temp_f + relh + solr + rain, data = Trailer_Data_with_closest_flaring_and_number)

summary(model_ozone_with_flares_20)
summary(model_ozone_with_flares_30)
summary(model_ozone_with_flares_50)
summary(model_ozone_with_flares_100)



#--------------------------------------------------------------------------------------------------#


