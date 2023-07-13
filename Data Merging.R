
#loading libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(modeest)

#R does not have standard mode in built in library, so the function below calculates mode
mode_func <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}


#---- Reading and applying changes to Nox(Nitric Oxide + nitrogen dioxide) Data
Nox <- read.csv("TrailerData/LNM_nox_2023_q2_v1.5.csv", header = FALSE)
names(Nox) <- Nox[2,]       #shifts the second row to heading
Nox <- Nox[-2:-1,]          #deletes the first two rows
Nox$time <-  as.POSIXct(as.numeric(Nox$time), origin = "1970-01-01", tz = "UTC")    #converting UNIX to UTC(time)
Nox_converted <- Nox
Nox_converted $time <- as.Date(Nox_converted $time) #converts it to Date format
Select_column_non_time_nox <- colnames(Nox)[-1] #selecting all columns except time
Nox_converted[Select_column_non_time_nox] <- lapply(Nox_converted[Select_column_non_time_nox], as.numeric) #converting all columns except time to numeric
Nox_daily_average <- Nox_converted  %>% group_by(time) %>%  summarise(across(everything(), mean, na.rm = TRUE)) #taking daily averages, na.rm = TRUE since some have NaN

#the hourly for nox
Nox_hourly <- Nox #creating new dataframe as original
Nox_hourly$hour <- format(Nox_hourly$time, "%H") #format new column to only get the hour
Select_column_nox_hourly <- colnames(Nox_hourly)[1:length(Nox_hourly)]
Nox_hourly[Select_column_nox_hourly] <- lapply(Nox_hourly[Select_column_nox_hourly], as.numeric) #making it numeric so we can apply mean since default is character
Nox_hourly <- Nox_hourly %>% select(-1) %>% group_by(hour) %>% summarise(across(everything(), mean, na.rm = TRUE))




#---- Reading and applying changes to Radioactivity Data
RadioActivity<- read.csv("TrailerData/LNM_rd_2023_q2_v1.5.csv", header = FALSE)
names(RadioActivity) <- RadioActivity[2,]
RadioActivity <- RadioActivity[-2:-1, ]
RadioActivity$time <- as.POSIXct(as.numeric(RadioActivity$time), origin = "1970-01-01", tz = "UTC")
RadioActivity_converted <- RadioActivity

      
RadioActivity_converted$time <- as.Date(RadioActivity_converted$time)
Select_column_non_time_RadioActivity <- colnames(RadioActivity_converted[-1]) #selecting all columns except time
RadioActivity_converted[Select_column_non_time_RadioActivity] <- lapply(RadioActivity_converted[Select_column_non_time_RadioActivity], as.numeric)
RadioActivity_daily_average <- RadioActivity_converted %>% group_by(time) %>% summarize(across(everything(), mean, na.rm = TRUE))


#the hourly for radioactivity
RadioActivity_hourly <- RadioActivity
RadioActivity_hourly$hour <- format(RadioActivity_hourly$time, "%H")
Select_column_RadioActivity_hourly <- colnames(RadioActivity_hourly)[1:length(RadioActivity_hourly)]
RadioActivity_hourly[Select_column_RadioActivity_hourly] <- lapply(RadioActivity_hourly[Select_column_RadioActivity_hourly], as.numeric)
RadioActivity_hourly <- RadioActivity_hourly %>% select(-1) %>% group_by(hour) %>% summarise(across(everything(), mean, na.rm = TRUE))

#---- Reading and applying changes to OzoneData
Ozone <- read.csv("TrailerData/LNM_o3_2023_q2_v1.5.csv", header = FALSE)
names(Ozone) <- Ozone[2,]
Ozone <- Ozone[-2:-1,]
Ozone <- Ozone[, -ncol(Ozone)]
Ozone$time <- as.POSIXct(as.numeric(Ozone$time), origin = "1970-01-01", tz = "UTC")
Ozone_converted <- Ozone
Ozone_converted$time <- as.Date(Ozone_converted$time)
Select_column_non_time_Ozone <- colnames(Ozone_converted)[-1] #selecting all columns except time
Ozone_converted[Select_column_non_time_Ozone] <- lapply(Ozone_converted[Select_column_non_time_Ozone], as.numeric)
Ozone_daily_average <- Ozone_converted %>% group_by(time) %>% summarize(across(everything(), mean, na.rm = TRUE)) #daily average

Ozone_hourly <- Ozone
Ozone_hourly$hour <- format(Ozone$time, "%H")
Select_column_Ozone_hourly <- colnames(Ozone_hourly)[1:length(Ozone_hourly)]
Ozone_hourly[Select_column_Ozone_hourly] <- lapply(Ozone_hourly[Select_column_Ozone_hourly], as.numeric)
Ozone_hourly <- Ozone_hourly %>% select(-1) %>% group_by(hour) %>% summarise(across(everything(), mean, na.rm = TRUE))

#---- Reading and formatting changes to Meteor Data
Meteor <- read.csv("TrailerData/LNM_met_2023_q2_v1.5.csv", header = FALSE)
names(Meteor) <- Meteor[2,]
Meteor <- Meteor[-2:-1, ]
Meteor$time <- as.POSIXct(as.numeric(Meteor$time), origin = "1970-01-01", tz = "UTC")
Meteor_converted <-  subset(Meteor, select = -wdr)
Meteor_converted$time <- as.Date(Meteor_converted$time)
Select_column_non_time_Meteor <- colnames(Meteor_converted)[-1]

Meteor_converted[Select_column_non_time_Meteor] <- lapply(Meteor_converted[Select_column_non_time_Meteor], as.numeric)

##breaks for finding splitting wind direction
test_2 <- c(seq(45, 500, by = 180))
breaks_8 <-c(seq(22.5, 360, by = 45))
breaks_16 <- c(seq(11.25, 360, by = 22.5))
labels_8 <- c("NE", "E", "SE", "S", "SW", "W", "NW")
labels_16 <- c("NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")

#creating new columns for wind direction
Meteor_wind_direction_hourly <- Meteor
Meteor_wind_direction_hourly[colnames(Meteor)[-1]] <- lapply(Meteor_wind_direction_hourly[colnames(Meteor)[-1]], as.numeric)

#Adding new columns so that it takes the mode of wind direction hourly
Meteor_wind_direction_hourly$wind_direction_8 <- cut(Meteor_wind_direction_hourly$wdr, breaks = breaks_8, labels = labels_8, right = FALSE)
Meteor_wind_direction_hourly$wind_direction_16 <- cut(Meteor_wind_direction_hourly$wdr, breaks = breaks_16, labels = labels_16, right = FALSE)
Meteor_wind_direction_hourly$wind_direction_8 <- as.character(Meteor_wind_direction_hourly$wind_direction_8)
Meteor_wind_direction_hourly$wind_direction_8[is.na(Meteor_wind_direction_hourly$wind_direction_8)] <- "N"
Meteor_wind_direction_hourly$wind_direction_16 <- as.character(Meteor_wind_direction_hourly$wind_direction_16)
Meteor_wind_direction_hourly$wind_direction_16[is.na(Meteor_wind_direction_hourly$wind_direction_16)] <- "N"
Meteor_wind_direction_hourly$day_hour <- format(Meteor_wind_direction_hourly$time, "%Y-%m-%d-%H")
Meteor_hourly_mode_wdr_8 <- Meteor_wind_direction_hourly %>% group_by(day_hour)%>% summarise(mode_direction_8 = mode_func(wind_direction_8))
Meteor_hourly_mode_wdr_16 <- Meteor_wind_direction_hourly %>% group_by(day_hour)%>% summarise(mode_direction_16 = mode_func(wind_direction_16))

#merging the two different hourly mode of wind direction
Meteor_hourly_mode_wdr_merged <- merge(Meteor_hourly_mode_wdr_8, Meteor_hourly_mode_wdr_16, by = "day_hour", all = TRUE)
Meteor_hourly_mode_wdr_merged$day_hour <- as.Date(Meteor_hourly_mode_wdr_merged$day_hour)
Meteor_hourly_mode_wdr_merged <- Meteor_hourly_mode_wdr_merged %>% rename(time = day_hour)
Meteor_daily_mode_wdr <- Meteor_hourly_mode_wdr_merged %>% group_by(time) %>% summarise(mode_direction_8 = mode_func(mode_direction_8), mode_direction_16 = mode_func(mode_direction_16))

#take mean of all variables(note: there is no "wind direction" column in Meteor_converted)
Meteor_daily_average <- Meteor_converted %>% group_by(time)%>% summarise(across(everything(), mean, na.rm = TRUE))

#Meteor hourly
Meteor_hourly <- Meteor
Meteor_hourly$hour <- format(Meteor$time, "%H")
Select_column_Meteor_hourly <- colnames(Meteor_hourly)[1:length(Meteor_hourly)]
Meteor_hourly[Select_column_Meteor_hourly] <-  lapply(Meteor_hourly[Select_column_Meteor_hourly], as.numeric)
Meteor_hourly <-  Meteor_hourly %>% select(-1) %>% group_by(hour) %>% summarise(across(everything(), mean, na.rm = TRUE))

Meteor_hourly_wind <- Meteor_wind_direction_hourly[, -ncol(Meteor_wind_direction_hourly)]
Meteor_hourly_wind$hour <- format(Meteor_hourly_wind$time, "%H")
Meteor_hourly_wind$hour <- as.numeric(Meteor_hourly_wind$hour)
Meteor_hourly_wind <- Meteor_hourly_wind %>% select(hour, wind_direction_8, wind_direction_16)
Meteor_hourly_wind <- Meteor_hourly_wind %>% group_by(hour) %>% summarise(mode_direction_8 = mode_func(wind_direction_8), mode_direction_16 = mode_func(wind_direction_16))

#merging the two datasets
Meteor_daily_average <- merge(Meteor_daily_mode_wdr, Meteor_daily_average)
Meteor_hourly_average <- merge(Meteor_hourly, Meteor_hourly_wind)

Merged_Data <- merge(merge(merge(Nox_daily_average, RadioActivity_daily_average, by= "time", all = TRUE), 
                          Ozone_daily_average, by ="time", all = TRUE), Meteor_daily_average, 
                    by = "time", all = TRUE)
Merged_hourly_data <- merge(merge(merge(Nox_hourly, Ozone_hourly, by = "hour", all = TRUE),
                                  RadioActivity_hourly, by = "hour", all = TRUE), Meteor_hourly_average, by = "hour", all = TRUE) %>% select(-wdr)



#saving the data into the data/folder -- 
saveRDS(Merged_Data, "TrailerData/Merged_Data.rds")
saveRDS(Merged_hourly_data, "TrailerData/Merged_Data_hourly.rds")






