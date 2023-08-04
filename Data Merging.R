
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
Nox_q3 <- read.csv("TrailerData/LNM_nox_2023_q3_v1.5.csv", header = FALSE)
names(Nox) <- Nox[2,]       #shifts the second row to heading
Nox <- Nox[-2:-1,]          #deletes the first two rows
names(Nox_q3) <- Nox_q3[2,]
Nox_q3 <- Nox_q3[-2:-1,] 

Nox <- rbind(Nox, Nox_q3) #merging the two csv file dataframes into one

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
RadioActivity_q3 <- read.csv("TrailerData/LNM_rd_2023_q3_v1.5.csv", header = FALSE)
names(RadioActivity) <- RadioActivity[2,]
RadioActivity <- RadioActivity[-2:-1, ]

names(RadioActivity_q3) <- RadioActivity_q3[2,]
RadioActivity_q3 <- RadioActivity_q3[-2:-1,]

RadioActivity <- rbind(RadioActivity, RadioActivity_q3)

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
Ozone_q3 <- read.csv("TrailerData/LNM_o3_2023_q3_v1.5.csv", header = FALSE)
names(Ozone) <- Ozone[2,]
Ozone <- Ozone[-2:-1,]

names(Ozone_q3) <- Ozone_q3[2,]
Ozone_q3 <- Ozone_q3[-2:-1,]
Ozone <- rbind(Ozone, Ozone_q3) #merging the two different quarters

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
Meteor_q3 <- read.csv("TrailerData/LNM_met_2023_q3_v1.5.csv", header = FALSE)
names(Meteor) <- Meteor[2,]
Meteor <- Meteor[-2:-1, ]

names(Meteor_q3) <- Meteor_q3[2,]
Meteor_q3 <- Meteor_q3[-2:-1, ]

Meteor <- rbind(Meteor, Meteor_q3)
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

######---- Importing new CSV FILES VOC, CH4, CO, H2S and doing same steps as above. 


#-----------METHANE
Methane <- read.csv("TrailerData/LNM_ch4_2023_q2_v1.5.csv", header = FALSE)
Methane_q3 <- read.csv("TrailerData/LNM_ch4_2023_q3_v1.5.csv", header = FALSE)

names(Methane) <- Methane[2,]       #shifts the second row to heading
Methane <- Methane[-2:-1,]          #deletes the first two rows
names(Methane_q3) <- Methane_q3[2,]
Methane_q3 <- Methane_q3[-2:-1,] 

Methane <- rbind(Methane, Methane_q3) #merging the two csv file dataframes into one

Methane$time <-  as.POSIXct(as.numeric(Methane$time), origin = "1970-01-01", tz = "UTC")    #converting UNIX to UTC(time)
Methane_converted <- Methane
Methane_converted $time <- as.Date(Methane_converted $time) 
Select_column_non_time_Methane <- colnames(Methane)[-1] 
Methane_converted[Select_column_non_time_Methane] <- lapply(Methane_converted[Select_column_non_time_Methane], as.numeric) 
Methane_daily_average <- Methane_converted  %>% group_by(time) %>%  summarise(across(everything(), mean, na.rm = TRUE)) 

#the hourly for Methane
Methane_hourly <- Methane #creating new dataframe as original
Methane_hourly$hour <- format(Methane_hourly$time, "%H") #format new column to only get the hour
Select_column_Methane_hourly <- colnames(Methane_hourly)[1:length(Methane_hourly)]
Methane_hourly[Select_column_Methane_hourly] <- lapply(Methane_hourly[Select_column_Methane_hourly], as.numeric) #making it numeric so we can apply mean since default is character
Methane_hourly <- Methane_hourly %>% select(-1) %>% group_by(hour) %>% summarise(across(everything(), mean, na.rm = TRUE))


#-----CARBON MONOXIDE
Carbon_Monoxide <- read.csv("TrailerData/LNM_co_2023_q2_v1.5.csv", header = FALSE)
Carbon_Monoxide_q3 <- read.csv("TrailerData/LNM_co_2023_q3_v1.5.csv", header = FALSE)

names(Carbon_Monoxide) <- Carbon_Monoxide[2,]
Carbon_Monoxide <- Carbon_Monoxide[-2:-1, ]

names(Carbon_Monoxide_q3) <- Carbon_Monoxide_q3[2,]
Carbon_Monoxide_q3 <- Carbon_Monoxide_q3[-2:-1,]

Carbon_Monoxide <- rbind(Carbon_Monoxide, Carbon_Monoxide_q3)
Carbon_Monoxide <- Carbon_Monoxide[1:(length(Carbon_Monoxide)-1)] #dropping last column

Carbon_Monoxide$time <- as.POSIXct(as.numeric(Carbon_Monoxide$time), origin = "1970-01-01", tz = "UTC")
Carbon_Monoxide_converted <- Carbon_Monoxide


Carbon_Monoxide_converted$time <- as.Date(Carbon_Monoxide_converted$time)
Select_column_non_time_Carbon_Monoxide <- colnames(Carbon_Monoxide_converted[-1]) #selecting all columns except time
Carbon_Monoxide_converted[Select_column_non_time_Carbon_Monoxide] <- lapply(Carbon_Monoxide_converted[Select_column_non_time_Carbon_Monoxide], as.numeric)
Carbon_Monoxide_daily_average <- Carbon_Monoxide_converted %>% group_by(time) %>% summarize(across(everything(), mean, na.rm = TRUE))

#the hourly for Carbon_Monoxide
Carbon_Monoxide_hourly <- Carbon_Monoxide
Carbon_Monoxide_hourly$hour <- format(Carbon_Monoxide_hourly$time, "%H")
Select_column_Carbon_Monoxide_hourly <- colnames(Carbon_Monoxide_hourly)[1:length(Carbon_Monoxide_hourly)]
Carbon_Monoxide_hourly[Select_column_Carbon_Monoxide_hourly] <- lapply(Carbon_Monoxide_hourly[Select_column_Carbon_Monoxide_hourly], as.numeric)
Carbon_Monoxide_hourly <- Carbon_Monoxide_hourly %>% select(-1) %>% group_by(hour) %>% summarise(across(everything(), mean, na.rm = TRUE))


#-----HYDROGEN SULFIDE
Hydrogen_Sulfide <- read.csv("TrailerData/LNM_h2s_2023_q2_v1.5.csv", header = FALSE)
Hydrogen_Sulfide_q3 <- read.csv("TrailerData/LNM_h2s_2023_q3_v1.5.csv", header = FALSE)

names(Hydrogen_Sulfide) <- Hydrogen_Sulfide[2,]
Hydrogen_Sulfide <- Hydrogen_Sulfide[-2:-1, ]

names(Hydrogen_Sulfide_q3) <- Hydrogen_Sulfide_q3[2,]
Hydrogen_Sulfide_q3 <- Hydrogen_Sulfide_q3[-2:-1,]

Hydrogen_Sulfide <- rbind(Hydrogen_Sulfide, Hydrogen_Sulfide_q3)

Hydrogen_Sulfide$time <- as.POSIXct(as.numeric(Hydrogen_Sulfide$time), origin = "1970-01-01", tz = "UTC")
Hydrogen_Sulfide_converted <- Hydrogen_Sulfide


Hydrogen_Sulfide_converted$time <- as.Date(Hydrogen_Sulfide_converted$time)
Select_column_non_time_Hydrogen_Sulfide <- colnames(Hydrogen_Sulfide_converted[-1]) #selecting all columns except time
Hydrogen_Sulfide_converted[Select_column_non_time_Hydrogen_Sulfide] <- lapply(Hydrogen_Sulfide_converted[Select_column_non_time_Hydrogen_Sulfide], as.numeric)
Hydrogen_Sulfide_daily_average <- Hydrogen_Sulfide_converted %>% group_by(time) %>% summarize(across(everything(), mean, na.rm = TRUE))

#the hourly for Hydrogen_Sulfide
Hydrogen_Sulfide_hourly <- Hydrogen_Sulfide
Hydrogen_Sulfide_hourly$hour <- format(Hydrogen_Sulfide_hourly$time, "%H")
Select_column_Hydrogen_Sulfide_hourly <- colnames(Hydrogen_Sulfide_hourly)[1:length(Hydrogen_Sulfide_hourly)]
Hydrogen_Sulfide_hourly[Select_column_Hydrogen_Sulfide_hourly] <- lapply(Hydrogen_Sulfide_hourly[Select_column_Hydrogen_Sulfide_hourly], as.numeric)
Hydrogen_Sulfide_hourly <- Hydrogen_Sulfide_hourly %>% select(-1) %>% group_by(hour) %>% summarise(across(everything(), mean, na.rm = TRUE))

###### VOC
Volatile_Organic_Compound <- read.csv("TrailerData/LNM_voc_2023_q2_v3.3.csv", header = FALSE)
Volatile_Organic_Compound_q3 <- read.csv("TrailerData/LNM_voc_2023_q3_v3.3.csv", header = FALSE)

names(Volatile_Organic_Compound) <- Volatile_Organic_Compound[2,]
Volatile_Organic_Compound <- Volatile_Organic_Compound[-2:-1, ]

names(Volatile_Organic_Compound_q3) <- Volatile_Organic_Compound_q3[2,]
Volatile_Organic_Compound_q3 <- Volatile_Organic_Compound_q3[-2:-1,]

Volatile_Organic_Compound <- rbind(Volatile_Organic_Compound, Volatile_Organic_Compound_q3)

Volatile_Organic_Compound$time <- as.POSIXct(as.numeric(Volatile_Organic_Compound$time), origin = "1970-01-01", tz = "UTC")
Volatile_Organic_Compound_converted <- Volatile_Organic_Compound

Volatile_Organic_Compound_converted$time <- as.Date(Volatile_Organic_Compound_converted$time)
Select_column_non_time_Volatile_Organic_Compound <- colnames(Volatile_Organic_Compound_converted[-1]) #selecting all columns except time
Volatile_Organic_Compound_converted[Select_column_non_time_Volatile_Organic_Compound] <- lapply(Volatile_Organic_Compound_converted[Select_column_non_time_Volatile_Organic_Compound], as.numeric)
Volatile_Organic_Compound_daily_average <- Volatile_Organic_Compound_converted %>% group_by(time) %>% summarize(across(everything(), mean, na.rm = TRUE))
#the hourly for Volatile_Organic_Compound
Volatile_Organic_Compound_hourly <- Volatile_Organic_Compound
Volatile_Organic_Compound_hourly$hour <- format(Volatile_Organic_Compound_hourly$time, "%H")
Select_column_Volatile_Organic_Compound_hourly <- colnames(Volatile_Organic_Compound_hourly)[1:length(Volatile_Organic_Compound_hourly)]
Volatile_Organic_Compound_hourly[Select_column_Volatile_Organic_Compound_hourly] <- lapply(Volatile_Organic_Compound_hourly[Select_column_Volatile_Organic_Compound_hourly], as.numeric)
Volatile_Organic_Compound_hourly <- Volatile_Organic_Compound_hourly %>% select(-1) %>% group_by(hour) %>% summarise(across(everything(), mean, na.rm = TRUE))


#-----FOR DAILY AVERAGES
Merged_Data <- merge(merge(merge(Nox_daily_average, RadioActivity_daily_average, by= "time", all = TRUE), 
                          Ozone_daily_average, by ="time", all = TRUE), Meteor_daily_average, 
                    by = "time", all = TRUE)
Merged_Data<- merge(merge(merge(merge(Merged_Data, Methane_daily_average, by = "time", all = TRUE), 
                                Carbon_Monoxide_daily_average, by = "time", all = TRUE), 
                          Hydrogen_Sulfide_daily_average, by = "time", all = TRUE), 
                    Volatile_Organic_Compound_daily_average, by = "time", all = TRUE) #same as the line above, combing line above results so that its easier to read


#---FOR HOURLY AVERAGES
Merged_hourly_data <- merge(merge(merge(Nox_hourly, Ozone_hourly, by = "hour", all = TRUE),
                                  RadioActivity_hourly, by = "hour", all = TRUE), Meteor_hourly_average, by = "hour", all = TRUE) %>% select(-wdr)

Merged_hourly_data <- merge(merge(merge(merge(Merged_hourly_data, Methane_hourly, by = "hour", all = TRUE), Carbon_Monoxide_hourly, by = "hour", all = TRUE), 
                            Hydrogen_Sulfide_hourly, by = "hour", all = TRUE), Volatile_Organic_Compound_hourly, by = "hour", all = TRUE)

#saving the data into the data/folder -- 
saveRDS(Merged_Data, "TrailerData/Merged_Data.rds")
saveRDS(Merged_hourly_data, "TrailerData/Merged_Data_hourly.rds")






