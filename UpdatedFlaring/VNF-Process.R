library(dplyr)
library(dbscan)
pkgs = c('data.table', 'dbscan', 'sf', 'tidyverse')
for(p in pkgs) require(p, character.only = TRUE)
rm(p, pkgs)


#Loading Data

PB_data <- readRDS("../Extracting/Permian/pb_vnf.rds")
EF_data <- readRDS("../Extracting/EagleFord/ef_vnf.rds")

#Filtering for Permian Basin
PB_data <- PB_data %>% filter(temp_bb > 1600, cloud_mask == 0)
PB_data <- PB_data %>% dplyr::select(-temp_bkg, -cloud_mask, -file_date)
PB_data$basin <- "Permian"


#Filtering for Western Gulf Basin
EF_data <- EF_data %>%  filter(temp_bb > 1600, cloud_mask == 0)
EF_data <- EF_data %>% dplyr::select(-temp_bkg, -cloud_mask, -file_date)
EF_data$basin <- "Western Gulf"

#Combining both
Data <- rbind(PB_data, EF_data) #Joining the two datasets


Data$uog_lat <- Data$lat
Data$uog_lon <- Data$lon

Data <- st_as_sf(Data, coords = c("uog_lon", "uog_lat"), crs = 4326)

Data.clust.list <- Data %>% mutate(year = year(date)) %>% as.data.frame()
Data.clust.list <- split(Data.clust.list, list(Data.clust.list$basin, Data.clust.list$year))


minpts.grid = 3:8 #the minpts used in HDBSCAN from 3-8 inclusive

#Applying HDBSCAN
for(c in 1:length(Data.clust.list)){
  if(nrow(Data.clust.list[[c]]) > 40e3){
    Data.clust.list = c(
      Data.clust.list,
      Data.clust.list[[c]] %>% filter(month(date) > 6) %>% list()
    )
    Data.clust.list[[c]] = Data.clust.list[[c]] %>%
      filter(month(date) <= 6)
  }
}


for(c in 1:length(Data.clust.list)){

  curr.vnf.year.minpts = NA
  curr.vnf.year.noises = NA
  curr.vnf.year.clusts = NA

  for(m in 1:length(minpts.grid)){
    curr.hdbscan = hdbscan(
      x = dplyr::select(Data.clust.list[[c]], lon, lat),
      minPts = minpts.grid[m]
    )

    gc(); gc(reset = TRUE)

    if(is.na(curr.vnf.year.noises)) {
      curr.vnf.year.minpts = minpts.grid[m]
      curr.vnf.year.noises = sum(curr.hdbscan$cluster == 0)
      curr.vnf.year.clusts = ifelse(curr.hdbscan$cluster > 0, "Yes", "No")
      curr.vnf.value = curr.hdbscan$cluster
    } else if(sum(curr.hdbscan$cluster == 0) < curr.vnf.year.noises) {
      curr.vnf.year.minpts = minpts.grid[m]
      curr.vnf.year.noises = sum(curr.hdbscan$cluster == 0)
      curr.vnf.year.clusts = ifelse(curr.hdbscan$cluster > 0, "Yes", "No")
      curr.vnf.value = curr.hdbscan$cluster
    }}
  
  Data.clust.list[[c]] = data.table(
    vnf_id = Data.clust.list[[c]]$vnf_id,
    clustered = curr.vnf.year.clusts, value = curr.vnf.value)
    rm(list = ls(pattern = "curr")); gc(); gc(reset = TRUE)


  }

#merges the list of objects
Output_Data <- Data.clust.list[[1]]
for (i in 2:length(Data.clust.list)){
  Output_Data <- merge(Output_Data, Data.clust.list[[i]], all = TRUE)
}

#Filter out Non clustered (cluster = 0)
Output_Data <- Output_Data %>% filter(value != 0)

#saving locally to folder
Final_Data <- merge(Data, Output_Data, by = "vnf_id")
saveRDS(Final_Data, "VNF_2023.rds")


