
library(dplyr)
#Importing Data
Data_2012_2016 <- readRDS("ComponentData/Flaring_Data_2012-2016.rds")
Data_2016_2022_PB <- readRDS("ComponentData/flare_full.rds")
Data_2016_2022_EF <- readRDS("ComponentData/EF_2016_2022_w_cluster.rds")
Data_2022_2023 <- readRDS("ComponentData/VNF_2022-2023.rds")

Data_2012_2016_filtered <- Data_2012_2016 %>% filter(cloud_mask == 0, value != 0)%>%
  select("vnf_id", "date", "lon", "lat", "temp_bb", "area_bb", "rhi", "value") %>% rename(long = "lon")

Data_2016_2022_PB_filtered <- Data_2016_2022_PB %>% filter(cluster != 0)%>%
  select("vnf_id", "date", "vnf_lon", "vnf_lat", "temp_bb", "area_bb", "rhi", "cluster") %>% rename(long = "vnf_lon", lat = "vnf_lat", "value" = cluster)

Data_2016_2022_EF_filtered <- Data_2016_2022_EF %>% filter(cloud_mask == 0, value != 0)%>%
  select("vnf_id", "date", "lon", "lat", "temp_bb", "area_bb", "rhi", "value") %>% rename(long = "lon")

Data_2022_2023_filtered <- Data_2022_2023 %>% filter(value != 0)%>% as.data.frame() %>% 
  select("vnf_id", "date", "lon", "lat", "temp_bb", "area_bb", "rhi", "value") %>% rename(long = "lon") 


Flaring_Data_2012_2023 <- rbind(Data_2012_2016_filtered, Data_2016_2022_PB_filtered, Data_2016_2022_EF_filtered,Data_2022_2023_filtered)

saveRDS(Flaring_Data_2012_2023, "VNF_Flaring_2012_2023.rds")



