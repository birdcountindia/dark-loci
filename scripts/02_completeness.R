require(tidyverse)
require(KnowBR)


# centroids required for KnowB()
g1centroids <- g1cells_sf %>% 
  st_centroid() %>% 
  mutate(CEN.LON = st_coordinates(.)[,1],
         CEN.LAT = st_coordinates(.)[,2]) %>% 
  st_drop_geometry()

district_centroids <- districts_sf %>% 
  st_centroid() %>% 
  mutate(CEN.LON = st_coordinates(.)[,1],
         CEN.LAT = st_coordinates(.)[,2]) %>% 
  st_drop_geometry()


### ###
# trial with subset
data1 <- data0 %>% slice_sample(n = 1000)
### ###


# data needs to be slightly modified for KnowB()

data2 <- data1 %>% 
  left_join(districts_cells) %>% 
  left_join(district_centroids) %>% 
  # this order of columns important
  group_by(COMMON.NAME, CEN.LON, CEN.LAT) %>% 
  summarise(TOT.OBS = n()) %>% 
  ungroup() %>% 
  mutate(COMMON.NAME = factor(COMMON.NAME),
         CEN.LON = as.numeric(CEN.LON),
         CEN.LAT = as.numeric(CEN.LAT),
         TOT.OBS = as.numeric(TOT.OBS)) %>% 
  as.data.frame()
  
data3 <- data0 %>%
  left_join(districts_cells) %>% 
  left_join(district_centroids) %>% 
  # this order of columns important
  group_by(COMMON.NAME, CEN.LON, CEN.LAT) %>%
  summarise(TOT.OBS = n()) %>%
  ungroup() %>%
  mutate(COMMON.NAME = factor(COMMON.NAME),
         CEN.LON = as.numeric(CEN.LON),
         CEN.LAT = as.numeric(CEN.LAT),
         TOT.OBS = as.numeric(TOT.OBS)) %>%
  as.data.frame()


# resolution of 25kmx25km cells is approx. 14 min (13.53, 14.33)

tictoc::tic("Calculating completenss")
data(adworld)
KnowBPolygon(data = data2, shape = districtmap, shapenames = "districtmap",
             save = "RData") # generate gridded file
tictoc::toc() 
# KnowB (cells) in 1309 sec (21 min) with 1000 rows of data (973 rows input)


