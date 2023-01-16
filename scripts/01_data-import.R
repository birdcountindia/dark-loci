require(lubridate)
require(tidyverse)
require(glue)
require(sf)

### required files in directory: ###
# - latest EBD release .RData file
# - spatial data (pre-processed) as "maps.RData" file
###   ###


# automated parameters ----------------------------------------------------


# date under consideration for current leaderboard
cur_date <- if (today() %>% day() == 31) { 
  (today() - days(1)) %>% floor_date(unit = "month")
} else {
  today() %>% floor_date(unit = "month")
}

rel_date <- if (today() %>% day() == 31) {
  ((today() - days(1)) - months(1)) %>%
    floor_date(unit = "month")
} else {
  (today() - months(1)) %>%
    floor_date(unit = "month")
}

cur_year <- cur_date %>% year()
cur_month_num <- cur_date %>% month()
cur_month_lab <- cur_date %>% month(label = T, abbr = T)

rel_year <- rel_date %>% year()
rel_month_num <- rel_date %>% month()
rel_month_lab <- rel_date %>% month(label = T, abbr = T) 


# maindatapath <-  glue("../ebird-datasets/EBD/ebd_IN_rel{rel_month_lab}-{rel_year}.RData")
# slicedatapath <-  glue("../ebird-datasets/EBD/ebd_IN_rel{rel_month_lab}-{rel_year}_slice.RData")
maindatapath <-  glue("../ebird-datasets/EBD/ebd_IN_relNov-2022.RData")
slicedatapath <-  glue("../ebird-datasets/EBD/ebd_IN_relNov-2022_slice.RData")


# preparing maps ----------------------------------------------------------

load("data/maps.RData")

load("data/maps_gridmapg1_IN.RData") # all cells within India

g1cells_sf <- gridmapg1_IN


india_sf <- indiamap %>% 
  st_as_sf() %>% 
  dplyr::select(-DISTRICT) 

# getting total number of cells for country and states
totcells_india <- g1cells_sf %>% 
  st_drop_geometry() %>% 
  dplyr::summarise(TOT.CELLS = n_distinct(CELL.ID))


states_sf <- statemap %>% 
  st_as_sf() %>% 
  dplyr::select(stname, geometry) %>% 
  magrittr::set_colnames(c("STATE", "geometry")) %>% 
  mutate(STATE = str_to_title(STATE)) %>% 
  # replacing ampersand with "and"
  mutate(STATE = str_replace(STATE, "&", "and")) %>% 
  # corrections for Madhu's SPDF values
  mutate(STATE = case_when(STATE == "Dadra and Nagar Have" ~ "Dadra and Nagar Haveli",
                           STATE == "Andaman and Nicobar" ~ "Andaman and Nicobar Islands",
                           TRUE ~ STATE))

states_cells <- states_sf %>% 
  st_set_crs(st_crs(g1cells_sf)) %>% 
  st_join(g1cells_sf) %>% 
  st_drop_geometry()

totcells_states <- states_cells %>% 
  group_by(STATE) %>% 
  dplyr::summarise(TOT.CELLS = n_distinct(CELL.ID))


districts_sf <- districtmap %>% 
  st_as_sf() %>% 
  dplyr::select(dtname) %>% 
  rename(DISTRICT.NAME = dtname) %>% 
  # some districts have two different rows (two different polygons) so need to combine
  # them into one polygon
  group_by(DISTRICT.NAME) %>% 
  summarise() # dplyr-sf magic :) 
  # https://gis.stackexchange.com/questions/421651/merging-two-multipolygon-shapefiles-and-removing-one-of-overlapping-polygons-usi

districts_cells <- districts_sf %>% 
  st_set_crs(st_crs(g1cells_sf)) %>% 
  st_join(g1cells_sf) %>% 
  st_drop_geometry() 
# losing some districts and grid cells, but doesn't matter. 

totcells_dist <- districts_cells %>% 
  group_by(DISTRICT.NAME) %>% 
  dplyr::summarise(TOT.CELLS = n_distinct(CELL.ID))



rm(indiamap, statemap, gridmapg1, gridmapg2, gridmapg3, gridmapg4,
   gridmapg1_IN, gridlevels, totalcells, areag2, areag3, areag4)

tictoc::tic("Writing sf map objects")
save(area, areag1, districts_cells, states_cells, districts_sf, india_sf, states_sf,
     g1cells_sf, districtmap, # non-sf required for KnowBR
     file = "data/maps_sf.RData")
tictoc::toc()


# joining cell info --------------------------------------------------------

load(maindatapath)

tictoc::tic("Joining cell info to full data")
data0 <- data %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), remove = F) %>% 
  st_join(g1cells_sf) %>% 
  st_drop_geometry() %>% 
  # using only complete lists >= 10 min to calculate completeness
  filter(ALL.SPECIES.REPORTED == 1 & 
           PROTOCOL.TYPE != "Incidental" &
           DURATION.MINUTES >= 10)
tictoc::toc()

save(data0, file = "data/data0.RData")
rm(data)