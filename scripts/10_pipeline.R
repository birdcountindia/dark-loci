library(lubridate)
library(tidyverse)
library(glue)
library(sf)
library(magrittr)
library(patchwork)
library(writexl)
library(readxl)


# importing data ----------------------------------------------------------

tictoc::tic("Data import completed")
source("scripts/01_data-import.R")
# load("../india-maps/outputs/maps_sf.RData")
sf_use_s2(FALSE)
# load("data/data0.RData")
tictoc::toc() # 18 mins

# in each state, how many districts
state_dists <- dists_sf %>% 
  st_drop_geometry() %>% 
  dplyr::select(-AREA) %>% 
  left_join(states_sf %>% dplyr::select(-AREA)) %>% 
  st_drop_geometry() %>% 
  group_by(STATE.NAME) %>% 
  dplyr::summarise(TOT.DIST = n_distinct(DISTRICT.NAME))


# identifying Dark ------------------------------------------------------------------

### district-wise completeness
tictoc::tic("Districtwise completeness analysis finished")
source("scripts/02_completeness.R")
# load("data/02_completeness.RData")
tictoc::toc() # 180 secs

### setting thresholds and classifying into concern categories
tictoc::tic("Threshold setting and concern classification completed")
source("scripts/03_thresholds.R")
tictoc::toc() # 55 secs


# identifying Dark Locus groups ------------------------------------------------------------------

source("scripts/04_id-loci.R")


# metric summaries to track and monitor ---------------------------------------------

source("scripts/05_track-metrics.R")
