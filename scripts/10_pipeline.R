library(lubridate)
library(tidyverse)
library(glue)
library(sf)
library(magrittr)
library(patchwork)
library(writexl)
library(readxl)
library(skimmr) # devtools::install_github("rikudoukarthik/skimmr")

source("scripts/functions.R")

load("../india-maps/outputs/maps_sf.RData")
# load(url("https://github.com/birdcountindia/india-maps/raw/main/outputs/maps_sf.RData"))
sf_use_s2(FALSE)

# region codes to link state/district names with their codes
load(url("https://github.com/birdcountindia/ebird-datasets/raw/main/region_codes.RData"))

# in each state, how many districts
state_dists <- dists_sf %>% 
  st_drop_geometry() %>% 
  dplyr::select(-AREA) %>% 
  left_join(states_sf %>% dplyr::select(-AREA)) %>% 
  st_drop_geometry() %>% 
  group_by(STATE.NAME) %>% 
  dplyr::summarise(TOT.DIST = n_distinct(DISTRICT.NAME))

source("scripts/00_params.R")


# main data
load(maindatapath)
data <- data_filt


# spatialising data (add mapvars) ------------------------------------------------------

# output is data_spat object

tictoc::tic("Data spatialisation completed")
source("scripts/01_data-spatialise.R")
tictoc::toc() # 18 mins


# # if already run above previously, just unhash and run 2 lines below
# load("data/01_data-spatialise.RData")


# identifying Dark ------------------------------------------------------------------

### district-wise completeness

# need EBD data (maindatapath) and output of 01_data-spatialise
# output is data object with invcomp calculated
tictoc::tic("Districtwise completeness analysis finished")
source("scripts/02_completeness.R")
# load(get_stage_obj_path("data", "completeness", add_rel_str = TRUE))
tictoc::toc() # 180 secs

### setting thresholds and classifying into concern categories
tictoc::tic("Threshold setting and concern classification completed")
source("scripts/03_classify-concern.R")
# load(get_stage_obj_path("data", "concern", add_rel_str = TRUE))
tictoc::toc() # 55 secs

# proportion of high concern districts can change MoM, e.g., when some additional birding somewhere
# results in addition of some species to the district, thus increasing S.EXP and decreasing INV.C
# For underbirded districts, we can expect the percent high concern metric to not
# follow a linear decrease, but to show a unimodal curve


# identifying Dark Locus groups ------------------------------------------------------------------

source("scripts/04_id-loci.R")


# metric summaries to track and monitor ---------------------------------------------

source("scripts/05_track-metrics.R")
