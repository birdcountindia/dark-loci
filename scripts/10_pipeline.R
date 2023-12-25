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

source("scripts/00_params.R")


# main data
load(maindatapath)
# data <- data_filt


# spatialising data (add mapvars) ------------------------------------------------------

# outputs are data_spat, admin_unit_mapping, dists_per_state objects

tictoc::tic("Data spatialisation completed")
source("scripts/01_data-spatialise.R")
tictoc::toc() # 18 mins


# # if already run above previously, just unhash and run 2 lines below
# load(get_stage_obj_path("data", "spat"))

# load("data/admin_units.RData")


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
# results in addition of some species to the district, thus increasing SPEC.EXP and decreasing INV.C
# For underbirded districts, we can expect the percent high concern metric to not
# follow a linear decrease, but to show a unimodal curve


# identifying Dark Locus groups ------------------------------------------------------------------

source("scripts/04_id-loci.R")
# load(get_stage_obj_path("data", "id"))


# metric summaries to track and monitor ---------------------------------------------

source("scripts/05_track-metrics.R")
