require(lubridate)
require(tidyverse)
require(glue)
require(sf)

### required files in directory: ###
# - latest EBD release .RData file
# - spatial data (pre-processed) as "maps.RData" file
###   ###

source("https://raw.githubusercontent.com/birdcountindia/bci-functions/main/summaries.R")
source("https://raw.githubusercontent.com/birdcountindia/bci-functions/main/mapping.R")

# region codes to link state/district names with their codes
load(url("https://github.com/birdcountindia/ebird-datasets/raw/main/region_codes.RData"))

# automated parameters ----------------------------------------------------


# date under consideration for current leaderboard
cur_date <- if (today() %>% day() < 16) { 
  (today() - months(1)) %>% floor_date(unit = "month")
} else {
  today() %>% floor_date(unit = "month")
}

rel_date <- if (today() %>% day() < 16) {
  ((today() - months(1)) - months(1)) %>%
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


maindatapath <-  glue("../ebird-datasets/EBD/ebd_IN_rel{rel_month_lab}-{rel_year}.RData")
slicedatapath <-  glue("../ebird-datasets/EBD/ebd_IN_rel{rel_month_lab}-{rel_year}_slice.RData")
# maindatapath <-  "../ebird-datasets/EBD/ebd_IN_relNov-2022.RData"
# slicedatapath <-  "../ebird-datasets/EBD/ebd_IN_relNov-2022_slice.RData"


# joining mapvars --------------------------------------------------------

# maps
load(maindatapath)
load(slicedatapath)

tictoc::tic("Joining mapvars to each unique list")
sf_use_s2(FALSE)
data0 <- join_map_sf(data_slice_G)
tictoc::toc()

save(data0, file = "data/data0.RData")
