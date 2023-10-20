require(lubridate)
require(tidyverse)
require(glue)
require(sf)

### required files in directory: ###
# - latest EBD release .RData file
# - spatial data (pre-processed) as "maps.RData" file
###   ###

source("https://raw.githubusercontent.com/birdcountindia/bci-functions/main/01_functions/summaries.R")
source("https://raw.githubusercontent.com/birdcountindia/bci-functions/main/01_functions/mapping.R")


# joining mapvars --------------------------------------------------------

# maps
load(maindatapath)
load(slicedatapath)

tictoc::tic("Joining mapvars to each unique list")
sf_use_s2(FALSE)
data0 <- join_map_sf(data_slice_G)
tictoc::toc()

save(data0, file = "data/data0.RData")
