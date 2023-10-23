# just adding our map vars to the sliced data object (which can be joined to main data)

### required files in directory: ###
# - latest EBD release .RData file
# - spatial data (pre-processed) as "maps.RData" file
###   ###


source("https://raw.githubusercontent.com/birdcountindia/bci-functions/main/01_functions/summaries.R")
source("https://raw.githubusercontent.com/birdcountindia/bci-functions/main/01_functions/mapping.R")


# joining mapvars --------------------------------------------------------

load(slicedatapath)
data_slice_G <- data_slice_G_filt

tictoc::tic("Joining mapvars to each unique list")
sf_use_s2(FALSE)
data_spat <- join_map_sf(data_slice_G)
tictoc::toc()

# saving object at each (monthly) iteration because time-consuming step
# but overwriting, because no need to have separate monthly checklist-map mappings
save(data_spat, file = "data/01_data-spatialise.RData")
