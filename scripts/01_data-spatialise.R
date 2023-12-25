# just adding our map vars to the sliced data object (which can be joined to main data)

### required files in directory: ###
# - latest EBD release .RData file
# - spatial data (pre-processed) as "maps.RData" file
###   ###


source("https://raw.githubusercontent.com/birdcountindia/bci-functions/main/01_functions/summaries.R")
source("https://raw.githubusercontent.com/birdcountindia/bci-functions/main/01_functions/mapping.R")

# region codes to link state/district names with their codes
load(url("https://github.com/birdcountindia/ebird-datasets/raw/main/region_codes.RData"))


# joining mapvars --------------------------------------------------------

load(slicedatapath)
# data_slice_G <- data_slice_G_filt

tictoc::tic("Joining mapvars to each unique list")
sf_use_s2(FALSE)
data_spat <- join_map_sf(data_slice_G) %>% 
  dplyr::select(STATE.CODE, STATE, COUNTY.CODE, COUNTY, LATITUDE, LONGITUDE, 
                SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER, GROUP.ID,
                DISTRICT.NAME, STATE.NAME, GRID.G1, GRID.G2, GRID.G3, GRID.G4)
tictoc::toc()

# mapping state/dist between EBD and sf  --------------------------------------------


# function to calc similarity of two strings (https://stackoverflow.com/a/11535768/13000254)
str_similarity <- function(x, y) {
  
  # Levenshtein edit distance
  str_dist <- adist(x, y) %>% as.vector()
  str_longer_length <- max(str_length(c(x, y)))
  
  str_simil <- 1 - (str_dist/str_longer_length)
  return(str_simil)
  
}

admin_unit_mapping <- data_spat %>% 
  group_by(STATE.CODE, STATE, COUNTY.CODE, COUNTY, STATE.NAME, DISTRICT.NAME) %>% 
  filter(!is.na(COUNTY.CODE)) %>% 
  reframe(NO.LISTS = n_distinct(GROUP.ID)) %>% 
  rowwise() %>% 
  mutate(NAME.SIMILARITY = str_similarity(COUNTY, DISTRICT.NAME)) %>% 
  arrange(desc(NAME.SIMILARITY), desc(NO.LISTS)) %>% 
  group_by(STATE.CODE, STATE, COUNTY.CODE, COUNTY) %>% 
  slice_head(n = 1) %>%
  ungroup() %>% 
  dplyr::select(-c(NAME.SIMILARITY, NO.LISTS)) %>% 
  arrange(STATE.CODE, COUNTY.CODE)

# in each state, how many districts (from EBD)
dists_per_state <- admin_unit_mapping %>% 
  group_by(STATE.CODE, STATE) %>% 
  dplyr::summarise(TOT.DIST = n_distinct(COUNTY.CODE))


# writing ---------------------------------------------------------------------------

# saving object at each (monthly) iteration because time-consuming step
# but overwriting, because no need to have separate monthly checklist-map mappings
save(data_spat, file = get_stage_obj_path("data", "spat"))

save(admin_unit_mapping, dists_per_state,
     file = "data/admin_units.RData")
