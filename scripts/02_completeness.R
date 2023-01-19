
# Peterson 2018 -----------------------------------------------------------

require(tidyverse)


calc_exp_spec <- function(s_obs, N, q1, q2) {
  
  s_exp <- s_obs + ( ((N - 1)/N) * ((q1*(q1 - 1)) / (2*(q2 + 1))) )
  
  return(s_exp)
  
}

calc_inv_comp <- function(s_exp, s_obs) {
  
  C <- s_obs/s_exp
  
  return(C)
  
}


# calculating S.EXP for each ecoregion, then INV.COMP per district, according to which 1-2
# ecoregions it comes under
data1 <- data0 %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE")) %>% 
  # should use list of districts from shapefile, because that is latest and will also 
  # give names for districts with zero eBird lists
  st_join(districts_sf) %>% 
  # joining ecoregion data
  st_join(ecoregions)


temp <- data1 %>% 
  # how many lists species reported from
  group_by(ECOREGION, COMMON.NAME) %>% 
  mutate(SPEC.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(SPEC.LISTS %in% 1:2) %>% 
  # how many species of Q1 and Q2
  mutate(Q = ifelse(SPEC.LISTS == 1, "Q1", "Q2")) %>% 
  group_by(ECOREGION, Q) %>% 
  summarise(Qn = n_distinct(COMMON.NAME)) %>% 
  pivot_wider(names_from = "Q", values_from = "Qn") %>% 
  mutate(Q1 = replace_na(Q1, 0),
         Q2 = replace_na(Q2, 0))
  
ecoregion_exp <- data1 %>% 
  filter(!is.na(ECOREGION)) %>% 
  # number of lists (N) and species (s_obs) from ecoregion 
  group_by(ECOREGION) %>% 
  summarise(N = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            S.OBS = n_distinct(COMMON.NAME)) %>% 
  left_join(temp) %>% 
  # calculating S.EXP for ecoregion
  mutate(S.EXP = calc_exp_spec(S.OBS, N, Q1, Q2) %>% floor())

data2 <- data1 %>% 
  left_join(ecoregion_exp) %>% 
  group_by(DISTRICT.NAME, S.OBS) %>% 
  # if district falls under more than one ecoregion, taking mean of S.EXP for those regions
  summarise(S.EXP = mean(S.EXP)) %>% 
  # calculating C
  mutate(INV.C = calc_inv_comp(S.EXP, S.OBS) %>% round(2))
  

#

# KnowBR ------------------------------------------------------------------

# require(tidyverse)
# require(KnowBR)
# 
# 
# # centroids required for KnowB()
# g1centroids <- g1cells_sf %>% 
#   st_centroid() %>% 
#   mutate(CEN.LON = st_coordinates(.)[,1],
#          CEN.LAT = st_coordinates(.)[,2]) %>% 
#   st_drop_geometry()
# 
# district_centroids <- districts_sf %>% 
#   st_centroid() %>% 
#   mutate(CEN.LON = st_coordinates(.)[,1],
#          CEN.LAT = st_coordinates(.)[,2]) %>% 
#   st_drop_geometry()
# 
# 
# ### ###
# # trial with subset
# data1 <- data0 %>% slice_sample(n = 1000)
# ### ###
# 
# 
# # data needs to be slightly modified for KnowB()
# 
# data2 <- data1 %>% 
#   left_join(districts_cells) %>% 
#   left_join(district_centroids) %>% 
#   # this order of columns important
#   group_by(COMMON.NAME, CEN.LON, CEN.LAT) %>% 
#   summarise(TOT.OBS = n()) %>% 
#   ungroup() %>% 
#   mutate(COMMON.NAME = factor(COMMON.NAME),
#          CEN.LON = as.numeric(CEN.LON),
#          CEN.LAT = as.numeric(CEN.LAT),
#          TOT.OBS = as.numeric(TOT.OBS)) %>% 
#   as.data.frame()
#   
# data3 <- data0 %>%
#   left_join(districts_cells) %>% 
#   left_join(district_centroids) %>% 
#   # this order of columns important
#   group_by(COMMON.NAME, CEN.LON, CEN.LAT) %>%
#   summarise(TOT.OBS = n()) %>%
#   ungroup() %>%
#   mutate(COMMON.NAME = factor(COMMON.NAME),
#          CEN.LON = as.numeric(CEN.LON),
#          CEN.LAT = as.numeric(CEN.LAT),
#          TOT.OBS = as.numeric(TOT.OBS)) %>%
#   as.data.frame()
# 
# 
# # resolution of 25kmx25km cells is approx. 14 min (13.53, 14.33)
# 
# tictoc::tic("Calculating completenss")
# data(adworld)
# KnowBPolygon(data = data2, shape = districtmap, shapenames = "districtmap",
#              save = "RData") # generate gridded file
# tictoc::toc() 
# # KnowB (cells) in 1309 sec (21 min) with 1000 rows of data (973 rows input)


