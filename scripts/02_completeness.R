
# Following Peterson (2018) 

require(tidyverse)


calc_exp_spec <- function(s_obs, N, q1, q2) {
  
  s_exp <- s_obs + ( ((N - 1)/N) * ((q1*(q1 - 1)) / (2*(q2 + 1))) )
  
  return(s_exp)
  
}

calc_inv_comp <- function(s_exp, s_obs) {
  
  # for districts like Nicobars, district has more species than ecoregion
  # so completeness > 1
  C <- ifelse(s_obs > s_exp, 1, s_obs/s_exp)
  
  return(C)
  
}


# importing ecoregions data ---------------------------------------------------

# ecoregions
sf_use_s2(FALSE)
temp <- st_read(dsn = "data/Ecoregions2017", layer = "Ecoregions2017")

ecoregions <- temp %>% 
  dplyr::select("ECO_NAME") %>% 
  st_set_crs(st_crs(india_sf)) %>% 
  st_intersection(india_sf)

# reclassification of ecoregion data (from discussion with group [SoIB hist_spread])
reclass <- read_csv("data/Ecoregions2017_reclassification.csv")

ecoregions <- ecoregions %>% 
  left_join(reclass) %>% 
  dplyr::select(-ECO_NAME, -ECO_RECLASS1, -ECO_RECLASS2, -FINAL_RECLASS) %>% 
  # to merge original ecoregions according to PJ_RECLASS
  group_by(PJ_RECLASS) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  ungroup() %>% 
  rename(ECOREGION = PJ_RECLASS)


# calculating expected species richness ---------------------------------------

# calculating S.EXP for each ecoregion, then INV.COMP per district, according to which 1-2
# ecoregions it comes under
tictoc::tic("Spatial joins of districts and ecoregions to main data")
data1 <- data0 %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE")) %>% 
  st_set_crs(st_crs(ecoregions)) %>% 
  # joining ecoregion data
  st_join(ecoregions)
tictoc::toc() # 28 sec

# linking districts to ecoregions
eco_dist_link <- ecoregions %>% 
  st_join(dists_sf) %>% 
  st_drop_geometry() %>% 
  distinct(DISTRICT.NAME, ECOREGION)

temp1 <- data %>% 
  # joining ecoregion information
  left_join(data1 %>% 
              st_drop_geometry() %>% 
              dplyr::select(SAMPLING.EVENT.IDENTIFIER, ECOREGION, DISTRICT.NAME)) %>% 
  # how many lists species reported from
  group_by(ECOREGION, COMMON.NAME) %>% 
  mutate(SPEC.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(SPEC.LISTS %in% 1:2) %>% 
  # how many species of Q1 and Q2
  mutate(Q = ifelse(SPEC.LISTS == 1, "Q1", "Q2")) %>% 
  group_by(ECOREGION, Q) %>% 
  summarise(Qn = n_distinct(COMMON.NAME)) %>% 
  pivot_wider(names_from = "Q", values_from = "Qn") %>% 
  mutate(Q1.ECO = replace_na(Q1, 0),
         Q2.ECO = replace_na(Q2, 0))
  
ecoregion_exp <- data %>% 
  # joining ecoregion information
  left_join(data1 %>% 
              st_drop_geometry() %>% 
              dplyr::select(SAMPLING.EVENT.IDENTIFIER, ECOREGION, DISTRICT.NAME)) %>% 
  filter(!is.na(ECOREGION)) %>% 
  # number of lists (N) and species (s_obs) from ecoregion 
  group_by(ECOREGION) %>% 
  summarise(N.ECO = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            S.OBS.ECO = n_distinct(COMMON.NAME)) %>% 
  left_join(temp1) %>% 
  # calculating S.EXP for ecoregion
  mutate(S.EXP.ECO = calc_exp_spec(S.OBS.ECO, N.ECO, Q1.ECO, Q2.ECO) %>% floor()) %>% 
  # linking ecoregions with districts, to average S.EXP.ECO across multiple
  # ecoregions per district
  right_join(eco_dist_link) %>% 
  group_by(DISTRICT.NAME) %>% 
  summarise(S.EXP.ECO = floor(mean(S.EXP.ECO)))


temp2 <- data %>% 
  # joining ecoregion information
  left_join(data1 %>% 
              st_drop_geometry() %>% 
              dplyr::select(SAMPLING.EVENT.IDENTIFIER, ECOREGION, DISTRICT.NAME)) %>% 
  # how many lists species reported from
  group_by(DISTRICT.NAME, COMMON.NAME) %>% 
  mutate(SPEC.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(SPEC.LISTS %in% 1:2) %>% 
  # how many species of Q1 and Q2
  mutate(Q = ifelse(SPEC.LISTS == 1, "Q1", "Q2")) %>% 
  group_by(DISTRICT.NAME, Q) %>% 
  summarise(Qn = n_distinct(COMMON.NAME)) %>% 
  pivot_wider(names_from = "Q", values_from = "Qn") %>% 
  mutate(Q1 = replace_na(Q1, 0),
         Q2 = replace_na(Q2, 0)) %>% 
  rename(Q1.DIST = Q1,
         Q2.DIST = Q2)

district_exp <- data %>% 
  # joining ecoregion information
  left_join(data1 %>% 
              st_drop_geometry() %>% 
              dplyr::select(SAMPLING.EVENT.IDENTIFIER, ECOREGION, DISTRICT.NAME)) %>% 
  filter(!is.na(DISTRICT.NAME)) %>% 
  # number of lists (N) and species (s_obs) from district 
  group_by(DISTRICT.NAME) %>% 
  summarise(N.DIST = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            S.OBS.DIST = n_distinct(COMMON.NAME)) %>% 
  left_join(temp2) %>% 
  # calculating S.EXP for district based on existing lists from district
  mutate(S.EXP.DIST = calc_exp_spec(S.OBS.DIST, N.DIST, Q1.DIST, Q2.DIST) %>% floor()) %>% 
  # this brings in districts with no lists
  full_join(ecoregion_exp) %>% 
  replace_na(list(N.DIST = 0, S.OBS.DIST = 0, S.EXP.DIST = 0)) %>% 
  # calculating S.EXP for district based on existing lists AND on S.EXP of ecoregion
  mutate(S.EXP = floor((S.EXP.DIST + S.EXP.ECO)/2))


# calculating inventory completeness --------------------------------------

data2 <- district_exp %>% 
  # calculating C
  mutate(INV.C = calc_inv_comp(S.EXP, S.OBS.DIST) %>% round(2))
  
  
# writing -----------------------------------------------------------------

save(data2, district_exp, ecoregion_exp, eco_dist_link, data1,
     ecoregions, reclass, 
     file = "data/02_completeness.RData")

