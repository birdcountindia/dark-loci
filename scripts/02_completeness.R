# Following AT Peterson et al (2018), Digital Accessible Knowledge 
# of the birds of India: characterizing gaps in time and space


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

# calculating SPEC.EXP for each ecoregion, then INV.COMP per district, according to which 1-2
# ecoregions it comes under
tictoc::tic("Spatial joins of districts and ecoregions to main data")
data_ecoreg_sf <- data_spat %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE")) %>% 
  st_set_crs(st_crs(ecoregions)) %>% 
  # joining ecoregion data
  st_join(ecoregions)
tictoc::toc() # 28 sec

data_ecoreg <- data %>% 
  # joining ecoregion information
  left_join(data_ecoreg_sf %>% 
              st_drop_geometry() %>% 
              dplyr::select(SAMPLING.EVENT.IDENTIFIER, ECOREGION),
            by = "SAMPLING.EVENT.IDENTIFIER", 
            relationship = "many-to-many") # some SEI have 2 GROUP.IDs


# linking districts to ecoregions
# our primary analysis focus is on districts in EBD, so avoiding using names from dists_sf
eco_dist_link <- data_ecoreg %>% 
  distinct(STATE.CODE, STATE, COUNTY.CODE, COUNTY, ECOREGION) %>% 
  filter(!is.na(COUNTY) & !is.na(COUNTY.CODE) & !is.na(ECOREGION))


# 1. SPEC.EXP for each ecoregion

temp1 <- data_ecoreg %>% 
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
  
ecoregion_exp <- data_ecoreg %>% 
  filter(!is.na(ECOREGION)) %>% 
  # number of lists (N) and species (s_obs) from ecoregion 
  group_by(ECOREGION) %>% 
  summarise(LISTS.ECO = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            SPEC.OBS.ECO = n_distinct(COMMON.NAME)) %>% 
  left_join(temp1) %>% 
  # calculating SPEC.EXP for ecoregion
  mutate(SPEC.EXP.ECO = calc_exp_spec(SPEC.OBS.ECO, LISTS.ECO, Q1.ECO, Q2.ECO) %>% floor()) %>% 
  # linking ecoregions with districts, to average SPEC.EXP.ECO across multiple
  # ecoregions per district
  right_join(eco_dist_link) %>% 
  group_by(STATE.CODE, STATE, COUNTY.CODE, COUNTY) %>% 
  # when district has multiple ecoregions, averaging across them
  summarise(SPEC.EXP.ECO = floor(mean(SPEC.EXP.ECO))) %>% 
  ungroup()


# 2. INV.COMP per district

temp2 <- data_ecoreg %>% 
  # how many lists species reported from
  group_by(STATE.CODE, STATE, COUNTY.CODE, COUNTY, COMMON.NAME) %>% 
  mutate(SPEC.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(SPEC.LISTS %in% 1:2) %>% 
  # how many species of Q1 and Q2
  mutate(Q = ifelse(SPEC.LISTS == 1, "Q1", "Q2")) %>% 
  group_by(STATE.CODE, STATE, COUNTY.CODE, COUNTY, Q) %>% 
  summarise(Qn = n_distinct(COMMON.NAME)) %>% 
  pivot_wider(names_from = "Q", values_from = "Qn") %>% 
  mutate(Q1 = replace_na(Q1, 0),
         Q2 = replace_na(Q2, 0)) %>% 
  rename(Q1.DIST = Q1,
         Q2.DIST = Q2)

district_exp <- data_ecoreg %>% 
  filter(!is.na(COUNTY.CODE)) %>% 
  # number of lists (N) and species (s_obs) from district 
  group_by(STATE.CODE, STATE, COUNTY.CODE, COUNTY) %>% 
  summarise(LISTS.DIST = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            SPEC.OBS.DIST = n_distinct(COMMON.NAME)) %>% 
  ungroup() %>% 
  left_join(temp2) %>% 
  # calculating SPEC.EXP for district based on existing lists from district
  mutate(SPEC.EXP.DIST = calc_exp_spec(SPEC.OBS.DIST, LISTS.DIST, Q1.DIST, Q2.DIST) %>% 
           floor()) %>% 
  # this brings in districts with no lists
  full_join(ecoregion_exp) %>% 
  replace_na(list(LISTS.DIST = 0, SPEC.OBS.DIST = 0, SPEC.EXP.DIST = 0)) %>% 
  # calculating SPEC.EXP for district based on existing lists AND on SPEC.EXP of ecoregion
  mutate(SPEC.EXP = floor((SPEC.EXP.DIST + SPEC.EXP.ECO)/2))


# calculating inventory completeness --------------------------------------

data_invcomp <- district_exp %>% 
  # calculating C
  mutate(INV.C = calc_inv_comp(SPEC.EXP, SPEC.OBS.DIST) %>% 
           round(2))
  
  
# writing -----------------------------------------------------------------

save(data_invcomp, 
     # district_exp, ecoregion_exp, eco_dist_link, data_ecoreg_sf, ecoregions, reclass, 
     file = get_stage_obj_path("data", "completeness", add_rel_str = TRUE))

