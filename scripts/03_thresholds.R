thresh_path <- "outputs/concern_thresh.xlsx"
class_path <- "outputs/concern_classification.xlsx"


temp0 <- data2 %>% 
  # join district geom then state names
  left_join(dists_sf %>% dplyr::select(-STATE.NAME, -AREA)) %>% 
  st_as_sf() %>% 
  st_make_valid() %>% # to prevent issue with largest join
  st_join(states_sf %>% st_make_valid(), largest = T) %>% 
  st_drop_geometry() %>% 
  # reordering columns
  dplyr::select(STATE.NAME, DISTRICT.NAME, S.OBS.DIST, S.EXP.DIST, S.EXP, N.DIST, INV.C) %>% 
  relocate(STATE.NAME, DISTRICT.NAME, S.OBS.DIST, S.EXP.DIST, S.EXP, N.DIST, INV.C) %>% 
  # removing >=75% completeness cos of no concern
  filter(!(INV.C >= 0.75))


thresh_lev1 <- seq(1, n_distinct(temp0$DISTRICT.NAME), length.out = 6)[2:5] # we want 5 groups
thresh_lev2 <- seq(1, n_distinct(temp0$DISTRICT.NAME), length.out = 4)[2:3] # we want only 3 groups


# selecting thresholds based on unique values of completeness, dividing into 5 groups
thresh1 <- temp0 %>% 
  distinct(DISTRICT.NAME, INV.C) %>% 
  dplyr::select(INV.C) %>% 
  arrange(INV.C) %>% 
  rownames_to_column("ROW") %>% 
  # selecting thresholds
  filter(ROW %in% floor(thresh_lev1)) %>% 
  pivot_wider(names_from = ROW, values_from = INV.C) %>% 
  set_colnames(c("THRESH.FINE1", "THRESH.FINE2", "THRESH.FINE3", "THRESH.FINE4"))

thresh2 <- temp0 %>% 
  distinct(DISTRICT.NAME, INV.C) %>% 
  dplyr::select(INV.C) %>% 
  arrange(INV.C) %>% 
  rownames_to_column("ROW") %>% 
  # selecting thresholds
  filter(ROW %in% floor(thresh_lev2)) %>% 
  pivot_wider(names_from = ROW, values_from = INV.C) %>% 
  set_colnames(c("THRESH.COARSE1", "THRESH.COARSE2"))


concern_class <- temp0 %>% 
  bind_cols(thresh1) %>% 
  bind_cols(thresh2) %>% 
  # levels of incompleteness based on thresholds (>=75% completeness of no concern)
  mutate(CONCERN.FINE = case_when(INV.C < 0.75 & INV.C >= THRESH.FINE4 ~ 1,
                                  INV.C < THRESH.FINE4 & INV.C >= THRESH.FINE3 ~ 2,
                                  INV.C < THRESH.FINE3 & INV.C >= THRESH.FINE2 ~ 3,
                                  INV.C < THRESH.FINE2 & INV.C >= THRESH.FINE1 ~ 4,
                                  INV.C < THRESH.FINE1 ~ 5)) %>% 
  mutate(CONCERN.COARSE = case_when(INV.C < 0.75 & INV.C >= THRESH.COARSE2 ~ "LOW",
                                    INV.C < THRESH.COARSE2 & INV.C >= THRESH.COARSE1 ~ "MID",
                                    INV.C < THRESH.COARSE1 ~ "HIGH")) %>% 
  mutate(CONCERN.COARSE = factor(CONCERN.COARSE, levels = c("LOW", "MID", "HIGH"))) %>% 
  arrange(desc(CONCERN.FINE), INV.C, desc(N.DIST), STATE.NAME, DISTRICT.NAME) %>% 
  # # removing LOW concern for now to focus only on MID & HIGH
  # filter(CONCERN.FINE != 1) %>% 
  mutate(across(contains("THRESH."), ~ as.null(.)))



# here, if original classification exists, we can read it in and left_join them
if (file.exists(class_path)) {
  
  # original threshold levels
  concern_class_orig <- read_xlsx(class_path) %>% 
    distinct(STATE.NAME, DISTRICT.NAME, CONCERN.FINE, CONCERN.COARSE) %>% 
    rename(ORIG.CONCERN.FINE = CONCERN.FINE, 
           ORIG.CONCERN.COARSE = CONCERN.COARSE)
  
  concern_class <- concern_class %>% 
    left_join(concern_class_orig) %>% 
    relocate(STATE.NAME, DISTRICT.NAME, S.OBS.DIST, S.EXP.DIST, S.EXP, N.DIST, INV.C,
             ORIG.CONCERN.FINE, CONCERN.FINE, ORIG.CONCERN.COARSE, CONCERN.COARSE)
  
}


# same with original threshold levels

thresh1 <- thresh1 %>% 
  mutate(YEAR = cur_year, MONTH = cur_month_num) %>% 
  relocate(YEAR, MONTH, THRESH.FINE1, THRESH.FINE2, THRESH.FINE3, THRESH.FINE4)

thresh2 <- thresh2 %>% 
  mutate(YEAR = cur_year, MONTH = cur_month_num) %>% 
  relocate(YEAR, MONTH, THRESH.COARSE1, THRESH.COARSE2)

if (file.exists(thresh_path)) {
  
  # original threshold levels
  thresh1_orig <- read_xlsx(thresh_path, sheet = "FINE") %>% slice(1) # choosing first classification 
  thresh2_orig <- read_xlsx(thresh_path, sheet = "COARSE") %>% slice(1) 
  
  thresh1 <- thresh1_orig %>% bind_rows(thresh1) %>% arrange(YEAR, MONTH)
  thresh2 <- thresh2_orig %>% bind_rows(thresh2) %>% arrange(YEAR, MONTH)
  
}


# writing objects
write_xlsx(x = list("Concern classifications" = concern_class),
           path = class_path)

write_xlsx(x = list("FINE" = thresh1, "COARSE" = thresh2),
           path = thresh_path)