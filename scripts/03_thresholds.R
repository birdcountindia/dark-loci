thresh_path <- "outputs/concern_thresh.xlsx"
class_path <- "outputs/concern_classification.xlsx"


# defining thresholds ---------------------------------------------------------------

# we should use the same threshold levels for one full year, only changing it in the next year
# this is to allow meaningful comparisons month-to-month for tracking

if (file.exists(thresh_path)) {
  
  # original threshold levels
  thresh1_orig <- read_xlsx(thresh_path, sheet = "FINE")
  thresh2_orig <- read_xlsx(thresh_path, sheet = "COARSE")
  
  test1 <- thresh1_orig %>% 
    slice_tail() %>% 
    mutate(DATE = str_c(YEAR, str_pad(MONTH, 2, pad = "0"), "01", sep = "-") %>% 
             as_date())
  
  test2 <- thresh2_orig %>% 
    slice_tail() %>% 
    mutate(DATE = str_c(YEAR, str_pad(MONTH, 2, pad = "0"), "01", sep = "-") %>% 
             as_date())
  
  # to ensure both are on same page
  if (test1$DATE != test2$DATE) {
    
    return("Date mismatch between threshold definitions of the two resolutions.")
    
  } else {     
    
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
    
    # only use new thresholds if one year has passed
    
    if (!(cur_date - test1$DATE >= 365)) {
      
      thresh1_cur <- thresh1_orig
      thresh2_cur <- thresh2_orig
      
      print("Using current threshold definitions.")
      
    } else {
      
      # since one year has passed, we define new thresholds
      
      thresh_lev1 <- seq(1, n_distinct(temp0$DISTRICT.NAME), length.out = 6)[2:5] # we want 5 groups
      thresh_lev2 <- seq(1, n_distinct(temp0$DISTRICT.NAME), length.out = 4)[2:3] # we want only 3 groups
      
      
      # selecting thresholds based on unique values of completeness, dividing into 5 groups
      thresh1_cur <- temp0 %>% 
        distinct(DISTRICT.NAME, INV.C) %>% 
        dplyr::select(INV.C) %>% 
        arrange(INV.C) %>% 
        rownames_to_column("ROW") %>% 
        # selecting thresholds
        filter(ROW %in% floor(thresh_lev1)) %>% 
        pivot_wider(names_from = ROW, values_from = INV.C) %>% 
        set_colnames(c("THRESH.FINE1", "THRESH.FINE2", "THRESH.FINE3", "THRESH.FINE4")) %>% 
        mutate(YEAR = cur_year, MONTH = cur_month_num) %>% 
        relocate(YEAR, MONTH, THRESH.FINE1, THRESH.FINE2, THRESH.FINE3, THRESH.FINE4)
      
      thresh2_cur <- temp0 %>% 
        distinct(DISTRICT.NAME, INV.C) %>% 
        dplyr::select(INV.C) %>% 
        arrange(INV.C) %>% 
        rownames_to_column("ROW") %>% 
        # selecting thresholds
        filter(ROW %in% floor(thresh_lev2)) %>% 
        pivot_wider(names_from = ROW, values_from = INV.C) %>% 
        set_colnames(c("THRESH.COARSE1", "THRESH.COARSE2")) %>% 
        mutate(YEAR = cur_year, MONTH = cur_month_num) %>% 
        relocate(YEAR, MONTH, THRESH.COARSE1, THRESH.COARSE2)
      
      
      # writing updated threshold definitions to file
      thresh1_upd <- thresh1_orig %>% bind_rows(thresh1_cur) %>% arrange(YEAR, MONTH)
      thresh2_upd <- thresh2_orig %>% bind_rows(thresh2_cur) %>% arrange(YEAR, MONTH)
      
      write_xlsx(x = list("FINE" = thresh1_upd, "COARSE" = thresh2_upd),
                 path = thresh_path)
      
      print("Using updated threshold definitions.")
      
      
    }

  }
  
} else {
  
  return("No previous file for threshold definitions!")
  
}


# classifying concern ---------------------------------------------------------------

concern_class_cur <- temp0 %>% 
  bind_cols(thresh1_cur %>% dplyr::select(-YEAR, -MONTH)) %>% 
  bind_cols(thresh2_cur %>% dplyr::select(-YEAR, -MONTH)) %>% 
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
  mutate(across(contains("THRESH."), ~ as.null(.))) %>% 
  mutate(YEAR = cur_year, MONTH = cur_month_num)
# not removing low concern category because this will be history of concern, so good to keep
# in the metrics to be tracked, they will be removed


# each time updating new classifications to old ones
if (!file.exists(class_path)) {
  
  concern_class_upd <- concern_class_cur %>% 
    arrange(STATE.NAME, DISTRICT.NAME, YEAR, MONTH, CONCERN.FINE, CONCERN.COARSE) %>% 
    # adding codes
    left_join(region_codes %>% distinct(STATE, STATE.CODE, COUNTY, COUNTY.CODE), 
              by = c("STATE.NAME" = "STATE", "DISTRICT.NAME" = "COUNTY")) %>% 
    relocate(STATE.CODE, COUNTY.CODE, STATE.NAME, DISTRICT.NAME, N.DIST, 
             YEAR, MONTH, CONCERN.FINE, CONCERN.COARSE, S.OBS.DIST, S.EXP.DIST, S.EXP, INV.C)
  
} else if (file.exists(class_path) &
    ((read_xlsx(class_path) %>% slice_tail())$YEAR != cur_year |
     ((read_xlsx(class_path) %>% slice_tail())$YEAR == cur_year & 
     (read_xlsx(class_path) %>% slice_tail())$MONTH != cur_month_num))) {
  
  concern_class_cur <- concern_class_cur %>% 
    # adding codes
    left_join(region_codes %>% distinct(STATE, STATE.CODE, COUNTY, COUNTY.CODE), 
              by = c("STATE.NAME" = "STATE", "DISTRICT.NAME" = "COUNTY")) %>% 
    relocate(STATE.CODE, COUNTY.CODE, STATE.NAME, DISTRICT.NAME, N.DIST, 
             YEAR, MONTH, CONCERN.FINE, CONCERN.COARSE, S.OBS.DIST, S.EXP.DIST, S.EXP, INV.C)
  
  concern_class_upd <- read_xlsx(class_path) %>% 
    bind_rows(concern_class_cur) %>% 
    arrange(STATE.NAME, DISTRICT.NAME, YEAR, MONTH, CONCERN.FINE, CONCERN.COARSE) %>% 
    relocate(STATE.CODE, COUNTY.CODE, STATE.NAME, DISTRICT.NAME, N.DIST, 
             YEAR, MONTH, CONCERN.FINE, CONCERN.COARSE, S.OBS.DIST, S.EXP.DIST, S.EXP, INV.C)
  
} else if (file.exists(class_path) &
           ((read_xlsx(class_path) %>% slice_tail())$YEAR == cur_year & 
             (read_xlsx(class_path) %>% slice_tail())$MONTH == cur_month_num)) {
  
  # this is if running the script more than once in any particular month
  
  concern_class_cur <- concern_class_cur %>% 
    # adding codes
    left_join(region_codes %>% distinct(STATE, STATE.CODE, COUNTY, COUNTY.CODE), 
              by = c("STATE.NAME" = "STATE", "DISTRICT.NAME" = "COUNTY")) %>% 
    relocate(STATE.CODE, COUNTY.CODE, STATE.NAME, DISTRICT.NAME, N.DIST, 
             YEAR, MONTH, CONCERN.FINE, CONCERN.COARSE, S.OBS.DIST, S.EXP.DIST, S.EXP, INV.C)
  
  concern_class_upd <- read_xlsx(class_path) %>% 
    filter(!(YEAR == cur_year & MONTH == cur_month_num)) %>% 
    bind_rows(concern_class_cur) %>% 
    arrange(STATE.NAME, DISTRICT.NAME, YEAR, MONTH, CONCERN.FINE, CONCERN.COARSE) %>% 
    relocate(STATE.CODE, COUNTY.CODE, STATE.NAME, DISTRICT.NAME, N.DIST, 
             YEAR, MONTH, CONCERN.FINE, CONCERN.COARSE, S.OBS.DIST, S.EXP.DIST, S.EXP, INV.C)
  
} 

# writing objects
write_xlsx(x = list("Concern classifications" = concern_class_upd),
           path = class_path)
