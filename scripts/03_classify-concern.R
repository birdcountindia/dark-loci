thresh_path <- "outputs/concern_thresh.xlsx"
class_path <- "outputs/concern_classification.xlsx"


# defining thresholds ---------------------------------------------------------------

# we should use the same threshold levels for one full year, only changing it in the next year
# this is to allow meaningful comparisons month-to-month for tracking

if (!file.exists(thresh_path)) {
  
  return("No previous file for threshold definitions!")
  
} else {
  
  # original database of threshold levels
  thresh1_data <- read_xlsx(thresh_path, sheet = "FINE") %>% 
    mutate(DATE = str_c(YEAR, str_pad(MONTH, 2, pad = "0"), "01", sep = "-") %>% as_date())
  
  thresh2_data <- read_xlsx(thresh_path, sheet = "COARSE") %>% 
    mutate(DATE = str_c(YEAR, str_pad(MONTH, 2, pad = "0"), "01", sep = "-") %>% as_date())
  
  
  thresh1_cur <- thresh1_data %>% slice_tail()
  thresh2_cur <- thresh2_data %>% slice_tail()
  
  # to ensure both are on same page
  if (thresh1_cur$DATE != thresh2_cur$DATE) {
    
    return("Date mismatch between threshold definitions of the two resolutions.")
    
  } else {     
    
    temp0 <- data_invcomp %>% 
      # reordering columns
      dplyr::select(STATE.CODE, STATE, COUNTY.CODE, COUNTY, 
                    S.OBS.DIST, S.EXP.DIST, S.EXP, N.DIST, INV.C) %>% 
      # removing districts with >=75% completeness cos they are of no concern
      filter(!(INV.C >= classify_concern(get_thresh_noconcern = TRUE)))
    
    
    # only create and use new thresholds if one year has passed
    
    if (!(date_real - thresh1_cur$DATE >= 365)) {

      print("Using current threshold definitions.")
      
    } else {
      
      # since one year has passed, we define new thresholds
      
      thresh_lev1 <- seq(1, n_distinct(temp0$DISTRICT.NAME), length.out = 6)[2:5] # we want 5 groups
      thresh_lev2 <- seq(1, n_distinct(temp0$DISTRICT.NAME), length.out = 4)[2:3] # we want only 3 groups
      
      # selecting thresholds based on unique values of completeness, dividing into 5 groups
      thresh1_cur <- temp0 %>% 
        distinct(STATE.CODE, STATE, COUNTY.CODE, COUNTY, INV.C) %>% 
        dplyr::select(INV.C) %>% 
        arrange(INV.C) %>% 
        rownames_to_column("ROW") %>% 
        # selecting thresholds
        filter(ROW %in% floor(thresh_lev1)) %>% 
        pivot_wider(names_from = ROW, values_from = INV.C) %>% 
        magrittr::set_colnames(c("THRESH.FINE1", "THRESH.FINE2", "THRESH.FINE3", "THRESH.FINE4")) %>% 
        mutate(YEAR = real_year, MONTH = real_month_num) %>% 
        relocate(YEAR, MONTH, THRESH.FINE1, THRESH.FINE2, THRESH.FINE3, THRESH.FINE4)
      
      thresh2_cur <- temp0 %>% 
        distinct(STATE.CODE, STATE, COUNTY.CODE, COUNTY, INV.C) %>% 
        dplyr::select(INV.C) %>% 
        arrange(INV.C) %>% 
        rownames_to_column("ROW") %>% 
        # selecting thresholds
        filter(ROW %in% floor(thresh_lev2)) %>% 
        pivot_wider(names_from = ROW, values_from = INV.C) %>% 
        magrittr::set_colnames(c("THRESH.COARSE1", "THRESH.COARSE2")) %>% 
        mutate(YEAR = real_year, MONTH = real_month_num) %>% 
        relocate(YEAR, MONTH, THRESH.COARSE1, THRESH.COARSE2)
      
      
      # writing updated threshold definitions to file
      thresh1_data <- thresh1_data %>% bind_rows(thresh1_cur) %>% arrange(YEAR, MONTH)
      thresh2_data <- thresh2_data %>% bind_rows(thresh2_cur) %>% arrange(YEAR, MONTH)
      
      write_xlsx(x = list("FINE" = thresh1_data, "COARSE" = thresh2_data),
                 path = thresh_path)
      
      print("Using updated threshold definitions.")
      
    }
  }
}


# classifying concern ---------------------------------------------------------------

concern_class_cur <- temp0 %>% 
  # adding columns of current year's thresholds
  bind_cols(thresh1_cur %>% dplyr::select(-YEAR, -MONTH, -DATE)) %>% 
  bind_cols(thresh2_cur %>% dplyr::select(-YEAR, -MONTH, -DATE)) %>% 
  classify_concern("fine") %>% 
  classify_concern("coarse") %>% 
  arrange(desc(CONCERN.FINE), INV.C, desc(N.DIST), STATE.CODE, COUNTY.CODE) %>% 
  mutate(across(contains("THRESH."), ~ as.null(.))) %>% # remove threshold columns
  # saving as year-month combo of EBD release (instead of real-time year-month)
  mutate(YEAR = currel_year, MONTH = currel_month_num) %>% 
  relocate(STATE.CODE, COUNTY.CODE, STATE, COUNTY, N.DIST, 
           YEAR, MONTH, CONCERN.FINE, CONCERN.COARSE, S.OBS.DIST, S.EXP.DIST, S.EXP, INV.C)

# not removing low concern category because this will be history of concern, so good to keep
# they will be removed in the metrics to be tracked


# whether or not to append new classifications to old ones each time

if (!file.exists(class_path)) {
  
  concern_class_upd <- concern_class_cur %>% 
    arrange(STATE.CODE, COUNTY.CODE, YEAR, MONTH)
  
} else {
  
  # need latest (month before current) classification for each district in the country
  concern_class_latest <- read_xlsx(class_path) %>% 
    group_by(pick(c(everything(), -MONTH))) %>% 
    arrange(MONTH) %>% 
    slice_tail() %>% 
    ungroup()
  
  # if running the script more than once in any particular month, don't want to create
  # duplicate row
  # but we still need the objects in the environment
  
  avoid_duplicate <- (unique(concern_class_latest$YEAR) == currel_year & 
                       unique(concern_class_latest$MONTH) == currel_month_num)

    concern_class_upd <- read_xlsx(class_path) %>% 
      {if (avoid_duplicate) {
        filter(., !(YEAR == currel_year & MONTH == currel_month_num)) # removing repeated rows
      }} %>%
      bind_rows(concern_class_cur) %>% 
      arrange(STATE.CODE, COUNTY.CODE, YEAR, MONTH) %>% 
      relocate(STATE.CODE, COUNTY.CODE, STATE, COUNTY, N.DIST, 
               YEAR, MONTH, CONCERN.FINE, CONCERN.COARSE, S.OBS.DIST, S.EXP.DIST, S.EXP, INV.C)
    
}

# writing objects
write_xlsx(x = list("Concern classifications" = concern_class_upd),
           path = class_path)

save(concern_class_upd, concern_class_cur, 
     file = get_stage_obj_path("data", "concern", add_rel_str = TRUE))
