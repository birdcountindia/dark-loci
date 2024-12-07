path_thresh <- "outputs/concern_thresh.xlsx"
path_class_cur <- get_stage_obj_path("outputs", "class", add_rel_str = TRUE)
path_class_prev <- get_stage_obj_path("outputs", "class", add_rel_str = TRUE, months_lag = 1)


# defining thresholds ---------------------------------------------------------------

# we should use the same threshold levels for one full year, only changing it in the next year
# this is to allow meaningful comparisons month-to-month for tracking

if (!file.exists(path_thresh)) {
  
  return("No previous file for threshold definitions!")
  
} else {
  
  # original database of threshold levels
  thresh1_data <- read_xlsx(path_thresh, sheet = "FINE") %>% 
    mutate(DATE = str_c(YEAR, str_pad(MONTH, 2, pad = "0"), "01", sep = "-") %>% as_date())
  
  thresh2_data <- read_xlsx(path_thresh, sheet = "COARSE") %>% 
    mutate(DATE = str_c(YEAR, str_pad(MONTH, 2, pad = "0"), "01", sep = "-") %>% as_date())
  
  # we want to continue to use original thresholds for meaningful tracking
  thresh1_orig <- thresh1_data %>% slice_head()
  thresh2_orig <- thresh2_data %>% slice_head()
  
  # to ensure both are on same page
  if (thresh1_orig$DATE != thresh2_orig$DATE) {
    
    return("Date mismatch between threshold definitions of the two resolutions.")
    
  } else {     
    
    temp0 <- data_invcomp %>% 
      # reordering columns
      dplyr::select(STATE.CODE, STATE, COUNTY.CODE, COUNTY, 
                    SPEC.OBS.DIST, SPEC.EXP.DIST, SPEC.EXP.ECO, 
                    SPEC.EXP, LISTS.DIST, INV.C) %>% 
      # need to retain districts with >= 75% completeness if they were <75% when 
      # current track cycle started, or in previous month out of nowhere
      {if (!(date_currel - get_track_dates(date_currel)$START < 27)) {
        left_join(., 
                  get_concern_class_start(date_currel) %>% 
                    reframe(COUNTY.CODE = COUNTY.CODE,
                            ESSENTIAL1 = 1),
                  by = "COUNTY.CODE") %>% 
          mutate(ESSENTIAL1 = replace_na(ESSENTIAL1, 0))
      } else {
        mutate(., ESSENTIAL1 = 0)
      }} %>% 
      # previous month (if some district crept in)
      {if (file.exists(path_class_prev)) {
        left_join(.,
                  read_xlsx(path_class_prev) %>% 
                    distinct(COUNTY.CODE) %>% 
                    reframe(COUNTY.CODE = COUNTY.CODE,
                            ESSENTIAL2 = 1),
                  by = "COUNTY.CODE") %>% 
          mutate(ESSENTIAL2 = replace_na(ESSENTIAL2, 0))
      } else {
        mutate(.,
               ESSENTIAL2 = ESSENTIAL1)
      }} %>% 
      mutate(REMOVE = case_when(
        ESSENTIAL1 != 1 & ESSENTIAL2 != 1 & 
          (INV.C >= classify_concern(get_thresh_noconcern = TRUE)) ~ 1,
        TRUE ~ 0
      )) %>% 
      # removing districts with >=75% completeness cos they are of no concern
      filter(REMOVE != 1) %>% 
      dplyr::select(-c(starts_with("ESSENTIAL"), REMOVE))
    
    
    # only create new thresholds if one year has passed
    
    if (!(date_currel - get_track_dates(date_currel)$START < 27)) {

      print("Using current threshold definitions.")
      
    } else {
      
      # since one year has passed, we define new thresholds
      # (but we continue to use original thresholds for tracking purposes)
      
      thresh_lev1 <- seq(1, n_distinct(temp0$COUNTY.CODE), length.out = 6)[2:5] # we want 5 groups
      thresh_lev2 <- seq(1, n_distinct(temp0$COUNTY.CODE), length.out = 4)[2:3] # we want only 3 groups
      
      # selecting thresholds based on unique values of completeness, dividing into 5 groups
      thresh1_cur_new <- temp0 %>% 
        distinct(STATE.CODE, STATE, COUNTY.CODE, COUNTY, INV.C) %>% 
        dplyr::select(INV.C) %>% 
        arrange(INV.C) %>% 
        rownames_to_column("ROW") %>% 
        # selecting thresholds
        filter(ROW %in% floor(thresh_lev1)) %>% 
        pivot_wider(names_from = ROW, values_from = INV.C) %>% 
        magrittr::set_colnames(c("THRESH.FINE1", "THRESH.FINE2", "THRESH.FINE3", "THRESH.FINE4")) %>% 
        mutate(YEAR = real_year, MONTH = real_month_num) %>% 
        relocate(YEAR, MONTH, THRESH.FINE1, THRESH.FINE2, THRESH.FINE3, THRESH.FINE4) %>% 
        mutate(DATE = date_real)
      
      thresh2_cur_new <- temp0 %>% 
        distinct(STATE.CODE, STATE, COUNTY.CODE, COUNTY, INV.C) %>% 
        dplyr::select(INV.C) %>% 
        arrange(INV.C) %>% 
        rownames_to_column("ROW") %>% 
        # selecting thresholds
        filter(ROW %in% floor(thresh_lev2)) %>% 
        pivot_wider(names_from = ROW, values_from = INV.C) %>% 
        magrittr::set_colnames(c("THRESH.COARSE1", "THRESH.COARSE2")) %>% 
        mutate(YEAR = real_year, MONTH = real_month_num) %>% 
        relocate(YEAR, MONTH, THRESH.COARSE1, THRESH.COARSE2) %>% 
        mutate(DATE = date_real)
      
      
      # writing updated threshold definitions to file
      thresh1_data_new <- thresh1_data %>% bind_rows(thresh1_cur_new) %>% arrange(YEAR, MONTH)
      thresh2_data_new <- thresh2_data %>% bind_rows(thresh2_cur_new) %>% arrange(YEAR, MONTH)
      
      write_xlsx(x = list("FINE" = thresh1_data_new, "COARSE" = thresh2_data_new),
                 path = path_thresh)

    }
  }
}


# classifying concern ---------------------------------------------------------------

concern_class_cur <- temp0 %>% 
  # adding columns of current year's thresholds
  # we still use original thresholds in order for meaningful tracking
  bind_cols(thresh1_orig %>% dplyr::select(-YEAR, -MONTH, -DATE)) %>% 
  bind_cols(thresh2_orig %>% dplyr::select(-YEAR, -MONTH, -DATE)) %>% 
  classify_concern("fine") %>% 
  classify_concern("coarse") %>% 
  arrange(desc(CONCERN.FINE), INV.C, desc(LISTS.DIST), STATE.CODE, COUNTY.CODE) %>% 
  mutate(across(contains("THRESH."), ~ as.null(.))) %>% # remove threshold columns
  # saving as year-month combo of EBD release (instead of real-time year-month)
  mutate(YEAR = currel_year, MONTH = currel_month_num) %>% 
  relocate(STATE.CODE, COUNTY.CODE, STATE, COUNTY, LISTS.DIST, 
           YEAR, MONTH, CONCERN.FINE, CONCERN.COARSE, 
           SPEC.OBS.DIST, SPEC.EXP.DIST, SPEC.EXP.ECO, SPEC.EXP, INV.C)

# not removing low concern category because this will be history of concern, so good to keep
# they will be removed in the metrics to be tracked


# whether or not to append new classifications to old ones each time

if (!file.exists(path_class_prev)) {
  
  concern_class_upd <- concern_class_cur %>% 
    arrange(STATE.CODE, COUNTY.CODE, YEAR, MONTH)
  
} else {
  
  # need latest (month before current) classification for each district in the country
  concern_class_latest <- read_xlsx(path_class_prev) %>% 
    group_by(STATE.CODE, STATE, COUNTY.CODE, COUNTY) %>% 
    arrange(YEAR, MONTH) %>% 
    slice_tail() %>% 
    ungroup()
  
  # if running the script more than once in any particular month, don't want to create
  # duplicate row
  # but we still need the objects in the environment
  
  avoid_duplicate <- (unique(concern_class_latest$YEAR) == currel_year & 
                       unique(concern_class_latest$MONTH) == currel_month_num)

    concern_class_upd <- read_xlsx(path_class_prev) %>% 
      {if (avoid_duplicate) {
        filter(., !(YEAR == currel_year & MONTH == currel_month_num)) # removing repeated rows
      } else {
        .
      }} %>%
      bind_rows(concern_class_cur) %>% 
      arrange(STATE.CODE, COUNTY.CODE, YEAR, MONTH) %>% 
      relocate(STATE.CODE, COUNTY.CODE, STATE, COUNTY, LISTS.DIST, 
               YEAR, MONTH, CONCERN.FINE, CONCERN.COARSE, 
               SPEC.OBS.DIST, SPEC.EXP.DIST, SPEC.EXP.ECO, SPEC.EXP, INV.C) %>% 
      complete(nesting(STATE.CODE, COUNTY.CODE, STATE, COUNTY),
               YEAR, MONTH)
    
}

# writing objects
write_xlsx(x = list("Concern classifications" = concern_class_upd),
           path = path_class_cur)

save(concern_class_upd, concern_class_cur, 
     file = get_stage_obj_path("data", "concern", add_rel_str = TRUE))
