# get relMON-YYYY or rel-YYYYMM string -----------------------------------------------------

get_rel_str <- function(verbose = TRUE) {
  
  # params must be loaded in environment
  
  rel_str <- if (verbose == TRUE) {
    glue("rel{currel_month_lab}-{currel_year}")
  } else {
    glue("rel-{currel_year}{currel_month_num %>% str_pad(2, pad = '0')}")
  }
  
  return(rel_str)
  
}

# get path strings for locations in repo ----------------------------------------

# we need to keep referring to different objects (data, outputs, scripts)
# of different stages in the analyses, so this function makes it easier
# to reference the appropriate paths

get_stage_obj_path <- function(folder, stage, filetype = NULL, add_rel_str = FALSE) {

  if (!folder %in% c("data", "outputs", "scripts")) {
    
    return("Incorrect folder specification.")
    
  } else if (is.null(filetype)) {
    
    if (folder == "data") {
      filetype <- ".RData"
    } else if (folder == "outputs") {
      filetype <- ".xlsx"
    } else if (folder == "scripts") {
      filetype <- ".R"
    } 
    
  } 
  
  stage_translation <- if (stage == "params") {
    "00_params"
  } else if (stage %in% c("spat", "spatialise", "data-spatialise")) {
    "01_data-spatialise"
  } else if (stage %in% c("comp", "completeness")) {
    "02_completeness"
  } else if (stage %in% c("class", "classify", "concern", "thresh", "thresholds")) {
    "03_classify-concern"
  } else if (stage %in% c("id", "id-loci")) {
    "04_id-loci"
  } else if (stage %in% c("track", "track-metrics")) {
    "05_track-metrics"
  } else {
    return("Incorrect stage specification.")
  }
  
  # adding rel-YYYY string if needed
  if (add_rel_str == TRUE) {
    stage_translation <- glue("{stage_translation}_{get_rel_str()}")
  }
  
  return(glue("{folder}/{stage_translation}{filetype}"))
  
}


# how many of each concern category -------------------------------------------------

get_concern_summary <- function(concern_data, scale, concern_col) {
  
  # scale may be national, state, or dl
  # concern_col is the column name with concern values (fine or coarse)
  
  concern_col_str <- as_label(enquo(concern_col))
  
  new_colnames <- if (str_detect(concern_col_str, "COARSE")) {
    c("CONCERN.LOW", "CONCERN.MID", "CONCERN.HIGH")
  } else {
    c("CONCERN.1", "CONCERN.2", "CONCERN.3", "CONCERN.4", "CONCERN.5")
  }

    
  if (scale == "nat") {
    
    # in country, how many high concern
    
    concern_summary <- concern_data %>% 
      # removing districts of no concern (NA)
      filter(!is.na({{ concern_col }})) %>%
      group_by({{ concern_col }}) %>%
      summarise(NO.DIST = n_distinct(COUNTY.CODE)) %>%
      ungroup() %>%
      # to calculate proportion
      mutate(TOT.DIST = n_distinct(admin_unit_mapping$COUNTY.CODE),
             PROP.DIST = round(100*NO.DIST/TOT.DIST, 2)) %>%
      dplyr::select(-NO.DIST, -TOT.DIST) %>%
      # filling zeroes
      {if (str_detect(concern_col_str, "COARSE")) {
        complete(., {{ concern_col }},
                 fill = list(PROP.DIST = 0))
      } else {
        complete(., {{ concern_col }} := 1:5,
                 fill = list(PROP.DIST = 0))
      }
      } %>%
      ungroup() %>% 
      pivot_wider(names_from = all_of(concern_col_str), values_from = "PROP.DIST") %>%
      magrittr::set_colnames(new_colnames) %>%
      mutate(across(everything(), ~ replace_na(.x, 0))) %>%
      {if (str_detect(concern_col_str, "COARSE")) {
        arrange(., desc(CONCERN.HIGH), desc(CONCERN.MID), desc(CONCERN.LOW)) %>%
          mutate(YEAR = currel_year, MONTH = currel_month_num) %>%
          relocate(YEAR, MONTH, CONCERN.LOW, CONCERN.MID, CONCERN.HIGH)
      } else {
        arrange(., desc(CONCERN.5), desc(CONCERN.4), desc(CONCERN.3), desc(CONCERN.2), desc(CONCERN.1))
      }
      }
    
    return(concern_summary)
    
  } else if (scale == "state") {
    
    # in state, how many high concern
    
    concern_summary <- concern_data %>% 
      # removing districts of no concern (NA)
      filter(!is.na({{ concern_col }})) %>%
      group_by(STATE.CODE, STATE, {{ concern_col }}) %>% 
      summarise(NO.DIST = n_distinct(COUNTY.CODE)) %>%
      ungroup() %>%
      # to calculate proportion
      left_join(dists_per_state, by = c("STATE.CODE", "STATE")) %>% 
      mutate(PROP.DIST = round(100*NO.DIST/TOT.DIST, 2)) %>%
      dplyr::select(-NO.DIST, -TOT.DIST) %>%
      # filling zeroes
      group_by(STATE.CODE, STATE) %>% 
      {if (str_detect(concern_col_str, "COARSE")) {
        complete(., {{ concern_col }},
                 fill = list(PROP.DIST = 0))
      } else {
        complete(., {{ concern_col }} := 1:5,
                 fill = list(PROP.DIST = 0))
      }} %>%
      ungroup() %>% 
      pivot_wider(names_from = all_of(concern_col_str), values_from = "PROP.DIST") %>%
      magrittr::set_colnames(c("STATE.CODE", "STATE", new_colnames)) %>% 
      mutate(across(contains("CONCERN."), ~ replace_na(.x, 0))) %>% ###
      {if (str_detect(concern_col_str, "COARSE")) {
        arrange(., desc(CONCERN.HIGH), desc(CONCERN.MID), desc(CONCERN.LOW)) %>%
          mutate(YEAR = currel_year, MONTH = currel_month_num) %>%
          relocate(STATE.CODE, STATE, YEAR, MONTH, CONCERN.LOW, CONCERN.MID, CONCERN.HIGH)
      } else {
        arrange(., desc(CONCERN.5), desc(CONCERN.4), desc(CONCERN.3), desc(CONCERN.2), desc(CONCERN.1))
      }}
    
    return(concern_summary)
    
  } else if (scale == "dl") {
    
    # in dark cluster, how many high concern
    
    concern_summary <- concern_data %>% 
      # removing districts of no concern (NA)
      filter(!is.na({{ concern_col }})) %>%
      inner_join(darkloci) %>% # inner because we don't want LOW districts ###
      group_by(DL.NAME, {{ concern_col }}) %>% ###
      summarise(NO.DIST = n_distinct(COUNTY.CODE)) %>%
      ungroup() %>%
      # to calculate proportion (joins number of districts per dark cluster)
      left_join(darkloci %>%
                  group_by(DL.NAME) %>%
                  dplyr::summarise(TOT.DIST = n_distinct(COUNTY.CODE))) %>% ###
      mutate(PROP.DIST = round(100*NO.DIST/TOT.DIST, 2)) %>%
      dplyr::select(-NO.DIST, -TOT.DIST) %>%
      filter(!is.na(DL.NAME)) %>% ###
      # filling zeroes
      group_by(DL.NAME) %>% ###
      {if (str_detect(concern_col_str, "COARSE")) {
        complete(., {{ concern_col }},
                 fill = list(PROP.DIST = 0))
      } else {
        complete(., {{ concern_col }} := 1:5,
                 fill = list(PROP.DIST = 0))
      }} %>%
      ungroup() %>%
      pivot_wider(names_from = all_of(concern_col_str), values_from = "PROP.DIST") %>%
      magrittr::set_colnames(c("DL.NAME", new_colnames)) %>% ###
      mutate(across(contains("CONCERN."), ~ replace_na(.x, 0))) %>%
      {if (str_detect(concern_col_str, "COARSE")) {
        arrange(., desc(CONCERN.HIGH), desc(CONCERN.MID), desc(CONCERN.LOW)) %>%
          mutate(YEAR = currel_year, MONTH = currel_month_num) %>%
          relocate(DL.NAME, YEAR, MONTH, CONCERN.LOW, CONCERN.MID, CONCERN.HIGH)
      } else {
        arrange(., desc(CONCERN.5), desc(CONCERN.4), desc(CONCERN.3), desc(CONCERN.2), desc(CONCERN.1))
      }}
    
    return(concern_summary)
    
  }
  
}


# functions for calculating inventory completeness ----------------------------------

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


# calculate MoM change --------------------------------------------------------------

calc_mom <- function(level) {
  
  # MoM change (only current month in focus)
  # retain only High Concern
  
  if (level == "nat") {
    
    metric_latest <- metric_nat_latest
    metric_cur <- metric_nat_cur
    new_colnames <- c("OLD", "CUR")
    
  } else if (level == "state") {
    
    metric_latest <- metric_state_latest
    metric_cur <- metric_state_cur
    new_colnames <- c("STATE.NAME", "STATE.CODE", "OLD", "CUR")
    
  } else if (level == "dl") {
    
    metric_latest <- metric_dl_latest
    metric_cur <- metric_dl_cur
    new_colnames <-c("DL.NAME", "OLD", "CUR")
    
  } else {
    return("level should be one of {nat, state, dl}")
  }

  # no MoM if first record
  if (nrow(metric_latest) == 0) {
    
    print("Current year-month is first record, so skipping MoM calculation and returning NA.")
    
    mom <- metric_cur %>% 
      {if (level == "state") {
        group_by(., STATE.CODE, STATE)
      } else if (level == "dl") {
        group_by(., DL.NAME)
      } else {
        .
      }} %>% 
      reframe(MoM = NA_real_)
    
  } else {
    
    mom <- metric_latest %>% 
      bind_rows(metric_cur) %>% 
      mutate(CONCERN.LOW = NULL, CONCERN.MID = NULL) %>% 
      arrange(YEAR, MONTH) %>% 
      mutate(YEAR = NULL) %>% 
      {if (level == "state") {
        group_by(., STATE.CODE, STATE)
      } else if (level == "dl") {
        group_by(., DL.NAME)
      } else {
        .
      }} %>% 
      pivot_wider(names_from = "MONTH", values_from = "CONCERN.HIGH") %>% 
      magrittr::set_colnames(new_colnames) %>% 
      dplyr::summarise(MoM = round(100*(CUR - OLD)/OLD, 4)) # also ungroups
    
  }

  return(mom)
  
}


# classify concern based on inventory completenss -----------------------------------

classify_concern <- function(data, which_concern = NULL, get_thresh_noconcern = FALSE) {
  
  # threshold inventory completeness above which is no concern for our analyses
  thresh_noconcern <- 0.75
  
  if (get_thresh_noconcern == TRUE) {
    
    return(thresh_noconcern)
    
  } else {
    
    if (which_concern == "fine") {
      
      classified <- data %>% 
        # levels of incompleteness based on thresholds (>=75% completeness of no concern)
        mutate(CONCERN.FINE = case_when(INV.C < thresh_noconcern & INV.C >= THRESH.FINE4 ~ 1,
                                        INV.C < THRESH.FINE4 & INV.C >= THRESH.FINE3 ~ 2,
                                        INV.C < THRESH.FINE3 & INV.C >= THRESH.FINE2 ~ 3,
                                        INV.C < THRESH.FINE2 & INV.C >= THRESH.FINE1 ~ 4,
                                        INV.C < THRESH.FINE1 ~ 5))
      
    } else if (which_concern == "coarse") {
      
      classified <- data %>% 
        mutate(CONCERN.COARSE = case_when(INV.C < thresh_noconcern & INV.C >= THRESH.COARSE2 ~ "LOW",
                                          INV.C < THRESH.COARSE2 & INV.C >= THRESH.COARSE1 ~ "MID",
                                          INV.C < THRESH.COARSE1 ~ "HIGH")) %>% 
        # converting to ordered factor
        mutate(CONCERN.COARSE = factor(CONCERN.COARSE, levels = c("LOW", "MID", "HIGH")))
        
    }
    
    return(classified)
    
  }
  
}

# get start and end dates for current tracking cycle --------------------------------

get_track_dates <- function(cur_ebd_rel) {
  
  # these dates are EBD release dates (not real-time year and month)
  
  track_cycles <- data.frame(
    START = seq(as_date("2023-02-01"), length.out = 10, by = "years"),
    END = seq(as_date("2024-01-01"), length.out = 10, by = "years"),
    CYCLE = 1:10
  ) %>% 
    mutate(CUR = case_when(cur_ebd_rel >= START & cur_ebd_rel <= END ~ TRUE,
                           TRUE ~ FALSE))
  
  return(track_cycles %>% filter(CUR == TRUE) %>% dplyr::select(-CUR))
  
}
