
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
      summarise(NO.DIST = n_distinct(DISTRICT.NAME)) %>%
      ungroup() %>%
      # to calculate proportion
      mutate(TOT.DIST = n_distinct(dists_sf$DISTRICT.NAME),
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
      pivot_wider(names_from = concern_col_str, values_from = "PROP.DIST") %>%
      magrittr::set_colnames(new_colnames) %>%
      mutate(across(everything(), ~ replace_na(.x, 0))) %>%
      {if (str_detect(concern_col_str, "COARSE")) {
        arrange(., desc(CONCERN.HIGH), desc(CONCERN.MID), desc(CONCERN.LOW)) %>%
          mutate(YEAR = cur_year, MONTH = cur_month_num) %>%
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
      group_by(STATE.NAME, {{ concern_col }}) %>% ###
      summarise(NO.DIST = n_distinct(DISTRICT.NAME)) %>%
      ungroup() %>%
      # to calculate proportion
      left_join(state_dists) %>% # joins number of districts per state ###
      mutate(PROP.DIST = round(100*NO.DIST/TOT.DIST, 2)) %>%
      dplyr::select(-NO.DIST, -TOT.DIST) %>%
      filter(!is.na(STATE.NAME)) %>% ###
      # filling zeroes
      group_by(STATE.NAME) %>% ###
      {if (str_detect(concern_col_str, "COARSE")) {
        complete(., {{ concern_col }},
                 fill = list(PROP.DIST = 0))
      } else {
        complete(., {{ concern_col }} := 1:5,
                 fill = list(PROP.DIST = 0))
      }
      } %>%
      ungroup() %>% 
      pivot_wider(names_from = concern_col_str, values_from = "PROP.DIST") %>%
      magrittr::set_colnames(c("STATE.NAME", new_colnames)) %>% ###
      mutate(across(contains("CONCERN."), ~ replace_na(.x, 0))) %>% ###
      {if (str_detect(concern_col_str, "COARSE")) {
        arrange(., desc(CONCERN.HIGH), desc(CONCERN.MID), desc(CONCERN.LOW)) %>%
          # adding state codes
          left_join(region_codes %>% distinct(STATE, STATE.CODE), by = c("STATE.NAME" = "STATE")) %>% ###
          mutate(YEAR = cur_year, MONTH = cur_month_num) %>%
          relocate(STATE.CODE, STATE.NAME, YEAR, MONTH, CONCERN.LOW, CONCERN.MID, CONCERN.HIGH)
      } else {
        arrange(., desc(CONCERN.5), desc(CONCERN.4), desc(CONCERN.3), desc(CONCERN.2), desc(CONCERN.1)) %>% 
          # adding state codes
          left_join(region_codes %>% distinct(STATE, STATE.CODE), by = c("STATE.NAME" = "STATE"))
      }
      }
    
    return(concern_summary)
    
  } else if (scale == "dl") {
    
    # in dark cluster, how many high concern
    
    concern_summary <- concern_data %>% 
      # removing districts of no concern (NA)
      filter(!is.na({{ concern_col }})) %>%
      inner_join(darkloci) %>% # inner because we don't want LOW districts ###
      group_by(DL.NAME, {{ concern_col }}) %>% ###
      summarise(NO.DIST = n_distinct(DISTRICT.NAME)) %>%
      ungroup() %>%
      # to calculate proportion (joins number of districts per dark cluster)
      left_join(darkloci %>%
                  group_by(DL.NAME) %>%
                  dplyr::summarise(TOT.DIST = n_distinct(DISTRICT.NAME))) %>% ###
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
      }
      }  %>%
      ungroup() %>%
      pivot_wider(names_from = concern_col_str, values_from = "PROP.DIST") %>%
      magrittr::set_colnames(c("DL.NAME", new_colnames)) %>% ###
      mutate(across(contains("CONCERN."), ~ replace_na(.x, 0))) %>%
      {if (str_detect(concern_col_str, "COARSE")) {
        arrange(., desc(CONCERN.HIGH), desc(CONCERN.MID), desc(CONCERN.LOW)) %>%
          mutate(YEAR = cur_year, MONTH = cur_month_num) %>%
          relocate(DL.NAME, YEAR, MONTH, CONCERN.LOW, CONCERN.MID, CONCERN.HIGH)
      } else {
        arrange(., desc(CONCERN.5), desc(CONCERN.4), desc(CONCERN.3), desc(CONCERN.2), desc(CONCERN.1))
      }
      }
    
    return(concern_summary)
    
  }
  
}



