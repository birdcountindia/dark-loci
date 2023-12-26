# get relMON-YYYY or rel-YYYYMM string -----------------------------------------------------

get_rel_str <- function(months_lag = 0, verbose = TRUE) {
  
  # params must be loaded in environment
  
  # negative lag will return future months
  req_year <- (date_currel - months(months_lag)) %>% year()
  req_month_lab <- (date_currel - months(months_lag)) %>% month(label = T, abbr = T)
  req_month_num <- (date_currel - months(months_lag)) %>% month()
  
  rel_str <- if (verbose == TRUE) {
    glue("rel{req_month_lab}-{req_year}")
  } else {
    glue("rel-{req_year}{req_month_num %>% str_pad(2, pad = '0')}")
  }
  
  return(rel_str)
  
}

# get path strings for locations in repo ----------------------------------------

# we need to keep referring to different objects (data, outputs, scripts)
# of different stages in the analyses, so this function makes it easier
# to reference the appropriate paths

get_stage_obj_path <- function(folder, stage, substage = NULL, 
                               filetype = NULL, add_rel_str = FALSE, months_lag = 0) {

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
  
  if (stage != "track") {
    if (!is.null(substage)) {
      return("Substage can be set only for outputs of 05_track-metrics")
    }
  } else if (stage == "track" & folder == "outputs") {
    if (is.null(substage)) {
      return("Please select substage, one of {track, full}")
    } else if (!(substage %in% c("track", "full"))) {
      return("Substage should be one of {track, full}")
    }
  }
  
  stage_translation <- if (stage == "params") {
    "00_params"
  } else if (stage %in% c("spat", "spatialise", "data-spatialise")) {
    "01_data-spatialise"
  } else if (stage %in% c("comp", "completeness")) {
    "02_completeness"
  } else if (stage %in% c("class", "classify", "concern", "thresh", "thresholds")) {
    if (folder == "outputs") {
      glue("concern_classification")
    } else {
      "03_classify-concern"
    }
  } else if (stage %in% c("id", "id-loci")) {
    "04_id-loci"
  } else if (stage %in% c("track", "track-metrics")) {
    if (folder == "outputs") {
      glue("metric_{substage}")
    } else {
      "05_track-metrics"
    }
  } else {
    return("Incorrect stage specification.")
  }
  
  # adding rel-YYYY string if needed
  if (add_rel_str == TRUE) {
    stage_translation <- glue("{stage_translation}_{get_rel_str(months_lag, verbose = FALSE)}")
  }
  
  if (folder == "outputs") {
    return(glue("{folder}/{currel_year}/{stage_translation}{filetype}"))
  } else {
    return(glue("{folder}/{stage_translation}{filetype}"))
  }
  
}


# tracking cycle functions ---------------------------------------------------

# get start and end dates for current tracking cycle

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


# get the concern classification data for the first month of current track cycle

get_concern_class_start <- function(cur_date = date_currel) {
  
  # first of current track cycle
  t1 <- cur_date %>% 
    get_track_dates() %>% 
    pull(START)
  
  t2 <- cur_date
  
  # number of months difference
  dt <- interval(t1, t2) %/% months(1)
  
  concern_class0 <- get_stage_obj_path("outputs", "class", add_rel_str = TRUE,
                                       months_lag = dt) %>% 
    read_xlsx()
  
  return(concern_class0)
  
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
    new_colnames <- c("STATE.CODE", "STATE.NAME", "OLD", "CUR")
    
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


# get values for updating gsheet ------------------------------------------

get_metrics_gsheet <- function(which_level) {
  
  # iterate over all required months
  if (which_level == "IN") {
    
    iterate <- data.frame(
      PATHS = c(get_stage_obj_path("outputs", "track", substage = "track",
                                   add_rel_str = TRUE),
                get_stage_obj_path("outputs", "track", substage = "track",
                                   add_rel_str = TRUE, months_lag = 1),
                get_stage_obj_path("outputs", "track", substage = "track",
                                   add_rel_str = TRUE, months_lag = 2),
                get_stage_obj_path("outputs", "track", substage = "track",
                                   add_rel_str = TRUE, months_lag = 3),
                get_stage_obj_path("outputs", "track", substage = "track",
                                   add_rel_str = TRUE, months_lag = 4),
                get_stage_obj_path("outputs", "track", substage = "track",
                                   add_rel_str = TRUE, months_lag = 5)),
      DATE = c(date_currel, date_currel - months(1), 
               date_currel - months(2), date_currel - months(3), 
               date_currel - months(4), date_currel - months(5))
    ) %>% 
      mutate(MONTH.LAB = DATE %>% month(label = TRUE, abbr = TRUE))
    
    data1 <- map2(iterate$PATHS, iterate$MONTH.LAB, ~ {
      if (file.exists(.x)) {
        read_xlsx(.x, "Country") %>% 
          bind_cols(tibble(MONTH.LAB = .y, 
                           METRIC = "Country"))
      } else {
        data.frame(CONCERN.HIGH = NA,
                   MoM = NA) %>% 
          bind_cols(tibble(MONTH.LAB = .y, 
                           METRIC = "Country"))
      }}) %>% 
      list_rbind()
    
    data2 <- map2(iterate$PATHS, iterate$MONTH.LAB, ~ {
      if (file.exists(.x)) {
        read_xlsx(.x, "Dark clusters") %>% 
          mutate(METRIC = factor(DL.NAME, levels = c("Northeast", "Magadha"))) %>% 
          arrange(METRIC) %>% 
          dplyr::select(-DL.NAME) %>% 
          bind_cols(tibble(MONTH.LAB = .y))
      } else {
        data.frame(CONCERN.HIGH = rep(NA, 2),
                   MoM = rep(NA, 2),
                   METRIC = factor(c("Northeast", "Magadha"), 
                                   levels = c("Northeast", "Magadha"))) %>% 
          bind_cols(tibble(MONTH.LAB = .y))
      }}) %>% 
      list_rbind()
    
    metrics <- data1 %>% 
      bind_rows(data2) %>% 
      mutate(METRIC = factor(METRIC, levels = c("Country", "Northeast", "Magadha"))) %>% 
      left_join(iterate %>% distinct(DATE, MONTH.LAB)) %>% 
      arrange(DATE) %>% 
      mutate(MoM = case_when(MONTH.LAB == currel_month_lab ~ MoM,
                             TRUE ~ NA)) %>% 
      dplyr::select(METRIC, MONTH.LAB, CONCERN.HIGH, MoM)
    
    metrics <- metrics %>% 
      dplyr::select(-MoM) %>% 
      pivot_wider(names_from = "MONTH.LAB", values_from = "CONCERN.HIGH") %>% 
      mutate(MoM = metrics %>% filter(!is.na(MoM)) %>% pull(MoM)) %>% 
      dplyr::select(-METRIC)

  } else if (which_level == "ST") {
    
    iterate <- data.frame(
      PATHS = c(get_stage_obj_path("outputs", "track", substage = "track",
                                   add_rel_str = TRUE),
                get_stage_obj_path("outputs", "track", substage = "track",
                                   add_rel_str = TRUE, months_lag = 1)),
      DATE = c(date_currel, date_currel - months(1))
    ) %>% 
      mutate(MONTH.LAB = DATE %>% month(label = TRUE, abbr = TRUE))
    
    data1 <- map2(iterate$PATHS, iterate$MONTH.LAB, ~ {
      if (file.exists(.x)) {
        read_xlsx(.x, "States") %>% 
          dplyr::select(c("STATE.CODE", "CONCERN.HIGH", "MoM")) %>% 
          bind_cols(tibble(MONTH.LAB = .y))
      } else {
        data.frame(CONCERN.HIGH = NA,
                   MoM = NA) %>% 
          bind_cols(admin_unit_mapping %>% distinct(STATE.CODE)) %>% 
          bind_cols(tibble(MONTH.LAB = .y))
      }}) %>% 
      list_rbind()
    
    metrics <- data1 %>% 
      left_join(iterate %>% distinct(DATE, MONTH.LAB)) %>% 
      arrange(DATE) %>% 
      mutate(MoM = case_when(MONTH.LAB == currel_month_lab ~ MoM,
                             TRUE ~ NA)) %>% 
      dplyr::select(STATE.CODE, MONTH.LAB, CONCERN.HIGH, MoM)
    
    metrics <- metrics %>% 
      dplyr::select(-MoM) %>% 
      pivot_wider(names_from = "MONTH.LAB", values_from = "CONCERN.HIGH") %>% 
      left_join(metrics %>% 
                  filter(!is.na(MoM)) %>% 
                  distinct(STATE.CODE, MoM),
                by = "STATE.CODE")
    
  }
  
  return(metrics)
  
}

# update gsheet ---------------------------------------------------------------

write_metrics_sheet <- function(metric_data, which_level, darkloci = FALSE) {
  
  if (!which_level %in% c("IN", "ST", "DT")) {
    stop('Please choose one of {"IN", "ST", "DT"} for which_level.')
  } else {
    if (darkloci == TRUE) {
      if (which_level == "DT") {
        stop('"DT" level not defined for dark loci metrics.')
      }
    }
  }
  
  sheet_suffix <- which_level
  
  sheet_name_prev <- glue("{sheet_prefix_prev}-{sheet_suffix}")
  sheet_name_cur <- glue("{sheet_prefix_cur}-{sheet_suffix}")
  
  # range of cells in GSheet to write to, based on which metric
  sheet_range <- if (darkloci == FALSE) {
    if (which_level == "IN") {
      "B1:H7"
    } else if (which_level == "ST") {
      "A2:J33"
    } else if (which_level == "DT") {
      "A2:K731"
    }
  } else if (darkloci == TRUE) {
    if (which_level == "IN") {
      "B8:H10"
    } else if (which_level == "ST") {
      "K2:M33"
    }
  }
  
  
  if (!sheet_name_cur %in% (gs4_get(our_gsheet) %>% pluck("sheets", "name"))) {
    
    if (darkloci == TRUE) {
      
      stop("Required sheet does not exist yet. Please run monthly BCI metrics before dark loci metrics.")
      
    } else {
      
      # first, create a copy of appropriate old sheet
      # refer https://googlesheets4.tidyverse.org/reference/sheet_copy.html
      
      sheet_copy(from_ss = our_gsheet, .before = 1, # place as the first sheet
                 from_sheet = sheet_name_prev, to_sheet = sheet_name_cur)
      
    }
    
  } else {
    
    if (darkloci == TRUE) {
      message("Writing dark loci metrics in sheet of interest.")
    } else {
      message(glue("Sheet of interest ({sheet_name_cur}) already exists. Rewriting range."))
    }

  }
  
  
  # need to arrange states in order as in sheet
  if (darkloci == TRUE & which_level == "ST") {
    
    states_order <- range_read(ss = our_gsheet, sheet = sheet_name_cur, 
                               range = "A3:A33", col_names = "STATE.CODE") %>% 
      mutate(STATE.CODE = glue("IN-{STATE.CODE}"))
    
    metric_data <- states_order %>% 
      left_join(metric_data, by = "STATE.CODE") %>% 
      dplyr::select(-STATE.CODE)
    
  }
  
  # then write our current/new data to specific range 
  range_write(ss = our_gsheet, data = metric_data,
              sheet = sheet_name_cur, range = sheet_range, 
              # we need col names (months) to be updated
              col_names = if (!(darkloci == TRUE & which_level == "IN")) TRUE else FALSE, 
              reformat = FALSE) # we want to retain existing formatting (conditional colours for YoY%)
  
  # on IN sheet, BCI website stats needs to be cleared cos PJ brings in separately
  if (which_level == "IN" & darkloci == FALSE) {
    range_write(ss = our_gsheet, data = data.frame(matrix(NA, nrow = 3, ncol = 7)),
                sheet = sheet_name_cur, range = "B11:H13", 
                col_names = FALSE, reformat = FALSE) 
  }
  
}
