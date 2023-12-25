path_metric_full_cur <- get_stage_obj_path("outputs", "track", substage = "full", 
                                            add_rel_str = TRUE)
path_metric_full_prev <- get_stage_obj_path("outputs", "track", substage = "full",
                                             add_rel_str = TRUE, months_lag = 1)
path_metric_track_cur <- get_stage_obj_path("outputs", "track", substage = "track", 
                                            add_rel_str = TRUE)
path_metric_track_prev <- get_stage_obj_path("outputs", "track", substage = "track",
                                             add_rel_str = TRUE, months_lag = 1)


track_cycle <- get_track_dates(date_currel)


# filtering districts that were of no concern (fine and coarse) at the start of the track cycle
temp <- concern_class_upd %>% 
  group_by(STATE.CODE, COUNTY.CODE, STATE, COUNTY) %>% 
  filter(YEAR == year(track_cycle$START) & MONTH == month(track_cycle$START)) %>% 
  transmute(ORIG.CONCERN.FINE = CONCERN.FINE,
            ORIG.CONCERN.COARSE = CONCERN.COARSE,
            ORIG.INV.C = INV.C) %>%
  ungroup()

concern_fine <- concern_class_cur %>% 
  left_join(temp, by = c("STATE.CODE", "COUNTY.CODE", "STATE", "COUNTY")) %>% 
  filter(!(ORIG.INV.C >= 0.75)) %>% 
  filter(ORIG.CONCERN.FINE != 1)

concern_coarse <- concern_class_cur %>% 
  left_join(temp) %>% 
  filter(!(ORIG.INV.C >= 0.75)) %>% 
  mutate(CONCERN.COARSE = factor(CONCERN.COARSE, levels = c("LOW", "MID", "HIGH"))) %>% 
  filter(ORIG.CONCERN.COARSE != "LOW")

# current status metrics ------------------------------------------------------------

# (fine resolution) 

# in country, how many of each category
status_nat <- get_concern_summary(concern_fine, "nat", CONCERN.FINE)

# in each state, how many of each category
status_state <- get_concern_summary(concern_fine, "state", CONCERN.FINE)

# in each dark cluster, how many of each concern category
status_dl <- get_concern_summary(concern_fine, "dl", CONCERN.FINE)

# writing summary objects
write_xlsx(x = list("Country" = status_nat,
                    "States" = status_state,
                    "Dark clusters" = status_dl),
           path = glue("outputs/{currel_year}/status_{get_rel_str(verbose = FALSE)}.xlsx"))


# current status plots ------------------------------------------------------------

# bins for number of lists per district
n_bins <- concern_fine %>% 
  distinct(LISTS.DIST) %>% 
  arrange(LISTS.DIST) %>% 
  rownames_to_column("ROW") %>% 
  mutate(ROW = as.numeric(ROW))

temp <- seq(min(n_bins$ROW), max(n_bins$ROW), length.out = 6) %>% floor()

n_bins <- n_bins %>% 
  # selecting thresholds
  filter(ROW %in% temp) %>% 
  dplyr::select(LISTS.DIST)


plot1_base <- concern_fine %>% 
  left_join(admin_unit_mapping, by = c("STATE.CODE", "STATE", "COUNTY.CODE", "COUNTY")) %>% 
  left_join(dists_sf %>% dplyr::select(-AREA), 
            by = c("STATE.NAME", "DISTRICT.NAME")) %>% 
  ggplot() +
  # india outline
  geom_sf(data = india_sf, fill = "#D3D6D9") +
  # DL outline
  geom_sf(data = darkloci_sf %>% filter(DL.NAME == "Magadha"),
          fill = NA, col = "turquoise", linewidth = 0.9) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

plot1 <- ((plot1_base +
             geom_sf(aes(geometry = DISTRICT.GEOM, fill = INV.C)) +
             scale_fill_viridis_c(option = "inferno", 
                                  name = "Inventory (species)\ncompleteness")) |
            (plot1_base +
               geom_sf(aes(geometry = DISTRICT.GEOM, fill = LISTS.DIST)) +
               scale_fill_viridis_b(option = "inferno", 
                                    breaks = n_bins$LISTS.DIST, 
                                    limits = c(min(n_bins$LISTS.DIST), max(n_bins$LISTS.DIST)),
                                    name = "Current no.\nof lists"))) /
  ((plot1_base +
      geom_sf(aes(geometry = DISTRICT.GEOM, fill = as.factor(CONCERN.FINE))) +
      scale_fill_viridis_d(option = "inferno", direction = -1,
                           name = "Concern level")) |
     # map with three concern colours
     (concern_coarse %>% 
        left_join(admin_unit_mapping, by = c("STATE.CODE", "STATE", "COUNTY.CODE", "COUNTY")) %>% 
        left_join(dists_sf, 
                  by = c("STATE.NAME", "DISTRICT.NAME")) %>% 
        ggplot() +
        # india outline
        geom_sf(data = india_sf, fill = "#D3D6D9") +
        theme_classic() +
        theme(axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank()) +
        geom_sf(aes(geometry = DISTRICT.GEOM, fill = as.factor(CONCERN.COARSE))) +
        geom_sf(data = darkloci_sf %>% filter(DL.NAME == "Magadha"),
                fill = NA, col = "turquoise", linewidth = 0.9) +
        scale_fill_viridis_d(option = "inferno", direction = -1,
                             name = "Concern level"))) 

ggsave(plot1, 
       filename = glue("outputs/{currel_year}/status_{get_rel_str(verbose = FALSE)}.png"),
       dpi = 300, width = 24, height = 24, units = "in")  


# proportions of concern districts per state
plot2_base <- status_state %>% 
  left_join(admin_unit_mapping, by = c("STATE.CODE", "STATE")) %>% 
  left_join(states_sf, by = "STATE.NAME") %>% 
  ggplot() +
  # india outline
  geom_sf(data = india_sf, fill = "#D3D6D9") +
  # DL outline
  geom_sf(data = darkloci_sf %>% filter(DL.NAME == "Magadha"),
          fill = NA, col = "turquoise", linewidth = 0.9) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

plot2 <- (plot2_base +
            geom_sf(aes(geometry = STATE.GEOM, fill = CONCERN.5)) +
            scale_fill_viridis_c(option = "inferno", direction = -1,
                                 name = "Prop. of concern 5\ndistricts")) |
  (plot2_base +
     geom_sf(aes(geometry = STATE.GEOM, fill = CONCERN.4)) +
     scale_fill_viridis_c(option = "inferno", direction = -1,
                          name = "Prop. of concern 4\ndistricts")) |
  (plot2_base +
     geom_sf(aes(geometry = STATE.GEOM, fill = CONCERN.3)) +
     scale_fill_viridis_c(option = "inferno", direction = -1,
                          name = "Prop. of concern 3\ndistricts"))

ggsave(plot2, 
       filename = glue("outputs/{currel_year}/propconcern_st_{get_rel_str(verbose = FALSE)}.png"),
       dpi = 300, width = 36, height = 14, units = "in")  


# proportions of concern districts per dark cluster
plot3 <- ((status_dl %>% 
             filter(DL.NAME == "Magadha") %>% 
             left_join(darkloci2) %>% 
             left_join(concern_class_cur %>% 
                         distinct(STATE.CODE, STATE, COUNTY.CODE, COUNTY, CONCERN.FINE)) %>% 
             ggplot() +
             geom_sf(aes(geometry = DISTRICT.GEOM, fill = as.factor(CONCERN.FINE))) +
             scale_fill_viridis_d(option = "inferno", direction = -1,
                                  name = "Concern level", na.value = "grey80") +
             labs(title = "Magadha")) |
            (status_dl %>% 
               filter(DL.NAME == "Northeast") %>% 
               left_join(darkloci1) %>% 
               left_join(concern_class_cur %>% 
                           distinct(STATE.CODE, STATE, COUNTY.CODE, COUNTY, CONCERN.FINE)) %>% 
               ggplot() +
               geom_sf(aes(geometry = DISTRICT.GEOM, fill = as.factor(CONCERN.FINE))) +
               scale_fill_viridis_d(option = "inferno", direction = -1,
                                    name = "Concern level", na.value = "grey80") +
               labs(title = "Northeast"))) &
  theme_void() +
  theme(plot.title = element_text(size = 24, hjust = 0.5))

ggsave(plot3, 
       filename = glue("outputs/{currel_year}/propconcern_dl_{get_rel_str(verbose = FALSE)}.png"),
       dpi = 300, width = 36, height = 14, units = "in")  



# values of interest & XoX change ------------------------------------------------------------------------

# (coarse resolution)

# metric 1: percentage high concern districts

# in country/each state/each dark cluster, how many high concern
metric_nat_cur <- get_concern_summary(concern_coarse, "nat", CONCERN.COARSE)
metric_state_cur <- get_concern_summary(concern_coarse, "state", CONCERN.COARSE)
metric_dl_cur <- get_concern_summary(concern_coarse, "dl", CONCERN.COARSE)

# each time updating new classifications to old ones
if (!file.exists(path_metric_full_prev) & !file.exists(path_metric_track_prev)) {
  
  metric_nat_upd <- metric_nat_cur %>% arrange(YEAR, MONTH)
  metric_state_upd <- metric_state_cur %>% arrange(STATE.CODE, STATE, YEAR, MONTH)
  metric_dl_upd <- metric_dl_cur %>% arrange(DL.NAME, YEAR, MONTH)
  
  write_xlsx(x = list("Country" = metric_nat_upd,
                      "States" = metric_state_upd,
                      "Dark clusters" = metric_dl_upd),
             path = path_metric_full_cur)
  
  
  metric_nat_track <- metric_nat_cur %>% 
    mutate(MoM = NA_integer_) %>% 
    dplyr::select(-YEAR, -MONTH, -CONCERN.LOW, -CONCERN.MID)
  
  metric_state_track <- metric_state_cur %>% 
    mutate(MoM = NA_integer_) %>% 
    dplyr::select(-YEAR, -MONTH, -CONCERN.LOW, -CONCERN.MID)
  
  metric_dl_track <- metric_dl_cur %>% 
    mutate(MoM = NA_integer_) %>% 
    dplyr::select(-YEAR, -MONTH, -CONCERN.LOW, -CONCERN.MID)
  
  write_xlsx(x = list("Country" = metric_nat_track,
                      "States" = metric_state_track,
                      "Dark clusters" = metric_dl_track),
             path = path_metric_track_cur)
  
  
  } else if (file.exists(path_metric_full_prev) & file.exists(path_metric_track_prev)) {
    
    metric_nat_data <- read_xlsx(path_metric_full_prev, sheet = 1)
    metric_state_data <- read_xlsx(path_metric_full_prev, sheet = 2)
    metric_dl_data <- read_xlsx(path_metric_full_prev, sheet = 3)
    

    # previous month's values (for MoM later)
    metric_nat_latest <- metric_nat_data %>% slice_tail()
    metric_state_latest <- metric_state_data %>% group_by(STATE.CODE, STATE) %>% slice_tail() %>% ungroup()
    metric_dl_latest <- metric_dl_data %>% group_by(DL.NAME) %>% slice_tail() %>% ungroup()
    
    # if running the script more than once in any particular month, don't want to reappend to database
    # but we still need the objects in the environment
    
    avoid_duplicate <- (metric_nat_latest$YEAR == currel_year & 
                         metric_nat_latest$MONTH == currel_month_num)
    
    if (avoid_duplicate) {
      # previous month's values (for MoM later)
      
      metric_nat_latest <- metric_nat_data %>% 
        filter(!(YEAR == currel_year & MONTH == currel_month_num)) %>% 
        slice_tail()
      
      metric_state_latest <- metric_state_data %>% 
        filter(!(YEAR == currel_year & MONTH == currel_month_num)) %>% 
        group_by(STATE.CODE, STATE) %>% 
        slice_tail() %>% 
        ungroup()
      
      metric_dl_latest <- metric_dl_data %>% 
        filter(!(YEAR == currel_year & MONTH == currel_month_num)) %>% 
        group_by(DL.NAME) %>% 
        slice_tail() %>% 
        ungroup()
    }

    
    metric_nat_upd <- metric_nat_data %>% 
      {if (avoid_duplicate) {
        filter(., !(YEAR == currel_year & MONTH == currel_month_num)) # removing repeated rows
      } else {
        .
      }} %>%
      bind_rows(metric_nat_cur) %>% 
      arrange(YEAR, MONTH)
    
    metric_state_upd <- metric_state_data %>% 
      {if (avoid_duplicate) {
        filter(., !(YEAR == currel_year & MONTH == currel_month_num)) # removing repeated rows
      } else {
        .
      }} %>%
      bind_rows(metric_state_cur) %>% 
      arrange(STATE.CODE, STATE, YEAR, MONTH)
    
    metric_dl_upd <- metric_dl_data %>% 
      {if (avoid_duplicate) {
        filter(., !(YEAR == currel_year & MONTH == currel_month_num)) # removing repeated rows
      } else {
        .
      }} %>%
      bind_rows(metric_dl_cur) %>% 
      arrange(DL.NAME, YEAR, MONTH)
    
    # writing objects
    write_xlsx(x = list("Country" = metric_nat_upd,
                        "States" = metric_state_upd,
                        "Dark clusters" = metric_dl_upd),
               path = path_metric_full_cur)

    
    # MoM change (only current month in focus)
    # retain only High Concern

    metric_nat_track <- metric_nat_cur %>% 
      bind_cols(calc_mom("nat")) %>% 
      dplyr::select(-YEAR, -MONTH, -CONCERN.LOW, -CONCERN.MID)
    
    metric_state_track <- metric_state_cur %>% 
      left_join(calc_mom("state")) %>% 
      dplyr::select(-YEAR, -MONTH, -CONCERN.LOW, -CONCERN.MID)
    
    metric_dl_track <- metric_dl_cur %>% 
      left_join(calc_mom("dl")) %>% 
      dplyr::select(-YEAR, -MONTH, -CONCERN.LOW, -CONCERN.MID)
    
    
    # writing objects
    write_xlsx(x = list("Country" = metric_nat_track,
                        "States" = metric_state_track,
                        "Dark clusters" = metric_dl_track),
               path = path_metric_track_cur)
    
}


# writing objects -------------------------------------------------------------------

save(concern_fine, concern_coarse, 
     status_nat, status_state, status_dl,
     metric_nat_track, metric_state_track, metric_dl_track,
     file = get_stage_obj_path("data", "track", add_rel_str = TRUE))
