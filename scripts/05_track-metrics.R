track_start <- "2023-03-01" %>% as_date()
track_end <- "2024-02-01" %>% as_date()


# filtering districts that were of no concern (fine and coarse) at the start of the track cycle
temp <- concern_class_upd %>% 
  group_by(STATE.NAME, DISTRICT.NAME) %>% 
  filter(YEAR == year(track_start) & MONTH == month(track_start)) %>% 
  transmute(ORIG.CONCERN.FINE = CONCERN.FINE,
            ORIG.CONCERN.COARSE = CONCERN.COARSE,
            ORIG.INV.C = INV.C) %>%
  ungroup()

concern_fine <- concern_class_cur %>% 
  left_join(temp, by = c("STATE.NAME", "DISTRICT.NAME")) %>% 
  filter(!(ORIG.INV.C >= 0.75)) %>% 
  filter(ORIG.CONCERN.FINE != 1)

concern_coarse <- concern_class_cur %>% 
  left_join(temp) %>% 
  filter(!(ORIG.INV.C >= 0.75)) %>% 
  mutate(CONCERN.COARSE = factor(CONCERN.COARSE, levels = c("LOW", "MID", "HIGH"))) %>% 
  filter(ORIG.CONCERN.COARSE != "LOW")

# current status metrics ------------------------------------------------------------

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
           path = glue("outputs/status_rel-{currel_year}{currel_month_num %>% str_pad(2, pad = '0')}.xlsx"))



# bins for number of lists per district
n_bins <- concern_fine %>% 
  distinct(N.DIST) %>% 
  arrange(N.DIST) %>% 
  rownames_to_column("ROW") %>% 
  mutate(ROW = as.numeric(ROW))

temp <- seq(min(n_bins$ROW), max(n_bins$ROW), length.out = 6) %>% floor()

n_bins <- n_bins %>% 
  # selecting thresholds
  filter(ROW %in% temp) %>% 
  dplyr::select(N.DIST)


plot1_base <- concern_fine %>% 
  left_join(dists_sf %>% dplyr::select(-AREA)) %>% 
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
               geom_sf(aes(geometry = DISTRICT.GEOM, fill = N.DIST)) +
               scale_fill_viridis_b(option = "inferno", 
                                    breaks = n_bins$N.DIST, 
                                    limits = c(min(n_bins$N.DIST), max(n_bins$N.DIST)),
                                    name = "Current no.\nof lists"))) /
  ((plot1_base +
      geom_sf(aes(geometry = DISTRICT.GEOM, fill = as.factor(CONCERN.FINE))) +
      scale_fill_viridis_d(option = "inferno", direction = -1,
                           name = "Concern level")) |
     # map with three concern colours
     (concern_coarse %>% 
        left_join(dists_sf) %>% 
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
       filename = glue("outputs/status_rel-{currel_year}{currel_month_num %>% str_pad(2, pad = '0')}.png"),
       dpi = 300, width = 24, height = 24, units = "in")  


# proportions of concern districts per state
plot2_base <- status_state %>% 
  left_join(states_sf) %>% 
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
       filename = glue("outputs/propconcern_st_rel-{currel_year}{currel_month_num %>% str_pad(2, pad = '0')}.png"),
       dpi = 300, width = 36, height = 14, units = "in")  


# proportions of concern districts per dark cluster
plot3 <- ((status_dl %>% 
             filter(DL.NAME == "Magadha") %>% 
             left_join(darkloci2) %>% 
             left_join(concern_class_cur %>% distinct(STATE.NAME, DISTRICT.NAME, CONCERN.FINE)) %>% 
             ggplot() +
             geom_sf(aes(geometry = DISTRICT.GEOM, fill = as.factor(CONCERN.FINE))) +
             scale_fill_viridis_d(option = "inferno", direction = -1,
                                  name = "Concern level", na.value = "grey80") +
             labs(title = "Magadha")) |
            (status_dl %>% 
               filter(DL.NAME == "Northeast") %>% 
               left_join(darkloci1) %>% 
               left_join(concern_class_cur %>% distinct(STATE.NAME, DISTRICT.NAME, CONCERN.FINE)) %>% 
               ggplot() +
               geom_sf(aes(geometry = DISTRICT.GEOM, fill = as.factor(CONCERN.FINE))) +
               scale_fill_viridis_d(option = "inferno", direction = -1,
                                    name = "Concern level", na.value = "grey80") +
               labs(title = "Northeast"))) &
  theme_void() +
  theme(plot.title = element_text(size = 24, hjust = 0.5))

ggsave(plot3, 
       filename = glue("outputs/propconcern_dl_rel-{currel_year}{currel_month_num %>% str_pad(2, pad = '0')}.png"),
       dpi = 300, width = 36, height = 14, units = "in")  



# values of interest & XoX change ------------------------------------------------------------------------

# metric 1: percentage high concern districts

# in country/each state/each dark cluster, how many high concern
metric1_nat_cur <- get_concern_summary(concern_coarse, "nat", CONCERN.COARSE)
metric1_state_cur <- get_concern_summary(concern_coarse, "state", CONCERN.COARSE)
metric1_dl_cur <- get_concern_summary(concern_coarse, "dl", CONCERN.COARSE)

# each time updating new classifications to old ones
if (!file.exists("outputs/metric1_full.xlsx") &
    !file.exists("outputs/metric1_track.xlsx")) {
  
  metric1_nat_upd <- metric1_nat_cur %>% arrange(YEAR, MONTH)
  metric1_state_upd <- metric1_state_cur %>% arrange(STATE.NAME, YEAR, MONTH)
  metric1_dl_upd <- metric1_dl_cur %>% arrange(DL.NAME, YEAR, MONTH)
  
  write_xlsx(x = list("Country" = metric1_nat_upd,
                      "States" = metric1_state_upd,
                      "Dark clusters" = metric1_dl_upd),
             path = "outputs/metric1_full.xlsx")
  
  
  metric1_nat_track <- metric1_nat_cur %>% 
    mutate(MoM = NA_integer_) %>% 
    dplyr::select(-YEAR, -MONTH, -CONCERN.LOW, -CONCERN.MID)
  
  metric1_state_track <- metric1_state_cur %>% 
    mutate(MoM = NA_integer_) %>% 
    dplyr::select(-YEAR, -MONTH, -CONCERN.LOW, -CONCERN.MID)
  
  metric1_dl_track <- metric1_dl_cur %>% 
    mutate(MoM = NA_integer_) %>% 
    dplyr::select(-YEAR, -MONTH, -CONCERN.LOW, -CONCERN.MID)
  
  write_xlsx(x = list("Country" = metric1_nat_track,
                      "States" = metric1_state_track,
                      "Dark clusters" = metric1_dl_track),
             path = "outputs/metric1_track.xlsx")
  
  
  } else if (file.exists("outputs/metric1_full.xlsx") &
             file.exists("outputs/metric1_track.xlsx")) {
    
    metric1_nat_data <- read_xlsx("outputs/metric1_full.xlsx", sheet = 1)
    metric1_state_data <- read_xlsx("outputs/metric1_full.xlsx", sheet = 2) %>% 
      # only one month has codes
      dplyr::select(-STATE.CODE) %>% 
      # adding state codes
      left_join(region_codes %>% distinct(STATE, STATE.CODE), by = c("STATE.NAME" = "STATE"))
    metric1_dl_data <- read_xlsx("outputs/metric1_full.xlsx", sheet = 3)
    

    # previous month's values (for MoM later)
    metric1_nat_latest <- metric1_nat_data %>% slice_tail()
    metric1_state_latest <- metric1_state_data %>% group_by(STATE.NAME) %>% slice_tail() %>% ungroup()
    metric1_dl_latest <- metric1_dl_data %>% group_by(DL.NAME) %>% slice_tail() %>% ungroup()
    
    # if running the script more than once in any particular month, don't want to reappend to database
    # but we still need the objects in the environment
    
    avoid_reappend <- (metric1_nat_latest$YEAR == real_year & metric1_nat_latest$MONTH == real_month_num)
    
    if (avoid_reappend) {
      # previous month's values (for MoM later)
      
      metric1_nat_latest <- metric1_nat_data %>% 
        filter(!(YEAR == real_year & MONTH == real_month_num)) %>% 
        slice_tail()
      
      metric1_state_latest <- metric1_state_data %>% 
        filter(!(YEAR == real_year & MONTH == real_month_num)) %>% 
        group_by(STATE.NAME) %>% 
        slice_tail() %>% 
        ungroup()
      
      metric1_dl_latest <- metric1_dl_data %>% 
        filter(!(YEAR == real_year & MONTH == real_month_num)) %>% 
        group_by(DL.NAME) %>% 
        slice_tail() %>% 
        ungroup()
    }

    
    metric1_nat_upd <- metric1_nat_data %>% 
      {if (avoid_reappend) {
        filter(., !(YEAR == real_year & MONTH == real_month_num)) # removing repeated rows
      }} %>%
      bind_rows(metric1_nat_cur) %>% 
      arrange(YEAR, MONTH)
    
    metric1_state_upd <- metric1_state_data %>% 
      {if (avoid_reappend) {
        filter(., !(YEAR == real_year & MONTH == real_month_num)) # removing repeated rows
      }} %>%
      bind_rows(metric1_state_cur) %>% 
      arrange(STATE.NAME, YEAR, MONTH)
    
    metric1_dl_upd <- metric1_dl_data %>% 
      {if (avoid_reappend) {
        filter(., !(YEAR == real_year & MONTH == real_month_num)) # removing repeated rows
      }} %>%
      bind_rows(metric1_dl_cur) %>% 
      arrange(DL.NAME, YEAR, MONTH)
    
    # writing objects
    write_xlsx(x = list("Country" = metric1_nat_upd,
                        "States" = metric1_state_upd,
                        "Dark clusters" = metric1_dl_upd),
               path = "outputs/metric1_full.xlsx")

    
    # MoM change (only current month in focus)
    # retain only High Concern

    metric1_nat_track <- metric1_nat_cur %>% 
      bind_cols(calc_mom("nat")) %>% 
      dplyr::select(-YEAR, -MONTH, -CONCERN.LOW, -CONCERN.MID)
    
    metric1_state_track <- metric1_state_cur %>% 
      left_join(calc_mom("state")) %>% 
      dplyr::select(-YEAR, -MONTH, -CONCERN.LOW, -CONCERN.MID)
    
    metric1_dl_track <- metric1_dl_cur %>% 
      left_join(calc_mom("dl")) %>% 
      dplyr::select(-YEAR, -MONTH, -CONCERN.LOW, -CONCERN.MID)
    
    
    # writing objects
    write_xlsx(x = list("Country" = metric1_nat_track,
                        "States" = metric1_state_track,
                        "Dark clusters" = metric1_dl_track),
               path = "outputs/metric1_track.xlsx")
    
}

