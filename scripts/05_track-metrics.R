require(patchwork)

track_start <- "2023-03-01" %>% as_date()
track_end <- "2024-02-01" %>% as_date()


# filtering districts that were of no concern (fine and coarse) at the start of the track cycle
temp <- concern_class_upd %>% 
  group_by(STATE.NAME, DISTRICT.NAME) %>% 
  filter(YEAR == year(track_start) & MONTH == month(track_start)) %>% 
  rename(ORIG.CONCERN.FINE = CONCERN.FINE,
         ORIG.CONCERN.COARSE = CONCERN.COARSE,
         ORIG.INV.C = INV.C) %>% 
  dplyr::select(ORIG.CONCERN.FINE, ORIG.CONCERN.COARSE, ORIG.INV.C) %>% 
  ungroup()

concern_fine <- concern_class_upd %>% 
  left_join(temp, by = c("STATE.NAME", "DISTRICT.NAME")) %>% 
  filter(!(ORIG.INV.C >= 0.75)) %>% 
  filter(ORIG.CONCERN.FINE != 1)

concern_coarse <- concern_class_upd %>% 
  left_join(temp) %>% 
  filter(!(ORIG.INV.C >= 0.75)) %>% 
  mutate(CONCERN.COARSE = factor(CONCERN.COARSE, levels = c("LOW", "MID", "HIGH"))) %>% 
  filter(ORIG.CONCERN.COARSE != "LOW")


# current status metrics ------------------------------------------------------------

# in each state, how many of each category
status_state <- concern_fine %>% 
  # removing districts of no concern (NA)
  filter(!is.na(CONCERN.FINE)) %>% 
  group_by(STATE.NAME, CONCERN.FINE) %>% 
  summarise(NO.DIST = n_distinct(DISTRICT.NAME)) %>% 
  ungroup() %>% 
  # to calculate proportion
  left_join(state_dists) %>% # joins number of districts per state
  mutate(PROP.DIST = NO.DIST/TOT.DIST) %>% 
  dplyr::select(-NO.DIST, -TOT.DIST) %>% 
  filter(!is.na(STATE.NAME)) %>% 
  # filling zeroes
  group_by(STATE.NAME) %>% 
  complete(CONCERN.FINE = 1:5,
           fill = list(PROP.DIST = 0)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "CONCERN.FINE", values_from = "PROP.DIST") %>% 
  set_colnames(c("STATE.NAME", "CONCERN.1", "CONCERN.2", "CONCERN.3", "CONCERN.4", "CONCERN.5")) %>% 
  mutate(across(2:6, ~ replace_na(.x, 0))) %>% 
  arrange(desc(CONCERN.5), desc(CONCERN.4), desc(CONCERN.3), desc(CONCERN.2), desc(CONCERN.1)) %>% 
  # adding state codes
  left_join(region_codes %>% distinct(STATE, STATE.CODE), by = c("STATE.NAME" = "STATE"))

# in country, how many of each category
status_nat <- concern_fine %>% 
  # removing districts of no concern (NA)
  filter(!is.na(CONCERN.FINE)) %>% 
  group_by(CONCERN.FINE) %>% 
  summarise(NO.DIST = n_distinct(DISTRICT.NAME)) %>% 
  ungroup() %>% 
  # to calculate proportion
  mutate(TOT.DIST = n_distinct(dists_sf$DISTRICT.NAME),
         PROP.DIST = NO.DIST/TOT.DIST) %>% 
  dplyr::select(-NO.DIST, -TOT.DIST) %>% 
  complete(CONCERN.FINE = 1:5,
           fill = list(PROP.DIST = 0)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "CONCERN.FINE", values_from = "PROP.DIST") %>% 
  set_colnames(c("CONCERN.1", "CONCERN.2", "CONCERN.3", "CONCERN.4", "CONCERN.5")) %>% 
  mutate(across(everything(), ~ replace_na(.x, 0))) %>% 
  arrange(desc(CONCERN.5), desc(CONCERN.4), desc(CONCERN.3), desc(CONCERN.2), desc(CONCERN.1))

# in each dark cluster, how many of each concern category
status_dl <- concern_fine %>% 
  # removing districts of no concern (NA)
  filter(!is.na(CONCERN.FINE)) %>% 
  inner_join(darkloci) %>% 
  group_by(DL.NAME, CONCERN.FINE) %>% 
  summarise(NO.DIST = n_distinct(DISTRICT.NAME)) %>% 
  ungroup() %>% 
  # to calculate proportion (joins number of districts per dark cluster)
  left_join(darkloci %>% 
              group_by(DL.NAME) %>% 
              dplyr::summarise(TOT.DIST = n_distinct(DISTRICT.NAME))) %>%
  mutate(PROP.DIST = NO.DIST/TOT.DIST) %>% 
  dplyr::select(-NO.DIST, -TOT.DIST) %>% 
  filter(!is.na(DL.NAME)) %>% 
  # filling zeroes
  group_by(DL.NAME) %>% 
  complete(CONCERN.FINE = 1:5,
           fill = list(PROP.DIST = 0)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "CONCERN.FINE", values_from = "PROP.DIST") %>% 
  set_colnames(c("DL.NAME", "CONCERN.1", "CONCERN.2", "CONCERN.3", "CONCERN.4", "CONCERN.5")) %>% 
  mutate(across(2:6, ~ replace_na(.x, 0))) %>% 
  arrange(desc(CONCERN.5), desc(CONCERN.4), desc(CONCERN.3), desc(CONCERN.2), desc(CONCERN.1))


# writing summary objects
write_xlsx(x = list("Country" = status_nat,
                    "States" = status_state,
                    "Dark clusters" = status_dl),
           path = glue("outputs/status_rel-{rel_year}{rel_month_num %>% str_pad(2, pad = '0')}.xlsx"))



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
  select(N.DIST)


plot1_base <- concern_fine %>% 
  left_join(dists_sf %>% dplyr::select(-AREA)) %>% 
  ggplot() +
  # india outline
  geom_sf(data = india_sf, fill = "#D3D6D9") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

plot1 <- ((plot1_base +
             geom_sf(aes(geometry = DISTRICT.GEOM, fill = INV.C)) +
             geom_sf(data = darkloci_sf %>% filter(DL.NAME == "Magadha"),
                     fill = NA, col = "turquoise", linewidth = 0.9) +
             scale_fill_viridis_c(option = "inferno", 
                                  name = "Inventory (species)\ncompleteness")) |
            (plot1_base +
               geom_sf(aes(geometry = DISTRICT.GEOM, fill = N.DIST)) +
               geom_sf(data = darkloci_sf %>% filter(DL.NAME == "Magadha"),
                       fill = NA, col = "turquoise", linewidth = 0.9) +
               scale_fill_viridis_b(option = "inferno", 
                                    breaks = n_bins$N.DIST, 
                                    limits = c(min(n_bins$N.DIST), max(n_bins$N.DIST)),
                                    name = "Current no.\nof lists"))) /
  ((plot1_base +
      geom_sf(aes(geometry = DISTRICT.GEOM, fill = as.factor(CONCERN.FINE))) +
      geom_sf(data = darkloci_sf %>% filter(DL.NAME == "Magadha"),
              fill = NA, col = "turquoise", linewidth = 0.9) +
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
       filename = glue("outputs/status_rel-{rel_year}{rel_month_num %>% str_pad(2, pad = '0')}.png"),
       dpi = 300, width = 24, height = 24, units = "in")  


# proportions of concern districts per state
plot2_base <- status_state %>% 
  left_join(states_sf) %>% 
  ggplot() +
  # india outline
  geom_sf(data = india_sf, fill = "#D3D6D9") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

plot2 <- (plot2_base +
            geom_sf(aes(geometry = STATE.GEOM, fill = CONCERN.5)) +
            geom_sf(data = darkloci_sf %>% filter(DL.NAME == "Magadha"),
                    fill = NA, col = "turquoise", linewidth = 0.9) +
            scale_fill_viridis_c(option = "inferno", direction = -1,
                                 name = "Prop. of concern 5\ndistricts")) |
  (plot2_base +
     geom_sf(aes(geometry = STATE.GEOM, fill = CONCERN.4)) +
     geom_sf(data = darkloci_sf %>% filter(DL.NAME == "Magadha"),
             fill = NA, col = "turquoise", linewidth = 0.9) +
     scale_fill_viridis_c(option = "inferno", direction = -1,
                          name = "Prop. of concern 4\ndistricts")) |
  (plot2_base +
     geom_sf(aes(geometry = STATE.GEOM, fill = CONCERN.3)) +
     geom_sf(data = darkloci_sf %>% filter(DL.NAME == "Magadha"),
             fill = NA, col = "turquoise", linewidth = 0.9) +
     scale_fill_viridis_c(option = "inferno", direction = -1,
                          name = "Prop. of concern 3\ndistricts"))

ggsave(plot2, 
       filename = glue("outputs/propconcern_st_rel-{rel_year}{rel_month_num %>% str_pad(2, pad = '0')}.png"),
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
             labs(title = "Magadha") +
             theme_void()) |
            (status_dl %>% 
               filter(DL.NAME == "Northeast") %>% 
               left_join(darkloci1) %>% 
               left_join(concern_class_cur %>% distinct(STATE.NAME, DISTRICT.NAME, CONCERN.FINE)) %>% 
               ggplot() +
               geom_sf(aes(geometry = DISTRICT.GEOM, fill = as.factor(CONCERN.FINE))) +
               scale_fill_viridis_d(option = "inferno", direction = -1,
                                    name = "Concern level", na.value = "grey80") +
               labs(title = "Northeast") +
               theme_void())) &
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 24, hjust = 0.5))

ggsave(plot3, 
       filename = glue("outputs/propconcern_dl_rel-{rel_year}{rel_month_num %>% str_pad(2, pad = '0')}.png"),
       dpi = 300, width = 36, height = 14, units = "in")  



# values of interest & XoX change ------------------------------------------------------------------------

# metric 1: percentage high concern districts

# in country, how many high concern
metric1_nat_cur <- concern_coarse %>% 
  # removing districts of no concern (NA)
  filter(!is.na(CONCERN.COARSE)) %>% 
  group_by(CONCERN.COARSE) %>% 
  summarise(NO.DIST = n_distinct(DISTRICT.NAME)) %>% 
  ungroup() %>% 
  # to calculate proportion
  mutate(TOT.DIST = n_distinct(dists_sf$DISTRICT.NAME),
         PROP.DIST = round(100*NO.DIST/TOT.DIST, 2)) %>% 
  dplyr::select(-NO.DIST, -TOT.DIST) %>% 
  complete(CONCERN.COARSE,
           fill = list(PROP.DIST = 0)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "CONCERN.COARSE", values_from = "PROP.DIST") %>% 
  set_colnames(c("CONCERN.LOW", "CONCERN.MID", "CONCERN.HIGH")) %>% 
  mutate(across(everything(), ~ replace_na(.x, 0))) %>% 
  arrange(desc(CONCERN.HIGH), desc(CONCERN.MID), desc(CONCERN.LOW)) %>% 
  mutate(YEAR = cur_year, MONTH = cur_month_num) %>% 
  relocate(YEAR, MONTH, CONCERN.LOW, CONCERN.MID, CONCERN.HIGH)

# in each state, how many high concern
metric1_state_cur <- concern_coarse %>% 
  # removing districts of no concern (NA)
  filter(!is.na(CONCERN.COARSE)) %>% 
  group_by(STATE.NAME, CONCERN.COARSE) %>% 
  summarise(NO.DIST = n_distinct(DISTRICT.NAME)) %>% 
  ungroup() %>% 
  # to calculate proportion
  left_join(state_dists) %>% # joins number of districts per state
  mutate(PROP.DIST = round(100*NO.DIST/TOT.DIST, 2)) %>% 
  dplyr::select(-NO.DIST, -TOT.DIST) %>% 
  filter(!is.na(STATE.NAME)) %>% 
  # filling zeroes
  group_by(STATE.NAME) %>% 
  complete(CONCERN.COARSE,
           fill = list(PROP.DIST = 0)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "CONCERN.COARSE", values_from = "PROP.DIST") %>% 
  set_colnames(c("STATE.NAME", "CONCERN.LOW", "CONCERN.MID", "CONCERN.HIGH")) %>% 
  mutate(across(2:4, ~ replace_na(.x, 0))) %>% 
  arrange(desc(CONCERN.HIGH), desc(CONCERN.MID), desc(CONCERN.LOW)) %>% 
  # adding state codes
  left_join(region_codes %>% distinct(STATE, STATE.CODE), by = c("STATE.NAME" = "STATE")) %>% 
  mutate(YEAR = cur_year, MONTH = cur_month_num) %>% 
  relocate(STATE.CODE, STATE.NAME, YEAR, MONTH, CONCERN.LOW, CONCERN.MID, CONCERN.HIGH)

# in each dark cluster, how many high concern
metric1_dl_cur <- concern_coarse %>% 
  # removing districts of no concern (NA)
  filter(!is.na(CONCERN.COARSE)) %>% 
  inner_join(darkloci) %>% 
  group_by(DL.NAME, CONCERN.COARSE) %>% 
  summarise(NO.DIST = n_distinct(DISTRICT.NAME)) %>% 
  ungroup() %>% 
  # to calculate proportion (joins number of districts per dark cluster)
  left_join(darkloci %>% 
              group_by(DL.NAME) %>% 
              dplyr::summarise(TOT.DIST = n_distinct(DISTRICT.NAME))) %>%
  mutate(PROP.DIST = round(100*NO.DIST/TOT.DIST, 2)) %>% 
  dplyr::select(-NO.DIST, -TOT.DIST) %>% 
  filter(!is.na(DL.NAME)) %>% 
  # filling zeroes
  group_by(DL.NAME) %>% 
  complete(CONCERN.COARSE,
           fill = list(PROP.DIST = 0)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "CONCERN.COARSE", values_from = "PROP.DIST") %>% 
  set_colnames(c("DL.NAME", "CONCERN.LOW", "CONCERN.MID", "CONCERN.HIGH")) %>% 
  mutate(across(2:4, ~ replace_na(.x, 0))) %>% 
  arrange(desc(CONCERN.HIGH), desc(CONCERN.MID), desc(CONCERN.LOW)) %>% 
  mutate(YEAR = cur_year, MONTH = cur_month_num) %>% 
  relocate(DL.NAME, YEAR, MONTH, CONCERN.LOW, CONCERN.MID, CONCERN.HIGH)


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
  
  
  metric1_nat_track <- metric1_nat_cur %>% mutate(MoM = NA_integer_) %>% 
    dplyr::select(-YEAR, -MONTH, -CONCERN.LOW, -CONCERN.MID)
  metric1_state_track <- metric1_state_cur %>% mutate(MoM = NA_integer_) %>% 
    dplyr::select(-YEAR, -MONTH, -CONCERN.LOW, -CONCERN.MID)
  metric1_dl_track <- metric1_dl_cur %>% mutate(MoM = NA_integer_) %>% 
    dplyr::select(-YEAR, -MONTH, -CONCERN.LOW, -CONCERN.MID)
  
  write_xlsx(x = list("Country" = metric1_nat_track,
                      "States" = metric1_state_track,
                      "Dark clusters" = metric1_dl_track),
             path = "outputs/metric1_track.xlsx")
  
  
  } else if (file.exists("outputs/metric1_full.xlsx") &
             file.exists("outputs/metric1_track.xlsx")) {
    
    metric1_nat_old <- read_xlsx("outputs/metric1_full.xlsx", sheet = 1)
    metric1_state_old <- read_xlsx("outputs/metric1_full.xlsx", sheet = 2) %>% 
      # only one month has codes
      dplyr::select(-STATE.CODE) %>% 
      # adding state codes
      left_join(region_codes %>% distinct(STATE, STATE.CODE), by = c("STATE.NAME" = "STATE"))
    metric1_dl_old <- read_xlsx("outputs/metric1_full.xlsx", sheet = 3)
    
    if ((metric1_nat_old %>% slice_tail())$YEAR != cur_year |
        ((metric1_nat_old %>% slice_tail())$YEAR == cur_year & 
         (metric1_nat_old %>% slice_tail())$MONTH != cur_month_num)) {
      
      # for MoM later
      metric1_nat_prev <- metric1_nat_old %>% slice_tail()
      metric1_state_prev <- metric1_state_old %>% group_by(STATE.NAME) %>% slice_tail() %>% ungroup()
      metric1_dl_prev <- metric1_dl_old %>% group_by(DL.NAME) %>% slice_tail() %>% ungroup()
      
      
      metric1_nat_upd <- metric1_nat_old %>% 
        bind_rows(metric1_nat_cur) %>% 
        arrange(YEAR, MONTH)
      
      metric1_state_upd <- metric1_state_old %>% 
        bind_rows(metric1_state_cur) %>% 
        arrange(STATE.NAME, YEAR, MONTH)
      
      metric1_dl_upd <- metric1_dl_old %>% 
        bind_rows(metric1_dl_cur) %>% 
        arrange(DL.NAME, YEAR, MONTH)
      
      # writing objects
      write_xlsx(x = list("Country" = metric1_nat_upd,
                          "States" = metric1_state_upd,
                          "Dark clusters" = metric1_dl_upd),
                 path = "outputs/metric1_full.xlsx")
      
      
      # only current month in focus with MoM
      # retain only High Concern
      mom_nat <- metric1_nat_prev %>% 
        mutate(CONCERN.LOW = NULL, CONCERN.MID = NULL) %>% 
        bind_rows(metric1_nat_cur %>% mutate(CONCERN.LOW = NULL, CONCERN.MID = NULL)) %>% 
        arrange(YEAR, MONTH) %>% 
        mutate(YEAR = NULL) %>% 
        pivot_wider(names_from = "MONTH", values_from = "CONCERN.HIGH") %>% 
        magrittr::set_colnames(c("OLD", "CUR")) %>% 
        dplyr::summarise(MoM = round(100*(CUR - OLD)/OLD, 4))
      
      mom_state <- metric1_state_prev %>% 
        mutate(CONCERN.LOW = NULL, CONCERN.MID = NULL) %>% 
        bind_rows(metric1_state_cur %>% mutate(CONCERN.LOW = NULL, CONCERN.MID = NULL)) %>% 
        arrange(YEAR, MONTH) %>% 
        mutate(YEAR = NULL) %>% 
        group_by(STATE.NAME) %>% 
        pivot_wider(names_from = "MONTH", values_from = "CONCERN.HIGH") %>% 
        magrittr::set_colnames(c("STATE.NAME", "STATE.CODE", "OLD", "CUR")) %>% 
        dplyr::summarise(MoM = round(100*(CUR - OLD)/OLD, 4))
      
      mom_dl <- metric1_dl_prev %>% 
        mutate(CONCERN.LOW = NULL, CONCERN.MID = NULL) %>% 
        bind_rows(metric1_dl_cur %>% mutate(CONCERN.LOW = NULL, CONCERN.MID = NULL)) %>% 
        arrange(YEAR, MONTH) %>% 
        mutate(YEAR = NULL) %>% 
        group_by(DL.NAME) %>% 
        pivot_wider(names_from = "MONTH", values_from = "CONCERN.HIGH") %>% 
        magrittr::set_colnames(c("DL.NAME", "OLD", "CUR")) %>% 
        dplyr::summarise(MoM = round(100*(CUR - OLD)/OLD, 4))
      
      
      metric1_nat_track <- metric1_nat_cur %>% bind_cols(mom_nat) %>% 
        dplyr::select(-YEAR, -MONTH, -CONCERN.LOW, -CONCERN.MID)
      metric1_state_track <- metric1_state_cur %>% left_join(mom_state) %>% 
        dplyr::select(-YEAR, -MONTH, -CONCERN.LOW, -CONCERN.MID)
      metric1_dl_track <- metric1_dl_cur %>% left_join(mom_dl) %>% 
        dplyr::select(-YEAR, -MONTH, -CONCERN.LOW, -CONCERN.MID)
      
      
      # writing objects
      write_xlsx(x = list("Country" = metric1_nat_track,
                          "States" = metric1_state_track,
                          "Dark clusters" = metric1_dl_track),
                 path = "outputs/metric1_track.xlsx")
      
  } else if ((metric1_nat_old %>% slice_tail())$YEAR == cur_year & 
         (metric1_nat_old %>% slice_tail())$MONTH == cur_month_num) {
    
    # for MoM later
    metric1_nat_prev <- metric1_nat_old %>% 
      filter(!(YEAR == cur_year & MONTH == cur_month_num)) %>% 
      slice_tail()
    metric1_state_prev <- metric1_state_old  %>% 
      filter(!(YEAR == cur_year & MONTH == cur_month_num)) %>% 
      group_by(STATE.NAME) %>% 
      slice_tail() %>% 
      ungroup()
    metric1_dl_prev <- metric1_dl_old %>% 
      filter(!(YEAR == cur_year & MONTH == cur_month_num)) %>% 
      group_by(DL.NAME) %>% 
      slice_tail() %>% 
      ungroup()
    
    
    metric1_nat_upd <- metric1_nat_old %>% 
      filter(!(YEAR == cur_year & MONTH == cur_month_num)) %>% 
      bind_rows(metric1_nat_cur) %>% 
      arrange(YEAR, MONTH)
    
    metric1_state_upd <- metric1_state_old %>% 
      filter(!(YEAR == cur_year & MONTH == cur_month_num)) %>% 
      bind_rows(metric1_state_cur) %>% 
      arrange(STATE.NAME, YEAR, MONTH)
    
    metric1_dl_upd <- metric1_dl_old %>% 
      filter(!(YEAR == cur_year & MONTH == cur_month_num)) %>% 
      bind_rows(metric1_dl_cur) %>% 
      arrange(DL.NAME, YEAR, MONTH)
    
    # writing objects
    write_xlsx(x = list("Country" = metric1_nat_upd,
                        "States" = metric1_state_upd,
                        "Dark clusters" = metric1_dl_upd),
               path = "outputs/metric1_full.xlsx")
    
    
    # only current month in focus with MoM
    # retain only High Concern
    mom_nat <- metric1_nat_prev %>% 
      mutate(CONCERN.LOW = NULL, CONCERN.MID = NULL) %>% 
      bind_rows(metric1_nat_cur %>% mutate(CONCERN.LOW = NULL, CONCERN.MID = NULL)) %>% 
      arrange(YEAR, MONTH) %>% 
      mutate(YEAR = NULL) %>% 
      pivot_wider(names_from = "MONTH", values_from = "CONCERN.HIGH") %>% 
      magrittr::set_colnames(c("OLD", "CUR")) %>% 
      dplyr::summarise(MoM = round(100*(CUR - OLD)/OLD, 4))
    
    mom_state <- metric1_state_prev %>% 
      mutate(CONCERN.LOW = NULL, CONCERN.MID = NULL) %>% 
      bind_rows(metric1_state_cur %>% mutate(CONCERN.LOW = NULL, CONCERN.MID = NULL)) %>% 
      arrange(YEAR, MONTH) %>% 
      mutate(YEAR = NULL) %>% 
      group_by(STATE.NAME) %>% 
      pivot_wider(names_from = "MONTH", values_from = "CONCERN.HIGH") %>% 
      magrittr::set_colnames(c("STATE.NAME", "STATE.CODE", "OLD", "CUR")) %>% 
      dplyr::summarise(MoM = round(100*(CUR - OLD)/OLD, 4))
    
    mom_dl <- metric1_dl_prev %>% 
      mutate(CONCERN.LOW = NULL, CONCERN.MID = NULL) %>% 
      bind_rows(metric1_dl_cur %>% mutate(CONCERN.LOW = NULL, CONCERN.MID = NULL)) %>% 
      arrange(YEAR, MONTH) %>% 
      mutate(YEAR = NULL) %>% 
      group_by(DL.NAME) %>% 
      pivot_wider(names_from = "MONTH", values_from = "CONCERN.HIGH") %>% 
      magrittr::set_colnames(c("DL.NAME", "OLD", "CUR")) %>% 
      dplyr::summarise(MoM = round(100*(CUR - OLD)/OLD, 4))
    
    
    metric1_nat_track <- metric1_nat_cur %>% bind_cols(mom_nat) %>% 
      dplyr::select(-YEAR, -MONTH, -CONCERN.LOW, -CONCERN.MID)
    metric1_state_track <- metric1_state_cur %>% left_join(mom_state) %>% 
      dplyr::select(-YEAR, -MONTH, -CONCERN.LOW, -CONCERN.MID)
    metric1_dl_track <- metric1_dl_cur %>% left_join(mom_dl) %>% 
      dplyr::select(-YEAR, -MONTH, -CONCERN.LOW, -CONCERN.MID)
    
    
    # writing objects
    write_xlsx(x = list("Country" = metric1_nat_track,
                        "States" = metric1_state_track,
                        "Dark clusters" = metric1_dl_track),
               path = "outputs/metric1_track.xlsx")
    
  } else {
  
    print("Metrics for latest months already available.")
    
  }
    
}

