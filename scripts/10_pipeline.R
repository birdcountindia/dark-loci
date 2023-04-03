library(lubridate)
library(tidyverse)
library(glue)
library(sf)
library(magrittr)
library(patchwork)
library(writexl)
library(readxl)


# importing data ----------------------------------------------------------

source("scripts/01_data-import.R")


# identifying Dark ------------------------------------------------------------------

### district-wise completeness

source("scripts/02_completeness.R")


### setting thresholds and classifying into concern categories
source("scripts/03_thresholds.R")


### summaries (metrics to monitor)

# in each state, how many districts
state_dists <- states_sf %>% 
  dplyr::select(-AREA) %>% 
  st_join(dists_sf %>% dplyr::select(-AREA, -STATE.NAME), join = st_covers) %>% 
  st_drop_geometry()

### find out why some districts disappear
# maybe don't do st_covers but do largest join

# in each state, how many of each category
summ_state <- concern_class %>% 
  group_by(STATE.NAME, CONCERN) %>% 
  summarise(NO.DIST = n_distinct(DISTRICT.NAME)) %>% 
  ungroup() %>% 
  filter(!is.na(STATE.NAME)) %>% 
  pivot_wider(names_from = "CONCERN", values_from = "NO.DIST") %>% 
  set_colnames(c("STATE.NAME", "CONCERN.2", "CONCERN.3", "CONCERN.4", "CONCERN.5")) %>% 
  mutate(across(2:5, ~ replace_na(.x, 0))) %>% 
  arrange(desc(CONCERN.5), desc(CONCERN.4), desc(CONCERN.3), desc(CONCERN.2))


# bins for number of lists per district
n_bins <- temp_sum %>% 
  distinct(N.DIST) %>% 
  arrange(N.DIST) %>% 
  rownames_to_column("ROW") 

temp <- seq(min(n_bins$ROW), max(n_bins$ROW), length.out = 6) %>% floor()

n_bins <- n_bins %>% 
  # selecting thresholds
  filter(ROW %in% temp) %>% 
  select(N.DIST)



plot1_base <- temp_sum %>% 
  left_join(dists_sf %>% dplyr::select(-STATE.NAME, -AREA)) %>% 
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
            scale_fill_viridis_c(option = "inferno", 
                                 name = "Inventory (species)\ncompleteness")) |
  (plot1_base +
     geom_sf(aes(geometry = DISTRICT.GEOM, fill = N.DIST)) +
     scale_fill_viridis_b(option = "inferno", 
                          breaks = n_bins$N.DIST, 
                          limits = c(min(n_bins$N.DIST), max(n_bins$N.DIST)),
                          name = "Current no.\nof lists"))) /
((plot1_base +
    geom_sf(aes(geometry = DISTRICT.GEOM, fill = as.factor(CONCERN))) +
    scale_fill_viridis_d(option = "inferno", direction = -1,
                         name = "Concern level")) |
   # map with three concern colours
  (temp_sum_cat %>% 
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
     geom_sf(aes(geometry = DISTRICT.GEOM, fill = as.factor(CONCERN))) +
     scale_fill_viridis_d(option = "inferno", direction = -1,
                          name = "Concern level"))) 
  
ggsave(plot1, filename = "outputs/plot1.png", 
       dpi = 300, width = 24, height = 24, units = "in")  


plot2_base <- temp_sum2 %>% 
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
            scale_fill_viridis_c(option = "inferno", direction = -1,
                                 name = "No. of concern 5\ndistricts")) |
  (plot2_base +
     geom_sf(aes(geometry = STATE.GEOM, fill = CONCERN.4)) +
     scale_fill_viridis_c(option = "inferno", direction = -1,
                          name = "No. of concern 4\ndistricts")) |
  (plot2_base +
     geom_sf(aes(geometry = STATE.GEOM, fill = CONCERN.3)) +
     scale_fill_viridis_c(option = "inferno", direction = -1,
                          name = "No. of concern 3\ndistricts"))

ggsave(plot2, filename = "outputs/plot2.png", 
       dpi = 300, width = 36, height = 14, units = "in")  



# identifying Loci ------------------------------------------------------------------


#