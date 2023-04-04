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
state_dists <- dists_sf %>% 
  st_drop_geometry() %>% 
  dplyr::select(-AREA) %>% 
  left_join(states_sf %>% dplyr::select(-AREA)) %>% 
  st_drop_geometry() %>% 
  group_by(STATE.NAME) %>% 
  dplyr::summarise(TOT.DIST = n_distinct(DISTRICT.NAME))

### find out why some districts disappear
# maybe don't do st_covers but do largest join

# in each state, how many of each category
summ_state <- concern_class %>% 
  # removing districts of no concern (NA)
  filter(!is.na(CONCERN.FINE)) %>% 
  group_by(STATE.NAME, CONCERN.FINE) %>% 
  summarise(NO.DIST = n_distinct(DISTRICT.NAME)) %>% 
  ungroup() %>% 
  # to calculate proportion
  left_join(state_dists) %>% 
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
  arrange(desc(CONCERN.5), desc(CONCERN.4), desc(CONCERN.3), desc(CONCERN.2), desc(CONCERN.1))

# in country, how many of each category
summ_nat <- concern_class %>% 
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


# writing summary objects
write_xlsx(x = list("National level" = summ_nat,
                    "State level" = summ_state),
           path = "outputs/summary1.xlsx")



# bins for number of lists per district
n_bins <- concern_class %>% 
  distinct(N.DIST) %>% 
  arrange(N.DIST) %>% 
  rownames_to_column("ROW") 

temp <- seq(min(n_bins$ROW), max(n_bins$ROW), length.out = 6) %>% floor()

n_bins <- n_bins %>% 
  # selecting thresholds
  filter(ROW %in% temp) %>% 
  select(N.DIST)



plot1_base <- summ_state %>% 
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




# identifying Loci ------------------------------------------------------------------


#