library(lubridate)
library(tidyverse)
library(glue)
library(sf)
library(magrittr)
library(patchwork)
library(writexl)
library(readxl)


# importing data ----------------------------------------------------------

tictoc::tic("Data import completed")
source("scripts/01_data-import.R")
# load("../india-maps/outputs/maps_sf.RData")
sf_use_s2(FALSE)
# load("data/data0.RData")
tictoc::toc() # 18 mins

# in each state, how many districts
state_dists <- dists_sf %>% 
  st_drop_geometry() %>% 
  dplyr::select(-AREA) %>% 
  left_join(states_sf %>% dplyr::select(-AREA)) %>% 
  st_drop_geometry() %>% 
  group_by(STATE.NAME) %>% 
  dplyr::summarise(TOT.DIST = n_distinct(DISTRICT.NAME))


# identifying Dark ------------------------------------------------------------------

### district-wise completeness
tictoc::tic("Districtwise completeness analysis finished")
source("scripts/02_completeness.R")
# load("data/02_completeness.RData")
tictoc::toc() # 180 secs

### setting thresholds and classifying into concern categories
tictoc::tic("Threshold setting and concern classification completed")
source("scripts/03_thresholds.R")
tictoc::toc() # 55 secs


# identifying Loci ------------------------------------------------------------------

source("scripts/04_id-loci.R")


# metric summaries to track and monitor ---------------------------------------------

# filtering districts that are of no concern (fine and coarse)
concern_class1 <- concern_class %>% 
  filter(!(INV.C >= 0.75)) %>% 
  filter(CONCERN.FINE != 1)

concern_class2 <- concern_class %>% 
  filter(!(INV.C >= 0.75)) %>% 
  filter(CONCERN.COARSE != "LOW")


# in each state, how many of each category
summ_state <- concern_class1 %>% 
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
  arrange(desc(CONCERN.5), desc(CONCERN.4), desc(CONCERN.3), desc(CONCERN.2), desc(CONCERN.1))

# in country, how many of each category
summ_nat <- concern_class1 %>% 
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
n_bins <- concern_class1 %>% 
  distinct(N.DIST) %>% 
  arrange(N.DIST) %>% 
  rownames_to_column("ROW") %>% 
  mutate(ROW = as.numeric(ROW))

temp <- seq(min(n_bins$ROW), max(n_bins$ROW), length.out = 6) %>% floor()

n_bins <- n_bins %>% 
  # selecting thresholds
  filter(ROW %in% temp) %>% 
  select(N.DIST)



plot1_base <- concern_class1 %>% 
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
  (concern_class2 %>% 
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
     scale_fill_viridis_d(option = "inferno", direction = -1,
                          name = "Concern level"))) 
  
ggsave(plot1, filename = "outputs/plot1.png", 
       dpi = 300, width = 24, height = 24, units = "in")  


# proportions of concern districts per state
plot2_base <- summ_state %>% 
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
                                 name = "Prop. of concern 5\ndistricts")) |
  (plot2_base +
     geom_sf(aes(geometry = STATE.GEOM, fill = CONCERN.4)) +
     scale_fill_viridis_c(option = "inferno", direction = -1,
                          name = "Prop. of concern 4\ndistricts")) |
  (plot2_base +
     geom_sf(aes(geometry = STATE.GEOM, fill = CONCERN.3)) +
     scale_fill_viridis_c(option = "inferno", direction = -1,
                          name = "Prop. of concern 3\ndistricts"))

ggsave(plot2, filename = "outputs/plot2.png", 
       dpi = 300, width = 36, height = 14, units = "in")  
