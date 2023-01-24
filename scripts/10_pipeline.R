library(lubridate)
library(tidyverse)
library(glue)
library(sf)
library(magrittr)
library(patchwork)
library(writexl)


# importing data ----------------------------------------------------------

# # if 01_data-import.R not run fully to output RData files, uncomment and run below
# source("scripts/01_data-import.R")

# else 
load("data/maps_sf.RData")
load("data/data0.RData")


# identifying Dark ------------------------------------------------------------------

### district-wise completeness

source("scripts/02_completeness.R")


### temporary summary to share

temp_sum <- data2 %>% 
  # join states
  left_join(districts_sf) %>% 
  st_as_sf() %>% 
  st_make_valid() %>% # to prevent issue with largest join
  st_join(states_sf %>% st_make_valid(), largest = T) %>% 
  st_drop_geometry() %>% 
  # reordering columns
  select(STATE, DISTRICT.NAME, S.OBS.DIST, S.EXP.DIST, S.EXP, N.DIST, INV.C) %>% 
  relocate(STATE, DISTRICT.NAME, S.OBS.DIST, S.EXP.DIST, S.EXP, N.DIST, INV.C) %>% 
  # removing >=75% completeness cos of no concern
  filter(!(INV.C >= 0.75))

thresh1 <- seq(1, n_distinct(temp_sum$DISTRICT.NAME), 
                     length.out = 6)[2:5] # we want 5 groups
  
thresh2 <- temp_sum %>% 
  distinct(DISTRICT.NAME, INV.C) %>% 
  select(INV.C) %>% 
  arrange(INV.C) %>% 
  rownames_to_column("ROW") %>% 
  # selecting thresholds
  filter(ROW %in% floor(thresh1)) %>% 
  pivot_wider(names_from = ROW, values_from = INV.C) %>% 
  set_colnames(c("THRESH.1", "THRESH.2", "THRESH.3", "THRESH.4"))


temp_sum <- temp_sum %>% 
  bind_cols(thresh2) %>% 
  # levels of incompleteness based on thresholds 
  mutate(CONCERN = case_when(INV.C < 0.75 & INV.C >= THRESH.4 ~ 1,
                             INV.C < THRESH.4 & INV.C >= THRESH.3 ~ 2,
                             INV.C < THRESH.3 & INV.C >= THRESH.2 ~ 3,
                             INV.C < THRESH.2 & INV.C >= THRESH.1 ~ 4,
                             INV.C < THRESH.1 ~ 5)) %>% 
  arrange(desc(CONCERN), INV.C, desc(N.DIST), STATE, DISTRICT.NAME) %>% 
  # removing LOW concern for now to focus only on MID & HIGH
  filter(CONCERN != 1) %>% 
  mutate(across(contains("THRESH."), ~ as.null(.)))

# in each state, how many of each category
temp_sum2 <- temp_sum %>% 
  group_by(STATE, CONCERN) %>% 
  summarise(NO.DIST = n()) %>% 
  ungroup() %>% 
  filter(!is.na(STATE)) %>% 
  pivot_wider(names_from = "CONCERN", values_from = "NO.DIST") %>% 
  set_colnames(c("STATE", "CONCERN.2", "CONCERN.3", "CONCERN.4", "CONCERN.5")) %>% 
  mutate(across(2:5, ~ replace_na(.x, 0))) %>% 
  arrange(desc(CONCERN.5), desc(CONCERN.4), desc(CONCERN.3), desc(CONCERN.2))

write_xlsx(x = list("Dist. inv.-comp." = temp_sum,
                    "States concern" = temp_sum2),
           path = "output/summary1.xlsx")


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
  left_join(districts_sf) %>% 
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
            geom_sf(aes(geometry = geometry, fill = INV.C)) +
            scale_fill_viridis_c(option = "inferno", 
                                 name = "Inventory (species)\ncompleteness")) |
  (plot1_base +
     geom_sf(aes(geometry = geometry, fill = N.DIST)) +
     scale_fill_viridis_b(option = "inferno", 
                          breaks = n_bins$N.DIST, 
                          limits = c(min(n_bins$N.DIST), max(n_bins$N.DIST)),
                          name = "Current no.\nof lists"))) /
((plot1_base +
    geom_sf(aes(geometry = geometry, fill = as.factor(CONCERN))) +
    scale_fill_viridis_d(option = "inferno", direction = -1,
                         name = "Concern level")) |
   # map with three concern colours
  (temp_sum %>% 
     filter(CONCERN != 2) %>% 
      left_join(districts_sf) %>% 
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
     geom_sf(aes(geometry = geometry, fill = as.factor(CONCERN))) +
     scale_fill_viridis_d(option = "inferno", direction = -1,
                          name = "Concern level"))) 
  
ggsave(plot1, filename = "output/plot1.png", 
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
            geom_sf(aes(geometry = geometry, fill = CONCERN.5)) +
            scale_fill_viridis_c(option = "inferno", direction = -1,
                                 name = "No. of concern 5\ndistricts")) |
  (plot2_base +
     geom_sf(aes(geometry = geometry, fill = CONCERN.4)) +
     scale_fill_viridis_c(option = "inferno", direction = -1,
                          name = "No. of concern 4\ndistricts")) |
  (plot2_base +
     geom_sf(aes(geometry = geometry, fill = CONCERN.3)) +
     scale_fill_viridis_c(option = "inferno", direction = -1,
                          name = "No. of concern 3\ndistricts"))

ggsave(plot2, filename = "output/plot2.png", 
       dpi = 300, width = 36, height = 14, units = "in")  



# identifying Loci ------------------------------------------------------------------


#