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

temp_sum <- data1 %>% 
  # join states
  left_join(data0 %>% distinct(COUNTY, STATE)) %>% 
  # reordering columns
  relocate(Q1, Q2, STATE, COUNTY, S.OBS, S.EXP, N, INV.C) %>% 
  # removing >=90% completeness cos of no concern
  filter(!(INV.C >= 0.9)) %>% 
  # levels of incompleteness based on arbitrary thresholds (after hist())
  mutate(CONCERN = case_when(INV.C < 0.9 & INV.C >= 0.78 ~ "LOW",
                             INV.C < 0.78 & INV.C >= 0.5 ~ "MID",
                             INV.C < 0.5 ~ "HIGH")) %>% 
  mutate(CONCERN = factor(CONCERN, levels = c("HIGH", "MID", "LOW"))) %>% 
  arrange(CONCERN, desc(INV.C), N, STATE, COUNTY) %>% 
  # removing LOW concern for now to focus only on MID & HIGH
  filter(CONCERN != "LOW")

# in each state, how many of each category
temp_sum2 <- temp_sum %>% 
  group_by(STATE, CONCERN) %>% 
  summarise(NO.DIST = n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "CONCERN", values_from = "NO.DIST") %>% 
  relocate(STATE, MID, HIGH) %>% 
  set_colnames(c("STATE", "N.MID", "N.HIGH")) %>% 
  mutate(across(2:3, ~ replace_na(.x, 0))) %>% 
  arrange(desc(N.HIGH), desc(N.MID))

write_xlsx(x = list("Dist. inv.-comp." = temp_sum,
                    "States concern" = temp_sum2),
           path = "output/summary1.xlsx")


plot1_base <- temp_sum %>% 
  left_join(districts_sf, by = c("COUNTY" = "DISTRICT.NAME")) %>% 
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

plot1 <- (plot1_base +
            geom_sf(aes(geometry = geometry, fill = INV.C)) +
            scale_fill_viridis_c(option = "inferno", 
                                 name = "Inventory (species)\ncompleteness")) |
  (plot1_base +
     geom_sf(aes(geometry = geometry, fill = CONCERN)) +
     scale_fill_viridis_d(option = "inferno", 
                          name = "Concern level")) |
  (plot1_base +
     geom_sf(aes(geometry = geometry, fill = N)) +
     scale_fill_viridis_c(option = "inferno", 
                          name = "Current no.\nof lists",
                          trans = "log10"))

ggsave(plot1, filename = "output/plot1.png", 
       dpi = 300, width = 36, height = 14, units = "in")  


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
            geom_sf(aes(geometry = geometry, fill = N.HIGH)) +
            scale_fill_viridis_c(option = "inferno", direction = -1,
                                 name = "No. of high\nconcern districts")) |
  (plot2_base +
     geom_sf(aes(geometry = geometry, fill = N.MID)) +
     scale_fill_viridis_c(option = "inferno", direction = -1,
                          name = "No. of medium\nconcern districts"))

ggsave(plot2, filename = "output/plot2.png", 
       dpi = 300, width = 24, height = 13, units = "in")  



# identifying Loci ------------------------------------------------------------------


#