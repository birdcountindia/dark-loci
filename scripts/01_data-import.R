require(tidyverse)

load("data/ebd_IN_relJul-2022.RData")
load("data/maps.RData")

# function to join map vars
source("https://raw.githubusercontent.com/ashwinv2005/soib_v2/master/hist_spread/hist_spread_functions.R")

data0 <- data %>% 
  filter(ALL.SPECIES.REPORTED == 1 & 
           PROTOCOL.TYPE != "Incidental" &
           DURATION.MINUTES >= 10) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  slice(1) %>% 
  ungroup()

rm(data)

save.image("data/01_data-import.RData")
