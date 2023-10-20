require(lubridate)
require(tidyverse)
require(glue)

# automated parameters ----------------------------------------------------

# date under consideration for current leaderboard
# cur_date <- if (today() %>% day() < 16) { 
#   (today() - months(1)) %>% floor_date(unit = "month")
# } else {
#   today() %>% floor_date(unit = "month")
# }
# 
# rel_date <- if (today() %>% day() < 16) {
#   ((today() - months(1)) - months(1)) %>%
#     floor_date(unit = "month")
# } else {
#   (today() - months(1)) %>%
#     floor_date(unit = "month")
# }
cur_date <- "2023-03-01" %>% as_date()
rel_date <- "2023-02-01" %>% as_date()


cur_year <- cur_date %>% year()
cur_month_num <- cur_date %>% month()
cur_month_lab <- cur_date %>% month(label = T, abbr = T)

rel_year <- rel_date %>% year()
rel_month_num <- rel_date %>% month()
rel_month_lab <- rel_date %>% month(label = T, abbr = T) 


maindatapath <-  glue("../ebird-datasets/EBD/ebd_IN_rel{rel_month_lab}-{rel_year}.RData")
slicedatapath <-  glue("../ebird-datasets/EBD/ebd_IN_rel{rel_month_lab}-{rel_year}_slice.RData")
# maindatapath <-  "../ebird-datasets/EBD/ebd_IN_relNov-2022.RData"
# slicedatapath <-  "../ebird-datasets/EBD/ebd_IN_relNov-2022_slice.RData"
