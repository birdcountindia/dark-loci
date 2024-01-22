library(googledrive)
library(googlesheets4)

# params ----------------------------------------------------------------------

our_gsheet <- "https://docs.google.com/spreadsheets/d/1ptmuVvQ7krxm8TlQQ7iz5Cr-YdngAGb53eewvhCvLcI/"

sheet_prefix_prev <- glue('{month(date_prevrel, label = TRUE, abbr = TRUE)}-{str_trunc(date_prevrel %>% year(), width = 2, side = "left", ellipsis = "")}')
sheet_prefix_cur <- glue('{currel_month_lab}-{str_trunc(currel_year, width = 2, side = "left", ellipsis = "")}')

# authenticating GDrive/GSheets for upload of coverage, metrics --------------

drive_auth(email = "birdcountindia@ncf-india.org")
gs4_auth(email = "birdcountindia@ncf-india.org")

# update gsheet ---------------------------------------------------------------

get_metrics_gsheet("IN") %>% 
  write_metrics_sheet("IN", darkloci = TRUE)

get_metrics_gsheet("ST") %>% 
  write_metrics_sheet("ST", darkloci = TRUE)
