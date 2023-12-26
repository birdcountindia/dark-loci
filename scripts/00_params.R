# automated parameters

get_param(date_currel = "2023-02-01")


maindatapath <-  glue("../ebird-datasets/EBD/ebd_IN_rel{currel_month_lab}-{currel_year}.RData")
slicedatapath <-  glue("../ebird-datasets/EBD/ebd_IN_rel{currel_month_lab}-{currel_year}_slice.RData")

# # test data
# maindatapath <-  glue("ebd_IN_rel{currel_month_lab}-{currel_year}.RData")
# slicedatapath <-  glue("ebd_IN_rel{currel_month_lab}-{currel_year}_slice.RData")

# subfolder for monthly outputs
if (!dir.exists(glue("outputs/{currel_year}"))) {
  dir.create(glue("outputs/{currel_year}"), recursive = TRUE)
}