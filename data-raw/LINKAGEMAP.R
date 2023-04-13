## code to prepare `LINKAGEMAP` dataset goes here
library(tidyverse)
library(pedprobr)

LINKAGEMAP = readxl::read_excel("data-raw/markerMap.xlsx") |>
  print()

# Test
x = LINKAGEMAP |>
  group_by(Pair) |>
  mutate(DistCM = diff(PosCM),
         rho = pedprobr::kosambi(cM = DistCM)) |>
  print()

usethis::use_data(LINKAGEMAP, overwrite = TRUE)
