## code to prepare `LINKAGEMAP` dataset goes here
library(tidyverse)
library(pedprobr)

LINKAGEMAP = readxl::read_excel("data-raw/markerMap.xlsx") |>
  print()

# Test
x = LINKAGEMAP |>
  group_by(Pair) |>
  mutate(CMdist = diff(CMpos),
         rho = pedprobr::kosambi(cM = CMdist)) |>
  print()

usethis::use_data(LINKAGEMAP, overwrite = TRUE)
