## code to prepare `LINKAGEMAP` dataset goes here
library(tidyverse)
library(readxl)


LINKAGEMAP = read_excel("data-raw/linkageMap50.xlsx")
usethis::use_data(LINKAGEMAP, overwrite = TRUE)


linkagePairList = NULL

for(i in unique.default(x$Chr)) {
  xi = x[x$Chr == i, , drop = FALSE]
  idx = closestPairs(xi$cM)
  linkagePairList = rbind(linkagePairList, xi[idx, , drop = FALSE])
}



# Old code ----------------------------------------------------------------

LINKAGEMAP9 = readxl::read_excel("data-raw/markerMap.xlsx") |>
  print()

# Test
x = LINKAGEMAP9 |>
  group_by(Pair) |>
  mutate(DistCM = diff(PosCM),
         rho = pedprobr::kosambi(cM = DistCM)) |>
  print()

usethis::use_data(LINKAGEMAP9, overwrite = TRUE)
