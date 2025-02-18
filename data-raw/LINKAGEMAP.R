## code to prepare `LINKAGEMAP` dataset goes here

LINKAGEMAP = readxl::read_excel("data-raw/linkageMap50.xlsx")
LINKAGEMAP
usethis::use_data(LINKAGEMAP, overwrite = TRUE)

