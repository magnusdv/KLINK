## code to prepare `LINKAGEMAP` dataset goes here
library(readxl)


LINKAGEMAP = read_excel("data-raw/linkageMap50.xlsx")

usethis::use_data(LINKAGEMAP, overwrite = TRUE)

