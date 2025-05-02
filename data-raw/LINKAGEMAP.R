## code to prepare `LINKAGEMAP` dataset goes here

# TODO: Remove (this has been replaced with norSTR::map50)
LINKAGEMAP = readxl::read_excel("data-raw/linkageMap50.xlsx")
LINKAGEMAP
usethis::use_data(LINKAGEMAP, overwrite = TRUE)

