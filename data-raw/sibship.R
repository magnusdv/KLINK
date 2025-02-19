## code to prepare `sibship` dataset goes here
library(pedsuite)

# Complete REFA database (50 markers)
db = pedFamilias::readFam("data-raw/NorskDB_2023.fam", verbose = F)
names(db) = sapply(db, `[[`, "name")

# Fusion6c (23 markers)
lm = KLINK::LINKAGEMAP
fusion = lm[grepl("^Fus", lm$Kit), ]$Marker

# True pedigree: Full siblings
ids = c("NN1", "NN2")
ped1 = nuclearPed(children = ids) |>
  setMarkers(locusAttributes = db[fusion]) |>
  profileSim(ids = ids, seed = 1729) |>
  setGenotype(marker = 7, ids = ids, geno = "9/12") |>
  setGenotype(marker = 8, ids = ids, geno = "13/13") |>
  setGenotype(marker = 14, ids = ids, geno = "14/20") |>
  setGenotype(marker = 15, ids = ids, geno = c("18/19.3", "19.3/21"))

# Alternative: Half siblings
ped2 = halfSibPed(type = "m") |>
  relabel(4:5, new = ids) |>
  transferMarkers(from = ped1, to = _)

sibship = list(FS = ped1, HS = ped2)

# linkedLR(sibship, LINKAGEMAP, lumpSpecial = T) |> KLINK::addTotals(c("LRnolink", "LRnomut","LRlinked"))

usethis::use_data(sibship, overwrite = TRUE)

pedFamilias::writeFam(sibship, famfile = "data-raw/sibship.fam", params = list(dbName = "NorskDB_2023"))
pedFamilias::writeFam(sibship, famfile = "inst/extdata/sibship.fam", params = list(dbName = "NorskDB_2023"))
