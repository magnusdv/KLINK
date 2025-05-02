## Code to prepare `sibship` dataset goes here
library(pedsuite)
library(norSTR)

# Fusion6c database
db = norwayDB[markerSets$fusion6c]

ids = c("NN1", "NN2")

# True pedigree: Full siblings
ped1 = nuclearPed(children = ids) |>
  profileSim(markers = db, ids = ids, seed = 1729) |>
  setMutmod(model = "stepwise", rate = list(female = 0.001, male = 0.002),
            rate2 = 1e-6, range = 0.1) |>
  setGenotype(marker = 7, ids = ids, geno = "9/12") |>
  setGenotype(marker = 8, ids = ids, geno = "13/13") |>
  setGenotype(marker = 14, ids = ids, geno = "14/20") |>
  setGenotype(marker = 15, ids = ids, geno = c("18/19.3", "19.3/21"))

# Alternative: Half siblings
ped2 = halfSibPed(type = "m") |>
  relabel(4:5, new = ids) |>
  transferMarkers(from = ped1, to = _)

sibship = list(FS = ped1, HS = ped2)

# linkedLR(sibship, norSTR::map50, lumpSpecial = T) |> KLINK::addTotals()

usethis::use_data(sibship, overwrite = TRUE)

pedFamilias::writeFam(sibship, famfile = "data-raw/sibship.fam", params = list(dbName = "NorskDB_2023"))
pedFamilias::writeFam(sibship, famfile = "inst/extdata/sibship.fam", params = list(dbName = "NorskDB_2023"))
