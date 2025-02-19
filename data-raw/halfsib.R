## code to prepare `halfsib` dataset goes here
library(pedsuite)
library(ibdsim2)

# Complete database, 50 markers
db = pedFamilias::readFam("data-raw/NorskDB_2023.fam", verbose = F)

map = KLINK::LINKAGEMAP

map$Mb = ibdsim2::convertPos(chrom = map$Chr, cM = map$cM)
nas = which(is.na(map$Mb))
map$Mb[nas] = sapply(loadMap(), physRange)[map$Chr[nas]]
as.data.frame(map)

# True pedigree: Half sibling
ids = LETTERS[1:3]
ped = halfSibPed(2, 1) |>
  relabel(4:6, new = ids) |>
  setMarkers(locusAttributes = db) |>
  setMap(map[c("Chr", "Marker", "Mb")])

ibd = ibdsim2::ibdsim(ped, ids = ids, seed = 1729)

ped1 = profileSimIBD(ped, ibd, ids = ids, seed = 1729) |>
  setAlleles(markers = c("D5S2500"), alleles = 0) |> # 1 empty marker
  setGenotype(ids = "C", marker = "D2S1360", geno = "27/27") |>
  setMap(map = data.frame(Chrom = NA, Marker = map$Marker, Mb = NA))

# Alternative: Half siblings
ped2 = list(nuclearPed(children = ids[1:2]), singleton("C")) |>
  transferMarkers(from = ped1, to = _)

halfsib = list(ped1, ped2)

# linkedLR(halfsib, LINKAGEMAP, lumpSpecial = T) |>  KLINK::addTotals()

usethis::use_data(halfsib, overwrite = TRUE)

pedFamilias::writeFam(halfsib, famfile = "data-raw/halfsib.fam", params = list(dbName = "NorskDB_2023"))
pedFamilias::writeFam(halfsib, famfile = "inst/extdata/halfsib.fam", params = list(dbName = "NorskDB_2023"))
