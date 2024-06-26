## code to prepare `paternity` dataset goes here
library(pedsuite)
library(ibdsim2)

# Markers in built-in LINKAGEMAP also appearing in NorwegianFrequencies
mmap = subset(LINKAGEMAP, Marker %in% names(NorwegianFrequencies))
db = NorwegianFrequencies[mmap$Marker]
ids = c("AF", "CH")

# True pedigree: Father & son
ped1 = nuclearPed(fa = "AF", ch = "CH") |>
  profileSim(ids = ids, markers = db, seed = 1729) |>
  setMutmod(model = "stepwise", rate = 0.002, range = 0.1, rate2 = 1e-6)

# Add mutation in first marker (12/15 -> 12/14)
ped1 = setGenotype(ped1, marker = 1, ids = "CH", geno = "12/14")

# Unrelated hypothesis
ped2 = singletons(ids) |>
  transferMarkers(from = ped1, to = _)

paternity = list(H1 = ped1, H2 = ped2)
usethis::use_data(paternity, overwrite = TRUE)
