## code to prepare `paternity` dataset goes here
library(pedsuite)
library(ibdsim2)
library(norSTR)

# Markers (unclear why these were chosen)
markers = c("D5S2500", "SE33", "D6S474", "D8S1132", "D8S1179", "D10S2325",
            "D11S554", "D12S391", "D18S51", "D19S433", "D21S2055")

map = subset(map50, Marker %in% markers)
db = norSTR::norwayDB[markers]
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
