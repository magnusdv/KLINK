# Internal dataset: PIC values for all markers in 'NorskDB_2023' database.
# Used for choosing markers to be included in Unlinked report
# Ideally, these choices should depend on the database, but this breaks consistency fo official reports.

library(pedFamilias)

# Function calculating PIC for a given frequency vector
# Equivalent to eq (3) in https://doi.org/10.1016/j.gene.2019.144175
PIC = function(p) 1 - sum(p^2) - sum(p^2)^2 + sum(p^4)

# Load database
db = readFam("NorskDB_2023.fam")

# Collect markers names and PIC values
df = data.frame(Marker = sapply(db, function(m) m$name),
                PIC    = sapply(db, function(m) PIC(m$afreq)))

PICnor = setNames(df$PIC, df$Marker)
PICnor

# Save as internal data
usethis::use_data(PICnor, internal = TRUE, overwrite = TRUE)
