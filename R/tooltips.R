TT = list(
  Alleles = "Number of distinct alleles in the database",
  PIC = "Polymorphic Information Content",
  MinFreq = "Smallest allele frequency in the database",
  Model = "Mutation model type",
  `Rate (f/m)` = "Mutation rate for females and males",
  Range = "Range - a parameter for stepwise models",
  Rate2 = "Rate2 - a parameter for stepwise models",
  Stationary = "Is the mutation model stationary?",
  Lumpable = "Are alleles lumpable under this model?",
  #
  Pair = "Linkage pair",
  Marker = "Marker name",
  PosCM = "Genetic position (cM)",
  LRsingle = "LR for each individual marker",
  LRnolink = "LR assuming no linkage",
  LRnomut = "LR assuming no mutations (but with linkage)",
  LRlinked = "LR considering pairwise linkage between indicated pairs"
)

addTooltips = function(gt_table) {
  boxhead = gt_table$`_boxhead`
  cols = boxhead$var
  current = boxhead$column_label |> as.character() |> setnames(cols)

  newlabs = list()
  for(col in intersect(cols, names(TT))) {
    tg = shiny::tags$abbr(title = TT[[col]],
                          style = "text-decoration: none; cursor: pointer",
                          current[[col]])
    newlabs[[col]] = tg |> as.character() |> gt::html()
  }

  gt_table |> cols_label(!!!newlabs)
}


