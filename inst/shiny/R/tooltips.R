# TT = list(
#   Alleles = "Number of distinct alleles in the database",
#   PIC = "Polymorphic Information Content",
#   MinFreq = "Smallest allele frequency in the database",
#   Model = "Mutation model type",
#   Rate = "Mutation rate (females / males)",
#   Rate2 = "Rate2 - a parameter for stepwise models",
#   Range = "Range - a parameter for stepwise models",
#   Stat = "Stationary mutation model?",
#   Lump = "Lumpable mutation model?",
#   #
#   Pair = "Linkage pair",
#   Marker = "Marker name",
#   cM = "Genetic position in centiMorgans",
#   LRsingle = "LR for each individual marker",
#   LRnolink = "LR assuming no linkage. For the linked pairs, this is the product of the individual LRs",
#   LRnomut = "LR assuming no mutations (but with linkage)",
#   LRlinked = "LR considering linkage between the indicated pairs.",
#   Lik1 = "Likelihood for Ped 1 (with linkage and mutations)",
#   Lik2 = "Likelihood for Ped 2 (with linkage and mutations)",
#   Loglik1 = "Log-likelihood for Ped 1 (with linkage and mutations)",
#   Loglik2 = "Log-likelihood for Ped 2 (with linkage and mutations)",
#   #
#   emptymarkers = "Hide or show markers with no genotype information. (Affects the 'LR table' in the app and in the Excel download.)",
#   likelihoods = "Hide or show likelihood columns? (Only affects the 'LR table' in the app.)"
# )
#
# addTooltips = function(gt_table) {
#   boxhead = gt_table$`_boxhead`
#   cols = boxhead$var
#   current = boxhead$column_label |> as.character() |> setnames(cols)
#
#   newlabs = list()
#   for(col in intersect(cols, names(TT))) {
#     tip = strwrap(TT[[col]], width = 50) |> paste0(collapse = "\n")
#     tg = shiny::tags$abbr(title = tip,
#                           style = "text-decoration: none; cursor: pointer",
#                           current[[col]])
#     newlabs[[col]] = tg |> as.character() |> gt::html()
#   }
#
#   gt_table |> cols_label(!!!newlabs)
# }
#
#
