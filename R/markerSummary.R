#' Generate table of marker data
#'
#' @param pedigrees A list of 2 pedigrees.
#' @param replaceNames A logical, indicating if IDs should be changed to
#'   Person1, Person2, ...
#'
#' @return A data frame.
#'
#' @examples
#' markerSummary(paternity)
#'
#' @export
markerSummary = function(pedigrees, replaceNames = FALSE) {
  ped1 = pedigrees[[1]]

  # Check if special (founder-type) lumping applies to all markers
  specLump = specialLumpability(pedigrees)

  # Genotypes
  geno = t.default(pedtools::getGenotypes(ped1, ids = typedMembers(ped1)))
  if(replaceNames)
    colnames(geno) = paste0("Person", 1:ncol(geno))


  # List of lists: Marker attributes
  locAttrs = pedtools::getLocusAttributes(ped1)

  reslist = lapply(locAttrs, function(a) {
    gg = geno[a$name, , drop = FALSE]
    mut = a$mutmod
    pars = pedmut::getParams(mut, c("model", "rate", "range", "rate2"), format = 3)
    colnames(pars) = c("Model", "Rate (f/m)", "Range", "Rate2")

    if(is.null(mut)) {
      lumptxt = stattxt = "-"
    }
    else {
      lumptxt = if(pedmut::alwaysLumpable(mut)) "Always" else if (specLump) "Special" else "No"
      stattxt = if(pedmut::isStationary(mut)) "Yes" else "No"
    }
    cbind.data.frame(Marker = a$name,
                     gg, Typed = sum(gg != "-/-"),
                     Alleles = length(a$alleles),
                     PIC = PIC(a$afreq),
                     MinFreq = sprintf("%.2g", min(a$afreq)),
                     pars, Stationary = stattxt, Lumpable = lumptxt)
  })

  res = do.call(rbind, reslist)

  rownames(res) = NULL
  res
}

# Polymorphic information content
PIC = function(afr) 1 - sum(afr^2) - sum(afr^2)^2 + sum(afr^4)

# Convert list of linked pairs to vector with pair indices and NAs elsewhere
lp2vec = function(markers, linkedPairs) {
  pairvec = rep(seq_along(linkedPairs), each = 2)
  names(pairvec) = unlist(linkedPairs)
  vec = as.integer(pairvec[markers])
  singles = which(tabulate(vec) == 1)
  vec[vec %in% singles] = NA
  vec
}
