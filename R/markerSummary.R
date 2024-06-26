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
  # ped2 = pedigrees[[2]] # not used?

  # Check if special (founder-type) lumping applies to all markers
  specLump = specialLumpability(pedigrees)

  # Genotypes
  geno = t.default(pedtools::getGenotypes(ped1, ids = typedMembers(ped1)))
  if(replaceNames)
    colnames(geno) = paste0("Person", 1:ncol(geno))

  # List of lists: Marker attributes
  locAttrs = pedtools::getLocusAttributes(ped1)

  reslist = lapply(locAttrs, function(a) {
    mut = a$mutmod
    pars = pedmut::getParams(mut, c("model", "rate", "range", "rate2"), format = 3)
    colnames(pars) = c("Model", "Rate", "Range", "Rate2")

    if(is.null(mut)) {
      lumptxt = stattxt = "-"
    }
    else {
      lumptxt = if(pedmut::alwaysLumpable(mut)) "Always" else if (specLump) "Yes" else "No"
      stattxt = if(pedmut::isStationary(mut)) "Yes" else "No"
    }
    cbind.data.frame(Marker = a$name,
                     Alleles = length(a$alleles),
                     PIC = PIC(a$afreq),
                     MinFreq = sprintf("%.2g", min(a$afreq)),
                     geno[a$name, , drop = FALSE],
                     pars, Stationary = stattxt, Lumpable = lumptxt)
  })

  res = do.call(rbind, reslist)

  if(any(grepl("/", res$Rate, fixed = TRUE)))
    colnames(res)[colnames(res) == "Rate"] = "Rate (f/m)"

  rownames(res) = NULL
  res
}

# Polymorphic information content
PIC = function(afr) 1 - sum(afr^2) - sum(afr^2)^2 + sum(afr^4)
