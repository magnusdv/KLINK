
markerSummary = function(x, linkageMap = NULL) {
  ped1 = x[[1]]
  ped2 = x[[2]]

  # Check if special (founder-type) lumping applies to all markers
  specLump = specialLumpability(x)

  # Genotypes
  geno = t.default(pedtools::getGenotypes(ped1, ids = typedMembers(ped1)))
  colnames(geno) = paste0("Geno", 1:ncol(geno))

  # List of lists: Marker attributes
  locAttrs = pedtools::getLocusAttributes(ped1)

  reslist = lapply(locAttrs, function(a) {
    mut = a$mutmod
    pars = pedmut::getParams(mut, c("model", "rate", "range", "rate2"), format = 3)
    colnames(pars) = c("Model", "Rate", "Range", "Rate2")

    lumptxt = if(pedmut::alwaysLumpable(mut)) "Always" else if (specLump) "Yes" else "No"
    stattxt = if(pedmut::isStationary(mut)) "Yes" else "No"
    cbind.data.frame(Marker = a$name,
                     Alleles = length(a$alleles),
                     MinFreq = sprintf("%.2g", min(a$afreq)),
                     geno[a$name, , drop = FALSE],
                     pars, Stationary = stattxt, Lumpable = lumptxt)
  })

  res = do.call(rbind, reslist)
  if(any(grepl("/", res$Rate, fixed = TRUE)))
    colnames(res)[colnames(res) == "Rate"] = "Rate (f/m)"

  if(!is.null(linkageMap)) {
    pairNo = linkageMap$Pair[match(res$Marker, linkageMap$Marker)]
    res = cbind(Pair = pairNo, res)
  }

  rownames(res) = NULL
  res
}

