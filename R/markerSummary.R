
markerSummary = function(x, markers = NULL, linkageMap = NULL) {
  ped = x[[1]][[1]]
  M = if(is.null(markers)) ped$MARKERS else pedtools::getMarkers(ped, markers)

  # Check if special (founder-type) lumping applies to all markers
  specLump = all(untypedMembers(x[[1]]) %in% founders(x[[1]])) &&
    all(untypedMembers(x[[2]]) %in% founders(x[[2]]))

  reslist = lapply(M, function(m) {
    mut = mutmod(m)
    pars = getParams(mut, c("model", "rate", "range", "rate2"), format = 3)
    colnames(pars) = c("Model", "Rate", "Range", "Rate2")

    stronglump = alwaysLumpable(mut)
    lumptxt = if(stronglump) "Yes (strongly)" else if (specLump) "Yes (special)" else "No"

    cbind.data.frame(Marker = name(m),
                     Alleles = nAlleles(m),
                     MinFreq = sprintf("%.2g", min(afreq(m))),
                     pars,
                     Stationary = if(isStationary(mut)) "Yes" else "No",
                     Lumpable = lumptxt)
  })

  res = do.call(rbind, reslist)
  if(any(grepl("/", res$Rate, fixed = TRUE)))
    colnames(res)[colnames(res) == "Rate"] = "Rate (F/M)"

  if(!is.null(linkageMap)) {
    pairNo = linkageMap$Pair[match(res$Marker, linkageMap$Marker)]
    res = cbind(Pair = pairNo, res)
  }

  res
}

