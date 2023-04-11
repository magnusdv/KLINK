
markerSummary = function(x, markers = NULL) {
  ped = x[[1]][[1]]
  M = if(is.null(markers)) ped$MARKERS else pedtools::getMarkers(ped, markers)

  # Check if special (founder-type) lumping applies to all markers
  specLump = all(untypedMembers(x[[1]]) %in% founders(x[[1]])) &&
    all(untypedMembers(x[[2]]) %in% founders(x[[2]]))

  reslist = lapply(M, function(m) {
    mut = mutmod(m)
    pars = getParams(mut, c("model", "rate", "range", "rate2"), format = 3)
    names(pars)[1] = "Model"

    stronglump = alwaysLumpable(mut)
    lumptxt = if(stronglump) "Yes (strongly)" else if (specLump) "Yes (special)" else "No"

    df = as.data.frame(list(Marker = name(m), Alleles = nAlleles(m),
                            MinFreq = sprintf("%.2g", min(afreq(m)))))
    cbind(df, pars, Lumpable = lumptxt)
  })

  res = do.call(rbind, reslist)
  res
}

