
prepareTable = function(pedigrees, linkageMap) {

  # Initialise table
  linkageMap$Pair = paste("Pair", linkageMap$Pair)
  res = linkageMap[c("Pair", "Marker", "CMpos")]

  p1 = pedigrees[[1]]

  # Reduce to markers included in pedigrees
  res = res[res$Marker %in% pedtools::name(p1), , drop = FALSE]

  # Genotypes
  geno = t.default(pedtools::getGenotypes(p1, ids = typedMembers(p1), markers = res$Marker))
  colnames(geno) = paste0("Geno", 1:ncol(geno))
  res = cbind(res, geno)

  # Group index
  res$Gindex = stats::ave(res$Pair, res$Pair, FUN = seq_along)
  res$Gsize = stats::ave(res$Pair, res$Pair, FUN = function(a) rep(length(a), length(a)))

  res
}


#' @export
linkedLR = function(pedigrees, linkageMap, mapfun = "Kosambi") {

  MAPFUN = switch(mapfun, Haldane = pedprobr::haldane, Kosambi = pedprobr::kosambi)

  restable = prepareTable(pedigrees, linkageMap)

  # Single-point LR
  lr1 = forrel::kinshipLR(pedigrees, markers = restable$Marker)
  restable$LRsingle = lr1$LRperMarker[,1]

  # Split linkage groups
  pairs = split(restable, restable$Pair)

  # LR nolink (product of singlepoint)
  lrProd = vapply(pairs, function(pp) prod(pp$LRsingle), FUN.VALUE = 1)
  restable$LRnolink = unlist(lapply(lrProd, function(a) c(a, log(a))))

  # Pairwise linkage
  lr2 = vapply(pairs, function(pp)
    .linkedLR(pedigrees, pp$Marker, cmpos = pp$CMpos, mapfun = MAPFUN, disableMut = FALSE), FUN.VALUE = 1)
  restable$LRlinked = unlist(lapply(lr2, function(a) c(a, log(a))))

  # Without mutmodels
  lr2 = vapply(pairs, function(pp)
    .linkedLR(pedigrees, pp$Marker, cmpos = pp$CMpos, mapfun = MAPFUN, disableMut = TRUE), FUN.VALUE = 1)
  restable$LRnomut = unlist(lapply(lr2, function(a) c(a, log(a))))

  restable
}


.linkedLR = function(peds, markerpair, cmpos, mapfun, disableMut = FALSE) {
  if(length(markerpair) < 2)
    return(NA_real_)

  rho = mapfun(diff(cmpos))

  H1 = pedtools::selectMarkers(peds[[1]], markerpair)
  H2 = pedtools::selectMarkers(peds[[2]], markerpair)

  if(disableMut) {
    H1 = H1 |> pedprobr::setMutationModel(model = NULL)
    H2 = H2 |> pedprobr::setMutationModel(model = NULL)
  }
  else {
    H1 = H1 |> reduceAllelesSpecial(1) |> reduceAllelesSpecial(2)
    H2 = H2 |> reduceAllelesSpecial(1) |> reduceAllelesSpecial(2)
  }

  numer = pedprobr::likelihood2(H1, marker1 = 1, marker2 = 2, rho = rho)
  denom = pedprobr::likelihood2(H2, marker1 = 1, marker2 = 2, rho = rho)
  numer/denom
}
