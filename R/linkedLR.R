#' LR with pairwise linked markers
#'
#' This function does the main LR calculations of the KLINK app.
#'
#' @param pedigrees A list of two pedigrees.
#' @param linkageMap A data frame with columns including `Pair`, `Marker`,
#'   `Chrom`, `PosCM`
#' @param markerData A data frame, normally extracted automatically from
#'   `pedigrees`.
#' @param mapfun Name of the map function to be used; either "Haldane" or
#'   "Kosambi" (default)
#'
#' @return A data frame with detailed LR results.
#'
#' @examples
#' library(forrel)
#'
#' ped1 = nuclearPed(fa = "AF", child = "CH") |>
#'   profileSim(markers = NorwegianFrequencies)
#'
#' ped2 = singletons(c("AF", "CH")) |>
#'   transferMarkers(from = ped1, to = _)
#'
#' pedigrees = list(ped1, ped2)
#'
#' linkedLR(pedigrees, LINKAGEMAP)
#'
#' @export
linkedLR = function(pedigrees, linkageMap, markerData = NULL, mapfun = "Kosambi") {
  if (getOption("KLINK.debug")) print("linkedLR")

  if(is.null(markerData))
    markerData = markerSummary(pedigrees, linkageMap)

  MAPFUN = switch(mapfun, Haldane = pedprobr::haldane, Kosambi = pedprobr::kosambi)

  # Initialise table: Pair, Marker, Geno
  res = markerData[c(1, 2, grep("Person", names(markerData), fixed = TRUE))]
  nr = nrow(res)

  # Add cM positions
  res$PosCM = linkageMap$PosCM[match(res$Marker, linkageMap$Marker)]

  # Replace missing pairs with dummy 1001, 1002, ... (otherwise lost in split)
  if(any(NApair <- is.na(res$Pair)))
    res$Pair[NApair] = 1000 + seq_along(which(NApair))

  # Group size (1 or 2)
  res$Gsize = stats::ave(1:nr, res$Pair, FUN = function(a) rep(length(a), length(a)))

  # Put (intact) pairs on top
  res = res[order(-res$Gsize, res$Pair, res$PosCM), , drop = FALSE]

  # Index within each group (do after ordering!)
  res$Gindex = stats::ave(1:nr, res$Pair, FUN = seq_along)

  # Special lumping
  if(specialLumpability(pedigrees))
    pedigrees = lapply(pedigrees, lumpAllSpecial)

  # Single-point LR
  lr1 = forrel::kinshipLR(pedigrees, markers = res$Marker)
  res$LRsingle = lr1$LRperMarker[,1]

  # No-mutation versions
  pedsNomut = lapply(pedigrees, function(x) setMutmod(x, model = NULL))
  LRnomut = forrel::kinshipLR(pedsNomut, markers = res$Marker)$LRperMarker[, 1]

  # Fix lost names when only 1 marker
  if(is.null(names(LRnomut)))
    names(LRnomut) = res$Marker

  # Split linkage groups
  pairs = split(res, res$Pair)

  res$LRnolink = NA_real_
  res$LRlinked = NA_real_
  res$LRnomut  = NA_real_

  for(pp in pairs) {
    m = pp$Marker
    idx1 = match(m[1], res$Marker)

    if(nrow(pp) == 2) {
      res$LRnolink[idx1] = prod(pp$LRsingle)
      res$LRlinked[idx1] = .linkedLR(pedigrees, m, cmpos = pp$PosCM, mapfun = MAPFUN)
      res$LRnomut[idx1]  = .linkedLR(pedsNomut, m, cmpos = pp$PosCM, mapfun = MAPFUN)
    }
    else {
      res$LRnolink[idx1] = res$LRlinked[idx1] = pp$LRsingle
      res$LRnomut[idx1] = LRnomut[[m]]
    }
  }

  # Repair "Pair" column
  res$Pair = ifelse(res$Gsize > 1, paste("Pair", res$Pair), "Unpaired")

  res
}


.linkedLR = function(peds, markerpair, cmpos, mapfun, disableMut = FALSE) {
  if (getOption("KLINK.debug")) print(paste(".linkedLR:", paste(markerpair, collapse = ", ")))
  if(length(markerpair) < 2)
    return(NA_real_)

  rho = mapfun(diff(cmpos))

  H1 = pedtools::selectMarkers(peds[[1]], markerpair)
  H2 = pedtools::selectMarkers(peds[[2]], markerpair)

  if(disableMut) {
    H1 = H1 |> setMutmod(model = NULL)
    H2 = H2 |> setMutmod(model = NULL)
  }

  numer = pedprobr::likelihood2(H1, marker1 = 1, marker2 = 2, rho = rho)
  denom = pedprobr::likelihood2(H2, marker1 = 1, marker2 = 2, rho = rho)
  numer/denom
}
