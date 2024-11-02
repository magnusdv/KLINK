#' LR with pairwise linked markers
#'
#' This function does the main LR calculations of the KLINK app.
#'
#' @param pedigrees A list of two pedigrees.
#' @param linkageMap A data frame with columns including `Marker`, `Chrom` and
#'   `PosCM`.
#' @param linkedPairs A list of marker pairs. If not supplied, calculated as
#'   `getLinkedPairs(markerData$Marker, linkageMap, maxdist = maxdist)`.
#' @param maxdist A number, passed onto `getLinkedMarkers()` if `linkedPairs` is
#'   NULL.
#' @param markerData A data frame with marker data, typically the output of
#'   `markerSummary(pedigrees)`.
#' @param mapfun Name of the map function to be used; either "Haldane" or
#'   "Kosambi" (default).
#' @param lumpSpecial A logical, by default FALSE.
#' @param verbose A logical, by default FALSE.
#'
#' @return A data frame with detailed LR results.
#'
#' @examples
#' linkedLR(paternity, KLINK::LINKAGEMAP)
#'
#' # For testing
#' # .linkedLR(paternity, markerpair = c("SE33", "D6S474"), linkageMap = LINKAGEMAP)
#'
#' @export
linkedLR = function(pedigrees, linkageMap, linkedPairs = NULL, maxdist = Inf,
                    markerData = NULL, mapfun = "Kosambi", lumpSpecial = FALSE, verbose = FALSE) {
  if (getOption("KLINK.debug")) {
    print("linkedLR")
    verbose = TRUE
  }

  st = Sys.time()

  if(is.null(markerData)) {
    markerData = markerSummary(pedigrees)
    ord = order(match(markerData$Marker, linkageMap$Marker))
    markerData = markerData[ord, , drop = FALSE]
  }

  # Genotype columns
  gcols = colsBetween(markerData, "MinFreq", "Model")

  # Find linked pairs, if not supplied
  if(is.null(linkedPairs))
    linkedPairs = getLinkedPairs(markerData$Marker, linkageMap, maxdist = maxdist)

  markerData$Pair = lp2vec(markerData$Marker, linkedPairs)

  MAPFUN = switch(mapfun, Haldane = pedprobr::haldane, Kosambi = pedprobr::kosambi)

  # Initialise table: Pair, Marker, Geno
  res = markerData[c("Pair", "Marker", gcols)]
  nr = nrow(res)

  # Add cM positions
  res$PosCM = linkageMap$PosCM[match(res$Marker, linkageMap$Marker)]

  # Replace missing pairs with dummy 1001, 1002, ... (otherwise lost in split)
  if(any(NApair <- is.na(res$Pair)))
    res$Pair[NApair] = 1000 + seq_along(which(NApair))

  # Group size (1 or 2)
  res$Gsize = stats::ave(1:nr, res$Pair, FUN = function(a) rep(length(a), length(a)))

  # Put (intact) pairs on top
  #res = res[order(-res$Gsize, res$Pair, res$PosCM), , drop = FALSE]

  # Index within each group (do after ordering!)
  res$Gindex = stats::ave(1:nr, res$Pair, FUN = seq_along)

  # Special lumping # TODO!
  if(lumpSpecial)
    warning("Special lumping may give a small bias\n", call. = FALSE)
  if(lumpSpecial && specialLumpability(pedigrees))
    pedigrees = lapply(pedigrees, lumpAllSpecial, verbose = verbose)

  # Single-point LR
  if(verbose)
    cat("Computing single-point LRs\n")
  lr1 = forrel::kinshipLR(pedigrees, markers = res$Marker)
  res$LRsingle = lr1$LRperMarker[,1]

  # No-mutation versions
  if(verbose)
    cat("Computing no-mutation LRs\n")
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

  if(verbose)
    cat("Looping through linkage pairs:\n")

  for(pp in pairs) {
    m = pp$Marker
    idx1 = match(m[1], res$Marker)

    if(nrow(pp) == 2) {
      ped1 = pedigrees[[1]]
      if(verbose)
        cat(sprintf("* %s (%d) - %s (%d)\n", m[1], nAlleles(ped1, m[1]), m[2], nAlleles(ped1, m[2])))
      res$LRnolink[idx1] = prod(pp$LRsingle)
      res$LRlinked[idx1] = .linkedLR(pedigrees, m, cmpos = pp$PosCM, mapfun = MAPFUN)$LR
      res$LRnomut[idx1]  = .linkedLR(pedsNomut, m, cmpos = pp$PosCM, mapfun = MAPFUN)$LR
    }
    else {
      res$LRnolink[idx1] = res$LRlinked[idx1] = pp$LRsingle
      res$LRnomut[idx1] = LRnomut[[m]]
    }
  }

  # Repair "Pair" column
  res$Pair = ifelse(res$Gsize > 1, paste("Pair", res$Pair), "Unpaired")

  if(verbose)
    cat("Time elapsed: ", format(Sys.time() - st, digits = 3), "\n")

  res
}


# Normally not run by end user
.linkedLR = function(peds, markerpair, cmpos = NULL, mapfun = "Kosambi",
                     linkageMap = NULL, disableMut = FALSE) {
  if (getOption("KLINK.debug")) print(paste(".linkedLR:", paste(markerpair, collapse = ", ")))

  if(length(markerpair) < 2)
    return(NA_real_)

  # For testing purposes
  if(is.null(cmpos))
    cmpos = linkageMap$PosCM[match(markerpair, linkageMap$Marker)]
  if(is.character(mapfun))
    mapfun = switch(mapfun, Haldane = pedprobr::haldane, Kosambi = pedprobr::kosambi,
                    stop2("Illegal map function: ", mapfun))

  rho = mapfun(diff(cmpos))

  H1 = pedtools::selectMarkers(peds[[1]], markerpair)
  H2 = pedtools::selectMarkers(peds[[2]], markerpair)

  # Not used, but useful for debugging
  if(disableMut) {
    H1 = H1 |> setMutmod(model = NULL)
    H2 = H2 |> setMutmod(model = NULL)
  }

  numer = pedprobr::likelihood2(H1, marker1 = 1, marker2 = 2, rho = rho)
  denom = pedprobr::likelihood2(H2, marker1 = 1, marker2 = 2, rho = rho)
  LR = numer/denom

  list(lnLik1 = log(numer), lnLik2 = log(denom), LR = LR)
}
