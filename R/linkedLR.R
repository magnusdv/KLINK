#' LR with pairwise linked markers
#'
#' This function does the main LR calculations of the KLINK app.
#'
#' @param pedigrees A list of two pedigrees.
#' @param linkageMap A data frame with columns including `Marker`, `Chr` and
#'   `cM`. By default, a built-in map `norSTR::map50` of 50 STR markers is used.
#' @param linkedPairs A list of marker pairs. If not supplied, calculated as
#'   `getLinkedPairs(markerData$Marker, linkageMap, maxdist = maxdist)`.
#' @param maxdist A number, passed onto `getLinkedMarkers()` if `linkedPairs` is
#'   NULL.
#' @param markerData A data frame with marker data, typically the output of
#'   `markerSummary(pedigrees)`.
#' @param mapfun Name of the map function to be used; either "Haldane" or
#'   "Kosambi" (default).
#' @param lumpSpecial A logical indicating if special lumping should be
#'   activated. This is strongly recommended in all cases with linked STR
#'   markers.
#' @param alleleLimit A number, by default 12, passed on to
#'   [pedprobr::likelihood2()].
#' @param verbose A logical, by default TRUE.
#' @param debug A logical, by default FALSE.
#'
#' @return A data frame with detailed LR results.
#'
#' @examples
#' linkedLR(paternity)
#'
#' # Detailed messages, including reports on lumping
#' linkedLR(paternity, debug = TRUE)
#'
#' # For testing
#' # .linkedLR(paternity, markerpair = c("SE33", "D6S474"))
#'
#' @export
linkedLR = function(pedigrees, linkageMap = map50, linkedPairs = NULL, maxdist = Inf,
                    markerData = NULL, mapfun = "Kosambi", lumpSpecial = TRUE,
                    alleleLimit = 12, verbose = TRUE, debug = FALSE) {

  if (getOption("KLINK.debug")) {
    print("linkedLR")
    verbose = debug = TRUE
  }

  st = Sys.time()

  MAPFUN = switch(tolower(mapfun), haldane = pedprobr::haldane, kosambi = pedprobr::kosambi)

  if(is.null(markerData)) {
    markerData = markerSummary(pedigrees)
    ord = order(matchMarkernames(markerData$Marker, linkageMap$Marker))
    markerData = markerData[ord, , drop = FALSE]
  }

  # Genotype columns
  gcols = colsBetween(markerData, "Marker", "Typed")

  if(!length(gcols))
    stop2("No genotyped individuals")

  mvec = markerData$Marker

  # Ensure map uses same marker spelling
  linkageMap$Marker = fixMarkernames(linkageMap$Marker, mvec)
  if(!is.null(linkedPairs))
    linkedPairs = lapply(linkedPairs, fixMarkernames, mvec)
  else
    linkedPairs = getLinkedPairs(mvec, linkageMap, maxdist = maxdist)

  # Remove pairings involving markers with less than 2 typed
  good = (markerData$Typed >= 2) |> setnames(mvec)
  linkedPairs = Filter(\(x) all(good[x]), linkedPairs)

  if(verbose) {
    cat("Map function:", mapfun, "\n")
    cat("Max distance considered:", maxdist, "cM\n")
    cat("Effective linkage pairs:", length(linkedPairs), "\n")
    cat("Special lumping:", lumpSpecial, "\n")
    cat("Allele limit:", alleleLimit, "\n")
  }

  # Pairing index
  pair = lp2vec(mvec, linkedPairs)
  cmpos = linkageMap$cM[match(mvec, linkageMap$Marker)] |> setnames(mvec)

  # Initialise result table
  res = cbind.data.frame(Pair = pair, markerData[c("Marker", gcols, "Typed")])
  nr = nrow(res)

  # Fix NAs in pair
  pair[is.na(pair)] = 1000L + seq_along(which(is.na(pair)))

  # Group size (1 or 2)
  res$Gsize = stats::ave(1:nr, pair, FUN = function(a) rep(length(a), length(a)))

  # Index within each group
  res$Gindex = stats::ave(1:nr, pair, FUN = seq_along)

  # Single-point LR
  if(verbose)
    cat("Computing single-point LRs...")
  likArgs = list(special = lumpSpecial, verbose = debug)
  lr1 = forrel::kinshipLR(pedigrees, markers = mvec, likArgs = likArgs)
  if(verbose)
    cat("done\n")

  LRsingle = lr1$LRperMarker[,1]
  liks = lr1$likelihoodsPerMarker

  # No-mutation versions
  if(verbose)
    cat("Computing no-mutation single-point LRs...")
  pedsNomut = lapply(pedigrees, function(x) setMutmod(x, model = NULL)) # TODO: |> lumpAlleles()
  LRnomut = forrel::kinshipLR(pedsNomut, markers = mvec, verbose = FALSE)$LRperMarker[, 1]
  if(verbose)
    cat("done\n")

  # Fix lost names when only 1 marker
  if(is.null(names(LRsingle)))
    names(LRsingle) = mvec
  if(is.null(names(LRnomut)))
    names(LRnomut) = mvec

  res$LRsingle = LRsingle
  res$LRlinked = NA_real_
  res$LRnolink = NA_real_
  res$LRnomut  = NA_real_
  res$Lik1 = NA_real_
  res$Lik2 = NA_real_

  ped1 = pedigrees[[1]]

  if(verbose)
    cat("\n===== PAIRWISE LINKAGE  =====\n\n")

  # Loop over linkage groups and fill in results
  for(lg in split(mvec, pair)) {

    if(verbose)
      cat(sprintf("* %s (# alleles = %s)\n", toString(lg), toString(nAlleles(ped1, lg))))

    idx1 = match(lg[1], res$Marker)

    if(length(lg) == 2) {
      res$LRnolink[idx1] = prod(LRsingle[lg])
      res$LRnomut[idx1]  = .linkedLR(pedsNomut, lg, cmpos = cmpos[lg],
                                     mapfun = MAPFUN, verbose = debug)$LR

      linkLR = .linkedLR(pedigrees, lg, cmpos = cmpos[lg], mapfun = MAPFUN,
                         lumpSpecial = lumpSpecial, alleleLimit = alleleLimit,
                         verbose = debug)

      res$LRlinked[idx1] = linkLR$LR
      res$Lik1[idx1] = linkLR$lik1
      res$Lik2[idx1] = linkLR$lik2

    }
    else {
      res$LRnolink[idx1] = res$LRlinked[idx1] = LRsingle[[lg]]
      res$LRnomut[idx1] = LRnomut[[lg]]
      res$Lik1[idx1] = liks[lg, 1]
      res$Lik2[idx1] = liks[lg, 2]
    }
  }

  if(verbose)
    cat("Time elapsed: ", format(Sys.time() - st, digits = 3), "\n")

  res$Loglik1 = safelog(res$Lik1)
  res$Loglik2 = safelog(res$Lik2)
  res
}


# Normally not run by end user
.linkedLR = function(peds, markerpair, cmpos = NULL, mapfun = "Kosambi",
                     linkageMap = map50, lumpSpecial = TRUE, alleleLimit = 12,
                     disableMut = FALSE, verbose = FALSE) {

  if(getOption("KLINK.debug"))
    print(paste(".linkedLR:", paste(markerpair, collapse = ", ")))

  if(length(markerpair) < 2)
    return(NA_real_)

  # For testing purposes
  if(is.null(cmpos))
    cmpos = linkageMap$cM[match(markerpair, linkageMap$Marker)]
  if(is.character(mapfun))
    mapfun = switch(tolower(mapfun), haldane = pedprobr::haldane, kosambi = pedprobr::kosambi,
                    stop2("Illegal map function: ", mapfun))

  rho = mapfun(diff(cmpos))

  H1 = pedtools::selectMarkers(peds[[1]], markerpair)
  H2 = pedtools::selectMarkers(peds[[2]], markerpair)

  # Not used in app, but useful for debugging
  if(disableMut) {
    H1 = H1 |> setMutmod(model = NULL)
    H2 = H2 |> setMutmod(model = NULL)
  }

  if(verbose) cat("Pedigree H1\n")
  numer = pedprobr::likelihood2(H1, marker1 = 1, marker2 = 2, rho = rho, special = lumpSpecial,
                                alleleLimit = alleleLimit, verbose = verbose)

  if(verbose) cat("Pedigree H2\n")
  denom = pedprobr::likelihood2(H2, marker1 = 1, marker2 = 2, rho = rho, special = lumpSpecial,
                                alleleLimit = alleleLimit, verbose = verbose)
  list(lik1 = numer, lik2 = denom, LR = numer/denom)
}
