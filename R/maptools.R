#' Genetic map utilities
#'
#' Functions for loading, subsetting and pairing markers in genetic linkage maps.
#'
#' `loadMap()` reads and sorts a genetic map from a text or Excel file, with required
#' columns `Marker`, `Chr`, `cM`, and optionally a column `Mb` with physical positions
#' used in karyograms.
#'
#' `subsetMap()` restricts a map to a given set of markers. Marker names are matched
#' flexibly, ignoring case and the separators "-", ".", "_" and space. The returned map
#' uses the marker spelling supplied in `markers`. If multiple map entries match the same
#' marker, only the first is retained.
#'
#' `getLinkedPairs()` identifies disjoint marker pairs from the supplied map. Within each
#' chromosome, pairs are selected greedily by successively choosing the closest available
#' markers. Each marker occurs in at most one pair.
#'
#' @param linkageMap A data frame with columns `Marker`, `Chr`, `cM` and (optionally)
#'   `Mb`, sorted by chromosome and position.
#' @param markers A character vector of marker names to retain.
#' @param maxdist A positive number indicating the maximum distance (in cM) between paired
#'   markers. Markers further apart are considered unlinked.
#' @param path Path to a genetic map file, either a tab-separated text file or an Excel
#'   file with extension `.xlsx`.
#'
#' @returns `loadMap()` and `subsetMap()` return data frames. `getLinkedPairs()` returns a
#'   list of character vectors, each containing two marker names.
#'
#' @examples
#' map = norSTR::map51
#'
#' # Keep only markers present in a pedigree
#' mvec = pedtools::name(sibship[[1]])
#' map2 = subsetMap(map, mvec)
#'
#' # Subsetting is insensitive to case and separators
#' mvec2 = sub(" ", ".", mvec) |> tolower()
#' subsetMap(map, mvec2)
#'
#' # Pair the remaining markers
#' getLinkedPairs(map2, maxdist = 50)
#'
#' @name maptools
NULL

#' @rdname maptools
#' @export
getLinkedPairs = function(linkageMap = map51, maxdist = Inf) {
  res = list()
  if(is.null(linkageMap) || !nrow(linkageMap))
    return(res)

  linkageMap = linkageMap[!is.na(linkageMap$Chr) & !is.na(linkageMap$cM), , drop = FALSE]

  for(i in unique.default(linkageMap$Chr)) {
    xi = linkageMap[linkageMap$Chr == i, , drop = FALSE]
    idxList = closestPairs(xi$cM, maxdist = maxdist)
    idxList = idxList[order(vapply(idxList, sum, 0))]
    res = c(res, lapply(idxList, \(idx) xi$Marker[idx]))
  }

  res
}

#' @rdname maptools
#' @export
subsetMap = function(linkageMap, markers) {
  idx = matchMarkernames(linkageMap$Marker, markers, nomatch = 0)
  keep = idx > 0 & !duplicated.default(idx)

  res = linkageMap[keep, , drop = FALSE]
  res$Marker = markers[idx[keep]]
  res
}


# Iteratively pick out closest pairs in a *sorted!* numeric vector
# Returns list of index pairs
closestPairs = function(v, maxdist = Inf) {
  dfs = diff.default(v) |> unname()
  if(any(dfs < 0))
    stop2("Input not sorted!")

  res = list()

  repeat {
    b = which.min(dfs)
    if(!length(b) || dfs[b] > maxdist)
      break

    res = c(res, list(b + 0:1))

    # Hide involved positions from next iteration
    dfs[c(b-1, b, b+1)] = NA # OK outside range!
  }

  res
}
