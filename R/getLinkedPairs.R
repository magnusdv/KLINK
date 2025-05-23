#' Identify linked marker pairs
#'
#' This function returns a "maximal" set of disjoint pairs of linked markers,
#' given a genetic marker map and a subset of the markers included in the map.
#' The pairs are identified in a greedy manner, successively choosing the
#' closest markers on each chromosome.
#'
#' @param markers A character vector containing marker names.
#' @param linkageMap A data frame with columns including `Marker`, `Chr` and
#'   `cM`.
#' @param maxdist A positive number indicating the maximum linkage distance (in
#'   cM). Markers further apart than this are considered unlinked.
#'
#' @return A list of character vectors, each containing two marker names.
#'
#' @examples
#' # Example using the default map of 50 STR markers
#' map = norSTR::map50
#'
#' getLinkedPairs(map$Marker, map, maxdist = 25)
#' @export
getLinkedPairs = function(markers, linkageMap = map50, maxdist = Inf) {
  if(is.null(markers))
    markers = linkageMap$Marker

  res = list()
  if(!length(markers) || is.null(linkageMap))
    return(res)

  idx = matchMarkernames(linkageMap$Marker, markers, nomatch = 0)
  x = linkageMap[idx > 0, , drop = FALSE]

  for(i in unique.default(x$Chr)) {
    xi = x[x$Chr == i, , drop = FALSE]
    idxList = closestPairs(xi$cM, maxdist = maxdist)
    idxList = idxList[order(sapply(idxList, sum))] # sort in order
    resi = lapply(idxList, function(idx) xi$Marker[idx])
    res = c(res, resi)
  }

  res
}


# Iteratively pick out closest pairs in a *sorted!* numeric vector
# Returns list of index pairs
closestPairs = function(v, maxdist = Inf) {
  dfs = diff.default(v) |> unname()
  if(any(dfs < 0))
    stop2("Input not sorted!")

  res = list()

  while(T) {
    b = which.min(dfs)
    if(!length(b) || dfs[b] > maxdist)
      break

    res = c(res, list(b + 0:1))

    # Hide involved positions from next iteration
    dfs[c(b-1, b, b+1)] = NA # OK outside range!
  }

  res
}
