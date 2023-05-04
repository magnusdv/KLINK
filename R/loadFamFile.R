
loadFamFile = function(path) {
  x = forrel::readFam(path, useDVI = FALSE, verbose = FALSE, prefixAdded = ":missing:")

  if(!length(x) || (!is.ped(x[[1]]) && !is.pedList(x[[1]])))
    stop("No pedigrees found in the Familias file.", call. = FALSE)

  if(all(sapply(x, pedtools::is.singleton)))
    stop("This Familias file contains only singletons", call. = FALSE)

  # Ensure each pedigree is an unnamed list
  x = lapply(x, function(xx) {
    if(pedtools::is.ped(xx))
      list(xx)
    else if(pedtools::is.pedList(xx))
      unname(xx)
    else
      stop("Unexpected content detected in the Familias file.")
  })

  if(length(x) == 1)
    stop("Only one pedigree found in the Familias file.", call. = FALSE)

  if(length(x) > 2) {
    warning("This familias file contains more than two pedigrees; only the first two are used", call. = FALSE)
    x = x[1:2]
  }

  if(is.null(names(x)))
    names(x) = c("Ped 1", "Ped 2")

  x
}


removeEmpty = function(x) {
  if(is.null(x))
    return(NULL)
  for(pedname in names(x)) {
    ped = x[[pedname]]
    empty = vapply(ped, function(comp) !any(unlist(comp$MARKERS)), FALSE)
    if(all(empty))
      stop(sprintf("Pedigree '%s' has no typed members", pedname))
    x[[pedname]] = ped[!empty]
  }
  x
}
