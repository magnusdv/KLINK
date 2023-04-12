
loadFamFile = function(path) {
  x = forrel::readFam(path, useDVI = FALSE, verbose = FALSE, prefixAdded = ":missing:")

  if(length(x) != 2)
    stop("Familias file should contain 2 pedigrees; this file has ", length(x))

  # Ensure each pedigree is an unnamed list
  x = lapply(x, function(xx) {
    if(is.ped(xx))
      list(xx)
    else if(is.pedList(xx))
      unname(xx)
    else
      stop("No pedigrees detected in Familias file.")
  })

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
