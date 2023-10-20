
#' Load .fam file
#'
#' @param path The path to a .fam file.
#' @param fallbackModel The name of a mutation model; passed on to
#'   [forrel::readFam()].
#'
#' @return A list of two `ped` objects.
#'
#' @examples
#' fam = system.file("extdata/halfsib-test.fam", package = "KLINK")
#' peds = loadFamFile(fam)
#' pedtools::plotPedList(peds)
#'
#' @export
loadFamFile = function(path, fallbackModel = "equal") {
  x0 = forrel::readFam(path, useDVI = FALSE, verbose = FALSE, prefixAdded = ":missing:",
                      fallbackModel = fallbackModel, includeParams = TRUE, simplify1 = FALSE)

  x = x0$main
  params = x0$params

  theta = params$theta
  if(length(theta) && !is.na(theta) && theta > 0)
    warning("Nonzero theta correction detected: theta = ", theta, call. = FALSE)

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

  if(!specialLumpability(x)) {
    alwLumpable = vapply(1:nMarkers(x[[1]]), FUN.VALUE = FALSE, function(i)
      is.null(mut <- mutmod(x[[1]], i)) || pedmut::alwaysLumpable(mut))
    if(!all(alwLumpable)) {
      x = lapply(x, function(ped)
        setMutmod(ped, markers = !alwLumpable, model = fallbackModel, update = TRUE))
      msg = sprintf("Pedigree prohibits lumping of complex mutation models; changed these to '%s'",
                    fallbackModel)
      warning(msg, call. = FALSE)
    }
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
