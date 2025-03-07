#' Load `.fam` file
#'
#' @param path The path to a `.fam` file.
#' @param fallbackModel The name of a mutation model; passed on to
#'   [pedFamilias::readFam()].
#' @param withParams A logical indicating if the Familias parameters should be
#'   included in the output. (See [pedFamilias::readFam()].)
#'
#' @return A list of two `ped` objects.
#'
#' @examples
#' fam = system.file("extdata/halfsib-test.fam", package = "KLINK")
#' peds = loadFamFile(fam)
#' pedtools::plotPedList(peds)
#'
#' @export
loadFamFile = function(path, fallbackModel = "equal", withParams = FALSE) {
  if (getOption("KLINK.debug")) print("loadFamFile")

  x0 = pedFamilias::readFam(path, useDVI = NA, verbose = FALSE,
                            prefixAdded = ":missing:", includeParams = TRUE,
                            fallbackModel = fallbackModel, simplify1 = FALSE)

  x = x0$main
  params = x0$params

  if(isTRUE(params$dvi))
    stop2("This file was exported from the DVI module of Familias. Such files cannot be used in KLINK.")

  theta = params$theta
  if(length(theta) && !is.na(theta) && theta > 0)
    warning("Nonzero theta correction detected: theta = ", theta, call. = FALSE)

  if(!length(x) || (!is.ped(x[[1]]) && !is.pedList(x[[1]])))
    stop2("No pedigrees found in the Familias file.")

  if(all(sapply(x, pedtools::is.singleton)))
    stop2("This Familias file contains only singletons")

  # Ensure each pedigree is an unnamed list
  x = lapply(x, function(xx) {
    if(pedtools::is.ped(xx))
      list(xx)
    else if(pedtools::is.pedList(xx))
      unname(xx)
    else
      stop2("Unexpected content detected in the Familias file.")
  })

  if(length(x) == 1) {
    warning("Only one pedigree found. Adding unrelated hypothesis", call. = FALSE)
    un = singletons(typedMembers(x[[1]]))
    un = transferMarkers(from = x[[1]], to = un)
    x = c(x, list(un))
  }

  if(length(x) > 2) {
    warning("This familias file contains more than two pedigrees; only the first two are used", call. = FALSE)
    x = x[1:2]
  }

  # TODO: Temporarily switch off special lumping
  if(TRUE) { # !specialLumpability(x)) {
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
  else if(any(msnm <- names(x) == ""))
    names(x)[msnm] = paste("Ped", which(msnm))

  if(withParams)
    list(peds = x, params = params)
  else
    x
}


removeEmpty = function(x) {
  if (getOption("KLINK.debug")) print("remove empty comps and/or markers")
  if(is.null(x))
    return(NULL)

  for(pedname in names(x)) {
    ped = x[[pedname]]
    empty = vapply(ped, function(comp) !any(unlist(comp$MARKERS)), FALSE)
    if(all(empty))
      warning(sprintf("Pedigree '%s' has no typed members", pedname))
    else
      x[[pedname]] = ped[!empty]
  }
  x
}
