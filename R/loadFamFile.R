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

  x1 = x[[1]]

  nMark = nMarkers(x1)
  if(nMark == 0)
    stop2("No markers are included in the Familias file.")

  typed = typedMembers(x1)
  if(length(typed) == 0)
    warning("No typed individuals found in the Familias file.", call. = FALSE)

  if(length(x) == 1) {
    warning("Only one pedigree found. Adding unrelated hypothesis", call. = FALSE)

    if(length(typed) == 0)
      stop2("The second pedigree is empty.")

    un = singletons(typed)
    un = transferMarkers(from = x1, to = un)
    x = c(x, list(un))
  }

  if(length(x) > 2) {
    warning("This familias file contains more than two pedigrees; only the first two are used",
            call. = FALSE)
    x = x[1:2]
  }

  if(is.null(names(x)))
    names(x) = c("Ped 1", "Ped 2")
  else if(any(msnm <- names(x) == ""))
    names(x)[msnm] = paste("Ped", which(msnm))

  # Check Lumpability
  lumpable = checkLumpability(x[[1]]) & checkLumpability(x[[2]])
  if(!all(lumpable)) {
    msg = sprintf("Pedigree prohibits lumping for %d markers; changed models '%s'",
                  sum(!lumpable), fallbackModel)
    warning(msg, call. = FALSE)
    x = lapply(x, function(ped)
      setMutmod(ped, which(!lumpable), model = fallbackModel, update = TRUE))
  }

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

checkLumpability = function(x) {

  nMark = nMarkers(x)

  # Regular lumpability
  alwLumpable = vapply(seq_len(nMark), function(i)
    pedmut::alwaysLumpable(mutmod(x, i)), FUN.VALUE = logical(1))

  if(all(alwLumpable))
    return(alwLumpable)

  # For special lumping, check U-signature
  untyped = untypedMembers(x)
  usign = pedprobr:::uSignature(x, untyped = untyped)
  if(usign[1] > 1 || (usign[3] + usign[4]) > 0)
    return(rep(FALSE, nMark))

  # If all markers have the same untyped, ok!
  res = rep(TRUE, nMark)
  gg = getGenotypes(x)
  if(all(colSums(gg == "-/-") == length(untyped)))
    return(res)

  # Otherwise, check each marker
  for(i in seq_len(nMark)) {
    unt = names(which(gg[, i] == "-/-"))
    us = if(setequal(unt, untyped)) usign else pedprobr:::uSignature(x, untyped = unt)
    if(us[1] > 1 || (us[3] + us[4]) > 0)
      res[i] = FALSE
    else if(us[2] > 3 && nAlleles(x, i) > 20)
      res[i] = FALSE
  }
  res
}
