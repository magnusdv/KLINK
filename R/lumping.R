lumpAllSpecial = function(x, verbose = FALSE, check = TRUE) {
  if(pedtools::is.pedList(x))
    return(lapply(x, function(comp) lumpAllSpecial(comp, verbose = verbose)))

  x$MARKERS = lapply(x$MARKERS, function(m) reduceAllelesSpecial(m, verbose = verbose, ped = if(check) x))
  x
}

# A modified version of pedprobr::reduceAlleles()
reduceAllelesSpecial = function(m, verbose = FALSE, ped = NULL) {

  if (all(m != 0)) {
    if(verbose) message("Lumping not needed - all members genotyped")
    return(m)
  }
  attrs = attributes(m)
  mut = attrs$mutmod

  # If stationary mutation model, return unchanged (special lumping unneeded)
  if(is.null(mut))# || pedmut::isStationary(mut))
    return(m)

  # If pedigree is included: Check if special lumping applies
  if(!is.null(ped)) {
    untyped = m[,1] == 0 & m[,2] == 0
    if(!all(ped$FID[untyped] == 0)) { # Are all untyped founders?
      if(verbose) message("Special lumping does not apply - returning marker unchanged")
      return(m)
    }
  }

  origAlleles = attrs$alleles

  # Index of observed alleles
  presentIdx = unique.default(m[m > 0])

  # No lumping if all, or all but one, are observed
  if(length(presentIdx) >= length(origAlleles) - 1) {
    if(verbose) message(sprintf("Lumping not needed: %d of %d alleles observed",
                                length(presentIdx), length(origAlleles)))
    return(m)
  }

  presentIdx = sort.int(presentIdx, method = "shell")
  lump = if(!length(presentIdx)) origAlleles else origAlleles[-presentIdx]

  m[] = match(m, presentIdx, nomatch = 0)
  attr(m, "alleles") = c(origAlleles[presentIdx], "lump")

  presentFreq = attrs$afreq[presentIdx]
  attr(m, "afreq") = c(presentFreq, 1 - sum(presentFreq))

  pedtools::mutmod(m) = lumpSpecial(mut, lump = lump)

  if(verbose)
    message(sprintf("Special lumping with untyped founders: %d -> %d alleles",
                    length(origAlleles), length(presentIdx) + 1))

  m
}

lumpSpecial = function(mut, lump, method = "foundersUntyped") {
  if(inherits(mut, "mutationModel")) {
    newmut = lapply(mut, function(m) lumpSpecial(m, lump = lump, method = method))
    return(pedmut::mutationModel(newmut))
  }

  als = colnames(mut)
  afr = attr(mut, "afreq")
  keep = setdiff(als, lump)
  wei = if(method == "foundersUntyped") afr[lump] else stop("Method not implemented: ", method)
  weiScaled = wei/sum(wei)

  m2 = mut[keep, keep, drop = FALSE]
  m2 = rbind(m2, lump = as.numeric(weiScaled %*% mut[lump, keep, drop = FALSE]))
  m2 = cbind(m2, lump = 1 - rowSums(m2))

  newfr = c(afr[keep], lump = sum(afr[lump]))
  pedmut::mutationMatrix("custom", matrix = m2, afreq = newfr)
}


# Check if special (founder-type) lumping applies to all markers
specialLumpability = function(x) {
  ped1 = x[[1]]
  ped2 = x[[2]]
  all(untypedMembers(ped1) %in% founders(ped1)) &&
    all(untypedMembers(ped2) %in% founders(ped2))
}
