
writeMasked = function(file, peds, params, map = NULL) {

  allids = unique.default(unlist(lapply(peds, labels),
                  recursive = TRUE, use.names = FALSE))
  ids = setnames(seq_along(allids), allids)

  # Mask first ped
  mask1 = maskPed(peds[[1]], ids = ids)
  keys = mask1$keys

  # Same masking (marker order, allele labels) for second ped
  mnames = keys$markerNames
  mnames = mnames[order(as.numeric(sub("M", "", mnames)))] # M1, M2, ...

  mask2 = maskPed(peds[[2]], ids = ids, markerNames = mnames,
                    alleleLabels = keys$alleleLabels)

  # Mask additional .fam parameters
  mparams = maskParams(params, keys)

  # Write to fam
  pedFamilias::writeFam(mask1$maskedPed, mask2$maskedPed, famfile = file,
                        params = mparams, verbose = FALSE)

  keys
}

maskParams = function(params, keys) {
  p = params

  if(!is.null(p$dropoutConsider))
    names(p$dropoutConsider) = keys$ids[names(p$dropoutConsider)]

  p$dbName = "<masked>"

  if(!is.null(p$dbSize))
    names(p$dbSize) = keys$markerNames[names(p$dbSize)]
  if(!is.null(p$dropoutValue))
    names(p$dropoutValue) = keys$markerNames[names(p$dropoutValue)]
  if(!is.null(p$maf))
    names(p$maf) = keys$markerNames[names(p$maf)]

  p
}


maskMap = function(map, keys) {
  oldnames = names(keys$markerNames)
  mp = map[matchMarkernames(oldnames, map$Marker, nomatch = 0L), , drop = FALSE]
  mp$Marker = keys$markerNames[mp$Marker]

  # Chromosomes -> 1,2, ...
  mp$Chr = mp$Chr |> factor(levels = unique.default(mp$Chr)) |> as.integer()
  mp = mp[order(mp$Chr, mp$cM), , drop = FALSE]

  # Move markers to beginning of chrom
  minpos = tapply(mp$cM, mp$Chr, min, simplify = TRUE)
  mp$cM = mp$cM - minpos[as.character(mp$Chr)]

  # Relabel pairs
  mp$Pair = mp$Pair |> factor(levels = unique.default(mp$Pair)) |> as.integer()

  mp$Kit = "<masked>"
  mp
}

writeKeys = function(keys, file) {
  ids = keys$ids
  mnames = keys$markerNames
  als = keys$alleleLabels

  orig = names(ids)
  new = as.character(ids)

  for(m in names(mnames)) {
    orig = c(orig, "----", m, names(als[[m]]))
    new = c(new, "", as.character(mnames[m]), as.character(als[[m]]))
  }

  mat = rbind(Original = orig, New = new) # transposed!
  write(mat, file = file, ncolumns = 2, sep = "\t")
}
