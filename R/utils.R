stop2 = function(...) {
  a = lapply(list(...), toString)
  a = append(a, list(call. = FALSE))
  do.call(stop, a)
}

safelog = function(v) {
  res = rep(NA_real_, length(v))
  pos = !is.na(v) & v > 0
  res[pos] = log(v[pos])
  res
}

`%||%` = function(x, y) {
  if(is.null(x)) y else x
}

setnames = function(x, nms = x) {
  names(x) = nms
  x
}

fixNA = function(x) {
  x[x == "NA"] = NA_character_
  x
}

# Extract column names between two columns
colsBetween = function(x, a, b) {
  nms = names(x)
  aidx = match(a, nms)
  bidx = match(b, nms)
  n = bidx - aidx - 1
  if(is.na(n) || n < 0)
    stop2("Columns out of order")
  nms[aidx + seq_len(n)]
}

# Capitilize first letter of each element
capit = function(x) {
  notna = !is.na(x)
  if(!length(x) || !any(notna))
    return(x)
  substr(x[notna], 1, 1) = toupper(substr(x[notna], 1, 1))
  x
}

# Marker names spelling issues-------------------------------------------------

# Normalise marker names, for safe matching (below)
normalise = function(x) {
  tolower(gsub("[-._ ]", "", x))
}

# Safe matching of marker names
matchMarkernames = function(m1, m2, ...) {
  match(normalise(m1), normalise(m2), ...)
}

# Return a version of m1 using the names of m2
fixMarkernames = function(m1, m2) {
  idx = matchMarkernames(m1, m2, nomatch = 0L)
  m1[idx > 0] = m2[idx]
  m1
}
