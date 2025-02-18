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

addTotals = function(x, cols) {
  # Add totals row
  y = rbind(x, NA)

  # Loop through columns
  for(cc in cols) {
    v = x[[cc]]
    tot = prod(v, na.rm = TRUE)
    y[[cc]] = c(v, tot)
  }

  y[nrow(y), "Marker"] = "Total LR"
  y
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

# Normalise marker names, for safe matching (below)
normalise = function(x) {
  tolower(gsub("[-._ ]", "", x))
}

# Safe matching of marker names
matchMarkernames = function(m1, m2, ...) {
  match(normalise(m1), normalise(m2), ...)
}
