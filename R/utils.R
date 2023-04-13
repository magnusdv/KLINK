
foldLabs = function(x, width = 10) {
  labs = unlist(labels(x), use.names = FALSE)
  if(all(nchar(labs) < 12))
    return(x)
  newlabs = vapply(labs, function(s) fold(s, width), FUN.VALUE = character(1))
  pedtools::relabel(x, old = labs, new = newlabs)
}

fold = function(s, width) {
  if(nchar(s) <= width + 1)
    return(s)
  pattern = sprintf("(.{1,%d})", width)
  trimws(gsub(pattern, '\\1\n', s), "right")
}

safelog = function(v) {
  res = rep(NA_real_, length(v))
  res[v > 0] = log(v[v > 0])
  res
}
