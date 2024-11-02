stop2 = function(...) {
  a = lapply(list(...), toString)
  a = append(a, list(call. = FALSE))
  do.call(stop, a)
}

safelog = function(v) {
  res = rep(NA_real_, length(v))
  res[v > 0] = log(v[v > 0])
  res
}

`%||%` = function(x, y) {
  if(is.null(x)) y else x
}

setnames = function(x, nms) {
  names(x) = nms
  x
}

# Extract column names between two columns
colsBetween = function(x, a, b) {
  nms = names(x)
  from = match(a, nms) + 1
  to = match(b, nms) - 1
  if(is.na(from) || is.na(to))
    stop2("Column not found")
  if(from > to)
    stop2("Columns out of order")
  nms[from:to]
}
