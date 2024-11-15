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
  from = match(a, nms) + 1
  to = match(b, nms) - 1
  if(is.na(from) || is.na(to))
    stop2("Column not found")
  if(from > to)
    stop2("Columns out of order")
  nms[from:to]
}

mylink = function(text, href, .noWS = "outside", ...) {
  if(missing(href))
    href = text
  shiny::a(text, href = href, .noWS = .noWS, target = "_blank", ...)
}
