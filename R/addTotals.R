#' Get LR totals
#'
#' Find, or add, the LR totals in a data frame with marker-wise LR values. The
#' totals are found by multiplying the values in each column, after removing
#' `NA`'s.
#'
#' Note that for the multiplication to respect linked markers, the input table
#' should include LR values for only one marker in each linkage group, and
#' `NA`'s elsewhere. This is the format used by [linkedLR()].
#'
#' @param x A data frame with LR results, typically the output of [linkedLR()].
#' @param cols A vector of column names, by default `c("LRlinked", "LRnolink",
#'   "LRnomut")`
#'
#' @returns `getTotals()` returns a named numeric. `addTotals` returns a data
#'   frame equal to the input, but with a row of totals added at the bottom.
#'
#' @examples
#' res = linkedLR(paternity)
#' getTotals(res)
#' addTotals(res)
#'
#' @export
getTotals = function(x, cols = c("LRlinked", "LRnolink", "LRnomut")) {
  cols = intersect(cols, names(x))
  if(length(cols) == 0)
    stop2("Columns not found")

  unlist(lapply(x[cols], function(v) prod(v, na.rm = TRUE)))
}

#' @rdname getTotals
#' @export
addTotals = function(x, cols = c("LRlinked", "LRnolink", "LRnomut")) {
  tots = getTotals(x, cols)

  # Add totals row
  y = rbind(x, NA)
  y[nrow(y), names(tots)] = tots
  y[nrow(y), "Marker"] = "Total LR"
  y
}
