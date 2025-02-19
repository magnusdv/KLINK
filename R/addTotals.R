#' Add totals to a data frame with LR results
#'
#' Adds a row with totals for the specified columns, formed by multiplying the
#' values in each column.
#'
#' Note that for the multiplication to respect linked markers, the input table
#' should include LR results for only one marker in each linkage group. This is
#' the the case e.g. if the input was produced by [linkedLR()].
#'
#' @param x A data frame with LR results.
#' @param cols A vector of column names, by default `c("LRlinked", "LRnolink",
#'   "LRnomut")`
#'
#' @returns The same data frame, but with an added row of total LRs.
#' @examples
#' res = linkedLR(paternity, KLINK::LINKAGEMAP) |> addTotals()
#'
#' # No effect of linkage in this case:
#' stopifnot(all.equal(res[nrow(res), "LRlinked"],
#'                     res[nrow(res), "LRnolink"]))
#' @export
addTotals = function(x, cols = c("LRlinked", "LRnolink", "LRnomut")) {
  cols = intersect(cols, names(x))

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
