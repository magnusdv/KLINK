% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addTotals.R
\name{addTotals}
\alias{addTotals}
\title{Add totals to a data frame with LR results}
\usage{
addTotals(x, cols = c("LRlinked", "LRnolink", "LRnomut"))
}
\arguments{
\item{x}{A data frame with LR results.}

\item{cols}{A vector of column names, by default \code{c("LRlinked", "LRnolink", "LRnomut")}}
}
\value{
The same data frame, but with an added row of total LRs.
}
\description{
Adds a row with totals for the specified columns, formed by multiplying the
values in each column.
}
\details{
Note that for the multiplication to respect linked markers, the input table
should include LR results for only one marker in each linkage group. This is
the the case e.g. if the input was produced by \code{\link[=linkedLR]{linkedLR()}}.
}
\examples{
res = linkedLR(paternity, KLINK::LINKAGEMAP) |> addTotals()

# No effect of linkage in this case:
stopifnot(all.equal(res[nrow(res), "LRlinked"],
                    res[nrow(res), "LRnolink"]))
}
