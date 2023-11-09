#' Built-in linkage map
#'
#' A genetic map including 9 pairs of linked STR markers.
#'
#' @format A data frame with 18 rows and 5 columns.
#'
"LINKAGEMAP"


#' Dataset for a paternity case
#'
#' A list of two pedigrees forming the hypotheses in a paternity case: H1 (`AF` is
#' the father of `CH`) and H2 (unrelated). `AF` and `CH` are genotyped with 11
#' markers, with allele frequencies from `forrel::NorwegianFrequencies`.
#'
#' @format A list of two pedigrees, named H1 and H2.
#'
#' @examples
#' pedtools::plotPedList(paternity, marker = "SE33")
#' forrel::kinshipLR(paternity)
"paternity"
