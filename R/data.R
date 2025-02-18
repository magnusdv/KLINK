#' Built-in linkage map
#'
#' A genetic map including 50 autosomal STR markers.
#'
#' @format A data frame with 50 rows and 4 columns: `Marker`, `Kit`, `Chr`,
#'   `cM`.
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
#' markerSummary(paternity)
#'
#' forrel::kinshipLR(paternity)
"paternity"

#' Dataset for a full vs half sibship case
#'
#' A list of two pedigrees forming opposing hypotheses about two individuals
#' `NN1` and `NN2`: `FS` (full siblings) vs. `HS` (half siblings). Both
#' individuals are genotyped with the 23 markers included in the Fusion 6C kit.
#' Three pairs of markers are linked:
#'
#' * TPOX and D2S441
#' * D5S818 and CSF1PO
#' * vWA and D12S391
#'
#' @format A list of two ped objects, named FS and HS.
#'
#' @examples
#' library(pedtools)
#' plotPedList(sibship, hatched = typedMembers)
#' markerSummary(sibship)
#' linkedLR(sibship, KLINK::LINKAGEMAP)
"sibship"

#' Dataset for a case involving a putative half sibling.
#'
#' A list of two pedigrees forming opposing hypotheses about three individuals:
#'
#' * H1: A and B are full sibs, and C is their half brother
#' * H2: A and B are full siblings, and C is unrelated to them
#'
#' The individuals are typed with 50 markers, but some genotypes are missing.
#'
#' @format A list of two ped objects, named H1 and H2.
#'
#' @examples
#' library(pedtools)
#' plotPedList(halfsib, hatched = typedMembers)
#' markerSummary(halfsib)
#' linkedLR(halfsib, KLINK::LINKAGEMAP)
"halfsib"
