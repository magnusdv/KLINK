#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @rawNamespace import(gt, except = html)
#' @importFrom shiny runApp
#' @importFrom shinyjs useShinyjs
#' @importFrom shinydashboard dashboardPage
#' @importFrom shinyBS bsTooltip
#' @import norSTR
#' @import openxlsx
#' @import pedtools
## usethis namespace: end
NULL


#' Genetic marker maps
#'
#' These objects are re-exported from [norSTR::map51] and [norSTR::map50].
#' `map51` is the current map, while `map50` is retained for reproducibility.
#'
#' @name map51
#' @export
map51 = norSTR::map51

#' @rdname map51
#' @export
map50 = norSTR::map50
