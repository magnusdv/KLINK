#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import shiny
#' @import shinydashboard
#' @import gt
#' @importFrom pedtools founders typedMembers untypedMembers
## usethis namespace: end
NULL


# Hack to avoid CRAN note. (load_all is only used in app.R which is in Rbuildignore.)
ignore_unused_imports = function() {
  pkgload::load_all
}
