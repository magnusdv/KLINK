#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @rawNamespace import(shiny, except = c(singleton, is.singleton))
#' @import gt
#' @import openxlsx
#' @import pedtools
#' @import shinydashboard
## usethis namespace: end
NULL


# Hack to avoid CRAN note. (load_all is only used in app.R which is in Rbuildignore.)
ignore_unused_imports = function() {
  zip::zip
}
