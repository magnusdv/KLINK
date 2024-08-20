#' Launch KLINK
#'
#' This launches the KLINK app. `runKLINK()` is a synonym for `launchApp()`, but
#' with an additional argument `version`.
#'
#' @param version A character, e.g. "1.0.0". If the installed version of KLINK
#'   differs from this, the program aborts with an error.
#'
#' @return No return value, called for side effects.
#'
#' @examples
#'
#' \dontrun{
#' launchApp()
#' }
#'
#' @export
launchApp = function() {
  shiny::runApp(system.file("shiny", package = "KLINK"))
}

#' @export
#' @rdname launchApp
runKLINK = function(version = NULL) {
  inst = utils::packageVersion("KLINK")
  supplied = as.package_version(version)
  if(length(supplied) && !identical(inst, supplied)) {
    msg = sprintf("The supplied version (%s) differs from the installed version (%s)\n",
                  supplied, inst)
    stop2(msg)
  }

  shiny::runApp(system.file("shiny", package = "KLINK"))
}
