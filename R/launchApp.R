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
runKLINK = function(version) {
  supplied = as.package_version(version)
  inst = utils::packageVersion("KLINK")
  if(inst != supplied) {
    msg = c("",
      sprintf("The supplied version number (%s) differs from the installed version (%s)", supplied, inst), "",
      sprintf("To install KLINK v%s, try restarting R and running this command:", supplied),
      sprintf('> remotes::install_version("KLINK", version = "%s")', supplied)
    )
    stop2(paste0(msg, collapse = "\n"))
  }

  shiny::runApp(system.file("shiny", package = "KLINK"))
}
