#' Launch KLINK
#'
#' This launches the KLINK app. `runKLINK()` is a synonym for `launchApp()`.
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
runKLINK = launchApp
