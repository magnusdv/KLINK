mylink = function(text, href, .noWS = "outside", ...) {
  if(missing(href))
    href = text
  shiny::a(text, href = href, .noWS = .noWS, target = "_blank", ...)
}
