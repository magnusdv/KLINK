mylink = function(text, href, .noWS = "outside", ...) {
  if(missing(href))
    href = text
  shiny::a(text, href = href, .noWS = .noWS, target = "_blank", ...)
}

`%NA|%` = function(x, y) if(is.na(x)) y else x


helpButton = function(id) {
  style = "padding:0; margin:0; color:lightgray; background:transparent; border:none; font-size:18px;"
  actionButton(id, label = NULL, icon = icon("question-circle"), style = style)
}

showHelpModal = function(filename) {
  path = helpFile(filename)
  m = modalDialog(
    title = NULL,
    div(includeMarkdown(path), style = "max-height:80vh; overflow-y:auto; padding: 0"),
    tags$style(HTML("code {
        background-color: #f8f8f8;
        color: #333;
        border: 1px solid #ccc;
        border-radius: 4px;
        padding: 1px 4px;}")),
    easyClose = TRUE,
    footer = modalButton("Close"),
    size = "m"
  )
  m$attribs$class = paste(m$attribs$class, "help-modal")
  showModal(m)
}

helpFile = function(filename) {
  p = system.file("shiny", "help", filename, package = "KLINK")
  if(nzchar(p))
    return(p)

  p = file.path("inst", "shiny", "help", filename)
  if(file.exists(p))
    return(p)

  p = file.path("help", filename)
  if(file.exists(p))
    return(p)

  stop("Help file not found: ", file)
}
