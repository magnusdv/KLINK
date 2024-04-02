suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(gt)
  library(KLINK)
  library(zip)
})


VERSION = packageDescription("KLINK")$Version

# Define UI
ui = dashboardPage(title = "KLINK",

  header = dashboardHeader(title = HTML("<b>KLINK:</b> Kinship with pairwise linked markers"),
                           titleWidth = 500,
                           dropdownMenuOutput("notificationMenu")),

  sidebar = dashboardSidebar(
    fluidRow(style = "padding: 5px 15px 0px 15px",
             column(3, h4(HTML("<b>INPUT</b>"), style = "margin-bottom: 0px")),
             column(5, align = "right",
                    actionButton("loadex",  "Example", class = "btn-sm btn btn-success",
                                 style = "padding: 1px 10px; background-color:#90ee90")),
             column(4, align = "right",
                    downloadButton("mask", "Mask", class = "btn-sm btn btn-light",
                                   style = "padding: 0px 6px; color: black; margin-top: 7px"))
    ),
    tags$div(class = "loadfile", fileInput("famfile", "Load .fam file", buttonLabel = icon("folder-open"))),

    fileInput("mapfile", "Marker map", buttonLabel = icon("folder-open"),
              placeholder = "BUILTIN"),
    hr(),
    h4(HTML("<b>SETTINGS</b>"), style = "padding-left:15px; margin-bottom: 0px; margin-top: 0px"),
    radioButtons("fallback", "Fallback mutation model", choices = c("equal", "proportional"),
                 selected = "equal", inline = TRUE),
    radioButtons("mapfunction", "Mapping function", choices = c("Haldane", "Kosambi"),
                 selected = "Kosambi", inline = TRUE),
    hr(),
    actionButton("compute", "Calculate LR", class = "btn-lg btn-danger", onclick = "buttonClick('compute')",
                 style = "margin-top:20px;background-color:#FF5c5c")
 ),

  body = dashboardBody(

   tags$head(
    tags$style(HTML("
        #hideEmptyCheck .checkbox {position:absolute; right:5px; top:5px; margin:0px; padding:0px;}
        #shiny-notification-panel {top:20%; left:30%; width:100%; max-width:580px;font-size:20px;}
        .shiny-notification {opacity:1}
        .fa-triangle-exclamation {font-size:24px; padding-bottom:5px;}
        .main-header .navbar-custom-menu {float:left;}
        #notificationMenu span.label.label-warning {font-size:14px;}
        #notificationMenu a.dropdown-toggle {padding-bottom:5px;}
        .sidebar-toggle {display: none !important;}
        #notificationMenu .dropdown-menu {width:380px;}
        #notificationMenu .dropdown-menu > li .menu > li > a {white-space:normal !important;}
        .form-group.shiny-input-container {margin-bottom:0px}
        .progress {margin-bottom:0px}
        .wait-cursor { cursor: wait !important; }
    ")),

    tags$script(HTML("
      Shiny.addCustomMessageHandler('waitCursor', function(message) {
        if(message) {
          $('body').addClass('wait-cursor');
        } else {
          $('body').removeClass('wait-cursor');
        }
      });
    ")),

    includeHTML(system.file("shiny/www/GA.html", package = "KLINK"))
   ),

   fluidRow(
     column(width = 4,
            box(title = tagList("Ped 1",
                                tags$div(checkboxInput("hideEmpty", "Hide untyped components", value = TRUE),
                                         id = "hideEmptyCheck",
                                         style = "position:absolute; right:5px; top:5px; margin:0px; padding:0px;")),
                width = NULL, status = "info", solidHeader = TRUE,
                plotOutput("pedplot1", height = "325px")),
            box(title = "Ped 2", width = NULL, status = "info", solidHeader = TRUE,
                plotOutput("pedplot2", height = "325px")),
     ),
     column(width = 8,
            tabBox(id = "tabs", width = NULL, selected = "Linkage map",
                   title = tagList(downloadButton('download', class = "btn btn-warning",
                                                  style = "position:absolute; right:10px; top:5px; margin:0px; padding:4px 8px; background:orange")),
                   tabPanel("Linkage map",
                            fluidRow(
                              column(6, gt::gt_output("linkage_table")),
                              column(6, plotOutput("karyo", height = "640px"))
                            )
                   ),
                   tabPanel("Marker data", gt::gt_output("marker_table")),
                   tabPanel("LR table", gt::gt_output("result_table"))
            ),
     ),
   ),
   p("This is KLINK version", VERSION, "(",
     a("changelog", href = "https://github.com/magnusdv/KLINK/blob/master/NEWS.md", target="_blank", .noWS = "outside"), " | ",
     a("official releases", href = "https://github.com/magnusdv/KLINK/releases", target="_blank", .noWS = "outside"), ").",
     "If you find something that isn't working properly, please file a ",
     a("bug report", href = "https://github.com/magnusdv/KLINK/issues", target="_blank", .noWS = "outside"),
     ". For more information about the R packages on which KLINK is based, see the",
     a("pedsuite", href = "https://magnusdv.github.io/pedsuite", target="_blank"), "homepage.")
   )
)

# Define server
server = function(input, output, session) {

  NOTES = reactiveVal(NULL)

  addNote = function(...) {
    oldnotes = NOTES()
    newnote = HTML(paste(..., sep = "<br>"))
    NOTES(c(oldnotes, newnote))
  }

  output$notificationMenu = renderMenu({
    if (getOption("KLINK.debug")) print("notificationMenu")
    notes = lapply(NOTES(), function(n) {notificationItem(HTML(n), status = "warning")})
    dropdownMenu(type = "notifications", .list = notes, badgeStatus = "warning")
  })

  # Error utility
  showNote = function(..., type = "error") {
    if (getOption("KLINK.debug")) print("showNote")
    showNotification(HTML(paste(..., sep = "<br>")), duration = NULL, type = type)
    invisible(NULL)
  }

  famfilename = reactiveVal(NULL)
  famparams = reactiveVal(NULL)
  pedigrees = reactiveValues(complete = NULL, reduced = NULL, active = NULL)

  observeEvent(input$famfile, {
    if (getOption("KLINK.debug")) print("famfile")
    fil = req(input$famfile)
    famfilename(fil$name)
    NOTES(NULL)
    peddata = tryCatch(
      error = showNote,
      withCallingHandlers(
        warning = function(w) addNote(conditionMessage(w)),
        KLINK::loadFamFile(fil$datapath, fallbackModel = input$fallback, withParams = TRUE)
      )
    )

    peds = req(peddata$peds)
    pedigrees$complete = peds
    famparams(peddata$params)

    allLabs = unlist(lapply(peds, labels), recursive = TRUE, use.names = FALSE)

    if(any(misspar <- startsWith(allLabs, ":missing:"))) {
      nmiss = sum(misspar)
      msg = if(nmiss == 1) "1 missing parent was added." else paste(nmiss, "missing parents were added.")
      msg = paste(msg, "LRs may deviate from Familias/FamLink2.")
      addNote(msg)
    }
  })

  observeEvent(input$loadex, {
    if (getOption("KLINK.debug")) print("loadex")
    fil = system.file("extdata", "halfsib-test.fam", package = "KLINK")
    NOTES(NULL)
    pedigrees$complete = KLINK::loadFamFile(fil)
    famfilename("halfsib-test.fam")
  })

  observeEvent(pedigrees$complete, {
    if (getOption("KLINK.debug")) print("Set reduced/active")
    peds = pedigrees$complete
    pedred = KLINK:::removeEmpty(peds)
    pedigrees$reduced = pedred
    pedigrees$active = if(input$hideEmpty) pedred else peds

    # Reset main table
    resultTable(NULL)

    # Update dropdown marker list
    markers = c(None = "", pedtools::name(peds[[1]]))
    updateSelectInput(session, "showmarker", choices = markers, selected = character(0))
  })


  # Pedigree plots ----------------------------------------------------------

  output$pedplot1 = renderPlot({
    if (getOption("KLINK.debug")) print("plot1")
    ped1 = req(pedigrees$active[[1]])
    KLINK:::plotPed(ped1, marker = input$showmarker, cex = 1.2, margin = 3)
  }, execOnResize = TRUE)

  output$pedplot2 = renderPlot({
    if (getOption("KLINK.debug")) print("plot2")
    ped2 = req(pedigrees$active[[2]])
    KLINK:::plotPed(ped2, marker = input$showmarker, cex = 1.2, margin = 3)
  }, execOnResize = TRUE)

  observeEvent(input$hideEmpty, {
    pedigrees$active = if(input$hideEmpty) pedigrees$reduced else pedigrees$complete
  })


  # Main LR table ------------------------------------------------------------------

  resultTable = reactiveVal(NULL)

  # Print LR result table
  output$result_table = render_gt({
    if (getOption("KLINK.debug")) print("LR table")
    res = resultTable()
    validate(need(!is.null(res), "No likelihood ratios have been calculated yet."))
    KLINK:::prettyTable(res)
  }, width = "100%", align = "left")

  # Compute LR
  observeEvent(input$compute, {
    if (getOption("KLINK.debug")) print("compute LR")
    ped = req(pedigrees$reduced)

    session$sendCustomMessage(type = 'waitCursor', message = TRUE)
    res = KLINK::linkedLR(ped, linkageMap(), markerData(), mapfun = input$mapfunction)
    session$sendCustomMessage(type = 'waitCursor', message = FALSE)

    resultTable(res)
    updateTabsetPanel(session, "tabs", selected = "LR table")
  })

  # Reset when changing map function
  observeEvent(input$mapfunction, resultTable(NULL))

  # Marker data table -------------------------------------------------------

  markerData = reactiveVal(NULL)

  observeEvent(pedigrees$complete, {
    if (getOption("KLINK.debug")) print("markerData")
    mtab = KLINK::markerSummary(pedigrees$complete, linkageMap = linkageMap())
    markerData(req(mtab))
    updateTabsetPanel(session, "tabs", selected = "Marker data")
  })

  # Print loaded marker data
  output$marker_table = render_gt({
    if (getOption("KLINK.debug")) print("marker table")
    mtab = markerData()
    validate(need(!is.null(mtab), "No data has been loaded."))
    KLINK:::prettyMarkerTable(mtab)
  }, width = "100%", align = "left")


  # Linkage map table ----------------------------------------------------
  linkageMap = reactiveVal(KLINK::LINKAGEMAP)

  output$karyo = renderPlot(KLINK:::karyogram(linkageMap()))

  # Change map file
  observeEvent(input$mapfile, {
    if (getOption("KLINK.debug")) print("mapfile")
    path = req(input$mapfile$datapath)
    header = readLines(path, n = 1) |> startsWith("Pair")
    map = utils::read.table(path, header = header, sep="\t")

    linkageMap(map)
    resultTable(NULL)
    updateTabsetPanel(session, "tabs", selected = "Linkage map")
  })

  # Print loaded genetic map
  output$linkage_table = render_gt({
    gt(req(linkageMap())) |> opt_stylize(6) |>
      tab_options(data_row.padding = px(3)) |>
      tab_style(style = cell_text(whitespace = "nowrap"),
                locations = cells_body())
  }, width = "100%", align = "left")



  # Download tables ---------------------------------------------------------

  output$download = downloadHandler(
    filename = function() {
      fam = famfilename()
      paste0("KLINK-", if(!is.null(fam)) sub(".fam", "", fam), ".xlsx")
    },
    content = function(file) {
      if (getOption("KLINK.debug")) print("download")

      KLINK::writeResult(
        resultTable(),
        pedigrees = pedigrees$reduced,
        linkageMap = linkageMap(),
        markerData = markerData(),
        outfile = file,
        notes = NOTES(),
        fam = famfilename())
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )


  # Mask / unmask -----------------------------------------------------------

  output$mask = downloadHandler(
    filename = function() {
      origfam = famfilename()
      if(is.null(origfam))
        origfam = "KLINK.fam"
      sub(".fam", "_MASKED.zip", origfam, fixed = TRUE)
    },
    content = function(file) {
      if (getOption("KLINK.debug")) print("mask")

      peds = req(pedigrees$complete)
      origfam = basename(req(famfilename()))
      if(is.null(origfam))
        return()
      tmp = tempdir()
      fam = sub(".fam", "_MASKED.fam", origfam, fixed = TRUE)
      map = sub(".fam", "_MASKED-map.txt", origfam, fixed = TRUE)
      key = sub(".fam", "_MASKED-keys.txt", origfam, fixed = TRUE)

      # Write .fam file with masked data
      keys = KLINK:::writeMasked(file.path(tmp, fam), peds = peds, params = famparams())

      # Masked linkage map
      mm = KLINK:::maskMap(map = linkageMap(), keys = keys)
      write.table(mm, file = file.path(tmp, map), quote = FALSE, sep = "\t", col.names = TRUE, row.names = FALSE)

      # Write keys to file
      KLINK:::writeKeys(keys, file = file.path(tmp, key))

      # Zip
      zip::zip(zipfile = file, files = c(fam, map, key), root = tmp)
    },
    contentType = "application/zip"
  )
}

# Run the application
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))

