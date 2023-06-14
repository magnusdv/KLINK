#' Launch KLINK
#'
#' This launches the shiny app KLINK.
#'
#' @return NULL
#'
#' @examples
#'
#' \dontrun{
#' runKLINK()
#' }
#'
#' @export
runKLINK = function() {

  VERSION = "0.5.0"

  # Define UI
  ui = dashboardPage(title = "KLINK",

    header = dashboardHeader(title = HTML("<b>KLINK:</b> Kinship with pairwise linked markers"),
                             titleWidth = 500,
                             dropdownMenuOutput("notificationMenu")),

    sidebar = dashboardSidebar(
      fluidRow(style = "padding: 5px 15px 0px 15px",
        column(6, h4(HTML("<b>Load data</b>"), style = "margin-bottom: 0px")),
        column(6, align = "right",
               actionButton("loadex",  "Example", class = "btn-sm btn btn-warning", style = "padding: 1px 10px;"))
      ),
      tags$div(class = "loadfile", fileInput("famfile", ".fam file", buttonLabel = icon("folder-open"))),

      fileInput("mapfile", "Marker map", buttonLabel = icon("folder-open"),
                placeholder = "BUILTIN"),
      radioButtons("fallback", "Fallback mutation model", choices = c("equal", "proportional"),
                   selected = "equal", inline = TRUE),
      hr(),
      radioButtons("mapfunction", "Mapping function", choices = c("Haldane", "Kosambi"),
                   selected = "Kosambi", inline = TRUE),
      actionButton("compute", "Calculate LR", class = "btn-lg btn-info", style = "margin-top:20px;")
      #checkboxInput("linkedonly", "Show linked only", value = FALSE),
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
      "))),

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
        a("changelog", href = "https://github.com/magnusdv/klink/blob/master/NEWS.md", target="_blank", .noWS = "outside"), ").",
        "If you find something that isn't working properly, please file a ",
        a("bug report", href = "https://github.com/magnusdv/klink/issues", target="_blank", .noWS = "outside"),
        ". For more information about the R packages on which KLINK is based, see the",
        a("ped suite", href = "https://magnusdv.github.io/pedsuite", target="_blank"), "homepage.")
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
      notes = lapply(NOTES(), function(n) {notificationItem(HTML(n), status = "warning")})
      dropdownMenu(type = "notifications", .list = notes, badgeStatus = "warning")
    })

    # Error utility
    showNote = function(..., type = "error") {
      showNotification(HTML(paste(..., sep = "<br>")), duration = NULL, type = type)
      invisible(NULL)
    }

    famfilename = reactiveVal(NULL)
    pedigrees = reactiveValues(complete = NULL, reduced = NULL, active = NULL)

    observeEvent(input$famfile, {
      fil = req(input$famfile)
      famfilename(fil$name)
      NOTES(NULL)
      peds = tryCatch(
        error = showNote,
        withCallingHandlers(
          warning = function(w) addNote(conditionMessage(w)),
          loadFamFile(fil$datapath, fallbackModel = input$fallback)
        )
      )

      pedigrees$complete = req(peds)
      allLabs = unlist(lapply(peds, labels), recursive = TRUE)

      if(any(misspar <- startsWith(allLabs, ":missing:"))) {
        nmiss = sum(misspar)
        msg = if(nmiss == 1) "1 missing parent was added." else paste(nmiss, "missing parents were added.")
        msg = paste(msg, "LRs may deviate from Familias/FamLink2.")
        addNote(msg)
      }
    })

    observeEvent(input$loadex, {
      fil = system.file("extdata", "halfsib-test.fam", package = "KLINK")
      NOTES(NULL)
      pedigrees$complete = loadFamFile(fil)
      famfilename("halfsib-test.fam")
    })

    observeEvent(pedigrees$complete, {
      peds = pedigrees$complete
      pedred = removeEmpty(peds)
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
      ped1 = req(pedigrees$active[[1]])
      plotPed(ped1, marker = input$showmarker, cex = 1.3)
    }, execOnResize = TRUE)

    output$pedplot2 = renderPlot({
      ped2 = req(pedigrees$active[[2]])
      plotPed(ped2, marker = input$showmarker, margin = 0.1, cex = 1.3)
    }, execOnResize = TRUE)

    observeEvent(input$hideEmpty, {
      pedigrees$active = if(input$hideEmpty) pedigrees$reduced else pedigrees$complete
    })


    # Main LR table ------------------------------------------------------------------

    resultTable = reactiveVal(NULL)

    # Print LR result table
    output$result_table = render_gt({
      res = resultTable()
      validate(need(!is.null(res), "No likelihood ratios have been calculated yet."))
      prettyTable(res)
    }, width = "100%", align = "left")

    # Compute LR
    observeEvent(input$compute, {
      ped = req(pedigrees$reduced)
      res = linkedLR(ped, linkageMap(), markerData(), mapfun = input$mapfunction)
      resultTable(res)
      updateTabsetPanel(session, "tabs", selected = "LR table")
    })

    # Reset when changing map function
    observeEvent(input$mapfunction, resultTable(NULL))

    # Marker data table -------------------------------------------------------

    markerData = reactiveVal(NULL)

    observeEvent(pedigrees$complete, {
      mtab = markerSummary(pedigrees$complete, linkageMap = linkageMap())
      markerData(req(mtab))
      updateTabsetPanel(session, "tabs", selected = "Marker data")
    })

    # Print loaded marker data
    output$marker_table = render_gt({
      mtab = markerData()
      validate(need(!is.null(mtab), "No data has been loaded."))
      prettyMarkerTable(mtab)
    }, width = "100%", align = "left")


    # Linkage map table ----------------------------------------------------
    linkageMap = reactiveVal(KLINK::LINKAGEMAP)

    output$karyo = renderPlot(karyogram(linkageMap()))

    # Change map file
    observeEvent(input$mapfile, {
      file = req(input$mapfile)
      map = utils::read.table(file$datapath, header=FALSE, sep="\t")
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
      filename = function() sprintf("KLINK-%s.xlsx", sub(".fam", "", famfilename())),
      content = function(file) {
        writeResult(linkageMap(), markerData(), resultTable(),
                    pedigrees$reduced, NOTES(),
                    file, famfilename())
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )

  }

  # Run the application
  shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
}
