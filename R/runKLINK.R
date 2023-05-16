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

  # Define UI
  ui = dashboardPage(title = "KLINK",

    header = dashboardHeader(title = HTML("<b>KLINK:</b> Kinship with pairwise linked markers"),
                             titleWidth = 500,
                             dropdownMenuOutput("notificationMenu")),

    sidebar = dashboardSidebar(
      fileInput("famfile", "Load .fam file", buttonLabel = icon("folder-open")),
      fileInput("mapfile", "Change marker map", buttonLabel = icon("folder-open"),
                placeholder = "BUILTIN"),
      hr(),
      actionButton("loadex",  "Load example", width = "50%", class = "btn btn-info"),
      actionButton("compute", "Calculate LR", width = "50%", class = "btn btn-danger"),
      #checkboxInput("uselog", "Show log(LR)", value = FALSE),
      radioButtons("mapfunction", "Mapping function", choices = c("Haldane", "Kosambi"),
                   selected = "Kosambi", inline = TRUE),
      checkboxInput("linkedonly", "Show linked only", value = FALSE)
      #selectInput("showmarker", "Plot genotypes: ", choices = c(None = ""))
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
          #notificationMenu .dropdown-menu {width:400px;}
          #notificationMenu .dropdown-menu > li .menu > li > a {white-space:normal !important;}
      "))),

      fluidRow(
        column(width = 4,
               box(title = tagList("Ped 1",
                                   tags$div(checkboxInput("hideEmpty", "Hide untyped components", value = TRUE),
                                            id = "hideEmptyCheck",
                                            style = "position:absolute; right:5px; top:5px; margin:0px; padding:0px;")),
                   width = NULL, status = "info", solidHeader = TRUE,
                   plotOutput("pedplot1", height = "330px")),
               box(title = "Ped 2", width = NULL, status = "info", solidHeader = TRUE,
                   plotOutput("pedplot2", height = "330px")),
        ),
        column(width = 8,
               tabBox(id = "tabs", width = NULL, selected = "Linkage map",
                 title = tagList(downloadButton('download', class = "btn btn-warning",
                            style = "position:absolute; right:10px; top:5px; margin:0px; padding:4px 8px; background:orange")),
                 tabPanel("Linkage map", gt::gt_output("linkage_table")),
                 tabPanel("Marker data", gt::gt_output("marker_table")),
                 tabPanel("LR table", gt::gt_output("result_table"))
               ),
        ),
      )
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
          loadFamFile(fil$datapath)
        )
      )

      pedigrees$complete = req(peds)
      allLabs = unlist(lapply(peds, labels), recursive = TRUE)

      if(any(startsWith(allLabs, ":missing:")))
        addNote("Some missing parents have been added! (See plots.)",
                "This may cause LR deviations from Familias/FamLink if non-stationary mutation models are used.")
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
      validate(need(!is.null(res), "Nothing to show yet. After loading a .fam file, press 'Calculate LR'"))

      if(input$linkedonly) {
        res = res[res$Gsize > 1, , drop = FALSE]
        validate(need(nrow(res) > 0, "There are no linked markers in the dataset. Uncheck 'Show linked only' to see all markers."))
      }
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
      validate(need(!is.null(mtab), "Nothing to show yet. Load a .fam file to get started!"))
      prettyMarkerTable(mtab)
    }, width = "100%", align = "left")


    # Linkage map table ----------------------------------------------------
    linkageMap = reactiveVal(KLINK::LINKAGEMAP)

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
      gt(req(linkageMap())) |> opt_stylize(6)
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
