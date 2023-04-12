
#' @export
openKLINK = function(...) {

  # Define UI
  ui = dashboardPage(title = "KLINK",

    header = dashboardHeader(title = HTML("<b>KLINK:</b> Kinship with pairwise linked markers"), titleWidth = 400),

    sidebar = dashboardSidebar(
      fileInput("famfile", "Upload .fam file", buttonLabel = icon("folder-open")),
      fileInput("mapfile", "Change marker map", buttonLabel = icon("folder-open"),
                placeholder = "BUILTIN"),
      hr(),
      actionButton("loadex",  "Load example", width = "50%", class = "btn btn-info"),
      actionButton("compute", "Calculate LR", width = "50%", class = "btn btn-danger"),
      hr(),
      radioButtons("mapfunction", "Mapping function", choices = c("Haldane", "Kosambi"),
                   selected = "Kosambi", inline = TRUE),
      selectInput("showmarker", "Plot genotypes: ", choices = c(None = ""))
    ),

    body = dashboardBody(

      tags$head(
        tags$style(HTML("
          .checkbox {position:absolute; right:5px; top:5px; margin:0px; padding:0px;}
          #shiny-notification-panel {top:25%; left:15%; width:100%; max-width:580px;font-size:20px;}

      "))),

      fluidRow(
        column(width = 4,
               box(title = tagList("Ped 1",
                                   tags$div(checkboxInput("hideEmpty", "Hide untyped components"), id = "hide",
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

    # Error utility
    showNote = function(...) {
      showNotification(HTML(paste(..., sep = "<br>")), duration = NULL, type = "error")
      invisible(NULL)
    }

    famfilename = reactiveVal(NULL)
    pedigrees = reactiveValues(complete = NULL, reduced = NULL, active = NULL)

    observeEvent(input$famfile, {
      fil = req(input$famfile)
      famfilename(fil$name)
      peds = tryCatch(loadFamFile(fil$datapath), error = showNote)
      pedigrees$complete = req(peds)
      if(any(startsWith(unlist(labels(peds[[1]])), ":missing:")))
        showNote(
        "Warning: Some missing parents have been added! (See plots.) <br>",
        "This may cause LR deviations from Familias/FamLink if nonstationary mutation models are used.<br>",
        "To avoid this problem, add the missing individuals in Familias before saving the .fam file.")
    })

    observeEvent(input$loadex, {
      fil = system.file("extdata", "halfsib-test.fam", package = "KLINK")
      pedigrees$complete = loadFamFile(fil)
      famfilename("halfsib-test.fam")
    })

    observeEvent(pedigrees$complete, {
      peds = pedigrees$complete
      pedred = removeEmpty(peds)
      pedigrees$reduced = pedred

      # Reset main table
      resultTable(NULL)

      # Uncheck "remove empty"
      if(input$hideEmpty)
        updateCheckboxInput(session, "hideEmpty", value = FALSE)
      else
        pedigrees$active = pedigrees$complete

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
      res = req(resultTable())
      prettyTable(res)
    }, width = "100%", align = "left")

    # Compute LR
    observeEvent(input$compute, {
      ped = req(pedigrees$reduced)
      res = linkedLR(ped, linkageMap = linkageMap(), mapfun = input$mapfunction)
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
      mtab = req(markerData())
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
        res = resultTable()
        LRcols = c("LRnolink",	"LRlinked",	"LRnomut")
        res[res$Gindex > 1, LRcols] = NA
        res$Gindex = res$Gsize = NULL

        # add totals row
        res = rbind(res, NA)
        res[nrow(res), LRcols] = apply(res[LRcols], 2, prod, na.rm = TRUE)
        res[nrow(res), 1] = "Total LR"

        data = list(linkageMap = linkageMap(),
                    markerData = markerData(),
                    LRtable = res)
        writeResult(data, file)
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )

  }

  # Run the application
  shinyApp(ui = ui, server = server, ...)
}
