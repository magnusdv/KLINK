suppressPackageStartupMessages({
  library(KLINK)
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  library(gt)
})

VERSION = packageDescription("KLINK")$Version

debug = function(x) {if (getOption("KLINK.debug")) print(x)}

if(Sys.getlocale("LC_CTYPE") == "C")
  Sys.setlocale("LC_CTYPE", locale = "en_US.UTF-8")

# Define UI
ui = dashboardPage(title = "KLINK",

  header = dashboardHeader(
    title = HTML("<b>KLINK:</b> Kinship with pairwise linked markers"),
    titleWidth = 500,
    dropdownMenuOutput("notificationMenu")
  ),

  sidebar = dashboardSidebar(
    fluidRow(style = "padding: 5px 15px 0px 15px",
             column(4, h4(HTML("<b>INPUT</b>"), style = "margin-bottom: 0px")),
             column(4, align = "right",
                    actionButton("loadex",  "Example", class = "btn-sm btn btn-success",
                                 style = "padding: 1px 8px; margin: 8px 0px 0px 0px; background-color:#90ee90")),
             column(4, align = "right",
                    actionButton("reset",  "Reset",width = "100%", class = "btn-sm btn btn-warning",
                                  style = "padding: 1px 8px; margin: 8px 0px 0px 0px;")),
    ),
    tags$div(class = "loadfile", fileInput("famfile", "Load .fam file", buttonLabel = icon("folder-open"), accept = ".fam")),
    tags$div(class = "loadfile", fileInput("xmlfile", "(Optional) .xml", buttonLabel = icon("folder-open"), accept = ".xml")),

    hr(),
    h4(HTML("<b>SETTINGS</b>"), style = "padding-left:15px; margin-bottom: 0px; margin-top: 0px"),
    radioButtons("mapfunction", "Mapping function", choices = c("Kosambi", "Haldane"),
                 selected = "Kosambi", inline = TRUE),
    radioButtons("maptype", "Marker map", inline = TRUE, width = "100%",
                 choices = c("Built-in" = "LINKAGEMAP", "Custom" = "custom")),
    conditionalPanel(
      condition = "input.maptype == 'custom'",
      fileInput("mapfile", NULL, buttonLabel = icon("folder-open"),
                accept = c("text/tab-separated-values", "text/plain", ".txt", ".map"))
    ),
    checkboxInput("speclump", "Special lumping"),
    numericInput("maxdist", label = "Ignore linkage above (cM)", value = 200, min = 0, step = 5),
    hr(),
    actionButton("compute", "Calculate LR", class = "btn-lg btn-danger", onclick = "buttonClick('compute')",
                 style = "margin-top:20px;background-color:#FF5c5c")
  ),

  body = dashboardBody(
   useShinyjs(),
   includeCSS("www/custom.css"),
   tags$head(includeHTML(system.file("shiny/www/GA.html", package = "KLINK"))),
   useBusyIndicators(spinners = FALSE, pulse = TRUE),

   # Embed JS here; cannot send gmail attachment with script.js
   tags$script(HTML("
     window.onbeforeunload = function(){ Shiny.onInputChange('browserClosed', Math.random()); };
   ")),

   fluidRow(
     column(width = 4,
            box(title = tagList("Ped 1",
                                tags$div(id = "hideEmptyCheck",
                                         checkboxInput("hideEmpty", HTML("Hide untyped<br>components"), value = TRUE))),
                width = NULL, status = "info", solidHeader = TRUE,
                plotOutput("pedplot1", height = "315px")),
            box(
              title = tagList("Ped 2",
                               tags$div(selectInput("showmarker", NULL, choices = c("Marker" = "")),
                                        id = "selectmarker",
                                        style = "position:absolute; right:5px; top:5px; margin:0px; padding:0px; width: 100px")),
              width = NULL, status = "info", solidHeader = TRUE,
              plotOutput("pedplot2", height = "315px")),
     ),
     column(width = 8,
            tabBox(id = "tabs", width = NULL, selected = "Linkage map",
                   title = tagList(downloadButton('download', class = "btn btn-warning",
                                                  style = "position:absolute; right:10px; top:5px; margin:0px; padding:4px 8px; background:orange")),
                   tabPanel("Linkage map",
                            fluidRow(
                              column(6, class = "col-lg-5", gt::gt_output("linkage_table")),
                              column(6, class = "col-lg-7", plotOutput("karyo", height = "700px")) # todo:680?
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
     "If you encounter problems, please file a ",
     a("bug report", href = "https://github.com/magnusdv/KLINK/issues", target="_blank", .noWS = "outside"),
     ". See also the ",
     a("KLINK homepage", href = "https://magnusdv.github.io/pedsuite/articles/web_only/klink.html", target="_blank"), "for more information.")
   )
)

# Define server
server = function(input, output, session) {

  # Close app when browser closes
  observeEvent(input$browserClosed, stopApp())

  # Main reactive variables
  famfile = reactiveValues(famname = NULL, params = NULL)
  pedigrees = reactiveValues(complete = NULL, reduced = NULL, active = NULL)
  XML = reactiveVal(NULL)
  NOTES = reactiveVal(NULL)

  shinyjs::disable("download")

  # Error utility
  showNote = function(..., type = "error") {
    debug("showNote")
    showNotification(HTML(paste(..., sep = "<br>")), duration = NULL, type = type)
    invisible(NULL)
  }

  # Notifications shown in header
  addNote = function(...) {
    oldnotes = NOTES()
    newnote = HTML(paste(..., sep = "<br>"))
    NOTES(c(oldnotes, newnote))
  }

  output$notificationMenu = renderMenu({
    debug("notificationMenu")
    notes = lapply(NOTES(), function(n) {notificationItem(HTML(n), status = "warning")})
    dropdownMenu(type = "notifications", .list = notes, badgeStatus = "warning")
  })

  observeEvent(input$famfile, {
    debug("famfile")
    fil = req(input$famfile)
    famfile$famname = fil$name
    shinyjs::reset("xmlfile")
    XML(NULL)
    NOTES(NULL)

    peddata = tryCatch(
      error = showNote,
      withCallingHandlers(
        warning = function(w) addNote(conditionMessage(w)),
        KLINK::loadFamFile(fil$datapath, fallbackModel = "equal", withParams = TRUE)
      )
    )

    peds = req(peddata$peds)
    pedigrees$complete = peds
    famfile$params = peddata$params

    allLabs = unlist(lapply(peds, labels), recursive = TRUE, use.names = FALSE)

    if(any(misspar <- startsWith(allLabs, ":missing:"))) {
      nmiss = sum(misspar)
      msg = if(nmiss == 1) "1 missing parent was added." else paste(nmiss, "missing parents were added.")
      msg = paste(msg, "LRs may deviate from Familias/FamLink2.")
      addNote(msg)
    }
  })

  observeEvent(input$xmlfile, {
    debug("xmlfile")
    fil = req(input$xmlfile)

    famname = famfile$famname
    peds = pedigrees$complete
    famids = if(!is.null(peds)) pedtools::typedMembers(peds[[1]]) else NULL

    stop2 = KLINK:::stop2
    warn = function(...) showNote(..., type = "warning")

    xmldat = tryCatch(error = showNote, {
      if(is.null(famname))
        stop2("Familias file must be loaded first")
      if(sub(".xml", "", fil$name) != sub(".fam", "", famname))
        stop2(paste("File names do not match", fil$name, famname, sep = "<br>"))

      dat = KLINK::parseXML(fil)

      # Check that IDs match the Familias file
      xmlids = dat$ID
      if(!setequal(famids, xmlids))
        stop2(paste(c("Individuals in XML file do not match `.fam` file", xmlids), collapse = "<br>"))

      # Enforce same order
      dat[match(famids, xmlids), , drop = FALSE]
    })

    if(is.null(xmldat)) {
      shinyjs::reset("xmlfile")
      return()
    }

    # Check AMEL
    amelsex = match(xmldat$AMEL, c("X-Y", "X-X"))
    sex = pedtools::getSex(peds[[1]], famids)
    if(!identical(amelsex, sex))
      warn("Warning: AMEL genotypes do not match sex given in `.fam` file")

    # Rename using initials found in XML
    inits = xmldat$Initials
    if(any(inits == ""))
      warn("Warning: Missing initials in `XML` file; cannot rename individuals")
    else if(anyDuplicated(inits))
      warn("Warning: Duplicated initials in `XML` file; cannot rename individuals")
    else {
      newpeds = lapply(peds, function(ped)
        pedtools::relabel(ped, old = xmldat$ID, new = inits))
      pedigrees$complete = newpeds
    }

    XML(xmldat)
  })

  observeEvent(input$loadex, {
    debug("loadex")
    fil = system.file("extdata", "halfsib-test.fam", package = "KLINK")
    shinyjs::reset("famfile")
    shinyjs::reset("xmlfile")
    NOTES(NULL)
    XML(NULL)
    pedigrees$complete = KLINK::loadFamFile(fil)
    famfile$famname = "halfsib-test.fam"
  })

  observeEvent(pedigrees$complete, {
    debug("Set reduced/active")
    peds = pedigrees$complete
    pedred = KLINK:::removeEmpty(peds)
    pedigrees$reduced = pedred
    pedigrees$active = if(input$hideEmpty) pedred else peds

    # Reset main table
    resultTable(NULL)

    # Update dropdown marker list
    markers = c("Marker" = "",  "(none)", pedtools::name(peds[[1]]))
    updateSelectInput(session, "showmarker", choices = markers)
  })


  # Pedigree plots ----------------------------------------------------------

  selectedMarker = reactive(if(input$showmarker == "(none)") NULL else input$showmarker)
  observeEvent(input$showmarker, {
    if(input$showmarker == "(none)") {
      markers = c("Marker" = "",  "(none)", pedtools::name(pedigrees$complete[[1]]))
      updateSelectInput(session, "showmarker", choices = markers)
    }
  })

  output$pedplot1 = renderPlot({
    debug("plot1")
    ped1 = req(pedigrees$active[[1]])
    m = input$showmarker
    if(m == "Marker") m = NULL
    KLINK:::plotPed(ped1, marker = selectedMarker(), cex = 1.2)
  }, execOnResize = TRUE)

  output$pedplot2 = renderPlot({
    debug("plot2")
    ped2 = req(pedigrees$active[[2]])
    m = input$showmarker
    if(m == "Marker") m = NULL
    KLINK:::plotPed(ped2, marker = selectedMarker(), cex = 1.2)
  }, execOnResize = TRUE)

  observeEvent(input$hideEmpty, {
    pedigrees$active = if(input$hideEmpty) pedigrees$reduced else pedigrees$complete
  })


  # Main LR table ------------------------------------------------------------------

  resultTable = reactiveVal(NULL)

  # Print LR result table
  output$result_table = render_gt({
    debug("LR table")
    res = resultTable()
    validate(need(!is.null(res), "No likelihood ratios have been calculated yet."))
    KLINK:::prettyResultTable(res)
  }, width = "100%", align = "left")

  # Compute LR
  observeEvent(input$compute, {
    debug("compute LR")
    peds = req(pedigrees$reduced)

    res = KLINK::linkedLR(pedigrees = peds,
                          linkageMap = linkageMap(),
                          linkedPairs = linkedPairs(),
                          markerData = markerData(),
                          mapfun = input$mapfunction,
                          lumpSpecial = input$speclump)

    resultTable(res)
    updateTabsetPanel(session, "tabs", selected = "LR table")
    shinyjs::enable("download")
  })

  # Reset when changing map function
  observeEvent(input$mapfunction, resultTable(NULL))

  # Marker data table -------------------------------------------------------

  markerData = reactiveVal(NULL)

  observeEvent(pedigrees$complete, {
    debug("markerData")
    mtab = KLINK::markerSummary(pedigrees$complete, replaceNames = is.null(XML()))
    req(mtab)

    # Sort according to map
    mtab = mtab[order(match(mtab$Marker, linkageMap()$Marker)), , drop = FALSE]
    markerData(mtab)
    updateTabsetPanel(session, "tabs", selected = "Marker data")
  })

  # Print loaded marker data
  output$marker_table = render_gt({
    debug("marker table")
    mtab = markerData()
    validate(need(!is.null(mtab), "No data has been loaded."))
    KLINK:::prettyMarkerTable(mtab, linkedPairs())
  }, width = "100%", align = "left")


  # Linkage map table ----------------------------------------------------

  # Complete linkage map
  linkageMap = reactiveVal(NULL)

  # Subset where only observed markers are included
  linkageMapSubset = reactive({
    debug("linkage map subset")
    fullmap = linkageMap()
    mdat = markerData()
    if(is.null(mdat))
      return(fullmap)
    fullmap[fullmap$Marker %in% mdat$Marker, , drop = FALSE]
  })

  # Linked pairs
  linkedPairs = reactive({
    debug("linked pairs")
    mdat = markerData()
    getLinkedPairs(mdat$Marker, linkageMap(), maxdist = req(input$maxdist))
  })

  # React to Marker map radio selection
  observeEvent(input$maptype, {
    debug("maptype")
    req(input$maptype != "custom")
    map = get(input$maptype, "package:KLINK")
    linkageMap(map)
  })

  # Change map file
  observeEvent(input$mapfile, {
    debug("mapfile")
    path = req(input$mapfile$datapath)
    header = grepl("marker", readLines(path, n = 1), ignore.case = TRUE)
    map = utils::read.table(path, header = header, sep="\t")

    linkageMap(map)
    resultTable(NULL)
    updateTabsetPanel(session, "tabs", selected = "Linkage map")
  })

  output$karyo = renderPlot(KLINK:::karyogram(linkageMapSubset(),
                                              linkedPairs = linkedPairs()))

  # Print loaded genetic map
  output$linkage_table = render_gt({
    debug("linkage map table")
    map = req(linkageMapSubset())
    KLINK:::prettyLinkageMap(map, linkedPairs())
  }, width = "100%", align = "left")



  # Download tables ---------------------------------------------------------

  output$download = downloadHandler(
    filename = function() {
      fam = famfile$famname
      paste0("KLINK-", if(!is.null(fam)) sub(".fam", "", fam), ".xlsx")
    },
    content = function(file) {
      debug("download")

      settings = list("KLINK version" = VERSION,
                      "Database" = famfile$params$dbName,
                      "Map function" = input$mapfunction,
                      "Genetic map" = input$maptype,
                      "Max distance" = input$maxdist)

      KLINK::writeResult(
        resultTable(),
        pedigrees = pedigrees$reduced,
        linkageMap = linkageMapSubset(),
        markerData = markerData(),
        outfile = file,
        notes = NOTES(),
        famname = famfile$famname,
        settings = settings,
        XML = XML())
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )


  # Reset -------------------------------------------------------------------

  observeEvent(input$reset, {
    shinyjs::reset("famfile")
    shinyjs::reset("xmlfile")
    famfile$famname = famfile$params = NULL
    NOTES(NULL)
    XML(NULL)
    updateRadioButtons(session, "maptype", selected = "LINKAGEMAP")
    updateNumericInput(session, "maxdist", value = 200)
    updateRadioButtons(session, "mapfunction", selected = "Kosambi")
    pedigrees$complete = pedigrees$reduced = pedigrees$active = NULL
    markerData(NULL)
    resultTable(NULL)
    shinyjs::disable("download")
  })

}

# Run the application
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))

