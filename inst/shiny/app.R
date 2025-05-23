suppressMessages(suppressPackageStartupMessages({
  library(KLINK)
  library(shiny)
  library(shinyjs)
  library(shinyBS)
  library(shinydashboard)
  library(gt)
}))

VERSION = packageDescription("KLINK")$Version

debug = function(...) {
  if (getOption("KLINK.debug"))
  cat(..., "\n")
}

if(Sys.getlocale("LC_CTYPE") == "C")
  Sys.setlocale("LC_CTYPE", locale = "en_US.UTF-8")


# UI ----------------------------------------------------------------------

ui = dashboardPage(title = "KLINK",

  header = dashboardHeader(
    title = HTML("<b>KLINK:</b> Kinship with pairwise linked markers"),
    titleWidth = 500,
    dropdownMenuOutput("notificationMenu"),
    tags$li(class = "dropdown", uiOutput("banner"))
  ),

  sidebar = dashboardSidebar(
    fluidRow(style = "padding: 5px 15px 0px 15px",
             column(6, h4(HTML("<b>INPUT</b>"), style = "margin-bottom: 0px")),
             column(6, align = "right",
                    actionButton("reset",  "RESET",  class = "btn-sm btn-warning",
                                  style = "font-weight:bolder; padding:0px 8px; margin: 8px 0px 0px 0px;")),
    ),
    tags$div(class = "loadfile", fileInput("famfile", "Load .fam file", buttonLabel = icon("folder-open"), accept = ".fam")),

    radioButtons("maptype", "Marker map", inline = TRUE, width = "100%",
                 choices = c("Built-in" = "map50", "Custom" = "custom")),
    conditionalPanel(
      condition = "input.maptype == 'custom'",
      fileInput("mapfile", NULL, buttonLabel = icon("folder-open"),
                accept = c("text/tab-separated-values", "text/plain", ".txt", ".map"))
    ),

    tags$div(class = "loadfile", fileInput("xmlfile", "(Optional) .xml", buttonLabel = icon("folder-open"), accept = ".xml")),

    fluidRow(style = "padding: 0px 15px 0px 15px",
      column(6, actionButton("loadex1",  "Example 1", class = "btn btn-success", style = "padding: 1px 8px; margin: 8px 0px 0px 0px; background-color:#90ee90")),
      column(6, align = "right", actionButton("loadex2",  "Example 2", class = "btn btn-success", style = "padding: 1px 8px; margin: 8px 0px 0px 0px; background-color:#90ee90"))
    ),
    hr(),
    h4(HTML("<b>SETTINGS</b>"), style = "padding-left:15px; margin-bottom: 0px; margin-top: 0px"),
    radioButtons("mapfunction", "Map function", choices = c("Kosambi", "Haldane"),
                 selected = "Kosambi", inline = TRUE),
    radioButtons("emptymarkers", "Empty markers", inline = TRUE, width = "100%",
                 choices = c("Hide" = "hide", "Show" = "show"), selected = "hide"),
    bsTooltip("emptymarkers",
              "Hide or show markers with no genotype information. (Affects the 'LR table' in the app and in the Excel download.)"),
    radioButtons("likelihoods", "Likelihoods", inline = TRUE, width = "100%",
                 choices = c("Hide" = "hide", "Show" = "show", "Loglik" = "loglik"), selected = "hide"),
    bsTooltip("likelihoods", "Hide or show likelihood columns? (Only affects the 'LR table' in the app.)"),
    numericInput("decimals", label = "Decimals", value = 3, min = 1, step = 1),
    numericInput("maxdist", label = "Ignore linkage above (cM)", value = 200, min = 0, step = 5),
    hr(),
    div(style = "margin-top:20px;padding-right:30px",
        actionButton("compute", "Calculate LR", width = "100%", class = "btn-lg btn-danger",
                 onclick = "buttonClick('compute')", style = "background-color:#FF5c5c;font-size:150%")
        )

  ),

  body = dashboardBody(

    # Various stuff
    useShinyjs(),
    includeCSS("www/custom.css"),
    tags$head(includeHTML(system.file("shiny/www/GA.html", package = "KLINK"))),
    useBusyIndicators(spinners = FALSE, pulse = TRUE),
    busyIndicatorOptions(pulse_height = "10px"),

    # Embed JS here; cannot send gmail attachment with script.js
    tags$script(HTML("
      window.onbeforeunload = function(){ Shiny.onInputChange('browserClosed', Math.random()); };
    ")),

   fluidRow(
     column(width = 4,
            box(title = tagList("Ped 1",
                                tags$div(id = "hideEmptyCheck",
                                         checkboxInput("hideEmptyComps", HTML("Hide untyped<br>components"), value = TRUE))),
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
     mylink("changelog", "https://github.com/magnusdv/KLINK/blob/master/NEWS.md"), " | ",
     mylink("official releases", "https://github.com/magnusdv/KLINK/releases"), ").",
     "If you encounter problems, please file a ",
     mylink("bug report", "https://github.com/magnusdv/KLINK/issues"), ". See also the ",
     mylink("KLINK homepage", "https://magnusdv.github.io/pedsuite/articles/web_only/klink.html"),
     " for more information.")
   )
)


# SERVER ------------------------------------------------------------------

server = function(input, output, session) {

  # Close app when browser closes
  observeEvent(input$browserClosed, stopApp())

  # Show banner with warning on shinyapps.io
  output$banner = renderUI({
    isShinyAppsIO = grepl("shinyapps.io", session$clientData$url_hostname)
    if(isShinyAppsIO) {
      div(style = "display:flex; align-items: center; color: black; background-color: #fff3cd;
          margin: 5px 10px 5px 150px; padding: 5px; border-radius: 10px; text-align: center; line-height:100%",
          icon("circle-exclamation", style = "margin: 0 10px; color:red; font-size: 25px;"),
          div(HTML("Avoid uploading sensitive data online.<br>To run KLINK locally, see instructions "),
              mylink("here", "https://magnusdv.github.io/pedsuite/articles/web_only/klink.html"), "."),
          icon("circle-exclamation", style = "margin: 0 10px; color:red; font-size: 25px;")
      )
      }
    })

  # Main reactive variables
  famfile = reactiveValues(famname = NULL, params = NULL)
  pedigrees = reactiveValues(complete = NULL, reduced = NULL, active = NULL)
  XML = reactiveVal(NULL)
  NOTES = reactiveVal(NULL)

  shinyjs::delay(0, shinyjs::disable("download"))

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
    debug("famfile", input$famfile$name)
    fil = req(input$famfile)

    shinyjs::reset("xmlfile")
    XML(NULL)
    NOTES(NULL)

    peddata = tryCatch(
      error = showNote,
      withCallingHandlers(
        warning = function(w) addNote(conditionMessage(w)), {
          if(!endsWith(fil$name, ".fam"))
            stop2("Input file must end with '.fam': ", fil$name)
          KLINK::loadFamFile(fil$datapath, fallbackModel = "equal", withParams = TRUE)
        }
      )
    )

    if(is.null(peddata)) {
      famfile$famname = famfile$params = NULL
      pedigrees$complete = NULL
      shinyjs::reset("famfile")
      return()
    }

    famfile$famname = fil$name
    pedigrees$complete = peds = peddata$peds
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
    debug("xmlfile", input$xmlfile$name)
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

    if(is.null(xmldat))
      return(shinyjs::reset("xmlfile"))

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

  exampleFile = reactiveVal(NULL)

  observeEvent(input$loadex1, exampleFile("sibship.fam"))
  observeEvent(input$loadex2, exampleFile("halfsib.fam"))
  observeEvent(exampleFile(), {
    debug("load example")
    filename = exampleFile()
    path = system.file("extdata", filename, package = "KLINK")
    shinyjs::reset("famfile")
    shinyjs::reset("xmlfile")
    updateRadioButtons(session, "maptype", selected = "map50")
    NOTES(NULL)
    XML(NULL)
    pedigrees$complete = KLINK::loadFamFile(path)
    famfile$famname = filename
  })

  observeEvent(pedigrees$complete, {
    debug("Set reduced/active")
    peds = pedigrees$complete
    pedred = KLINK:::removeEmpty(peds)
    pedigrees$reduced = pedred
    pedigrees$active = if(input$hideEmptyComps) pedred else peds
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

  observeEvent(input$hideEmptyComps, {
    pedigrees$active = if(input$hideEmptyComps) pedigrees$reduced else pedigrees$complete
  })


  # Main LR table ------------------------------------------------------------------

  resultTable = reactiveVal(NULL)

  # Print LR result table
  output$result_table = render_gt({
    debug("LR table")
    res = resultTable()
    validate(need(!is.null(res), "No LRs have been calculated yet."))
    KLINK:::prettyResultTable(res, linkedPairs(),
                              hide = input$emptymarkers == "hide",
                              likelihoods = input$likelihoods, decimals = input$decimals)
  }, width = "100%", align = "left")

  # Compute LR
  observeEvent(input$compute, {
    debug("compute LR")
    peds = req(pedigrees$reduced)

    if(is.null(linkageMap()))
      return(showNote("No marker map has been loaded."))

    res = tryCatch(error = showNote, {
      KLINK::linkedLR(pedigrees = peds,
                      linkageMap = linkageMap(),
                      linkedPairs = linkedPairs(),
                      markerData = markerData(),
                      mapfun = input$mapfunction,
                      verbose = FALSE, debug = FALSE,
                      lumpSpecial = TRUE)
    })

    resultTable(res)
    updateTabsetPanel(session, "tabs", selected = "LR table")
  })

  # Reset LR table
  observe({input$mapfunction; resultTable(NULL)})

  # Activate download button when LR table is ready
  observeEvent(resultTable(), {
    if(!is.null(resultTable()))
      shinyjs::enable("download")
    else
      shinyjs::disable("download")
  }, ignoreNULL = FALSE)

  prevLinkedPairs = reactiveVal(NULL)

  observe({
    currentPairs = linkedPairs()
    if(!identical(currentPairs, prevLinkedPairs())) {
      resultTable(NULL)
      prevLinkedPairs(currentPairs)
    }
  })

  # Marker data table -------------------------------------------------------

  markerData = reactive({
    debug("markerData")
    if(is.null(peds <- pedigrees$complete))
      return(NULL)
    mdat = KLINK::markerSummary(peds, replaceNames = is.null(XML()))
    req(mdat)

    # Sort according to map (NB: use normalised marker names to avoid Penta D/Penta.D/PentaD)
    ord = order(KLINK:::matchMarkernames(mdat$Marker, linkageMap()$Marker))
    mdat[ord, , drop = FALSE]
  })

  observeEvent(pedigrees$complete,
    updateTabsetPanel(session, "tabs", selected = "Marker data"))

  # Print loaded marker data
  output$marker_table = render_gt({
    debug("marker table")
    mtab = markerData()
    validate(need(!is.null(mtab), "No data has been loaded."))
    KLINK:::prettyMarkerTable(mtab, linkedPairs(), hide = input$emptymarkers == "hide")
  }, width = "100%", align = "left")


  # Linkage map table ----------------------------------------------------

  # Complete linkage map
  linkageMap = reactiveVal(NULL)
  mapname = reactiveVal(NULL)

  # Storage used when switching back to built-in
  customMap = reactiveValues(map = NULL, name = NULL)

  # React to Marker map radio selection
  observeEvent(input$maptype, {
    debug("maptype", input$maptype)

    map = if(input$maptype == "custom") customMap$map else get(input$maptype, "package:KLINK")
    name = if(input$maptype == "custom") customMap$name else "BUILT-IN"

    linkageMap(map)
    mapname(name)
  })

  # Change map file
  observeEvent(input$mapfile, {
    debug("mapfile")

    map = tryCatch(error = showNote, {
      KLINK::loadMap(req(input$mapfile$datapath))
    })

    if(is.null(map)) {
      customMap$map = customMap$name = NULL
      linkageMap(NULL)
      mapname(NULL)
      shinyjs::reset("mapfile")
      return()
    }

    name = input$mapfile$name
    customMap$map = map
    customMap$name = name

    linkageMap(map)
    mapname(name)

    resultTable(NULL)
    updateTabsetPanel(session, "tabs", selected = "Linkage map")
  })

  # Subset where only observed markers are included
  linkageMapSubset = reactive({
    debug("linkage map subset")
    fullmap = linkageMap()
    mdat = markerData()
    if(is.null(mdat))
      return(fullmap)
    idx = KLINK:::matchMarkernames(fullmap$Marker, mdat$Marker, nomatch = 0)
    fullmap[idx > 0, , drop = FALSE]
  })

  # Linked pairs
  linkedPairs = reactive({
    debug("linked pairs")
    getLinkedPairs(markerData()$Marker, linkageMap(), maxdist = req(input$maxdist))
  })

  # Karyogram plot
  output$karyo = renderPlot(KLINK:::karyogram(linkageMapSubset(),
                                              linkedPairs = linkedPairs()))

  # Print loaded genetic map
  output$linkage_table = render_gt({
    debug("linkage map table")
    map = linkageMapSubset()
    validate(need(!is.null(map), "No marker map has been loaded."))
    KLINK:::prettyLinkageMap(map, linkedPairs(), hide = input$emptymarkers == "hide", decimals = input$decimals)
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
                      "Familias file" = famfile$famname,
                      "Genetic map" = mapname(),
                      "Database" = famfile$params$dbName %||% "unknown",
                      "Map function" = input$mapfunction,
                      "Max distance" = input$maxdist)

      KLINK::writeResult(
        resultTable(),
        pedigrees = pedigrees$reduced,
        linkageMap = linkageMapSubset(),
        markerData = markerData(),
        outfile = file,
        notes = NOTES(),
        famname = famfile$famname,
        hideEmpty = input$emptymarkers == "hide",
        settings = settings,
        XML = XML())
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )


  # Reset -------------------------------------------------------------------

  observeEvent(input$reset, {
    shinyjs::reset("famfile")
    shinyjs::reset("xmlfile")
    shinyjs::reset("mapfile")
    famfile$famname = famfile$params = NULL
    customMap$map = customMap$name = NULL
    NOTES(NULL)
    XML(NULL)
    updateRadioButtons(session, "maptype", selected = "map50")
    updateNumericInput(session, "decimals", value = 3)
    updateNumericInput(session, "maxdist", value = 200)
    updateRadioButtons(session, "mapfunction", selected = "Kosambi")
    updateRadioButtons(session, "emptymarkers", selected = "hide")
    updateRadioButtons(session, "likelihoods", selected = "hide")
    pedigrees$complete = pedigrees$reduced = pedigrees$active = NULL
    resultTable(NULL)
  })

}

# Run the application
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))

