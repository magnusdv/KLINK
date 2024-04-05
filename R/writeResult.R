#' Write data and results to Excel
#'
#' This function produces an Excel document containing the genotype data and
#' various LR tables.
#'
#' @param linkageMap A data frame.
#' @param markerData A data frame.
#' @param resultTable A data frame.
#' @param pedigrees A list of two `ped` objects.
#' @param notes A character vector.
#' @param outfile The output file name.
#' @param fam The name of the input `.fam` file.
#'
#' @return NULL
#'
#' @examples
#' \donttest{
#' # Built-in dataset `paternity`
#' peds = paternity
#' map = LINKAGEMAP
#' mdata = markerSummary(peds, map)
#'
#' # Result table
#' LRtab = linkedLR(pedigrees = peds, linkageMap = map, markerData = mdata)
#'
#' # Write to excel
#' tmp = paste0(tempfile(), ".xlsx")
#' writeResult(LRtab,
#'             pedigrees = peds,
#'             linkageMap = map,
#'             markerData = mdata,
#'             outfile = tmp)
#'
#' # openxlsx::openXL(tmp)
#' }
#' @export
writeResult = function(resultTable, pedigrees, linkageMap, markerData,
                       outfile, notes = NULL, fam = NULL) {

  sheets = list("Linkage map" = linkageMap,
                "Marker data" = markerData %||% "No marker data loaded",
                "LR table" = outputLRcomplete(resultTable),
                Notifications = outputNotes(notes),
                "Full report" = NULL,
                "Unlinked report" = NULL)

  hs = createStyle(textDecoration = "bold")
  wb = buildWorkbook(sheets, headerStyle = hs, colWidths = "auto")

  # Write main report with special styling
  report = outputLRreport(resultTable)
  writeReportSheet(wb, "Full report", report, pedigrees, fam, linked = TRUE)

  # Similar report with only unlinked markers
  reportUnl = outputLRunlinked(resultTable)
  writeReportSheet(wb, "Unlinked report", reportUnl, pedigrees, fam, linked = FALSE)

  activeSheet(wb) = "Full report"
  saveWorkbook(wb, file = outfile)
}

writeReportSheet = function(wb, sheet, report, pedigrees, fam, linked = TRUE) {

  if(is.null(report))
    return()

  nr = nrow(report) + 2 # top line + header
  nc = ncol(report)
  LRcol = match("LR", names(report))

  # Write main content
  title = "Resultatrapport KLINK"
  if(!is.null(fam))
    title = paste0(title, ": ", sub(".fam", "", basename(fam), fixed = TRUE))
  writeData(wb, sheet, title)
  writeData(wb, sheet, report, startRow = 2, borders = "all", borderColour = "gray85",
            headerStyle = createStyle(textDecoration = "bold", border = "LeftRight",
                                      borderColour = "gray85"))

  # Styling utility
  addSt = function(rows, cols, stack = TRUE, ...) {
    style = createStyle(...)
    addStyle(wb, sheet, style, rows = rows, cols = cols, gridExpand = TRUE, stack = stack)
  }

  # Overall title
  mergeCells(wb, sheet, cols = 1:LRcol, rows = 1)
  addSt(c = 1, r = 1, textDecoration = "bold", fontSize = 12)
  addSt(c = 2:LRcol, r = 2, fgFill = "gray95")

  if(linked) {
    # Merge & centre LR column for linked pairs
    for(i in which(is.na(report$LR)) + 2) { # NB top line + header line
      mergeCells(wb, sheet, cols = LRcol, rows = c(i-1, i))
      #addSt(c = 2:(LRcol-1), r = i-1, border = "bottom", borderColour = "gray95")
    }

    # Single LRs small
    if(nc > LRcol)
      addSt(c = nc, r = 2:nr, fontSize = 9)
  }

  addSt(c = c(1,LRcol:nc), r = 2:nr, halign = "center")
  addSt(c = LRcol, r = 3:nr, valign = "center")
  addSt(c = 1:nc, r = nr, textDecoration = "bold")
  mergeCells(wb, sheet, cols = 2:(LRcol-1), rows = nr)

  # Borders
  addSt(c = 1, r = 2:nr, borderStyle = "medium", border = "left")
  addSt(c = LRcol, r = 2:nr, borderStyle = "medium", border = "right")
  addSt(c = 1:LRcol, r = 2, borderStyle = "medium", border = "top")
  addSt(c = 1:LRcol, r = nr, borderStyle = "medium", border = "bottom")
  addSt(c = 2:LRcol, r = 2, borderStyle = "thin", border = "bottom")
  addSt(c = 2:LRcol, r = nr-1, borderStyle = "thin", border = "bottom")
  addSt(c = 2, r = 2:nr, borderStyle = "thin", border = "left")

  setColWidths(wb, sheet, c(1,nc), "auto", ignoreMergedCells = TRUE)
  setColWidths(wb, sheet, 2, "13.00")
  setColWidths(wb, sheet, 3:LRcol, "10.00")

  ### Additional legends
  legendRw = 2
  legendCl = LRcol + 3
  ids = typedMembers(pedigrees[[1]])
  idsPers = paste0("Person", seq_along(ids))

  # Relationships
  if(length(ids) == 2) {
    rels = lapply(pedigrees, function(ped) {
      pednew = relabel(ped, old = ids, new = idsPers)
      r = verbalisr::verbalise(pednew, idsPers) |> format(includePaths = FALSE)
      s = sub(".*: ", "", r) # remove "Lineal of degree 1: " etc
      paste(s, collapse = " AND ")
    })
    relDf = data.frame(Relationship = as.character(rels), row.names = paste("Ped", 1:2))
    writeLegend(wb, sheet, relDf, c = legendCl, r = legendRw, fill = "#C7F6B6")
    legendRw = legendRw + length(rels) + 3
  }

  # Key to Person1, Person2, ...
  key = data.frame("Sample " = paste(ids, ""),
                   row.names = paste(idsPers, ""),
                   check.names = FALSE)
  writeLegend(wb, sheet, key, c = legendCl, r = legendRw, fill = "#ffffe0")

  setColWidths(wb, sheet, legendCl + 0:1, "auto")
}

outputLRcomplete = function(resultTable) {
  if(is.null(res <- resultTable))
    return("No likelihood ratios calculated")

  LRcols = c("LRnolink",	"LRlinked",	"LRnomut")
  res[res$Gindex > 1, LRcols] = NA
  res$Gindex = res$Gsize = NULL

  # add totals row
  res = rbind(res, NA)
  res[nrow(res), LRcols] = apply(res[LRcols], 2, prod, na.rm = TRUE)
  res[nrow(res), 1] = "Total LR"

  # Return
  res
}


# REFA report
outputLRreport = function(resultTable) {
  if(is.null(res <- resultTable))
    return()

  # Remove missing & handle linkage pairs
  res = removeMissing(res)

  # Mute single-LR column for unlinked markers
  res$LRsingle[res$Gsize == 1] = NA

  # Prepare merge
  res$LRlinked[res$Gindex > 1] = NA

  # Total (to be added below)
  total = prod(res$LRlinked, na.rm = TRUE)

  # Select columns
  res$Idx = 1:nrow(res)
  genoCols = grep("^Person", names(res), value = TRUE)
  res = res[c("Idx", "Marker", genoCols, "LRlinked", "LRsingle")]
  names(res)[names(res) == "LRlinked"] = "LR"

  # Round to 3 decimals
  res$LR = ifelse(is.na(res$LR), NA_character_, sprintf("%.3f", res$LR))
  res$LRsingle = ifelse(is.na(res$LRsingle), NA_character_, sprintf("%.3f", res$LRsingle))

  # Add total
  res = rbind(res, NA)
  res$Marker[nrow(res)] = "Total LR"
  res$LR[nrow(res)] = sprintf("%.4g", total)

  # Fix names
  names(res)[c(1,ncol(res))] = ""
  rownames(res) = NULL

  # Return
  res
}


# REFA report 2: Unlinked only
outputLRunlinked = function(resultTable) {

  if(is.null(res <- resultTable))
    return(NULL)

  # Keep only these markers if paired
  keep = c("D5S2500",  # not D5S2800
           "SE33",     # not D6S474
           "D8S1132",  # not D8S1179
           "D10S2325", # not D10S1435
           "D11S554",  # not D11S2368
           "D12S391",  # not vWA
           "D18S51",   # not D18S1364
           "D19S253",  # not D19S433
           "D21S2055"  # not Penta D
           )

  # Remove markers with no data (and make linked leftovers into singles)
  res = removeMissing(res)

  # Select unlinked markers
  res = res[res$Gsize == 1 | res$Marker %in% keep, , drop = FALSE]
  nr = nrow(res)
  if(nr == 0)
    return("No markers to report")

  # Columns
  res$Idx = 1:nrow(res)
  genoCols = grep("^Person", names(res), value = TRUE)
  res = res[c("Idx", "Marker", genoCols, "LRsingle")]
  names(res)[c(1, ncol(res))] = c("", "LR")

  # Total (to be added below)
  total = prod(res$LR, na.rm = TRUE)

  # Round to 3 decimals
  res$LR = ifelse(is.na(res$LR), NA_character_, sprintf("%.3f", res$LR))

  # Add totals row
  res = rbind(res, NA)
  res$Marker[nrow(res)] = "Total LR"
  res$LR[nrow(res)] = sprintf("%.4g", total)

  rownames(res) = NULL

  # Return
  res
}

outputNotes = function(notes) {
  if(is.null(notes))
    "No notifications recorded"
  else
    data.frame(Notifications = notes)
}

writeLegend = function(wb, sheet, df, r, c, fill) {
  hs2 = createStyle(textDecoration = "bold", border = "TopBottomLeftRight")
  writeData(wb, sheet, df, startCol = c, startRow = r, headerStyle = hs2,
            borders = "all", rowNames = TRUE)
  addStyle(wb, sheet, rows = r + 0:nrow(df), cols = c + 0:1, gridExpand = TRUE, stack = TRUE,
           style = createStyle(fgFill = fill))
}


# Removes markers with no data after sorting. (Ensures same order in both REFA reports.)
removeMissing = function(resTable) {
  res = resTable

  # Identify markers with missing data
  genoCols = grep("^Person", names(res), value = TRUE)
  res$miss = apply(res, 1, function(v) all(v[genoCols] == "-/-"))

  # Incomplete pairs ...
  miss2 = res$miss & res$Gsize == 2
  incomp = res$Pair %in% unique(res$Pair[miss2])

  # ... convert to singlepoint
  if(any(miss2)) {
    res$LRlinked[incomp] = res$LRsingle[incomp]
    res$Gindex[incomp] = res$Gsize[incomp] = 1L
  }

  # Put pairs at bottom, and incomplete pairs at very bottom
  res = res[order(res$Gsize, incomp), , drop = FALSE]

  res[!res$miss, , drop = FALSE]
}


outputLRlinkedOnly = function(resultTable) {
  if(is.null(res <- resultTable))
    return("No likelihood ratios calculated")

  res = removeMissing(res)

  LRcols = c("LRlinked")
  res = res[res$Gsize == 2, , drop = FALSE]
  res[res$Gindex > 1, LRcols] = NA
  res$Gindex = res$Gsize = res$PosCM = res$LRnolink = res$LRnomut = NULL

  nr = nrow(res)
  if(nr == 0)
    return("No markers to report")

  # Add totals row
  res = rbind(res, NA)
  res$LRsingle[nr+1] = prod(res$LRsingle, na.rm = TRUE)
  res$LRlinked[nr+1] = prod(res$LRlinked, na.rm = TRUE)
  res[nr+1, 1] = "Total LR"

  # Return
  res
}
