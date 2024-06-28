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
#' @param famname The name of the input `.fam` file.
#' @param settings A list of KLINK settings to be included in the output
#' @param XML Optional data from .xml file.
#'
#' @return NULL
#'
#' @examples
#' \donttest{
#' # Built-in dataset `paternity`
#' peds = paternity
#' map = LINKAGEMAP
#' mdata = markerSummary(peds)
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
                       outfile, notes = NULL, famname = NULL,
                       settings = NULL, XML = NULL) {

  LRtable = outputLRcomplete(resultTable)

  sheets = list("Linkage map" = linkageMap,
                "Marker data" = markerData %||% "No marker data loaded",
                "LR table" = LRtable,
                Notifications = outputNotes(notes),
                "Full report" = NULL,
                "Unlinked report" = NULL)

  hs = createStyle(textDecoration = "bold")
  wb = buildWorkbook(sheets, headerStyle = hs, colWidths = "auto")

  if(is.null(markerData)) {
    saveWorkbook(wb, file = outfile)
    return()
  }

  if(!is.null(XML)) {
    idsLong = XML$ID
    idsShort = XML$Initials # = typedMembers(pedigrees[[1]])
  }
  else {
    idsLong = idsShort = typedMembers(pedigrees[[1]])
    if("Person1" %in% names(resultTable)) {
      idsShort = paste0("Person", seq_along(idsLong))
      pedigrees = lapply(pedigrees, relabel, old = idsLong, new = idsShort) # for rels
    }
  }

  # By now, table and pedigrees all have short names; long names only exist here:
  nameKeys = setnames(idsLong, idsShort)

  # Write main report with special styling
  report = outputLRreport(resultTable, gcols = idsShort, AMEL = XML$AMEL)
  writeReportSheet(wb, "Full report", report, pedigrees, famname, nameKeys, settings, notes, linked = TRUE)

  # Similar report with only unlinked markers
  pic = setnames(markerData$PIC, markerData$Marker)
  reportUnl = outputLRunlinked(resultTable, gcols = idsShort, AMEL = XML$AMEL, pic = pic)
  writeReportSheet(wb, "Unlinked report", reportUnl, pedigrees, famname, nameKeys, settings, notes, linked = FALSE)

  # REFA request: add LR column from unlinked report to "LR table" sheet
  addUnlinkedColumn(wb, LRtable, reportUnl, hs = hs)

  activeSheet(wb) = "Full report"
  saveWorkbook(wb, file = outfile, overwrite = T)
}

writeReportSheet = function(wb, sheet, report, pedigrees, famname, nameKeys,
                            settings, notes, linked = TRUE, norsk = TRUE) {

  if(is.null(report))
    return()

  # Styling utility
  addSt = function(rows, cols, stack = TRUE, ...) {
    style = createStyle(...)
    addStyle(wb, sheet, style, rows = rows, cols = cols,
             gridExpand = TRUE, stack = stack)
  }

  nc = ncol(report)
  nr = nrow(report) + 2 # top line + header
  LRcol = match("LR", names(report))

  # Write main content
  title = if(norsk) "Resultatrapport" else "Results Report"
  if(!is.null(famname))
    title = paste0(title, ": ", sub(".fam", "", basename(famname), fixed = TRUE))
  writeData(wb, sheet, title)

  # Main table
  report2 = report
  if(norsk)
    names(report2)[names(report2) == "Marker"] = "Mark\u00F8r"

  writeData(wb, sheet, report2, startRow = 2, borders = "all", borderColour = "gray85",
            headerStyle = createStyle(textDecoration = "bold", border = "LeftRight",
                                      borderColour = "gray85"))

  # Overall title
  mergeCells(wb, sheet, cols = 1:LRcol, rows = 1)
  addSt(c = 1, r = 1, textDecoration = "bold", fontSize = 12)
  addSt(c = 2:LRcol, r = 2, fgFill = "gray95")

  if(linked) {
    naLR = is.na(report$LR) & !startsWith(report$Marker, "AMEL")

    # Merge & centre LR column for linked pairs
    for(i in which(naLR) + 2)  # skip top line, header
      mergeCells(wb, sheet, cols = LRcol, rows = c(i-1, i))

    # Single LRs small
    if(nc > LRcol)
      addSt(c = nc, r = 2:nr, fontSize = 9)
  }

  addSt(c = seq_len(nc)[-2], r = 1:nr, halign = "center")
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

  # Key to IDs
  idLegend = getIdLegend(nameKeys)
  writeLegend(wb, sheet, idLegend, c = legendCl, r = legendRw, fill = "#C7E4F6")
  legendRw = legendRw + nrow(idLegend) + 3

  # Relationships
  relLegend = getRelLegend(pedigrees, ids)
  writeLegend(wb, sheet, relLegend, c = legendCl, r = legendRw, fill = "#C7F6B6")
  legendRw = legendRw + nrow(relLegend) + 3

  # Notes
  noteLegend = getNoteLegend(notes)
  writeLegend(wb, sheet, noteLegend, c = legendCl, r = legendRw, fill = "#F6C7C7")
  legendRw = legendRw + nrow(noteLegend) + 3

  # Settings
  settingsLegend = getSettingsLegend(settings)
  writeLegend(wb, sheet, settingsLegend, c = legendCl, r = legendRw, fill = "#ffffe0")
  legendRw = legendRw + nrow(settingsLegend) + 3

  setColWidths(wb, sheet, legendCl + 0:1, "auto")
}

outputLRcomplete = function(resultTable) {
  if(is.null(res <- resultTable))
    return()

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
outputLRreport = function(resultTable, gcols, AMEL = NULL) {

  # Remove missing & handle linkage pairs
  res = removeMissing(resultTable, gcols)

  # Mute single-LR column for unlinked markers
  res$LRsingle[res$Gsize == 1] = NA

  # Prepare merge
  res$LRlinked[res$Gindex > 1] = NA

  # Total (to be added below)
  total = prod(res$LRlinked, na.rm = TRUE)

  # Select columns
  res$Idx = 1:nrow(res)
  res = res[c("Idx", "Marker", gcols, "LRlinked", "LRsingle")]
  names(res)[names(res) == "LRlinked"] = "LR"

  # Change allele separators to "-"
  res[gcols] = lapply(res[gcols], \(x) sub("/", "-", x))

  # Round to 3 decimals
  res$LR = ifelse(is.na(res$LR), NA_character_, sprintf("%.3f", res$LR))
  res$LRsingle = ifelse(is.na(res$LRsingle), NA_character_, sprintf("%.3f", res$LRsingle))

  # Add total
  res = rbind(res, NA)
  res$Marker[nrow(res)] = "Total LR"
  res$LR[nrow(res)] = sprintf("%.4g", total)

  # Add AMEL?
  if(!is.null(AMEL))
    res = addAMEL(res, AMEL)

  # Fix names
  names(res)[c(1,ncol(res))] = ""
  rownames(res) = NULL

  # Return
  res
}


# REFA report 2: Unlinked only
outputLRunlinked = function(resultTable, gcols, AMEL = NULL, pic) {

  # Remove markers with no data (and make linked leftovers into singles)
  res = removeMissing(resultTable, gcols)

  # From each pair, keep marker with highest PIC
  r = resultTable[resultTable$Gsize == 2, , drop = FALSE]
  keep = sapply(split(r, r$Pair), function(sub) sub$Marker[which.max(pic[sub$Marker])])

  # Select unlinked markers
  res = res[res$Gsize == 1 | res$Marker %in% keep, , drop = FALSE]
  nr = nrow(res)
  if(nr == 0)
    return("No markers to report")

  # Columns
  res$Idx = 1:nrow(res)
  res = res[c("Idx", "Marker", gcols, "LRsingle")]
  names(res)[ncol(res)] = "LR"

  # Total (to be added below)
  total = prod(res$LR, na.rm = TRUE)

  # Change allele separator to "-"
  res[gcols] = lapply(res[gcols], \(x) sub("/", "-", x))

  # Round to 3 decimals
  res$LR = ifelse(is.na(res$LR), NA_character_, sprintf("%.3f", res$LR))

  # Add totals row
  res = rbind(res, NA)
  res$Marker[nrow(res)] = "Total LR"
  res$LR[nrow(res)] = sprintf("%.4g", total)

  # Add AMEL?
  if(!is.null(AMEL))
    res = addAMEL(res, AMEL)

  names(res)[1] = ""
  rownames(res) = NULL

  # Return
  res
}


# Removes markers with no data after sorting. (Ensures same order in both REFA reports.)
removeMissing = function(resTable, gcols) {
  res = resTable

  # Identify markers with missing data
  res$miss = apply(res, 1, function(v) all(v[gcols] == "-/-"))

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


addAMEL = function(report, AMEL) {
  amelRow = c(NA, "AMELOGENIN", AMEL)
  length(amelRow) = ncol(report)
  names(amelRow) = names(report)
  rbind(as.data.frame(as.list(amelRow)), report)
}

outputNotes = function(notes) {
  if(is.null(notes))
    "No notifications recorded"
  else
    data.frame(Notifications = notes)
}

### For legends

writeLegend = function(wb, sheet, df, r, c, fill) {
  hs2 = createStyle(textDecoration = "bold", border = "TopBottomLeftRight")

  writeData(wb, sheet, df, startCol = c, startRow = r, headerStyle = hs2,
            borders = "all", rowNames = TRUE)
  addStyle(wb, sheet, rows = r + 0:nrow(df), cols = c + 0:1,
           gridExpand = TRUE, stack = TRUE, style = createStyle(fgFill = fill))

  # Numeric row names: small and right-aligned
  if(rownames(df)[1] == "1")
    addStyle(wb, sheet, rows = r + 0:nrow(df), cols = c, stack = TRUE,
             style = createStyle(halign = "right", fontSize = 10))
}

getIdLegend = function(nameKeys) {
  idsLong = as.character(nameKeys)
  idsShort = names(nameKeys)
  data.frame("Samples " = paste(idsLong, ""),
             row.names = paste(idsShort, ""),
             check.names = FALSE)
}

getRelLegend = function(pedigrees, ids) {
  if(length(ids) != 2)
    return(data.frame(Relationship = "No output (only for 2 individuals)"))

  rels = lapply(pedigrees, function(ped) {
    s = verbalisr::verbalise(ped, ids) |>
      format(includePaths = FALSE, simplify = TRUE)
    paste(s, collapse = " AND ")
  })

  data.frame("Relationship " = as.character(rels), row.names = paste("Ped", 1:2),
             check.names = FALSE)
}

getNoteLegend = function(notes) {
  data.frame("Notifications " = notes %||% "No notifications recorded",
             check.names = FALSE)
}

getSettingsLegend = function(settings) {
  if(is.null(settings))
    return(data.frame("Settings " = "No settings included", check.names = FALSE))

  # A few tweaks
  if(!is.null(dist <- settings[["Max distance"]]))
    settings[["Max distance"]] = paste(dist, "cM")

  if(identical(settings[["Genetic map"]], "LINKAGEMAP"))
    settings[["Genetic map"]] = "Built-in"

  # Convert to data frame
  s = paste(names(settings), settings, sep = ": ")
  data.frame("Settings " = s, check.names = FALSE)
}


addUnlinkedColumn = function(wb, LRtable, reportUnl, hs = NULL) {
  lrs = setnames(reportUnl$LR, reportUnl$Marker)
  newcol = lrs[LRtable$Marker]
  newcol[length(newcol)] = lrs["Total LR"]
  df = data.frame("LR.unl.report" = newcol)

  # Insert two columns away
  sheet = "LR table"
  cl = ncol(LRtable) + 2

  writeData(wb, sheet, x = df, startCol = cl, headerStyle = hs)
  addStyle(wb, sheet, cols = cl, rows = 1:(nrow(LRtable)+1), stack = TRUE,
           style = createStyle(halign = "center"))
  setColWidths(wb, sheet, cl, "12.14")

}
