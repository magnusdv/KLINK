
writeResult = function(linkageMap, markerData, resultTable, file, fam) {

  hs = createStyle(textDecoration = "bold")
  wb = buildWorkbook(list(linkageMap = linkageMap,
                          markerData = markerData,
                          LRtable = outputLRcomplete(resultTable)),
                     headerStyle = hs, colWidths = "auto")

  if(is.null(resultTable)) {
    saveWorkbook(wb, file = file)
    return()
  }

  # Report table
  report = outputLRreport(resultTable)
  nr = nrow(report) + 2 # top line + header
  nc = ncol(report)
  LRcol = match("LR", names(report))

  addWorksheet(wb, "Report")
  title = sprintf("Resultatrapport KLINK (fra %s)",
                  sub(".fam", "", basename(fam), fixed = TRUE))
  writeData(wb, sheet = "Report", title)
  writeData(wb, sheet = "Report", report, startRow = 2,
            headerStyle = createStyle(textDecoration = "bold", border = "LeftRight", borderColour = "gray85"),
            borders = "all", borderColour = "gray85")

  # Styling utility
  addSt = function(rows, cols, stack = TRUE, ...) {
    style = createStyle(...)
    addStyle(wb, "Report", style, rows = rows, cols = cols, gridExpand = TRUE, stack = stack)
  }

  # Merge & center LR column for linked pairs
  for(i in which(is.na(report$LR)) + 2) { # NB top line + header line
    mergeCells(wb, "Report", cols = LRcol, rows = c(i-1, i))
    #addSt(c = 2:(LRcol-1), r = i-1, border = "bottom", borderColour = "gray95")
  }

  # Overall title
  mergeCells(wb, "Report", cols = 1:LRcol, rows = 1)
  addSt(c = 1, r = 1, textDecoration = "bold", fontSize = 12)
  addSt(c = 2:LRcol, r = 2, fgFill = "gray95")

  # Single LRs small
  if(nc > LRcol)
    addSt(c = nc, r = 2:nr, fontSize = 9)

  addSt(c = c(1,LRcol:nc), r = 2:nr, halign = "center")
  addSt(c = LRcol, r = 3:nr, valign = "center")
  addSt(c = 1:nc, r = nr, textDecoration = "bold")
  mergeCells(wb, "Report", cols = 2:(LRcol-1), rows = nr)

  # Borders
  addSt(c = 1, r = 2:nr, borderStyle = "medium", border = "left")
  addSt(c = LRcol, r = 2:nr, borderStyle = "medium", border = "right")
  addSt(c = 1:LRcol, r = 2, borderStyle = "medium", border = "top")
  addSt(c = 1:LRcol, r = nr, borderStyle = "medium", border = "bottom")
  addSt(c = 2:LRcol, r = 2, borderStyle = "thin", border = "bottom")
  addSt(c = 2:LRcol, r = nr-1, borderStyle = "thin", border = "bottom")
  addSt(c = 2, r = 2:nr, borderStyle = "thin", border = "left")

  setColWidths(wb, "Report", c(1,nc), "auto", ignoreMergedCells = TRUE)
  setColWidths(wb, "Report", 2, "13.00")
  setColWidths(wb, "Report", 3:LRcol, "10.00")

  activeSheet(wb) = "Report"
  saveWorkbook(wb, file = file)
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
outputLRreport = function(resultTable) {
  if(is.null(res <- resultTable))
    return()

  names(res)[names(res) == "LRlinked"] = "LR"

  # Identify markers with missing data
  genoCols = grep("^Geno", names(res), value = TRUE)
  res$miss = apply(res, 1, function(v) all(v[genoCols] == "-/-"))

  # Incomplete pairs ...
  miss2 = res$miss & res$Gsize == 2
  res$incomp = res$Pair %in% unique(res$Pair[miss2])

  # ... convert to singlepoint
  if(any(miss2)) {
    res$LR[res$incomp] = res$LRsingle[res$incomp]
    res$Gindex[res$incomp] = res$Gsize[res$incomp] = 1L
  }

  # Put pairs at bottom, and incomplete pairs at very bottom
  res = res[order(res$Gsize, res$incomp), , drop = FALSE]

  res$LRsingle[res$Gsize == 1] = NA

  # Now remove all missing (whether paired or single)
  res = res[!res$miss, , drop = FALSE]

  # Prepare merge
  res$LR[res$Gindex > 1] = NA

  # Total (to be added below)
  total = prod(res$LR, na.rm = TRUE)

  # Select columns
  res$Idx = 1:nrow(res)
  res = res[c("Idx", "Marker", genoCols, "LR", "LRsingle")]

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
