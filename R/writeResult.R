
writeResult = function(linkageMap, markerData, resultTable, pedigrees,
                       notes, file, fam) {

  sheets = list("Linkage map" = linkageMap,
                "Marker data" = markerData %||% "No marker data loaded",
                "LR table" = outputLRcomplete(resultTable),
                Notifications = outputNotes(notes),
                Report = NULL,
                "Unlinked" = outputLRunlinked(resultTable),
                "Linked only" = outputLRlinkedOnly(resultTable)
  )

  hs = createStyle(textDecoration = "bold")
  wb = buildWorkbook(sheets, headerStyle = hs, colWidths = "auto")

  # Write report with special styling
  writeReportSheet(wb, resultTable, pedigrees = pedigrees, fam = fam)

  saveWorkbook(wb, file = file)
}

writeReportSheet = function(wb, resultTable, pedigrees, fam) {

  if(is.null(resultTable))
    return()

  # Report table
  report = outputLRreport(resultTable)
  nr = nrow(report) + 2 # top line + header
  nc = ncol(report)
  LRcol = match("LR", names(report))

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

  # Merge & centre LR column for linked pairs
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
    writeLegend(wb, relDf, c = legendCl, r = legendRw, fill = "#C7F6B6")
    legendRw = legendRw + length(rels) + 3
  }

  # Key to Person1, Person2, ...
  key = data.frame("Sample " = paste(ids, ""),
                   row.names = paste(idsPers, ""),
                   check.names = FALSE)
  writeLegend(wb, key, c = legendCl, r = legendRw, fill = "#ffffe0")

  setColWidths(wb, "Report", legendCl + 0:1, "auto")
  activeSheet(wb) = "Report"
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

outputLRunlinked = function(resultTable) {
  if(is.null(res <- resultTable))
    return("No likelihood ratios calculated")

  # Keep only these markers if paired
  keep = c("D5S2500", "SE33", "D8S1132", "D10S1435", "D11S2368", "vWA",
           "D18S51", "D19S253", "Penta D")

  res = removeMissing(res)

  # Columns
  cols = c("Marker", grep("^Person", names(res), value = TRUE), "LRsingle")

  res = res[res$Gsize == 1 | res$Marker %in% keep, cols, drop = FALSE]
  nr = nrow(res)
  if(nr == 0)
    return("No markers to report")

  res = cbind(Idx = seq_len(nr), res)

  # Add totals row
  res = rbind(res, NA)
  res$LRsingle[nr+1] = prod(res$LRsingle, na.rm = TRUE)
  res[nr+1, 1] = "Total LR"

  names(res)[1] = ""

  # Return
  res
}

# REFA report
outputLRreport = function(resultTable) {
  if(is.null(res <- resultTable))
    return()

  names(res)[names(res) == "LRlinked"] = "LR"

  # Identify markers with missing data
  genoCols = grep("^Person", names(res), value = TRUE)
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

outputNotes = function(notes) {
  if(is.null(notes))
    "No notifications recorded"
  else
    data.frame(Notifications = notes)
}

writeLegend = function(wb, df, r, c, fill) {
  hs2 = createStyle(textDecoration = "bold", border = "TopBottomLeftRight")
  writeData(wb, "Report", df, startCol = c, startRow = r, headerStyle = hs2,
            borders = "all", rowNames = TRUE)
  addStyle(wb, "Report", rows = r + 0:nrow(df), cols = c + 0:1, gridExpand = TRUE, stack = TRUE,
           style = createStyle(fgFill = fill))
}


# Inspired by manipulations inside `outputLRreport()`
removeMissing = function(resTable) {
  res = resTable

  # Identify markers with missing data
  genoCols = grep("^Person", names(res), value = TRUE)
  miss = apply(res, 1, function(v) all(v[genoCols] == "-/-"))

  # Incomplete pairs ...
  miss2 = miss & res$Gsize == 2
  incomp = res$Pair %in% unique(res$Pair[miss2])

  # ... convert to singlepoint
  if(any(miss2)) {
    res$LRlinked[incomp] = res$LRsingle[incomp]
    res$Gindex[incomp] = res$Gsize[incomp] = 1L
  }

  res[!miss, , drop = FALSE]
}
