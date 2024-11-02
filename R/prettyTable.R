# -------------------------------------------------------------------------
# Formatting functions for main tables
# -------------------------------------------------------------------------

# Formatting common to all 3 tables
prepTable = function(tab, linkedPairs, markerNos = FALSE, size = "100%") {

  tab$Pair = NULL # TODO

  # Linkage pair info
  lpFct = factor(lp2vec(tab$Marker, linkedPairs))
  tab = cbind(Pair = lpFct, No = if(markerNos) seq_along(lpFct) else "", tab)

  #fontSize = if(nrow(tab) > 32) "90%" else "95%"

  # Prepare GT table
  tabgt = gt(tab) |>
    opt_stylize(6) |>
    tab_options(data_row.padding = px(1),
                container.padding.y = px(3),
                table.font.size = size) |>
    tab_style(
      style = cell_text(size = pct(105)),
      locations = cells_column_labels()
    ) |>
    sub_missing(missing_text = "") |>
    tab_style(style = cell_text(whitespace = "nowrap"),
              locations = cells_body()) |>
    tab_style_body(
      columns = "Marker",
      style = cell_text(size = "90%"),
      values = "D22GATA198B05") |>
    cols_hide("Pair") |>
    cols_label(No = "") |>
    tab_style(
      locations = cells_body(columns = No),
      style = list(cell_text(size = "80%", align = "center"),
                   "padding-right:0px; padding-left:0px;")
    ) |>
    cols_width(No ~ px(15))

  # Colour linked pairs
  if(any(!is.na(lpFct)))
    tabgt = data_color(tabgt,
      columns = Pair,
      target_columns = No,
      rows = !is.na(Pair),
      palette = KARYOPALETTE
    )

  tabgt
}

prettyMarkerTable = function(mtab, linkedPairs = NULL) {
  prepTable(mtab, linkedPairs, markerNos = FALSE) |>
    fmt_number("PIC", decimals = 3)
}

prettyLinkageMap = function(map, linkedPairs = NULL) {
  prepTable(map, linkedPairs, markerNos = TRUE)
}

prettyResultTable = function(restable, linkedPairs = NULL) {
  if(is.null(restable) || (nr <- nrow(restable)) == 0)
    return("Nothing to show")

  LRcols = c("LRnolink", "LRlinked", "LRnomut")
  gcols = colsBetween(restable, "Marker", "PosCM")

  res = addTotals(restable, LRcols)

  prepTable(res, linkedPairs, markerNos = FALSE) |>
    cols_label(LRsingle ~ "LR", LRnolink ~ "Unlinked", LRlinked ~ "Linked",
               LRnomut  ~ "No mut") |>
    cols_hide(columns = c(Gindex, Gsize, PosCM)) |>
    tab_style(
      style = cell_borders(sides = "right", color = "lightgray", style = "dashed"),
      locations = cells_body(columns = "LRsingle")
    ) |>
    fmt_number(c("LRsingle", LRcols), decimals = 3, rows = 1:nr) |>
    fmt(
      columns = LRcols, rows = nr+1,
      fns = function(x) {
        if(!is.finite(x) || x <= 0)
          return(x)
        ifelse(abs(log10(x)) >= 4,
               vec_fmt_scientific(x, decimals = 2, output = "html"),
               vec_fmt_number(x, n_sigfig = 4, use_seps = FALSE, output = "html"))
      }) |>
    tab_style(
      style = cell_fill(color = "lightgray"),
      locations = cells_body(rows = nr + 1)
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = list(
        cells_column_labels(columns = LRlinked),
        cells_body(columns = LRlinked),
        cells_body(columns = c("Marker", "LRlinked"), rows = nr+1))
    ) |>
    tab_style(
      style = cell_text(size = pct(110)),
      locations = cells_body(rows = nr+1)
    ) |>
    sub_missing(missing_text = "")
}


utils::globalVariables(c("PosCM","Gindex","Gsize","LRlinked","LRsingle","Pair", "No"))
