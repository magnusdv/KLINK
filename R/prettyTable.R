# -------------------------------------------------------------------------
# Formatting functions for main tables
# -------------------------------------------------------------------------

# Formatting common to all 3 tables
prepTable = function(tab, linkedPairs, hide = FALSE, size = "100%") {

  if(hide) {
    keep = tab$Typed > 0 | is.na(tab$Typed)
    tab = tab[keep, , drop  = FALSE]
  }

  tab$Pair = lp2vec(tab$Marker, linkedPairs) |> factor(levels = seq_along(linkedPairs))

  # Prepare GT table
  tabgt = gt(tab) |>
    opt_stylize(6) |>
    tab_options(data_row.padding = px(1),
                container.padding.y = px(3),
                table.font.size = size,
                table.layout = "auto",
                table.additional_css = ".gt_table {width: max-content !important}") |>
    tab_style(
      style = cell_text(size = pct(105)),
      locations = cells_column_labels()
    ) |>
    sub_missing(missing_text = "") |>
    tab_style(style = cell_text(whitespace = "nowrap"),
              locations = list(cells_body(), cells_column_labels())) |>
    tab_style(
      locations = cells_body(rows = Typed == 0),
      style = cell_text(color = "gray")
    ) |>
    tab_style_body(
      columns = Marker,
      style = cell_text(size = "90%"),
      values = "D22GATA198B05") |>
    cols_hide(c(Pair, Typed)) |>
    cols_move_to_start(annot) |>
    cols_label(annot = "") |>
    tab_style(
      locations = cells_body(columns = annot),
      style = list(cell_text(size = "75%", align = "center"),
                   "padding-right:0px; padding-left:0px;")
    ) |>
    data_color(
      columns = Pair,
      target_columns = annot,
      rows = !is.na(Pair),
      palette = KARYOPALETTE
    ) |>
    cols_width(annot ~ px(15))

  tabgt
}



# Actual tables -----------------------------------------------------------


prettyLinkageMap = function(map, linkedPairs = NULL, hide = FALSE, typed = NULL, decimals = 3) {
  # First column annot: Index
  map$annot = seq_len(nrow(map))
  map$Typed = NA_integer_

  prepTable(map, linkedPairs, hide = hide) |>
    fmt_number("cM", decimals = decimals) |>
    addTooltips()
}

prettyMarkerTable = function(mtab, linkedPairs = NULL, hide = FALSE, decimals = 3) {
  # First column annot: Uninformative for linkage
  uninf = mtab$Typed < 2 & matchMarkernames(mtab$Marker, unlist(linkedPairs), nomatch = 0L) > 0L
  mtab$annot = ifelse(uninf, "u", "")

  if(all(is.na(mtab$Model))) {
    mtab$Model = "None"
    mtab$Rate = mtab$Range = mtab$Rate2 = mtab$Stat = mtab$Lump = NULL
  }
  else if(!any(mtab$Model == "Stepwise", na.rm = TRUE)) {
    mtab$Range = mtab$Rate2 = NULL
  }

  prepTable(mtab, linkedPairs, hide = hide) |>
    fmt_number("PIC", decimals = decimals) |>
    addTooltips()
}

prettyResultTable = function(restab, linkedPairs = NULL, hide = FALSE, likelihoods = "show", decimals = 3) {
  if(is.null(restab) || nrow(restab) == 0)
    return("Nothing to show")

  LRcols = c("LRlinked", "LRnolink", "LRnomut")

  # Which likelihood columns to *hide*
  hideCols = switch(likelihoods,
    hide = c("Lik1", "Lik2", "Loglik1", "Loglik2"),
    show = c("Loglik1", "Loglik2"),
    loglik = c("Lik1", "Lik2"))

  extraDec = decimals - 3

  # First column annot: Uninformative for linkage
  uninf = restab$Typed < 2 & restab$Marker %in% unlist(linkedPairs)
  restab$annot = ifelse(uninf, "u", "")

  # Add totals
  restab = addTotals(restab, LRcols)

  prepTable(restab, linkedPairs, hide = hide) |>
    cols_label("LRsingle" = "LR", "LRnolink" = "No link",
               "LRlinked" = "Linked", "LRnomut" = "No mut") |>
    cols_hide(columns = c("Gindex", "Gsize", hideCols)) |>
    fmt_number(matches("^LR|^Loglik"), decimals = 3 + extraDec, rows = seq_len(length(Marker) - 1)) |>
    fmt_scientific(matches("^Lik[12]"), n_sigfig = 4 + extraDec, exp_style = "e") |>
    fmt(
      columns = LRcols, rows = length(Marker),
      fns = function(x) {
        if(!is.finite(x) || x <= 0)
          return(x)
        ifelse(abs(log10(x)) >= 4,
               vec_fmt_scientific(x, decimals = max(1,2 + extraDec), output = "html"),
               vec_fmt_number(x, n_sigfig = 4 + extraDec, use_seps = FALSE, output = "html"))
      }) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = list(
        cells_column_labels(columns = LRlinked),
        cells_body(columns = LRlinked),
        cells_body(columns = c(Marker, LRlinked), rows = length(Marker)))
    ) |>
    tab_style(
      style = list(cell_fill(color = "lightgray"),
                   cell_text(size = pct(110))),
      locations = cells_body(rows = length(Marker))
    ) |>
    tab_style_body(
      style = cell_text(color = "red"),
      columns = matches("^LR"),
      values = 0
    ) |>
    sub_values(matches("^LR"), fn = is.nan, replacement = "\u2014") |>
    addTooltips()
}


utils::globalVariables(c("LRlinked","Pair","Typed","Marker","annot"))

