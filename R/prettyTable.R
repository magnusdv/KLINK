prettyResultTable = function(restable, style = 6) {

  if(nrow(restable) == 0)
    return("Nothing to show")

  LRcols = c("LRnolink", "LRlinked", "LRnomut")
  gcols = (match("Marker", names(restable)) + 1):(match("PosCM", names(restable)) - 1)

  restable |>
    gt(groupname_col = "Pair") |>
    opt_stylize(style = style) |>
    tab_options(
      row_group.padding = px(1),
      data_row.padding = px(0),
      table.font.size = px(14)
    ) |>
    cols_hide(columns = c(Gindex, Gsize, PosCM)) |>
    tab_style(
      style = cell_text(size = pct(90)),
      locations = cells_body(columns = gcols)
    ) |>
    tab_style(
      style = cell_text(size = pct(85), weight = "bold", style = "italic"),
      locations = cells_row_groups()
    ) |>
    cols_label(
      LRsingle ~ "LR",
      LRnolink ~ "Unlinked",
      LRlinked ~ "Linked",
      LRnomut  ~ "No mut"
    ) |>
    tab_spanner(
      label = "Combined LR",
      columns = LRcols
    ) |>
    grand_summary_rows(
      columns = LRcols,
      fns = list("Total LR" = ~ prod(.[Gindex == 1])),
      fmt = list(~fmt_scientific(., columns = where(~ is.finite(.x) && .x > 0 && abs(log10(.x)) >= 4),
                                 decimals = 2),
                 ~fmt_number(., columns = where(~ is.finite(.x) && .x > 0 && abs(log10(.x)) < 4),
                             n_sigfig = 4, use_seps = FALSE)),
      missing_text = ""
    ) |>
    fmt_number(c("LRsingle", LRcols), decimals = 3) |>
    tab_style(
      style = cell_fill(color = "greenyellow"),
      locations = cells_grand_summary(columns = LRlinked)
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = list(
        cells_column_labels(columns = LRlinked),
        cells_body(columns = LRlinked),
        cells_grand_summary(columns = LRlinked),
        cells_stub_grand_summary()
      )
    ) |>
    tab_style(
      style = cell_text(size = pct(110)),
      locations = list(cells_grand_summary(), cells_stub_grand_summary())
    ) |>
    sub_missing(missing_text = "") |>
    tab_style(style = cell_fill(color = "azure3"),
              locations = cells_stub(rows = Gsize == 1))
}


prettyMarkerTable = function(mtab, linkedPairs = NULL) {
  mtab = cbind(Pair = factor(lp2vec(mtab$Marker, linkedPairs)),
               No = "",
               mtab)

  tab = gt(mtab) |>
    opt_stylize(6) |>
    tab_options(data_row.padding = px(1),
                table.font.size = if(nrow(mtab) > 32) "90%" else "95%") |>
    sub_missing(missing_text = "") |>
    tab_style(style = cell_text(whitespace = "nowrap"),
              locations = cells_body()) |>
    tab_style_body(
      columns = "Marker",
      style = cell_text(size = "80%"),
      values = "D22GATA198B05") |>
    cols_hide("Pair") |>
    cols_label(No = "") |>
    cols_width(No ~ px(10), Alleles ~ px(10)) |>
    fmt_number("PIC", decimals = 3)

  if(any(!is.na(mtab$Pair)))
    tab = data_color(tab,
      columns = Pair,
      target_columns = No,
      rows = !is.na(Pair),
      palette = KARYOPALETTE
    )

  tab
}

prettyLinkageMap = function(map, linkedPairs = NULL) {
  #linkedPairs = getLinkedPairs(map$Marker, map)
  map = cbind(Pair = factor(lp2vec(map$Marker, linkedPairs)),
              No = seq_along(map$Marker),
              map)

  tab = gt(map) |>
    opt_stylize(6) |>
    tab_options(data_row.padding = px(1),
                table.font.size = if(nrow(map) > 32) "90%" else "95%") |>
    tab_style(style = list(cell_text(size = "80%", align = "center"),
                           "padding-right:2px;padding-left:2px;"),
              locations = cells_body(columns = No)) |>
    tab_style(style = cell_text(whitespace = "nowrap"),
              locations = cells_body()) |>
    tab_style_body(columns = "Marker",
                   style = cell_text(size = "80%"),
                   values = "D22GATA198B05") |>
    cols_hide("Pair") |>
    cols_label(No = "")

  if(any(!is.na(map$Pair)))
    tab = data_color(tab,
      columns = Pair,
      target_columns = No,
      rows = !is.na(Pair),
      palette = KARYOPALETTE
    )

  tab
}

utils::globalVariables(c("PosCM","Gindex","Gsize","LRlinked","LRsingle","Pair", "No"))
