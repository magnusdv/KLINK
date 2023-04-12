prettyTable = function(restable, style = 6) {
  res = restable |>
    gt(groupname_col = "Pair") |>
    opt_stylize(style = style) |>
    cols_hide(columns = c(Gindex, Gsize)) |>
    fmt_number(CMpos, decimals = 2) |>
    tab_style(
      style = cell_text(size = pct(95)),
      locations = cells_body(columns = starts_with("Geno"))
    ) |>
    tab_style(
      style = cell_text(size = pct(80), weight = "bold", style = "italic"),
      locations = cells_row_groups()
    ) |>
    tab_options(
      row_group.padding = px(1),
      data_row.padding = px(3)
    )

  if(!"LRlinked" %in% names(restable))
    return(res)

  LRcols = c("LRnolink", "LRlinked", "LRnomut")

  res |>
    cols_label(
      LRsingle ~ "LR",
      LRnolink ~ "Unlinked",
      LRlinked ~ md("**Linked**"),
      LRnomut  ~ "No mut"
    ) |>
    tab_spanner(
      label = "Combined LR / (log)",
      columns = LRcols
    ) |>
    grand_summary_rows(
      columns = LRcols,
      fns = list("Total LR" = ~ prod(.[Gindex == 1])),
      fmt = ~fmt_number(.),
      missing_text = ""
    ) |>
    fmt_number(LRsingle, decimals = 2) |>
    fmt_number(LRcols, rows = Gsize > 1, decimals = 3) |>
    fmt_number(LRcols, rows = Gindex == 2, pattern = "({x})") |>
    tab_style(
      style = cell_fill(color = "lightcyan"),
      locations = cells_body(columns = LRlinked)
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = list(
        cells_body(columns = LRlinked, rows = Gsize > 1 & Gindex == 1),
        cells_grand_summary(columns = LRlinked)
      )
    ) |>
    tab_style(
      style = cell_text(color = "gray50"),
      locations = cells_body(columns = LRcols, rows = Gindex > 1)
    ) |>
    tab_style(
      style = cell_text(size = pct(110)),
      locations = list(cells_grand_summary(), cells_stub_grand_summary())
    )
}


prettyMarkerTable = function(mtab) {
  mtab |> gt() |>
    opt_stylize(6) |>
    tab_style(style = cell_text(color = "gray"),
              locations = cells_body(rows = is.na(Pair))) |>
    sub_missing(missing_text = "") |>
    tab_style(
      style = cell_borders(sides = "left", style = "dashed"),
      locations = cells_body(columns = "Marker")
    ) |>
    tab_spanner(
      label = "Mutation model",
      columns = match("Model", names(mtab)):ncol(mtab)
    )
}

utils::globalVariables(c("CMpos","Gindex","Gsize","LRlinked","LRsingle","Pair"))
