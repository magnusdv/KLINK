#' @rdname maptools
#' @export
loadMap = function(path) {
  formatMsg = "\n\nThe map file must be a tab-separated text file or Excel file\ncontaining columns 'Marker', 'Chr', 'cM', and optionally 'Mb'."

  # File extension
  ext = tolower(tools::file_ext(path))

  # Read file
  map = tryCatch(
    if(ext == "xlsx")
      openxlsx::read.xlsx(path, sheet = 1, colNames = TRUE)
    else
      utils::read.table(path, header = TRUE, sep = "\t", colClasses = "character",
                        comment.char = "", quote = "", check.names = FALSE),
    error = function(e)
      stop2(sprintf("Error reading map file `%s`:\n%s", path, e$message), formatMsg)
  )

  # Ad hoc to avoid downstream issues
  if(ext == "xlsx")
    map[] = lapply(map, as.character)

  # Required columns
  req = c(Marker = "^marker", Chr = "^chr", cM = "^cm")

  # Check/fix column names
  orignames = names(map)
  lownames = tolower(orignames)
  for(cc in names(req)) {
    i = grep(req[cc], lownames)
    if(!length(i)) stop2(sprintf("Missing required column: %s", cc),
                         formatMsg, "\n\nColumns found in the file:\n",
                         paste(paste0(seq_along(orignames), ". ", orignames), collapse = "\n"))
    names(map)[i[1]] = cc
  }

  # Optional physical positions
  i = grep("^mb", lownames)
  if(length(i))
    names(map)[i[1]] = "Mb"

  # Chromosome names: chr1 -> 1
  map$Chr = suppressWarnings(as.integer(sub("chr", "", map$Chr, ignore.case = TRUE)))

  # Positions: Convert to numeric (allow comma as decimal separator)
  map$cM = suppressWarnings(as.numeric(chartr(",", ".", map$cM)))
  if("Mb" %in% names(map))
    map$Mb = suppressWarnings(as.numeric(chartr(",", ".", map$Mb)))

  # Sort rows
  map = map[order(map$Chr, map$cM, na.last = TRUE), , drop = FALSE]

  # Return df with standardised column names
  map
}
