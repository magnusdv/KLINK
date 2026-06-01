#' Load genetic map
#'
#' Loads a genetic map from a tab-separated text file, or from the first sheet
#' of an Excel file with extension `.xlsx`.
#'
#' The file should contain columns `Marker`, `Chr` and `cM` (slight
#' variations in column names are allowed). Chromosomes should be positive
#' integers, optionally prefixed by "chr". Positions must be nonnegative
#' numeric values. Both period and comma are accepted as decimal separators.
#'
#' @param path Path to the map file, either as plain text or Excel.
#'
#' @returns A data frame with standardised column names and rows sorted by
#'   chromosome and position.
#'
#' @examples
#' tmp = tempfile(fileext = ".map")
#' map1 = as.data.frame(norSTR::map50)
#' write.table(map1, tmp, sep = "\t", quote = FALSE, row.names = FALSE)
#'
#' map2 = loadMap(tmp)
#' stopifnot(all.equal(map1, map2))
#'
#' @export
loadMap = function(path) {
  formatMsg = "\n\nThe map file must be tab-separated and contain\nthe following named columns: Marker, Chr, cM."

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
  req = c(Marker = "^marker", Chr = "^chr", cM = "^(pos|cm)")

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

  # Chrom labels
  if(any(is.na(map$Chr) | map$Chr == ""))
    stop2("Missing elements in chromosome column; please check the map file", formatMsg)

  # Chromosome names: chr1 -> 1
  chr = suppressWarnings(as.integer(sub("chr", "", map$Chr, ignore.case = TRUE)))
  badchr = is.na(chr) | chr <= 0
  if(any(badchr))
    stop2("Illegal chromosome label: ", map$Chr[badchr][1],
          "\n\nChrom names must be positive integers, or with prefix 'chr'")
  map$Chr = chr

    # Positions: Convert to numeric (allow comma as decimal separator)
  cm = suppressWarnings(as.numeric(chartr(",", ".", map$cM)))

  badpos = is.na(cm) | !is.finite(cm) | cm < 0
  if(any(badpos))
    stop2("Illegal position value: ", map$cM[badpos][1], formatMsg)
  map$cM = cm

  # Sort rows
  map = map[order(chr, map$cM), , drop = FALSE]

  # Return df with standardised column names
  map
}
