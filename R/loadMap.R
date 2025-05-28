#' Load genetic map
#'
#' @param path Path to a map file, which should contain tab-separated columns
#'   `Marker`, `Chr`, `cM`. (Slight variations are allowed.)
#'
#' @returns A data frame.
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

  tryCatch(
    map <- utils::read.table(path, header = TRUE, sep = "\t"),
    error = function(e)
      stop2(sprintf("Error reading map file `%s`:\n%s", path, e$message), formatMsg)
  )

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

  if(any(is.na(map$Chr) | map$Chr == ""))
    stop2("Missing elements in chromosome column; please check the map file", formatMsg)

  if(!is.numeric(map$cM) || !all(is.finite(map$cM)))
    stop2("Non-numeric elements in position column; please check the map file")
  if(any(map$cM<0))
    stop2("Negative elements in position column; please check the map file")

  # Chromosome names: chr1 -> 1
  chr = suppressWarnings(as.integer(sub("chr", "", map$Chr, ignore.case = TRUE)))
  badchr = is.na(chr) | chr <= 0
  if(any(badchr))
    stop2("Illegal chromosome found in map file: ", map$Chr[badchr][1],
          "\n\nChrom names must be positive integers, or with prefix 'chr'")
  map$Chr = chr

  # Sort rows
  map = map[order(chr, map$cM), , drop = FALSE]

  # Return df with standardised column names
  map
}
