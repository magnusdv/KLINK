#' Load genetic map
#'
#' @param path Path to a map file, which should contain columns `Marker`, `Chr`,
#'   `cM`. (Slight variations are allowed.)
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
  map = utils::read.table(path, header = TRUE, sep = "\t")

  # Required columns
  req = c(Marker = "^marker", Chr = "^chr", cM = "^(pos|cm)")

  # Check/fix column names
  lownames = tolower(names(map))
  for(cc in names(req)) {
    i = grep(req[cc], lownames)
    if(!length(i)) stop2(paste("Missing required column:", cc))
    names(map)[i[1]] = cc
  }

  if(!is.numeric(map$cM))
    stop2("Column `cM` is not numeric; please check the map file")

  # Return df with standardised column names
  map
}
