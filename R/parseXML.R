#' Parse XML file associated with .fam file
#'
#' @param xml Path to a file with extension .xml.
#' @param famname (Optional) The basename of the associated .fam file, used to
#'   test that the files belong together.
#' @param famids (Optional) A character of the ID strings recorded in the .fam
#'   file. These should also be present in the XML file.
#'
#' @examples
#' # (No example included)
#'
#' @importFrom xml2 read_xml xml_ns_strip xml_find_all xml_find_first xml_text
#' @export
parseXML = function(xml, famname = NULL, famids = NULL) {

  if(is.list(xml)) {
    xmlname = xml$name
    xmlpath = xml$datapath
  }
  else {
    xmlname = xmlpath = xml
  }

  if(tools::file_ext(xmlname) != "xml")
    stop2("The selected file must have extension .xml")

  if(is.null(famname))
    stop2("Familias file must be loaded first")

  if(sub(".xml", "", xmlname) != sub(".fam", "", famname))
    stop2(paste("File names do not match:", xmlname, famname, sep = "<br>"))

  x = xml2::read_xml(xmlpath)
  xml2::xml_ns_strip(x)

  # Locate all individuals in file
  specimens = xml2::xml_find_all(x, ".//SPECIMEN")

  parsedata = function(spec) {
    id = spec |> xml2::xml_find_first(".//SPECIMENID") |> xml2::xml_text()
    initials = spec |> xml2::xml_find_first(".//SPECIMENINITIALS") |> xml2::xml_text()

    # Locate Amelogenin locus
    amel = spec |> xml2::xml_find_first(".//LOCUS[LOCUSNAME='Amelogenin']")

    # Extract alleles of Amelogenin
    alleles = amel |> xml2::xml_find_all(".//ALLELEVALUE") |> xml2::xml_text()

    c(ID = id, Initials = initials, AMEL = paste(alleles, collapse = "-"))
  }

  # Apply function to each specimen (individual)
  alldata = lapply(specimens, parsedata)

  # Convert to data frame
  res = do.call(rbind, alldata) |> as.data.frame()

  # Check typed members
  if(!is.null(famids)) {
    xmlids = res$ID
    if(!setequal(famids, xmlids))
      stop2(paste(c("Individuals in XML file do not match .fam file:", xmlids), collapse = "<br>"))

    # Enforce same order
    res = res[match(famids, xmlids), , drop = FALSE]
  }

  res
}
