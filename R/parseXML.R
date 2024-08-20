#' Parse XML file associated with .fam file
#'
#' @param xml Path to a file with extension .xml.
#'
#' @examples
#' # (No example included)
#'
#' @importFrom xml2 read_xml xml_ns_strip xml_find_all xml_find_first xml_text
#' @export
parseXML = function(xml) {

  if(is.list(xml)) {
    xmlname = xml$name
    xmlpath = xml$datapath
  }
  else {
    xmlname = xmlpath = xml
  }

  if(tools::file_ext(xmlname) != "xml")
    stop2("The selected file must have extension .xml")

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
  do.call(rbind, alldata) |> as.data.frame(check.names = FALSE)
}
