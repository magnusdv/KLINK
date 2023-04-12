
writeResult = function(data, file) {
  hs = openxlsx::createStyle(textDecoration = "bold")
  openxlsx::write.xlsx(data, file = file, headerStyle = hs, colWidths = "auto")
}
