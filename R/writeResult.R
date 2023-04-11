
writeResult = function(data, file) {
  hs = createStyle(textDecoration = "bold")
  write.xlsx(data, file = file, headerStyle = hs, colWidths = "auto")
}
