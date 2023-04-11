
plotPed = function(x, marker = NULL, ...) {
  x = foldLabs(x)
  if(!is.null(marker) && marker == "") marker = NULL
  miss = grep(":missing:", unlist(labels(x)), value = TRUE, fixed = TRUE)

  plotPedList(x, marker = marker, hatched = typedMembers, frames = FALSE,
              col = list(red = miss), ...)
}
