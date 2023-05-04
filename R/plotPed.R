
plotPed = function(x, marker = NULL, ...) {
  x = foldLabs(x)

  if(!is.null(marker) && marker == "")
    marker = NULL

  allLabs = unlist(labels(x))
  miss = grep(":missing:", allLabs, value = TRUE, fixed = TRUE)
  nonmiss = setdiff(allLabs, miss)

  plotPedList(x, marker = marker, hatched = typedMembers, frames = FALSE, labs = nonmiss,
              col = list(red = miss), lwd = list("2" = miss), lty = list(dashed = miss), ...)
}
