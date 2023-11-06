
plotPed = function(x, cex = 1.2, marker = NULL, ...) {
  x = foldLabs(x)

  if(!is.null(marker) && marker == "")
    marker = NULL

  allLabs = unlist(labels(x))
  miss = grep(":missing:", allLabs, value = TRUE, fixed = TRUE)
  nonmiss = setdiff(allLabs, miss)

  iter = 0
  tryAgain = TRUE

  while(tryAgain) {
    iter = iter + 1
    if(iter > 1)
      cex = cex * 0.9

    tryCatch({
      plot(x, marker = marker, hatched = typedMembers, frames = FALSE, labs = nonmiss,
           col = list(red = miss), lwd = list("2" = miss), lty = list(dashed = miss),
           cex = cex, autoScale = TRUE, minsize = 0.15, ...)
      tryAgain = FALSE
    },
    error = function(e) {
      msg = conditionMessage(e)
      if(!grepl("reduce cex", msg))
        stop(msg, call. = FALSE)
      else if(iter == 10)
        stop("Autoscale fail: Pedigree too large for plot window.\n\n(This does not affect the LR calculations.)", call. = FALSE)
    })
  }
}
