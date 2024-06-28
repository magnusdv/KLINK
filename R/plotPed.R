
plotPed = function(x, cex = 1.2, marker = NULL, margins = c(2,3,2,3), ...) {
  if(!is.null(marker) && marker == "")
    marker = NULL

  allLabs = labels(x)
  miss = grep(":missing:", allLabs, value = TRUE, fixed = TRUE)
  nonmiss = setdiff(allLabs, miss)

  iter = 0
  tryAgain = TRUE

  while(tryAgain) {
    iter = iter + 1
    if(iter > 1) {
      cex = cex * 0.8
      margins = margins * 0.8
    }
    tryCatch({
      plot(x, marker = marker, hatched = typedMembers, frames = FALSE,
           labs = nonmiss, foldLabs = 10, col = list(red = miss), showEmpty = TRUE,
           lwd = list("2" = miss), lty = list(dashed = miss),
           cex = cex, autoScale = TRUE, minsize = 0.2, margins = margins, ...)
      tryAgain = FALSE
    },
    error = function(e) {
      msg = conditionMessage(e)
      if(!grepl("reduce cex|minsize", msg))
        stop(msg, call. = FALSE)
      else if(iter == 10)
        stop("Autoscale fail: Pedigree too large for plot window.\n\n(This does not affect LR calculations.)", call. = FALSE)
    })
  }
}
