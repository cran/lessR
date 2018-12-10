ViolinPlot <-
function(...) {

  vp.fill <- getOption("violin.fill")
  do.sub <- FALSE

  #  allow fill parameter to pass to violin.fill
  lst <- as.list(match.call()) 
  if (!is.null(names(lst))) {  # at least one named argument
    for (i in 1:length(lst)) {
      if (names(lst[i]) == "fill") {
        vp.fill <- lst[[i]]
        do.sub <- TRUE
      }
    }
  }

  if (!do.sub)
    Plot(fun.call=match.call(), vbs.plot="v",  ...)
  else
    Plot(fun.call=match.call(), vbs.plot="v", violin.fill=vp.fill, ...)

}
