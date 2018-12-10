BoxPlot <-
function(...) {

  bx.fill <- getOption("box.fill")
  do.sub <- FALSE

  #  allow fill parameter to pass to box.fill
  lst <- as.list(match.call()) 
  if (!is.null(names(lst))) {  # at least one named argument
    for (i in 1:length(lst)) {
      if (names(lst[i]) == "fill") {
        bx.fill <- lst[[i]]
        do.sub <- TRUE
      }
    }
  }

  # still sets fill, which is pt.fill, but not relevant as points not plotted
  if (!do.sub)
    Plot(fun.call=match.call(), vbs.plot="b",  ...)
  else
    Plot(fun.call=match.call(), vbs.plot="b", box.fill=bx.fill, ...)

}
