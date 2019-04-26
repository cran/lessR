BoxPlot <-
function(...) {

  bx_fill <- getOption("box_fill")
  do.sub <- FALSE

  #  allow fill parameter to pass to box_fill
  lst <- as.list(match.call()) 
  if (!is.null(names(lst))) {  # at least one named argument
    for (i in 1:length(lst)) {
      if (names(lst[i]) == "fill") {
        bx_fill <- lst[[i]]
        do.sub <- TRUE
      }
    }
  }

  # still sets fill, which is pt_fill, but not relevant as points not plotted
  if (!do.sub)
    Plot(fun_call=match.call(), vbs_plot="b",  ...)
  else
    Plot(fun_call=match.call(), vbs_plot="b", box_fill=bx_fill, ...)

}
