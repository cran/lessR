ViolinPlot <-
function(...) {

  vp_fill <- getOption("violin_fill")
  do.sub <- FALSE

  #  allow fill parameter to pass to violin_fill
  lst <- as.list(match.call()) 
  if (!is.null(names(lst))) {  # at least one named argument
    for (i in 1:length(lst)) {
      if (names(lst[i]) == "fill") {
        vp_fill <- lst[[i]]
        do.sub <- TRUE
      }
    }
  }

  if (!do.sub)
    Plot(fun_call=match.call(), vbs_plot="v",  ...)
  else
    Plot(fun_call=match.call(), vbs_plot="v", violin_fill=vp_fill, ...)

}
