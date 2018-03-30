ScatterPlot <-
function(...) {

 cat("ScatterPlot is now just a special case of the Plot function with\n",
     "vbs.plot=\"s\", i.e., only a scatterplot instead of integrated\n",
     "with a violin plot and a box plot (VBS plot)")
 cat("\n\n")
 Plot(fun.call=match.call(), vbs.plot="s", ...)

}
