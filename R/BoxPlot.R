BoxPlot <-
function(...) {

 cat("BoxPlot is now just a special case of the Plot function with\n",
     "vbs.plot=\"b\", i.e., only a box plot instead of integrated\n",
     "with a violin plot and a scatterplot (VBS plot)")
 cat("\n\n")
 Plot(fun.call=match.call(), vbs.plot="b", ...)

}
