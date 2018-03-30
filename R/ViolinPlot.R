ViolinPlot <-
function(...) {

 cat("The violin plot is just a special case of the Plot function with\n",
     "vbs.plot=\"v\", i.e., only a violin plot instead of integrated\n",
     "with a box plot and a scatterplot (VBS plot)")
 cat("\n\n")
 Plot(fun.call=match.call(), vbs.plot="v", ...)

}
