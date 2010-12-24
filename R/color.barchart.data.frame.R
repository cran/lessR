color.barchart.data.frame <-
function(x, ...)  {

  fname <- paste("BarCharts.",deparse(substitute(x)),".pdf",sep="")
  pdf(file=fname)

  for (i in 1:ncol(x))
    if (!is.numeric(x[,i])) {
      tlbl <- paste("Bar Chart for", names(x)[i])
      if (nlevels(factor(x[,i])) < length(x[,i]))
        color.barchart.default(x[,i], xlab=names(x)[i], main=tlbl, font.main=1, ...)
      else cat("\n", names(x)[i], "appears to contain unique Names or IDs", "\n")
    }
  
  dev.off()
  
  if (getwd() == "/")
    workdir <- "top level (root) of your file system"
  else
    workdir <- getwd()
  cat("\n\npdf file of bar charts:",  fname, "\n")
  cat("\nWritten at current working directory:", workdir, "\n\n")

}
