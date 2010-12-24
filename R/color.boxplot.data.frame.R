color.boxplot.data.frame <-
function(x, ...)  {

  fname <- paste("Boxplots.",deparse(substitute(x)),".pdf",sep="")
  pdf(file=fname)

  for (i in 1:ncol(x))
    if (is.numeric(x[,i])) {
      tlbl <- paste("Boxplot for", names(x)[i])
      color.boxplot.default(x[,i], xlab=names(x)[i], main=tlbl, ...)
    }
  
  dev.off()
  
  if (getwd() == "/")
    workdir <- "top level (root) of your file system"
  else
    workdir <- getwd()
  cat("\n\npdf file of histograms:",  fname, "\n")
  cat("\nWritten at current working directory:", workdir, "\n\n")

}
