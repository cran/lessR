bc.data.frame <-
function(x, ncut, ...)  {

  fname <- paste("Barcharts.", format(Sys.time(), "%d_%H_%M_%S"), ".pdf",sep="")
  pdf(file=fname)

  for (i in 1:ncol(x)) {

    nu <- length(unique(na.omit(x[,i])))
    if (!is.numeric(x[,i]) || nu <= ncut) {
      tlbl <- paste("Bar Chart for", names(x)[i])
      if (nlevels(factor(x[,i])) < length(x[,i])) {
        x.name <<- names(x)[i]
        bc.default(x[,i], xlab=names(x)[i], main=tlbl, font.main=1, ...)
      if (is.numeric(x[,i]) && nu <= ncut)
        cat(">>> Variable is numeric, but only has", nu, "<= ncut =", ncut, "levels,",
            "so treat as a categorical variable.\n",
            "   To obtain the numeric summary, decrease  ncut  to specify a",
            "lower number of unique values.\n",
            "   Suggest making this variable a factor with R factor function.\n")
      }
      else cat("\n", names(x)[i], "appears to contain unique Names or IDs", "\n")
    }
  }

  dev.off()
  
  if (getwd() == "/")
    workdir <- "top level (root) of your file system"
  else
    workdir <- getwd()
  cat("\n\npdf file of bar charts:",  fname, "\n")
  cat("\nWritten at current working directory:", workdir, "\n\n")

}
