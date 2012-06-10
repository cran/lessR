hst.data.frame <-
function(x, ncut, ...)  {


  fname <- paste("Hist.", format(Sys.time(), "%d_%H_%M_%S"), ".pdf",sep="")
  pdf(file=fname)

  for (i in 1:ncol(x)) {

    nu <- length(unique(na.omit(x[,i])))

    x.name <- names(x)[i]
    options(xname = x.name)

    if (is.numeric(x[,i]) && nu > ncut) {
      tlbl <- paste("Histogram for", names(x)[i])
      hst.default(x[,i], xlab=names(x)[i], main=tlbl, ...)
    }

    if (is.numeric(x[,i]) && nu <= ncut)
      cat("\n>>> ", x.name,  "is numeric, but only has", nu, "<= ncut =", ncut, "levels,",
          "so treat as a categorical variable.\n",
          "   To obtain the histogram decrease  ncut  to specify a",
          "lower number of unique values.\n",
          "   Suggest making this variable a factor with R factor function.\n")

  }
  
  dev.off()
  
  .showfile(fname, "histograms")
}
