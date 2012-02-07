ss.data.frame <-
function(x, ncut, ...)  {

  for (i in 1:ncol(x)) {

    nu <- length(unique(na.omit(x[,i])))

    x.name <<- names(x)[i]
    if (is.numeric(x[,i]) && nu > ncut) ss.numeric(x[,i], ...)
    else if (is.factor(x[,i]) || nu <= ncut) {
      ss.factor(x[,i], ...)
      if (is.numeric(x[,i]) && nu <= ncut)
        cat(">>> Variable is numeric, but only has", nu, "<= ncut =", ncut, "levels,",
            "so treat as a categorical variable.\n",
            "   To obtain the numeric summary, decrease  ncut  to specify a",
            "lower number of unique values.\n",
            "   Suggest making this variable a factor with R factor function.\n")
    }
    else if (is.character(x[,i])) ss.factor(factor(x[,i]), ...)
    else cat("\n>>>The following type of variable not processed: ", 
             class(x[,i]), "\n")
  }
  cat("\n")

}
