.ss.data.frame <-
function(x, n.cat, brief, ...)  {

  for (i in 1:ncol(x)) {

    nu <- length(unique(na.omit(x[,i])))

    x.name <- names(x)[i]
    options(xname = x.name)

    if (is.numeric(x[,i]) && nu > n.cat) .ss.numeric(x[,i], brief=brief, ...)

    else if (is.factor(x[,i]) || nu <= n.cat) {
      .ss.factor(x[,i], ...)
      if (is.numeric(x[,i]) && nu <= n.cat)
    cat("\n>>> Variable is numeric, but only has", nu, "<= n.cat =", n.cat, "levels,",
      "so treat as categorical.\n",
      "   To obtain the numeric summary, decrease  n.cat  to indicate a lower\n",
      "   number of unique values such as with function: set.\n", 
      "   Perhaps make this variable a factor with the R factor function.\n")
    }
    else if (is.character(x[,i])) .ss.factor(factor(x[,i]), ...)
    else cat("\n>>> The following type of variable not processed: ", 
             class(x[,i]), "\n")
  }
  cat("\n")

}
