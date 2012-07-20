bc.data.frame <-
function(x, n.cat, text.out, ...)  {


  for (i in 1:ncol(x)) {

    nu <- length(unique(na.omit(x[,i])))
    if (!is.numeric(x[,i]) || nu <= n.cat) {
 
      if (nlevels(factor(x[,i])) < length(x[,i])) {
        x.name <- names(x)[i]
        options(xname = x.name)
        fname <- paste("BarChart_", x.name, ".pdf", sep="")
        bc.default(x[,i], text.out=text.out, 
                   pdf.file=fname, font.main=1, ...)

      if (is.numeric(x[,i]) && nu <= n.cat)
        cat(">>> Variable is numeric, but only has", nu, "<= n.cat =", n.cat, "levels,",
            "so treat as a categorical variable.\n",
            "   To obtain the numeric summary, decrease  n.cat  to specify a",
            "lower number of unique values.\n",
            "   Suggest making this variable a factor with R factor function.\n")
      }
      else cat("\n", names(x)[i], "appears to contain unique Names or IDs", "\n")
    }

  }

}
