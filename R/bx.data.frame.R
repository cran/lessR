bx.data.frame <-
function(x, n.cat, text.out, ...)  {


  for (i in 1:ncol(x)) {

    nu <- length(unique(na.omit(x[,i])))

    x.name <- names(x)[i]
    options(xname = x.name)

    if (is.numeric(x[,i]) && nu > n.cat) {
      fname <- paste("BoxPlot_", x.name, ".pdf", sep="")
      bx.default(x[,i], text.out=text.out, pdf.file=fname, ...)
    }

    if (is.numeric(x[,i]) && nu <= n.cat)
      cat("\n>>> ", x.name,  "is numeric,",
          "but only has", nu, "<= n.cat =", n.cat, "levels,",
          "so treat as a categorical variable.\n",
          "   To obtain the boxplot decrease  n.cat  to specify a",
          "lower number of unique values.\n",
          "   Suggest making this variable a factor with R factor function.\n")

  }
  
}
