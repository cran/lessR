cr.data.frame <-
function(x, ncut=4, digits.d=NULL, ...)  {

  if (!is.null(digits.d) && digits.d<1) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",      
        "\n>>> digits d is ", digits.d,  " and must be at least 1.\n")
  }

  max.digits <- 0

  # check for valid numeric variables and get largest decimal digits
  for (i in 1:ncol(x)) {

    nu <- length(unique(na.omit(x[,i])))

    x.name <- names(x)[i]
    options(xname = x.name)

    if (is.numeric(x[,i]) && nu <= ncut) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "\n>>> ", x.name,  " is numeric, but only has ", nu, "<= ncut =", ncut,
          " levels, so treat as a categorical variable.\n",
          "   To obtain the correlations decrease  ncut  to specify a ",
          "lower number of unique values.\n",
          "   Suggest making this variable a factor with R factor function.\n")

    }

    if (!is.numeric(x[,i])) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",      
          "\n>>> ", x.name,  " is not numeric, so cannot correlate.\n")
    }

    if (is.null(digits.d)) {
      dig.dec <- .max.dd(x[,i]) + 1 
      if (dig.dec > max.digits) max.digits <- dig.dec
      if (dig.dec > 10) {
        cat("\nThese data values contain ", dig.dec, " decimal digits. To enhance\n",
            "the readability of the output, only 4 decimal digits are\n",
            "displayed.  To customize this setting, use the digits.d  parameter.\n",
            "Example for Variables Y and X:  > ss(Y, by=X, digits.d=3)\n\n",
            sep="")
        dig.dec <- 4
      }
    }
  }
  
  if (!is.null(digits.d)) max.digits <- digits.d + 1

  round(cor(x, ...), digits=max.digits)
  
}
