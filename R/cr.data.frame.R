.cr.data.frame <-
function(x, miss, show.n, n.cat, digits.d, heat.map, colors,
         main, bottom, right, 
         pdf.file, pdf.width, pdf.height, ...)  {

  if (!is.null(digits.d) && digits.d<1) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",      
        "\n>>> digits d is ", digits.d,  " and must be at least 1.\n")
  }

  if (miss == "pairwise") miss.type <- "pairwise.complete.obs"
  else if (miss == "listwise") miss.type <- "complete.obs"
  else if (miss == "everything") miss.type <- "everything"

  max.digits <- 0
  dig.warning <- FALSE

  # check for valid numeric variables and get largest decimal digits
  for (i in 1:ncol(x)) {

    nu <- length(unique(na.omit(x[,i])))

    x.name <- names(x)[i]
    options(xname = x.name)

    if (is.numeric(x[,i]) && nu <= n.cat) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "\n>>> ", x.name,  " is numeric, but only has ", nu, "<= n.cat =", n.cat,
          " levels, so treat as a categorical variable.\n",
          "   To obtain the correlations decrease  n.cat  to specify a ",
          "lower number of unique values.\n",
          "   Suggest making this variable a factor with R factor function.\n")

    }

    if (!is.numeric(x[,i])) {
      cat("\n"); stop(call.=FALSE, "\n","------\n", 
          "\n>>> ", x.name,  " is not numeric, so cannot correlate.\n")
    }

    if (is.null(digits.d)) {
      dig.dec <- .max.dd(x[,i]) + 1 
      if (dig.dec < 2) dig.dec <- 2
      if (dig.dec > max.digits  &&  !dig.warning) max.digits <- dig.dec
      if (dig.dec > 10  &&  !dig.warning) {
        cat("\nThese data values contain ", dig.dec, " decimal digits. To enhance\n",
            "the readability of the output, only 4 decimal digits are\n",
            "displayed.  To customize this setting, use the digits.d  parameter.\n",
            "Example for Variables Y and X:  > cr(Y, by=X, digits.d=3)\n\n",
            sep="")
        dig.warning <- TRUE
        max.digits <- 4
      }
    }
  }
  
  if (!is.null(digits.d)) max.digits <- digits.d

  # calculate correlations, store in mycor
  myc <- round(cor(x, use=miss.type, ...), digits=max.digits)
  assign("mycor", myc, pos=.GlobalEnv) 

  n.vars <- nrow(myc)
  cat("\nCorrelation matrix calculated\n",
      "  Name:  mycor\n",
      "  Number of variables: ", n.vars, "\n",
      "  Missing data deletion: ", miss, "\n") 

  if (miss == "listwise") 
    cat("   Sample size after deleted rows:", sum(complete.cases(x)), "\n\n")

  else if (miss == "pairwise") {
    n <- as.integer(myc)
    n <- matrix(n, nrow=n.vars)
    dimnames(n) <- dimnames(myc)
    for (i in 1:n.vars) {
      for (j in 1:n.vars) {
        n[i,j] <- sum(!is.na(x[i] - x[j]))  # non-missing values
      }
    }
    options(xname = "Missing Data Analysis")
    .ss.factor(as.vector(n), brief=TRUE)
    tot.miss <- sum(is.na(x))
    if (tot.miss == 0) 
      cat(">>> No missing data\n\n")
    else {
      if (n.vars <=15) 
        print(n)
      else
        cat("To view the sample size for each correlation, re-run analysis\n",
            "with show.n=TRUE\n", sep="")
      cat("\n")
    }
  }

  if (n.vars <= 15) {
    cat("--- Correlation Matrix mycor ---\n\n")
    print(myc)
    cat("\n")
  }
  else cat("\nTo view the correlation matrix enter:  mycor\n\n")

  if (heat.map) {
    if (is.null(main)) main <- "Correlations"
   .corcolors(mycor, n.vars, colors, main, bottom, right, diag=0,
              pdf.file, pdf.width, pdf.height)
  }
  
}