.cr.data.frame <-
function(x, miss, show.n, n.cat, digits.d,
         graphics, main, bottom, right, 
         pdf, pdf.width, pdf.height, ...)  {

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
  not.num <- integer(length=ncol(x))
  i.not <- 0
  for (i in 1:ncol(x)) {

    x.name <- names(x)[i]
    options(xname = x.name)

    nu <- length(unique(na.omit(x[,i])))
    if (is.numeric(x[,i]) && nu <= n.cat) {
      i.not <- i.not + 1
      not.num[i.not] <- i
      cat("\n")
      cat("\n>>> Note:", x.name,  "is technically numeric, but only has ", 
          nu, "<= n.cat =", n.cat, " levels,\n",
         "      so treat as a categorical variable.\n",
         "    To obtain the correlations decrease  n.cat  to specify a",
         "lower number\n",
         "      of unique values, such as with the function: set.\n",
         "    Perhaps make this variable a factor with the R factor function.\n")
    }

    if (!is.numeric(x[,i])) {
      i.not <- i.not + 1
      not.num[i.not] <- i
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

  if (i.not > 0) {
    cat("\n>>> Note: The following variables are not numeric and are deleted",
        "from the analysis.\n\n")
    for (i in 1:i.not) cat(i, ". ", names(x)[not.num[i]], "\n", sep="")
    cat("\n")
    x <- x[, -not.num[1:i.not]]
    if (is.null(dim(x))) { 
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "A correlation matrix requires at least 2 variables.\n\n")
    }
  }
  
  if (!is.null(digits.d)) max.digits <- digits.d

  # calculate correlations, store in mycor
  myc <- round(cor(x, use=miss.type, ...), digits=max.digits)

  n.vars <- nrow(myc)
  cat("\nCorrelation matrix calculated\n",
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
    tot.miss <- sum(is.na(x))
    if (tot.miss == 0) 
      cat("\n>>> No missing data\n\n")
    else {
      .ss.factor(as.vector(n), brief=TRUE)
      if (n.vars <= 15  ||  show.n) 
        print(n)
      else
        cat("To view the sample size for each correlation coefficient, re-run the\n",
            "analysis with show.n=TRUE\n", sep="")
      cat("\n")
    }
  }

  if (n.vars <= 15) {
    cat("--- Correlation Matrix ---\n\n")
    print(myc)
    cat("\n")
  }
  else {
    cat("\nVariables in the correlation matrix:\n")
    cat(names(x))
    cat("\n\nTo view the correlation matrix, enter its name such as  mycor\n\n")
  }


  if (graphics  ||  pdf) {

    # set up graphics system for to 2 windows
    if (!pdf) {
      .graphwin(2)
      dev.set(which=3)
    }
    else { 
      pdf.file <- "Cor_SPmatrix.pdf"
      pdf(file=pdf.file, width=pdf.width, height=pdf.height)
    }
  
    # scatter plot matrix
    col.pts <- getOption("col.stroke.pt")
    suppressWarnings(pairs(x, panel=panel.smooth,
                     col=col.pts, col.smooth="black", lwd=1.5))

    if (pdf) {  # terminate pdf graphics
      dev.off()
      .showfile(pdf.file, "scatter plot matrix")
    }

    # heat map
    if (!pdf) 
      dev.set(which=4) 
    else { 
      pdf.file <- "Cor_HeatMap.pdf"
      pdf(file=pdf.file, width=pdf.width, height=pdf.height)
    }

    if (is.null(main)) main <- "Correlations"

    for (i in 1:n.vars) mycor[i,i] <- 0
    cat("\nNote: To provide more color separation for off-diagonal\n",
        "      elements, the diagonal elements of the matrix for\n",
        "      computing the heat map are set to 0.\n", sep="")

    max.color <- getOption("col.heat")
    hmcols <- colorRampPalette(c("white",max.color))(256)

    heatmap(mycor[1:n.vars,1:n.vars], Rowv=NA, Colv="Rowv", symm=TRUE,
      col=hmcols, margins=c(bottom, right), main=main)

    if (pdf) {  # terminate pdf graphics
      dev.off()
      .showfile(pdf.file, "heat map")
      cat("\n\n")
    }
  }

  else
    cat("\n>>> To view a heat map and scatter plot matrix set:  graphics=TRUE\n\n")

  invisible(myc)

}
