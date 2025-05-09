.cr.data.frame <-
function(x, miss, show, digits_d,
         heat_map, fill_low, fill_hi, main, bottom, right, quiet, 
         pdf_file, width, height, ...)  {

  if (!is.null(digits_d) && digits_d<1) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",      
        "\n>>> digits d is ", digits_d,  " and must be at least 1.\n")
  }

  if (miss == "pairwise") miss_type <- "pairwise.complete.obs"
  else if (miss == "listwise") miss_type <- "complete.obs"
  else if (miss == "everything") miss_type <- "everything"

  max.digits <- 0
  dig.warning <- FALSE

  # check for valid numeric variables and get largest decimal digits
  not.num <- integer(length=ncol(x))
  i.not <- 0
  for (i in 1:ncol(x)) {

    x.name <- names(x)[i]
    options(xname = x.name)  # sets for each variable   WHY???

    if (!is.numeric(x[,i])) {
      i.not <- i.not + 1
      not.num[i.not] <- i
    }

    if (is.null(digits_d)) {
      digits_d <- .max.dd(x[,i]) + 1 
      if (digits_d < 2) digits_d <- 2
      if (digits_d > max.digits  &&  !dig.warning) max.digits <- digits_d
      if (digits_d > 10  &&  !dig.warning) {
        cat("\nThese data values contain ", digits_d, " decimal digits.\n",
            "To enhance the readability of the output, only 4 decimal digits\n",
            "are displayed.  To customize, use the digits_d  parameter.\n",
            "Example for Variables Y and X:  > cr(Y, by=X, digits_d=3)\n\n",
            sep="")
        dig.warning <- TRUE
        max.digits <- 4
      }
    }
  }  # column check for numeric variables

  # remove non-numeric variables from data frame, background
  tx <- character(length = 0)
  if (i.not > 0) {
    tx[length(tx)+1] <- paste("The following non-numeric variables are deleted",
        "from the analysis")
    for (i in 1:i.not) tx[length(tx)+1] <- paste(i, ". ", names(x)[not.num[i]],
      sep="")
    x <- x[, -not.num[1:i.not]]
    if (is.null(dim(x))) { 
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "A correlation matrix requires at least 2 variables.\n\n")
    }
  }
  
  # compute correlations
  if (!is.null(digits_d)) max.digits <- digits_d
  crs <- round(cor(x, use=miss_type, ...), digits=max.digits)  # cor matrix

  n.vars <- nrow(crs)
  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- paste("The correlation matrix contains",
    n.vars, "variables")
  txb <- tx


  # missing values
  tot.miss <- sum(is.na(x))
  if (!quiet) {
    if (tot.miss == 0) 
      cat("\n>>> No missing data\n\n")
    else {
      cat("\nMissing data deletion: ", miss, "\n")
      if (show != "missing")
        cat("Rerun with  show=\"missing\"  for more information.\n\n")
    }
  }

  tx <- character(length = 0)

  if (miss == "listwise") 
    tx[length(tx)+1] <- paste("   Sample size after deleted rows:",
      sum(complete.cases(x)), "\n")

  else if (miss == "pairwise") {
    n <- as.integer(crs)
    n <- matrix(n, nrow=n.vars)
    dimnames(n) <- dimnames(crs)
    for (i in 1:n.vars) {
      for (j in 1:n.vars) {
        n[i,j] <- sum(!is.na(x[i] - x[j]))  # non-missing values
      }
    }
    options(xname = "Missing Data Analysis")
    if (tot.miss != 0) {

      .ss.factor(as.vector(n), brief=TRUE, x.name=" ")  # x.name KLUDGE
      if (show == "missing") {
        txn <- .prntbl(n, 2, cc=" ")
        for (i in 1:length(txn)) tx[length(tx)+1] <- txn[i]
      }
    }
  }  # end pairwise
  txm <- tx


  # cor matrix
  tx <- character(length = 0)
  tx[length(tx)+1] <- paste("Correlation Matrix")
  txcrs <- .prntbl(crs, 2, cc=" ")
  for (i in 1:length(txcrs)) tx[length(tx)+1] <- txcrs[i]
  txc <- tx


  if (heat_map  ||  !is.null(pdf_file)) {

    # heat map
    if (is.null(pdf_file)) {
      manage.gr <- .graphman()  # manage graphics?
      if (manage.gr) {
        .graphwin(1)
        dev.set(which=3) 
      }
    }

    .heatmap(crs[1:ncol(crs),1:ncol(crs)], nrow(crs), main=main,
               bm=NULL, rm=NULL, diag=0,
               pdf_file=pdf_file, width=width, height=height)
  }

  return(list(txb=txb, txm=txm, txc=txc, R=crs))
}
