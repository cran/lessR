.cr.data.frame <-
function(x, miss, show_n, digits_d,
         heat_map, fill_low, fill_hi, main, bottom, right, 
         pdf, width, height, ...)  {

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

    #nu <- length(unique(na.omit(x[,i])))
    #if (.is.num.cat(x[,i], n_cat)) {
      #i.not <- i.not + 1
      #not.num[i.not] <- i
      #cat("\n")
      #cat("\n>>> Note:", x.name,  "is technically numeric, but only has ", 
          #nu, "<= n_cat =", n_cat, " levels,\n",
         #"      so treat as a categorical variable.\n",
         #"    To obtain the correlations decrease  n_cat  to specify a",
         #"lower number\n",
         #"      of unique values, such as with the function: style\n",
         #"    Perhaps make this variable a factor with the R factor function.\n")
    #}

    if (!is.numeric(x[,i])) {
      i.not <- i.not + 1
      not.num[i.not] <- i
    }

    if (is.null(digits_d)) {
      digits_d <- .max.dd(x[,i]) + 1 
      if (digits_d < 2) digits_d <- 2
      if (digits_d > max.digits  &&  !dig.warning) max.digits <- digits_d
      if (digits_d > 10  &&  !dig.warning) {
        cat("\nThese data values contain ", digits_d, " decimal digits. To enhance\n",
            "the readability of the output, only 4 decimal digits are\n",
            "displayed.  To customize this setting, use the digits_d  parameter.\n",
            "Example for Variables Y and X:  > cr(Y, by=X, digits_d=3)\n\n",
            sep="")
        dig.warning <- TRUE
        max.digits <- 4
      }
    }
  }

  # background
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
  
  if (!is.null(digits_d)) max.digits <- digits_d
  crs <- round(cor(x, use=miss_type, ...), digits=max.digits)  # cor matrix

  n.vars <- nrow(crs)
  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- paste("The correlation matrix contains",
    n.vars, "variables")

  txb <- tx


  # missing
  tx <- character(length = 0)

  tx[length(tx)+1] <- paste("Missing data deletion: ", miss)

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
    tot.miss <- sum(is.na(x))
    if (tot.miss == 0) 
      tx[length(tx)+1] <- paste("\n>>> No missing data")
    else {
      .ss.factor(as.vector(n), brief=TRUE, x.name=" ")  # x.name KLUDGE
      if (n.vars <= 15  ||  show_n) {
        txn <- .prntbl(n, 2, cc=" ")
        for (i in 1:length(txn)) tx[length(tx)+1] <- txn[i]
      }
      else
        tx[length(tx)+1] <- paste("To view the sample size for each ",
          "correlation, re-run the\n",
          "analysis with show_n=TRUE\n", sep="")
    }
  }  # end pairwise

  txm <- tx


  # cor matrix
  tx <- character(length = 0)

  if (n.vars <= 15) {
    tx[length(tx)+1] <- paste("Correlation Matrix")
    txcrs <- .prntbl(crs, 2, cc=" ")
    for (i in 1:length(txcrs)) tx[length(tx)+1] <- txcrs[i]
  }
  else {
    tx[length(tx)+1] <- "Variables in the correlation matrix:"
    tx[length(tx)+1] <- toString(names(x))
    tx[length(tx)+1] <- paste("\nTo view the correlation matrix, ",
     "enter the name of the returned object\n", 
     "followed by  $R  such as  mycor$R\n", sep="")
  }

  txc <- tx


  if (heat_map  ||  pdf) {

    # heat map
    if (!pdf) {
      manage.gr <- .graphman()  # manage graphics?
      if (manage.gr) {
        .graphwin(1)
        dev.set(which=3) 
      }
    }
    else { 
      pdf_file <- "Cor_HeatMap.pdf"
      pdf(file=pdf_file, width=width, height=height)
    }

    # NEED to integrate into .corcolors
    if (is.null(fill_low)  &&  is.null(fill_hi)) {      
      if (getOption("theme") %in% c("colors", "dodgerblue", "blue", 
                                    "lightbronze")) {
        fill_low <- "rusts"
        fill_hi <- "blues"
      }
      else if (getOption("theme") %in% c("darkred", "red", "rose")) {
        fill_low <- "turquoises" 
        fill_hi <- "reds"
      }
      else if (getOption("theme") %in% c("darkgreen", "green")) {
        fill_low <- "violets" 
        fill_hi <- "greens"
      }
      else if (getOption("theme") %in% c("gold", "brown", "sienna")) {
        fill_low <- "blues" 
        fill_hi <- "browns"
      }
      else if (getOption("theme") %in% c("gray", "white")) {
        fill_low <- "gray90"
        fill_hi <- "black"
      }
    }

    else if (is.null(fill_low) || is.null(fill_hi)) { 
      fill_low <- "white"
      fill_hi <- "gray20"
    }

    # fill_low and fill_hi "blues", etc, then divergent, else sequential`
#   if (getOption("theme") %in% c("gray", "white"))
#     hmcols <- getColors("grays", "grays", l=c(10,100))
#   else
      hmcols <- getColors(fill_low, fill_hi, l=c(20,80))
#   hmcols <- colorRampPalette(c(fill_low, fill_hi))(256)

    axis_x_cex <- ifelse(is.null(getOption("axis_x_cex")),
        getOption("axis_cex"), getOption("axis_x_cex"))
    axis_y_cex <- ifelse(is.null(getOption("axis_y_cex")),
        getOption("axis_cex"), getOption("axis_y_cex"))

    for (i in 1:nrow(crs)) crs[i,i] <- 0
    heatmap(crs[1:ncol(crs),1:ncol(crs)], Rowv=NA, Colv="Rowv", symm=TRUE,
      col=hmcols, margins=c(bottom, right), main=main,
      cexRow=axis_x_cex, cexCol=axis_y_cex)
    for (i in 1:nrow(crs)) crs[i,i] <- 1.0

    if (pdf) {  # terminate pdf graphics
      dev.off()
      .showfile(pdf_file, "heat map")
      cat("\n\n")
    }
  }

  else
    if (is.null(options()$knitr.in.progress))
      cat("\n>>> To view comments, enter the name of the saved object, e.g., mycor\n")

  return(list(txb=txb, txm=txm, txc=txc, R=crs))

}
