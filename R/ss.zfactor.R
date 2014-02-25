.ss.factor <-
function(x, by=NULL, brief=FALSE, digits.d=NULL, ...)  {

  # get variable labels if exist
  gl <- .getlabels()
  x.name <- gl$xn; x.lbl <- gl$xl
  y.name <- gl$yn; y.lbl <- gl$yl
  
  # save ordered status before converting x to a table
  if (is.ordered(x) && is.null(by)) order.x <- TRUE else order.x <- FALSE
  if (is.ordered(by)) order.y <- TRUE else order.y <- FALSE

  # convert to table, with variable names, if needed
  if (!is.table(x) && !is.matrix(x)) {  # bc yields a table or matrix
    if (!is.null(by)) 
      x <- table(by,x, dnn=c(y.name,x.name)) 
    else x <- table(x, dnn=NULL)
  }

  # print table, chi-square analysis
  if (is.null(by) || (!is.null(x.lbl) || !is.null(y.lbl))) #  one var or labels
    .title(x.name, y.name, x.lbl, y.lbl, is.null(by))
  else cat("\n")  # no title if two vars and no labels
 
  if (!is.null(by) || is.matrix(x)) {  # two variables
    if (!brief) 
      { .dash(30); cat("Joint and Marginal Frequencies\n"); .dash(30) }
    print(addmargins(x))
    if (!brief) { 
      nan.flag <- FALSE

      cat("\n\n"); .dash(30); cat("Cell Proportions and Marginals\n"); .dash(30); 
      print(round(addmargins(prop.table(x)),3))
      cat("\n"); .dash(30); cat("Proportions within Each Column\n"); .dash(30);
      x.col <- prop.table(x, margin=2)
      Sum <- numeric(ncol(x.col))
      for (i in 1:ncol(x.col)) {
        Sum[i] <- sum(x.col[,i])
        if (is.nan(Sum[i])) nan.flag <- TRUE
      }
      x.col2 <- round(rbind(x.col,Sum),3)
      names(dimnames(x.col2)) <- names(dimnames(x.col))
      print(x.col2)

      cat("\n"); .dash(27); cat("Proportions within Each Row\n"); .dash(27); 
      x.row <- prop.table(x, margin=1)
      Sum <- numeric(nrow(x.row))
      for (i in 1:nrow(x.row)) {
        Sum[i] <- sum(x.row[i,])
        if (is.nan(Sum[i])) nan.flag <- TRUE
      }
      x.row2 <- round(cbind(x.row,Sum),3)
      names(dimnames(x.row2)) <- names(dimnames(x.row))
      print(x.row2)

      if (nan.flag)
        cat("\nNote: NaN results from all values missing for that cell or margin.\n",
                 "     so any division to compute a proportion is undefined.\n")
    }
  }

  else {  # one variable
    proceed <- TRUE

    #if (length(x) > 20) {
      #proceed <- FALSE
      #print(x)
    #}
    if (length(names(x)) == sum(x)) {
      proceed <- FALSE
      cat("\nAll values are unique.  Perhaps a row ID instead of a variable.\n",
          "If so, use  row.names  option when reading. See help(read.table).\n\n", sep="")
      if (sum(x) < 100) print(names(x))
      else cat("\nOnly the first 100 values listed.  To see all, use\n",
               "the  values  function.\n\n")
    }
    if (proceed) {
      max.ln <- integer(length=0)
      for (i in 1:length(x)) {
        ln.nm <- nchar(names(x[i]))
        ln.vl <- nchar(as.character(x[i]))
        max.ln[i] <- max(ln.nm, ln.vl) + 1
        if (max.ln[i] < 6) max.ln[i] <- 6
      }
      cat("             ")
      w <- nchar(as.character(sum(x)))
      for (i in 1:length(x)) cat(.fmtc(names(x[i]), w=max.ln[i]))
      cat(.fmtc("Total", w=w+6))
      cat("\n")
      cat("Frequencies: ")
      for (i in 1:length(x)) cat(.fmti(x[i], w=max.ln[i]))
      cat(.fmti(sum(x), w=w+6))
      cat("\n")
      cat("Proportions: ")
      sum.x <- sum(x)
      xp <- numeric(length=0)
      xp <- x/sum.x
      for (i in 1:length(x)) cat(.fmt(xp[i], 3, max.ln[i]))
      cat(.fmtc("1.000", w=w+6))
      cat("\n")
      return(list(freq=x, prop=xp))  # back to SummaryStats
    }
  }  # one variable

}
