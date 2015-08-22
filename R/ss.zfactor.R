.ss.factor <-
function(x, by=NULL, brief=FALSE, digits.d=NULL, ...)  {


# construct a cross-tabs 
.prnfreq <- function(x, type, max.ln, max.c1, n.dash, ttl) {
  tx <- character(length = 0)

  # title
  tx[length(tx)+1] <- .dash2(n.dash);
  tx[length(tx)+1] <- ttl 
  tx[length(tx)+1] <- .dash2(n.dash);

  # col labels
  tx[length(tx)+1] <-  .fmtc(x.name, w=max.c1+3)
  tx[length(tx)+1] <-  format(y.name, width=max.c1, justify="left")
  w <- nchar(as.character(sum(x)))
  for (i in 1:ncol(x))
    tx[length(tx)] <- paste(tx[length(tx)], .fmtc(colnames(x)[i], w=max.ln[i]),
      sep="")

  # values
  for (i in 1:nrow(x)) {
    rwnm <- paste(" ", rownames(x)[i])
    tx[length(tx)+1] <-  format(rwnm, width=max.c1, justify="left")
    for (j in 1:ncol(x)) {
      if (type=="r") {
        tx[length(tx)] <- paste(tx[length(tx)], .fmt(x[i,j], d=3, w=max.ln[j]),
          sep="")
      }
      else if (type=="i")
        tx[length(tx)] <- paste(tx[length(tx)], .fmti(x[i,j], w=max.ln[j]),
          sep="")
    }
  }

  return(tx)
}  # end .prnfreq


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


  # no title if two vars and no labels
  txttl <- ""
  if (is.null(by) || (!is.null(x.lbl) || !is.null(y.lbl))) { #  one var or labels
    txttl <- .title2(x.name, y.name, x.lbl, y.lbl, is.null(by), new.ln=FALSE)
  }

  # print table, chi-square analysis
  # -------------------------------------
  # two variables 
  if (!is.null(by) || is.matrix(x)) { 
    n.dim <- 2

    xx <- addmargins(x)

    # width of column 1
    if (!is.null(y.name))
      max.c1 <- nchar(y.name)
    else
      max.c1 <- 0
    for (i in 1:nrow(xx)) {
      c1 <- nchar(rownames(xx)[i])
      if (c1 > max.c1) max.c1 <- c1
    }
    max.c1 <- max.c1 + 2
    if (max.c1 < 5) max.c1 <- 5

    # width of data columns
    max.ln <- integer(length=0)
    for (i in 1:ncol(xx)) {
      ln.nm <- nchar(colnames(xx)[i])
      ln.vl <- nchar(as.character(xx)[i])
      max.ln[i] <- max(ln.nm, ln.vl) + 1
      if (max.ln[i] < 4) max.ln[i] <- 4
    }

    # Cell frequencies
    txfrq <- .prnfreq(xx, "i", max.ln, max.c1, n.dash=32,
                      ttl="Joint and Marginal Frequencies")

    if (brief)
      return(list(n.dim=n.dim, txttl=txttl, txfrq=txfrq))


    # full analysis
    nan.flag <- FALSE

    for (i in 1:ncol(xx)) {
      if (max.ln[i] < 6) max.ln[i] <- 6
    }

    # Cell Proportions and Marginals
    xx <- round(addmargins(prop.table(x)),3)
    txprp <- .prnfreq(xx, "r", max.ln, max.c1, n.dash=30,
                      ttl="Cell Proportions and Marginals")

    # Cell Proportions within Each Column
    x.col <- prop.table(x, margin=2)
    Sum <- numeric(ncol(x.col))
    for (i in 1:ncol(x.col)) {
      Sum[i] <- sum(x.col[,i])
      if (is.nan(Sum[i])) nan.flag <- TRUE
    }
    x.col2 <- round(rbind(x.col,Sum),3)
    names(dimnames(x.col2)) <- names(dimnames(x.col))

    txcol <- .prnfreq(x.col2, "r", max.ln, max.c1, n.dash=35,
                      ttl="Cell Proportions within Each Column")

    # Cell Proportions within Each Row
    x.row <- prop.table(x, margin=1)
    Sum <- numeric(nrow(x.row))
    for (i in 1:nrow(x.row)) {
      Sum[i] <- sum(x.row[i,])
      if (is.nan(Sum[i])) nan.flag <- TRUE
    }
    x.row2 <- round(cbind(x.row,Sum),3)
    names(dimnames(x.row2)) <- names(dimnames(x.row))

    txrow <- .prnfreq(x.row2, "r", max.ln, max.c1, n.dash=32,
                      ttl="Cell Proportions within Each Row")

    if (nan.flag)
      cat("\nNote: NaN results from all values missing for that cell or margin.\n",
                 "     so any division to compute a proportion is undefined.\n")

    return(list(n.dim=n.dim, txttl=txttl, txfrq=txfrq, txprp=txprp,
                txcol=txcol, txrow=txrow))  # back to ss or ss data frame

    # end full analysis

  }  # end two variable


  else {  # one variable
    n.dim <- 1

    if (length(names(x)) == sum(x)) {
      cat("\nAll values are unique.  Perhaps a row ID instead of a variable.\n",
          "If so, use  row.names  option when reading. See help(read.table).\n\n", sep="")
      if (sum(x) < 100) print(names(x))
      else cat("\nOnly the first 100 values listed.  To see all, use\n",
               "the  values  function.\n\n")
    }
    else {

      max.ln <- integer(length=0)
      for (i in 1:length(x)) {
        ln.nm <- nchar(names(x[i]))
        ln.vl <- nchar(as.character(x[i]))
        max.ln[i] <- max(ln.nm, ln.vl) + 1
        if (max.ln[i] < 6) max.ln[i] <- 6
      }

      tx <- character(length = 0)

      tx[length(tx)+1] <-  format("", width=13)
      w <- nchar(as.character(sum(x)))
      for (i in 1:length(x))
        tx[length(tx)] <- paste(tx[length(tx)], .fmtc(names(x[i]), w=max.ln[i]))
      tx[length(tx)] <- paste(tx[length(tx)], .fmtc("Total", w=w+6))

      tx[length(tx)+1] <- "Frequencies: "
      for (i in 1:length(x))
        tx[length(tx)] <- paste(tx[length(tx)], .fmti(x[i], w=max.ln[i]))
      tx[length(tx)] <- paste(tx[length(tx)], .fmti(sum(x), w=w+6))

      tx[length(tx)+1] <- "Proportions: "
      sum.x <- sum(x)
      xp <- numeric(length=0)
      xp <- x/sum.x
      for (i in 1:length(x))
        tx[length(tx)] <- paste(tx[length(tx)], .fmt(xp[i], 3, max.ln[i]))
      tx[length(tx)] <- paste(tx[length(tx)], .fmtc("1.000", w=w+6))

      return(list(n.dim=n.dim, title=txttl, tx=tx, frq=x, prp=xp))  # back to SummaryStats
    }
  }  # one variable

}
