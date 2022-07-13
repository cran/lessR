.bx.stats <-
function(x, ID=NULL, k.iqr=1.5, box_adj=FALSE, a=-4, b=3, digits_d=2, ...) {      

  x.name <- getOption("xname")

  tx=""
  if (is.null(digits_d)) {
    digits_d <- .max.dd(x) + 1
    if (digits_d == 1) digits_d <- 2
  }
  options(digits_d=digits_d)


  # summarize data
  if (!box_adj)
    bv <- boxplot.stats(x, coef=k.iqr)
  else
    bv <- adjboxStats(x, coef=k.iqr, a=a, b=b)

  lo.whisker <- bv$stats[1]
  hi.whisker <- bv$stats[5]

  if (is.null(ID))
    ID <- character(length=length(x))

  # ----------------------------
  # outliers
  outliers <- bv$out
  n.out <- length(outliers)

  tx <- character(length = 0)

  if (length(outliers>0) && length(unique(na.omit(x)>3))) {

    tx[length(tx)+1] <- " "
    tx[length(tx)+1] <- paste("(Box plot) Outliers:", n.out)

    ind.lo <- which(x < lo.whisker)  # lower outliers
    if (length(ind.lo) > 0) {
      x.lo <- x[ind.lo]
      ID.lo <- ID[ind.lo]
      lo.len <- length(x.lo)
      ord <- order(x.lo, decreasing=FALSE)
      x.lo <- x.lo[ord]
      ID.lo <- ID.lo[ord] 
      xc.lo <- character(length=length(x.lo))
      for (i in 1:length(x.lo)) xc.lo[i] <- .fmt(x.lo[i], d=digits_d-1)  
      ID.lo <- as.character(ID.lo)
      max.lo <- max(nchar(xc.lo))
      max.ID.lo <- max(nchar(as.character(ID.lo)))
    }
    else {
      xc.lo <- ""
      max.lo <- 0
      ID.lo <- "    "
      max.ID.lo <- 5
      lo.len <- 0
    }
    
    ind.hi <- which(x > hi.whisker)  # upper outliers
    if (length(ind.hi) > 0) {
      x.hi <- x[ind.hi]
      hi.len <- length(x.hi)
      ID.hi <- ID[ind.hi]
      ord <- order(x.hi, decreasing=TRUE)
      x.hi <- x.hi[ord]
      ID.hi <- ID.hi[ord] 
      xc.hi <- character(length=length(x.hi))
      for (i in 1:length(x.hi)) xc.hi[i] <- .fmt(x.hi[i], d=digits_d-1)  
      max.hi <- max(nchar(xc.hi))
      max.ID.hi <- max(nchar(as.character(ID.hi)))
    }
    else {
      xc.hi <- ""
      max.hi <- 0
      ID.hi <- "   "
      max.ID.hi <- 5
      hi.len <- 0
    }

    max.list <- 18   # max number of lines of outlier output
    n.lines <- min(max(lo.len, hi.len), max.list)
    ID.hi <- as.character(ID.hi)
    for (i in 1:n.lines) {
      if (i > lo.len) {
        length(xc.lo) <- length(xc.lo) + 1
        xc.lo[length(xc.lo)] <- ""
        ID.lo[length(xc.lo)] <- ""
      }
      if (i > hi.len) {
        length(xc.hi) <- length(xc.hi) + 1
        xc.hi[length(xc.hi)] <- ""
        ID.hi[length(xc.hi)] <- ""
      }
    }

    adj <- nchar("Small") - max.ID.lo
    buf <- ifelse (adj > 0, 3-adj, 3) 
    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste(
         .fmtc("Small", max.ID.lo, "left"), .fmtc("", max.lo), .fmtc("", buf),
         .fmtc("Large", max.ID.hi, "left"))
    tx[length(tx)+1] <- paste(
         .fmtc("-----", max.ID.lo, "left"), .fmtc("", max.lo), .fmtc("", buf),
         .fmtc("-----", max.ID.hi, "left"))
    for (i in 1:n.lines)
      tx[length(tx)+1] <- paste(
         .fmtc(ID.lo[i], max.ID.lo, "left"), .fmtc(xc.lo[i], w=max.lo),  "   ",
         .fmtc(ID.hi[i], max.ID.hi, "left"), .fmtc(xc.hi[i], w=max.hi))

    if (max(lo.len, hi.len) > n.lines)
      tx[length(tx)+1] <- paste("\n+", n.out-max.list, "more outliers")

  }  # end there were outliers

  else
    tx <- "No (Box plot) outliers"

  txotl <- tx


  # -----------------------------
  # stats

  # first the title with any variable labels
  txlbl <- .title2(x.name, y.name=NULL, x.lbl=NULL, y.lbl=NULL, isnullby=TRUE)
  if (length(txlbl) > 1) if (substr(txlbl[2],1,1) ==  "\n")
    txlbl[2] <- sub("\n", "", txlbl[2])
  tx <- txlbl

  n <- sum(!is.na(x))
  n.miss <- sum(is.na(x))
  mn <- .fmt(min(x, na.rm=TRUE))
  lw <- .fmt(bv$stats[1], digits_d)
  lh <- .fmt(bv$stats[2], digits_d)
  md <- .fmt(bv$stats[3], digits_d)
  uh <- .fmt(bv$stats[4], digits_d)
  uw <- .fmt(bv$stats[5], digits_d)
  mx <- .fmt(max(x, na.rm=TRUE), digits_d) 
  IQR <- .fmt(IQR(x, na.rm=TRUE), digits_d)
  avg <- .fmt(mean(x, na.rm=TRUE), digits_d)
  std <- .fmt(sd(x, na.rm=TRUE), digits_d)
  mc <- .fmt(mc(x, na.rm=TRUE, doScale=FALSE), digits_d)

  tx <- character(length = 0)
  tx <- txlbl
  tx[length(tx)+1] <- paste("Present:", n)
  tx[length(tx)+1] <- paste("Missing:", n.miss)
  tx[length(tx)+1] <- paste("Total  :", length(x))
  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- paste("Mean         :", avg)
  tx[length(tx)+1] <- paste("Stnd Dev     :", std)
  tx[length(tx)+1] <- paste("IQR          :", IQR)
  tx[length(tx)+1] <- paste("Skew         :", mc, "  [medcouple, -1 to 1]")
  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- paste("Minimum      :", mn)
  tx[length(tx)+1] <- paste("Lower Whisker:", lw) 
  tx[length(tx)+1] <- paste("1st Quartile :",
     .fmt(quantile(x, na.rm=TRUE)[2], digits_d))
  #tx[length(tx)+1] <- paste("Lower Hinge  :", lh) 
  tx[length(tx)+1] <- paste("Median       :", md) 
  tx[length(tx)+1] <- paste("3rd Quartile :",
     .fmt(quantile(x, na.rm=TRUE)[4], digits_d))
  #tx[length(tx)+1] <- paste("Upper Hinge  :", uh) 
  tx[length(tx)+1] <- paste("Upper Whisker:", uw) 
  tx[length(tx)+1] <- paste("Maximum      :", mx )
  txstat <- tx

  return(list(txotl=txotl, txstat=txstat))

}

