.ss.factor <-
function(x, by=NULL, brief=FALSE, digits_d=NULL, x.name, y.name=NULL,
         x.lbl=NULL, y.lbl=NULL, label_max=20,
         x.miss=NULL, by.miss=NULL, out_size=NULL, ...)  {


# print a cross-tabs
.prnfreq <- function(x, type, max.ln, max.c1, n.dash, ttl, msg=FALSE) {
  tx <- character(length = 0)

  # title
  tx[length(tx)+1] <- ttl 
  tx[length(tx)+1] <- .dash2(n.dash)
  tx[length(tx)+1] <- ""

  # col labels
  if (!is.null(x.name))
    tx[length(tx)+1] <-  .fmtc(x.name, w=max.c1+3)
  tx[length(tx)+1] <- format(y.name, width=max.c1, justify="left")
  w <- nchar(as.character(sum(x)))
  for (i in 1:ncol(x))
    tx[length(tx)] <- paste(tx[length(tx)], .fmtc(colnames(x)[i], w=max.ln[i]),
      sep="")

  if (max(nchar(tx)) < getOption("width")) {  # horizontal layout

    # values
    for (i in 1:nrow(x)) {
      rwnm <- paste(" ", rownames(x)[i])
      tx[length(tx)+1] <-  format(rwnm, width=max.c1, justify="left")
      for (j in 1:ncol(x)) {
        if (type=="r") {
          tx[length(tx)] <- paste(tx[length(tx)],
             .fmt(x[i,j], d=3, w=max.ln[j]), sep="")
        }
        else if (type=="i")
          tx[length(tx)] <- paste(tx[length(tx)], .fmti(x[i,j], w=max.ln[j]),
            sep="")
      }
    }
  }

  else {  # vertical layout

    tx <- ""

    if (nrow(x) * (ncol(x)-1) > 30) { 
      if (msg  &&  getOption("notes"))
        message("Table output is vertical to fit in window, but > 30 rows\n",
                "To view the complete table, save the output\n",
                "  to an object, e.g., b <- BarChart(...)\n",
                "  then b$freq\n")
    }

    else {  # write
        
      mx.cx <- max(nchar(x.name), max(nchar(colnames(x))))
      mx.c3 <- max(nchar(.fmt(x, d=3))) + 1
      by.name <- getOption("byname")

      tx[length(tx)+1] <- paste(
         .fmtc(x.name, w=mx.cx, j="left"),
         .fmtc(by.name, w=max.c1+1, j="left"),
         .fmtc("Count", w=mx.c3, j="right"))
        for (i in 1:ncol(x)) {
          for (j in 1:nrow(x)) {
             tx[length(tx)+1] <- paste(
                .fmtc(colnames(x)[i], w=mx.cx, j="left"), 
                .fmtc(rownames(x)[j], w=max.c1, j="left"))
               # .fmt(x[j,i], d=digits_d, w=max.ln-3))
          if (type=="r") {
            tx[length(tx)] <- paste(tx[length(tx)],
               .fmt(x[j,i], d=3, w=mx.c3), sep="")
          }
          else if (type=="i")
            tx[length(tx)] <- paste(tx[length(tx)], .fmti(x[j,i], w=mx.c3),
              sep="")
          }  # end j
        }  # end i
      }  # write
    }


  return(tx)
}  # end .prnfreq


  # begin
  # ---------------------------------

  # maximum number of output text columns
  c.nm <- NULL  # storage for full value labels when they are abbreviated

  # convert to table with variable names if needed 
  if (!is.table(x) && !is.matrix(x)) {  # bc could send a table or matrix
    if (is.null(by)) 
      x <- table(x, dnn=NULL)  # if missing data
    else
      x <- table(by, x, dnn=c(y.name, x.name)) 
  }

  # no title if two vars and no labels
  txttl <- ""
  dims <- length(dim(x))
  if (dims == 1 || (!is.null(x.lbl) || !is.null(y.lbl))) {  #  one var or labels
    txttl <- .title2(x.name, y.name, x.lbl, y.lbl, is.null(by), new.ln=TRUE)
  }


  # two variables 
  # print table, chi-square analysis
  # -------------------------------------
  if (!is.null(by) || is.matrix(x)) { 
    n.dim <- 2

    # potential abbreviation of column labels
    mx.chr <- max(nchar(colnames(x)))
    if (mx.chr > label_max) {
      c.nm <- colnames(x)  # store for later use
      colnames(x) <- .abbrev(colnames(x), label_max)
    }
    
    # use for returned output, x is a 2-way table
    freq_df <- data.frame(t(x), stringsAsFactors=TRUE)

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
      for (j in 1:nrow(xx)) {
        ln.vl <- nchar(as.character(xx[j,i]))
      }
        max.ln[i] <- max(ln.nm, ln.vl) + 1
        if (max.ln[i] < 4) max.ln[i] <- 4
    }

    # cell frequencies
    txfrq <- .prnfreq(xx, "i", max.ln, max.c1, n.dash=30,
                      ttl="Joint and Marginal Frequencies", msg=TRUE)

    tx <- character(length = 0)
    ch <- summary(as.table(x))
    if (!is.nan(ch$statistic)) {
      min_rc <- min(nrow(x)-1, ncol(x)-1)
      V <- sqrt(ch$statistic / (min_rc * ch$n.cases))
      txt <- ifelse(ch$parameter == 1, " (phi)", "") 
      txt <- paste("Cramer\'s V", txt, ":", sep="")
      tx[length(tx)+1] <- paste(txt, .fmt(V,3))
      tx[length(tx)+1] <- ""
      tx[length(tx)+1] <- paste("Chi-square Test", 
          ":  Chisq = ", .fmt(ch$statistic,3), ", df = ", ch$parameter,
          ", p-value = ", .fmt(ch$p.value,3), sep="")
      if (!ch$approx.ok) 
        tx[length(tx)+1] <- paste(">>> Low cell expected frequencies,",
            "chi-squared approximation may not be accurate")
      #tx[length(tx)+1] <- ""
    }
    else
      tx[length(tx)+1] <- paste(
          "Cross-tabulation table not well-formed, usually too many zeros\n",
          "Cramer's V and the chi-squared analysis not possible\n\n", sep="")
    txXV <- tx

    if (brief)
      return(list(n.dim=n.dim, txttl=txttl, txfrq=txfrq, txXV=txXV,
                  freq_df=freq_df, pvalue=ch$p.value))


    # full analysis
    nan.flag <- FALSE

    for (i in 1:ncol(xx)) {
      if (max.ln[i] < 6) max.ln[i] <- 6
    }

    # cell proportions and marginals
    xx <- round(addmargins(prop.table(x)),3)
    txprp <- .prnfreq(xx, "r", max.ln, max.c1, n.dash=30,
                      ttl="Cell Proportions and Marginals")

    # cell proportions within each column
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

    # cell proportions within each row
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
      
    txlbl <- ""
    tx <- character(length = 0)
    if (!is.null(c.nm)) {
      tx[length(tx)+1] <- "Labels"
      tx[length(tx)+1] <- "--------------------"
      tx[length(tx)+1] <- paste(.fmtc(colnames(x), w=max(nchar(colnames(x))),
                                   j="left"), "  ", c.nm, sep="", collapse="\n")
      txlbl <- tx
    }

    # back to ss or ss data frame
    return(list(n.dim=n.dim, txttl=txttl, txlbl=txlbl, txfrq=txfrq,
                txXV=txXV, txprp=txprp, txcol=txrow, txrow=txcol,
                freq_df=freq_df, pvalue=ch$p.value))
    # end full analysis

  }  # end two variable


  else {  # one variable
    lnx <- length(names(x))
    if (lnx == sum(x)) {  # x is a vector of the counts
      if (length(x) > 100)
        cat("\nOnly the first 100 value out of", lnx, "listed.\n\n")
      nms <- character(length=0)
      for (i in 1:min(length(x), 100)) nms[i] <- names(x)[i]
      cat("\n")
      cat("Values:", nms, "\n")
      cat("\n",  #; stop(call.=FALSE, "\n","------\n",
          "All values for ", x.name, " are unique\n",
          "Perhaps a row ID instead of a variable for analysis\n",
          "If so, use  row.names=n  option for Read, where n refers to the ",
          "nth column\n\n", sep="")
      return(list(n.dim=1, title="", counts="", miss="", 
                  chi="", lbl="", freq=x, freq_df="", prop="",
                  pvalue=""))
    }


    else {  # table not of unique values, so proceed
      n.dim <- 1

      # potential abbreviation of column labels
      #  if (is.na(names(x)[length(x)])) names(x)[length(x)] <- "missing"
      mx.chr <- max(nchar(names(x)))

      c.nm <- NULL
      if (mx.chr > label_max) {
        c.nm <- names(x)  # store for later use
        names(x) <- .abbrev(names(x), label_max)
      }

       max.ln <- integer(length=0)      
       for (i in 1:length(x)) {
         ln.nm <- nchar(names(x[i]))
         ln.vl <- nchar(as.character(x[i]))
         max.ln[i] <- max(ln.nm, ln.vl) + 1
         if (max.ln[i] < 6) max.ln[i] <- 6
       }

      tx <- character(length=0)

      tx[length(tx)+1] <- format("", width=13)
      w <- nchar(as.character(sum(x)))

      for (i in 1:length(x))
        tx[length(tx)] <- paste(tx[length(tx)], .fmtc(names(x[i]), w=max.ln[i]))
      tx[length(tx)] <- paste(tx[length(tx)], .fmtc("Total", w=w+6))
      col.width <- nchar(tx[length(tx)])

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
      txcnt <- tx

      max.clmns <- ifelse (is.null(out_size), getOption("width"), out_size)
      if (col.width > max.clmns) {  # vertical display
        mx.nm <- max(nchar(names(x)), nchar("Total"))
        mx.fr <- nchar(sum(x)) + 2
        tx <- character(length=0)
        xnm <- ifelse (nchar(x.name) > 13, .abbrev(x.name, 13), x.name)
        tx[length(tx)+1] <- .fmtc(xnm, w=mx.nm)
        tx[length(tx)] <- paste(tx[length(tx)], .fmtc("Count", w=mx.fr))
        tx[length(tx)] <- paste(tx[length(tx)], .fmtc("Prop", w=6))

        tx[length(tx)+1] <- .dash2(mx.nm + mx.fr + 9)
        for (i in 1:length(x)) {
          tx[length(tx)+1] <- .fmtc(names(x[i]), w=mx.nm)
          tx[length(tx)] <- paste(tx[length(tx)], .fmti(x[i], w=mx.fr)) 
          tx[length(tx)] <- paste(tx[length(tx)], .fmt(xp[i], 3, w=7)) 
        }
        tx[length(tx)+1] <- .dash2(mx.nm + mx.fr + 9)
        tx[length(tx)+1] <- .fmtc("Total", w=mx.nm)
        tx[length(tx)] <- paste(tx[length(tx)], .fmti(sum(x), w=mx.fr))
        tx[length(tx)] <- paste(tx[length(tx)],  .fmtc("1.000", w=7))
        txcnt <- tx
      }

      txmis <- NULL
      if (!is.null(x.miss)) {
        tx <- character(length = 0)
        txt <- paste("Missing Values of ", x.name, ":", sep="")
        tx[length(tx)+1] <- paste(txt, x.miss) 
        txmis <- tx    
      }

      txchi <- ""
      txlbl <- ""
      ch <- NULL
      if (nrow(x) > 1) {
        tx <- character(length = 0)
        ch <- suppressWarnings(chisq.test(x))  # provide own warning of small n
        tx[length(tx)+1] <- 
          "Chi-squared test of null hypothesis of equal probabilities"
        tx[length(tx)+1] <- paste("  Chisq = ", .fmt(ch$statistic,3), ", df = ",
          ch$parameter, ", p-value = ", .fmt(ch$p.value,3), sep="")
        if (any(ch$expected < 5)) 
          tx[length(tx)+1] <- paste(">>> Low cell expected frequencies,",
              "so chi-squared approximation may not be accurate", "\n")
        txchi <- tx
        
        txlbl <- ""
        tx <- character(length = 0)
        if (!is.null(c.nm)) {
          tx[length(tx)+1] <- "Unabbreviated labels"
          tx[length(tx)+1] <- "--------------------"
          tx[length(tx)+1] <- paste(c.nm, sep="", collapse="\n")
          txlbl <- tx
        }
      }

      freq_df <- data.frame(x, stringsAsFactors=TRUE)
      names(freq_df)[1] <- x.name

      return(list(n.dim=n.dim, title=txttl, counts=txcnt, miss=txmis, 
                  chi=txchi, lbl=txlbl, freq=x, freq_df=freq_df, prop=xp,
                  pvalue=ch$p.value))
    }
  }  # one variable

}
