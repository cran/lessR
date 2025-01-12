.ss.numeric <-
function(x, by=NULL, digits_d=NULL, brief, y.name=NULL, facet1.nm=FALSE,
         x.name=NULL, ...) {

  # get variable labels if exist
  # graph.win=FALSE turns off call to par, so blank window in R not produced
  gl <- .getlabels(graph.win=FALSE)

  if (!is.null(gl$yn))  # from Plot(Salary, stat="count")
    y.name <- ifelse (is.null(y.name), gl$yn, y.name)  # sometimes need facet1.name

  if (is.null(x.name)) {
    x.name <- gl$xn
    x.lbl <- gl$xl
  }
  else  # option just for pc.main
    x.lbl <- NULL

  if (facet1.nm) {
    gl <- .getlabels(graph.win=FALSE, facet1.nm=TRUE)
  }
  y.lbl <- gl$yl

  max.char <- 0
  if (is.null(by))
    n.lines <- 1
  else {
    bu <- as.factor(unique(na.omit(by)))
    n.lines <- nlevels(bu)
    for (i in 1:nlevels(bu))  # largest level name
      if (nchar(levels(bu)[i]) > max.char) max.char <- nchar(levels(bu)[i])
    vectors <- split(x, by)
  }

  if (is.null(digits_d)) {
    digits_d <- .max.dd(x) + 1
    if (digits_d == 1) digits_d <- 2
  }
  options(digits_d=digits_d)
  if (digits_d > 10  && is.null(digits_d)) {
    cat("\nThese data values contain ", digits_d, " decimal digits. To enhance\n",
        "the readability of the output, only 4 decimal digits are\n",
        "displayed.  To customize this setting, use the digits_d  parameter.\n",
        "Example for Variables Y and X:  > ss(Y, by=X, digits_d=3)\n\n",
        sep="")
    digits_d <- 4
  }

  # get maximum chars in 1st three columns
  max.lv <- 0; max.n <- 0; max.nm <- 0
  if (n.lines > 1) {
    for (i in 1:n.lines) {
      nch.lv <- nchar(as.character(levels(as.factor(by))[i]))
      nch.n <- nchar(as.character(sum(!is.na(vectors[[i]]))))
      nch.nm <- nchar(as.character(sum(is.na(vectors[[i]]))))
      if (nch.lv > max.lv) max.lv <- nch.lv
      if (nch.n > max.n) max.n <- nch.n
      if (nch.nm > max.nm) max.nm <- nch.nm
    }
  }
  else {
    if (max.lv < 3) max.lv <- 3
    max.n <- nchar(as.character(sum(!is.na(x))))
    max.nm <- nchar(as.character(sum(is.na(x))))
  }

  # get max.ln, maximum length of the individual fields
  # (this computes the mean and sd twice for each line, also for output)
  max.ln <- 0
  for (i in 1:n.lines) {
    if (n.lines == 1)
      xx <- x
    else {
      lv <- levels(as.factor(by))[i]
      xx <- vectors[[i]]
    }
    m <- mean(xx, na.rm=TRUE)
    s <- sd(xx, na.rm=TRUE)
    if (is.na(s) || is.null(s))
      n.ln <- nchar(as.character(round(m, digits_d))) + digits_d
    else
      n.ln <- max(nchar(as.character(round(m, digits_d))),
                  nchar(as.character(round(s, digits_d)))) + digits_d
    if (n.ln > max.ln) max.ln <- n.ln
  }
  if (max.ln < 5) max.ln <- max.ln + 1
  if (max.ln < 10) max.ln <- max.ln + 1

  tx <- character(length = 0)

  # ------
  # output

  # first the title with any variable labels
  txlbl <- .title2(x.name, y.name, x.lbl, y.lbl, is.null(by))
  if (length(txlbl) > 1) if (substr(txlbl[2],1,1) ==  "\n")
    txlbl[2] <- sub("\n", "", txlbl[2])
# txlbl[length(txlbl)+1] <- ""
  tx <- txlbl
  advance <- ifelse (x.name != "*NONE*", 1, 0)
  if (x.name == "*NONE*") tx <- ""
  # --------------------------------
  # the stats loop
  for (i in 1:n.lines) {
    if (n.lines == 1) {
      xx <- x
      p.lv <- ""
    }
    else {
      lv <- levels(as.factor(by))[i]
      xx <- vectors[[i]]
      p.lv <- format(lv, width=max.char+1)
    }

    m <- NA; s <- NA; mn <- NA; mx <- NA; md=NA
    sk <- NA; kt <- NA; q1 <- NA; q3 <- NA; qr <- NA
    # get the descriptive statistics
    n <- sum(!is.na(xx))
    n.miss <- sum(is.na(xx))
    xx <- na.omit(xx)
    if (n>0) m <- mean(xx)
    if (n>1) s <- sd(xx)

    # skewness:  adjusted Fisher-Pearson standardized moment coefficient
    sk <- skew(xx)

    # kurtosis
    kt <- kurtosis(xx)

    # order stats
    if (n > 0) {
      mn <- min(xx)
      q1 <- quantile(xx, probs=0.25)
      md <- median(xx)
      q3 <- quantile(xx, probs=0.75)
      mx <- max(xx)
      qr <- IQR(xx)
    }


    if (i == 1) { # heading labels
      if (max.ln < 4) max.ln <- max.ln + 2
      if (max.ln < 8) max.ln <- max.ln + 1
      nbuf <- ifelse (n.lines==1, 3, 5)

      n.lbl <- .fmtc("n", nchar(as.character(n))+nbuf+max.lv-2)
      miss_lbl <- .fmtc("miss", nchar(as.character(n.miss))+5)
      m.lbl <- .fmtc("mean", max.ln)
      s.lbl <- .fmtc("sd", max.ln)
      mn.lbl <- .fmtc("min", max.ln)
      md.lbl <- .fmtc("mdn", max.ln)
      mx.lbl <- .fmtc("max", max.ln)

      tx[length(tx)+1] <- ""
      if (brief)
        tx[length(tx)+advance] <- paste(n.lbl, miss_lbl, m.lbl, s.lbl, mn.lbl,
                                        md.lbl, mx.lbl)
      else {
        sk.lbl <- .fmtc("skew", max.ln)
        kt.lbl <- .fmtc("krts", max.ln)
        q1.lbl <- .fmtc("qrt1", max.ln)
        q3.lbl <- .fmtc("qrt3", max.ln)
        qr.lbl <- .fmtc("IQR", max.ln)
        tx[length(tx)+1] <- paste(n.lbl, miss_lbl, m.lbl, s.lbl, sk.lbl,
              kt.lbl, mn.lbl, q1.lbl, md.lbl, q3.lbl, mx.lbl, qr.lbl)
      }
    }  # end first line, labels

    # write values
    lvl <- .fmtc(p.lv, max.lv)
    n.c <- .fmti(n, max.n+1)
    miss_c <- .fmti(n.miss, max.nm+5)

    if (n == 0)
      tx[length(tx)+1] <- paste(lvl, n.c, miss_c)

    else if (n == 1) {
        m.c <- .fmt(m, digits_d, max.ln)
        tx[length(tx)+1] <- paste(lvl, n.c, miss_c, m.c)
    }
    else if (n > 1) {
      m.c <- .fmt(m, digits_d, max.ln)
      s.c <- .fmt(s, digits_d, max.ln)
      mn.c <- .fmt(mn, digits_d, max.ln)
      md.c <- .fmt(md, digits_d, max.ln)
      mx.c <- .fmt(mx, digits_d, max.ln)

      if (brief)
        tx[length(tx)+1] <- paste(lvl, n.c, miss_c, m.c, s.c, mn.c, md.c, mx.c)
      else {
        sk.c <- .fmt(sk, digits_d, max.ln)
        kt.c <- .fmt(kt, digits_d, max.ln)
        q1.c <- .fmt(q1, digits_d, max.ln)
        q3.c <- .fmt(q3, digits_d, max.ln)
        qr.c <- .fmt(qr, digits_d, max.ln)
        tx[length(tx)+1] <- paste(lvl, n.c, miss_c, m.c, s.c, sk.c, kt.c,
             mn.c, q1.c, md.c, q3.c, mx.c, qr.c)
      }
    }

  }  # for each line
  tx[length(tx)+1] <- ""

  if (n.lines == 1)
    return(list(txlbl=txlbl, tx=tx, n=n, n.miss=n.miss, m=m, s=s, sk=sk,
                kt=kt, mn=mn, q1=q1, md=md, q3=q3, mx=mx, qr=qr))
  else
    return(list(tx=tx))  # contains title with var labels and stats

}
