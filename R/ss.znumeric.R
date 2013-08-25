.ss.numeric <-
function(x, by=NULL, data, digits.d=NULL, brief, ...) {

  # get variable labels if exist
  gl <- .getlabels()
  x.name <- gl$xn; x.lbl <- gl$xl;
  y.name <- gl$yn; y.lbl <- gl$yl

  max.char <- 0
  if (is.null(by)) n.lines <- 1
  else {
    bu <- as.factor(unique(na.omit(by)))
    n.lines <- length(bu)
    for (i in 1:nlevels(bu))  # largest level name
      if (nchar(levels(bu)[i]) > max.char) max.char <- nchar(levels(bu)[i])
    # split data into the by groups (vectors)
    if (exists(x.name, where=.GlobalEnv)) {
      df <- data.frame(x, by)
      vectors <- split(df$x, df$by)
      rm(df)
     }
    else {  # data only needed if a by variable exists
      vectors <- split(data[,x.name], data[,y.name])
    }
  }

  if (is.null(digits.d)) {
    dig.dec <- .max.dd(x) + 1
    if (dig.dec == 1) dig.dec <- 2
  }
  else dig.dec <- digits.d
  options(digits.d=dig.dec)
  if (dig.dec > 10  && is.null(digits.d)) {
    cat("\nThese data values contain ", dig.dec, " decimal digits. To enhance\n",
        "the readability of the output, only 4 decimal digits are\n",
        "displayed.  To customize this setting, use the digits.d  parameter.\n",
        "Example for Variables Y and X:  > ss(Y, by=X, digits.d=3)\n",
        sep="")
    dig.dec <- 4
  }

  # output
  ndash <- 20
  .title(x.name, y.name, x.lbl, y.lbl, is.null(by))

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
  max.ln <- 0
  for (i in 1:n.lines) {
    if (n.lines == 1) xx <- x
    else {
      lv <- levels(as.factor(by))[i]
      xx <- vectors[[i]]
    } 
    m <- mean(xx)
    s <- sd(xx)
    n.ln <- max(nchar(as.character(round(m, dig.dec))), 
                nchar(as.character(round(s, dig.dec)))) + dig.dec
    if (n.ln > max.ln) max.ln <- n.ln
  }
  if (max.ln < 5) max.ln <- max.ln + 1
  if (max.ln < 10) max.ln <- max.ln + 1

  # the stats
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
    n <- sum(!is.na(xx))
    n.miss <- sum(is.na(xx))
    xx <- na.omit(xx)
    m <- mean(xx)
    s <- sd(xx)
    # skewness:  adjusted Fisher-Pearson standardized moment coefficient
    sk.coef <- n / ((n-1)*(n-2))
    sk.sum <- 0
    for (j in 1:n) sk.sum <- sk.sum + (( (xx[j]-m) / s)^3) 
    sk <- sk.coef * sk.sum
    # kurtosis
    kt.coef1 <- (n*(n+1)) / ((n-1)*(n-2)*(n-3))
    kt.coef2 <- 3 * ( ((n-1)^2) / ((n-2)*(n-3)) )
    kt.sum <- 0
    for (j in 1:n) kt.sum <- kt.sum + ( (xx[j]-m)^4 )
    kt <- ( kt.coef1 * (kt.sum/(var(xx)^2)) ) - kt.coef2
    # order stats
    mn <- min(xx)
    q1 <- quantile(xx, probs=0.25)
    md <- median(xx)
    q3 <- quantile(xx, probs=0.75)
    mx <- max(xx)
    qr <- IQR(xx)

    # print
    if (!brief) {
      out <- c(n, n.miss, m, s, sk, kt, mn, q1, md, q3, mx, qr)
      names(out) <- c("n", "miss", "mean", "sd", "skew", "krts",
                      "min", "qrt1", "mdn", "qrt3", "max", "IQR")
    }
    else {
      out <- c(n, n.miss, m, s, mn, md, mx)
      names(out) <- c("n", "miss", "mean", "sd", "min", "mdn", "max")
    }
    # write names
    if (i == 1) { 
      if (max.ln < 4) max.ln <- max.ln + 2
      if (max.ln < 8) max.ln <- max.ln + 1
      if (n.lines == 1) nbuf <- 1 else nbuf <- 2
      cat(format(names(out[1]), width=nchar(as.character(out[1]))+nbuf+max.lv, 
          justify="right", sep=""))
      nbuf <- 5
      cat(format(names(out[2]), 
          width=nchar(as.character(out[2]))+nbuf, justify="right", sep=""))
      for (i in 3:length(out))
         cat(format(names(out[i]), width=max.ln, justify="right", sep=""))
      cat("\n")
    }
    # write values
    cat(format(p.lv, width=max.lv, justify="right", sep=""))
    cat(format(out[1], width=max.n+1, justify="right", sep=""))
    cat(format(out[2], width=max.nm+5, justify="right", sep=""))
    if (n > 1) for (i in 3:length(out)) 
      cat(format(sprintf("%.*f", dig.dec, out[i]), width=max.ln,
          justify="right", sep=""))
    else cat(format(sprintf("%.*f", dig.dec, out[3]), 
             width=max.ln, justify="right", sep=""))
    cat("\n")
  }

  outliers <- boxplot.stats(xx)$out
  if (length(outliers>0) && unique(na.omit(xx)>3)) {
    outliers <- sort(outliers, decreasing=TRUE)
    ln <- length(outliers)
    if (ln <= 50) {
      cat("\nOutlier")
      if (ln > 1) cat("s: ") else cat(": ")
      for (i in 1:ln) cat(format(outliers[i], scientific=FALSE), " ")
    }
    else {
      cat("\nNumber of outliers:", length(outliers), "\n")
      cat("\n25 largest outliers:\n")
      for (i in 1:25) cat(format(outliers[i], scientific=FALSE), " ")
      cat("\n\n25 smallest outliers:\n")
      for (i in (ln-25):ln) cat(format(outliers[i], scientific=FALSE), " ")
    }
    cat("\n")
  }

  if (n.lines == 1)
    if (!brief) 
      return(list(n=n, miss=n.miss, mean=m, sd=s, skew=sk, kurtosis=kt, 
                  min=mn, quartile1=q1, median=md, quartile3=q3, max=mx, IQR=qr))
    else 
      return(list(n=n, miss=n.miss, mean=m, sd=s, min=mn, median=md, max=mx))

  cat("\n")

}
