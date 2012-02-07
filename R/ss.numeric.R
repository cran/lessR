ss.numeric <-
function(x, by=NULL, dframe=mydata, 
         digits.d=NULL, brief=TRUE, ...) {

  dash <- function(ndash) { for (i in 1:(ndash)) cat("-"); cat("\n") }
  
  max.dd <- function(x) {
  
    n.dec <-function(x) {
      xc <- as.character(x)
      nchar(xc)
      ipos <- 0
      for (i in 1:nchar(xc)) if (substr(xc,i,i)==".") ipos <- i
      if (ipos > 0) n.dec <- nchar(xc)-ipos else n.dec <- 0
      return(n.dec)
    }
     
    max.dd <- 0
    for (i in 1:length(x))
      if (!is.na(x[i])) if (n.dec(x[i]) > max.dd ) max.dd <- n.dec(x[i])   
    return(max.dd)
  }
 
  
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
    else { 
      vectors <- split(dframe[,x.name], dframe[,y.name])
}
  }

  if (is.null(digits.d)) dig.dec <- max.dd(x) + 1 else dig.dec <- digits.d
  if (dig.dec > 10  && is.null(digits.d)) {
    cat("\nThese data values contain ", dig.dec, " decimal digits. To enhance\n",
        "the readability of the output, only 4 decimal digits are\n",
        "displayed.  To customize this setting, use the digits.d  parameter.\n",
        "Example for Variables Y and X:  > ss(Y, by=X, digits.d=3)\n\n",
        sep="")
    dig.dec <- 4
  }

  # use variable label if it exists
  x.lbl <- ""
  y.lbl <- ""
  if (exists("mylabels")) {
    x.lbl <- as.character(mylabels[which(row.names(mylabels)==x.name), "label"])
    if (length(x.lbl) == 0) x.lbl <- "" 
    if (!is.null(by)) {
      y.lbl <- as.character(mylabels[which(row.names(mylabels)==y.name), "label"])
      if (length(y.lbl) == 0) y.lbl <- ""
    }
  }

  # output
  ndash <- 20
  txt1 <- x.name
  if (nchar(x.lbl) > 0) {
    txt1 <- paste(txt1, ", ", x.lbl, sep="")
    ndash <- nchar(txt1)
  }
  if (is.null(by)) txt1 <- paste("---", txt1, "---")
  if (nchar(x.lbl) > 0) txt1 <- paste(txt1)
  if (!is.null(by)) {
    txt2 <- paste("  by\n", y.name, sep="")
    if (nchar(y.lbl) > 0) {
      txt2 <- paste(txt2, ", ", y.lbl, sep="") 
      ndash <- max(nchar(txt1),nchar(txt2))
    }
  }

  cat("\n")
  cat(txt1, "\n")
  if (!is.null(by)) {
    cat(txt2, "\n")
  }

  if (is.null(by)) cat("\n") else dash(ndash)

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
    # skewness
    sk.coef <- n / ((n-1)*(n-2))
    sk.sum <- 0
    for (j in 1:n) sk.sum <- sk.sum + (( (xx[j]-m) / s)^3) 
    sk <- sk.coef * sk.sum
    # kurtosis
    kt.coef1 <- (n*(n+1)) /  ((n-1)*(n-2)*(n-3))
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
    # print
    if (!brief) {
      out <- c(n, n.miss, m, s, sk, kt, mn, q1, md, q3, mx)
      names(out) <- c("n", "miss", "mean", "sd", "skew", "krts",
                      "min", "1stQrt", "median", "3rdQrt", "max")
    }
    else {
      out <- c(n, n.miss, m, s, mn, md, mx)
      names(out) <- c("n", "miss", "mean", "sd", "min", "median", "max")
    }
    max.ln <- max(nchar(as.character(ceiling(m))), nchar(as.character(ceiling(s)))) + dig.dec
    if (max.ln < 4) max.ln <- max.ln+5 else max.ln <- max.ln+4
    if (i == 1) {
      if (n.lines == 1) nbuf <- 1 else nbuf <- 2
      cat(format(names(out[1]), width=nchar(as.character(out[1]))+nbuf+max.lv, 
          justify="right", sep=""))
      if (n.lines == 1) nbuf <- 5 else nbuf <- 5
      cat(format(names(out[2]), 
          width=nchar(as.character(out[2]))+nbuf, justify="right", sep=""))
    for (i in 3:length(out))
       cat(format(names(out[i]), width=max.ln, justify="right", sep=""))
    cat("\n")
    }
    cat(format(p.lv, width=max.lv, justify="right", sep=""))
    cat(format(out[1], width=max.n+1, justify="right", sep=""))
    cat(format(out[2], width=max.nm+5, justify="right", sep=""))
    if (n > 1) for (i in 3:length(out)) 
      cat(format(sprintf("%.*f", dig.dec, out[i]), width=max.ln, justify="right", sep=""))
    else cat(format(sprintf("%.*f", dig.dec, out[3]), width=max.ln-1, justify="right", sep=""))
    cat("\n")
  }
  
  outliers <- boxplot.stats(xx)$out
  if (length(outliers>0) && unique(na.omit(xx)>3)) {
    cat("\nOutlier")
    if (length(outliers) > 1) cat("s: ") else cat(": ")
    for (i in 1:length(outliers)) cat(format(outliers[i], scientific=FALSE), " ")
    cat("\n")
  }

cat("\n")

}
