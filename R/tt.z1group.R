.OneGroup  <-
function(Y, Ynm, mu0=NULL, n=NULL, m=NULL, s=NULL, 
         brief, bw1, from.data, conf.level, digits.d, mmd, msmd,
         graph, show.title, pdf.file, pdf.width, pdf.height) { 

  # get variable label if exists
  gl <- .getlabels()
  y.lbl <- gl$yl

  if ( (!is.null(y.lbl)) ) {
    cat("Response Variable:  ", Ynm, ", ", as.character(y.lbl), sep="", "\n")
    cat("\n")
  }

  if (!brief) cat("\n------ Description ------\n\n")

  if (from.data) {
    n <- sum(!is.na(Y))
    n.miss <- sum(is.na(Y))
    Y <- na.omit(Y)
    m <- mean(Y)
    s <- sd(Y)
    v <- var(Y)
  }
  else {
    v <- s^2
  }

  clpct <- paste(toString(round((conf.level)*100, 2)), "%", sep="")

  if (Ynm != "Y") cat(Ynm,  ": ", sep="")
  if (from.data) cat(" n.miss = ", n.miss, ",  ", sep="") 
  cat("n = ", n, ",   mean = ", .fmt(m, digits.d), 
      ",  sd = ", .fmt(s, digits.d), sep="", "\n")

  if (brief) cat("\n")
  else  {
    if (from.data) {

      cat("\n\n------ Normality Assumption ------\n\n")
      # Normality
      if (n > 30) {
        cat("Sample mean is normal because n>30, so no test needed.", sep="", "\n")
      }
      else {
        cat("Null hypothesis is a normal distribution", sep="")
        if (Ynm != "Y") cat(" of ", Ynm, ".", sep="") else cat(".")
        cat("\n")
        if (n > 2 && n < 5000) {
          nrm1 <- shapiro.test(Y)
          W.1 <- round(nrm1$statistic,min(4,digits.d+1))
          p.val1 <- round(nrm1$p.value,min(4,digits.d+1))
          cat(nrm1$method, ":  W = ", W.1, ",  p-value = ", p.val1, sep="", "\n")
        }
        else
          cat("Sample size out of range for Shapiro-Wilk normality test.", "\n")
      }  
    } 
  }

  if (!brief) cat("\n\n------ Inference ------\n\n")

  # t-test
  if (!is.null(mu0)) m.dist <- m - mu0
  df <- n - 1
  tcut <- qt((1-conf.level)/2, df=df, lower.tail=FALSE)
  sterr <- s * sqrt(1/n)
  if (from.data) {
    if (!is.null(mu0)) mu.null <- mu0 else mu.null <- 0
    ttest <- t.test(Y, conf.level=conf.level, mu=mu.null)
    df <- ttest$parameter
    lb <- ttest$conf[1]
    ub <- ttest$conf[2]
    E <- (ub-lb)/2
    if (!is.null(mu0)) {
      tvalue <- ttest$statistic
      pvalue <- ttest$p.value
    }
  }
  else {
    E <- tcut * sterr
    lb <- m - E
    ub <- m + E
    if (!is.null(mu0)) {
      tvalue <- m.dist/sterr
      pvalue <- 2 * pt(abs(tvalue), df=df, lower.tail=FALSE)
    }
  }     

  if (!brief) {
    cat("t-cutoff: tcut = ", .fmt(tcut,3), "\n") 
    cat("Standard Error of Mean: SE = ", .fmt(sterr), "\n\n")
  }
  if (!is.null(mu0)) {
    cat("Hypothesized Value H0: mu =", mu0, "\n")
    txt <- "Hypothesis Test of Mean:  t-value = "
    cat(txt, .fmt(tvalue,3), ",  df = ", df, ",  p-value = ", .fmt(pvalue,3), sep="", "\n\n")
  }
  if (!brief)
    cat("Margin of Error for ", clpct, " Confidence Level:  ", .fmt(E), sep="", "\n")
  txt <- " Confidence Interval for Mean:  "
  cat(clpct, txt, .fmt(lb), " to ", .fmt(ub), sep="", "\n")

  # mean difference from mu0 and standardized mean difference
  if (!is.null(mu0)) {
    mdiff <- m - mu0
    smd <- abs(mdiff/s)
    if (!brief) cat("\n\n------ Effect Size ------\n\n") else cat("\n")
    cat("Distance of sample mean from hypothesized:  " , .fmt(mdiff), "\n",
        "Standardized Distance, Cohen's d:  ", .fmt(smd),
        sep="", "\n")
  }
  # densities
  if (from.data && graph && !is.null(mu0)) {

    .opendev(pdf.file, pdf.width, pdf.height)

    .OneGraph(Y, bw1, Ynm, digits.d, brief,
         n, m, mu0, mdiff, s, smd, mmd, msmd,
         clpct, tvalue, pvalue, ub, lb,
         show.title, pdf.file, pdf.width, pdf.height)

    if (!is.null(pdf.file)) {
      dev.off()
      .showfile(pdf.file, "graph of both groups")
    }
  }

}  # End One Group

