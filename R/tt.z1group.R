.OneGroup  <-
function(Y, Ynm, mu0=NULL, n=NULL, m=NULL, s=NULL, 
         brief, from.data, conf.level, digits.d) { 

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

  m.out <- round(m,digits.d)
  s.out <- round(s,digits.d)
  if (Ynm != "Y") cat(Ynm,  ": ", sep="")
  if (from.data) cat(" n.miss = ", n.miss, ",  ", sep="") 
  cat("n = ", n, ",   mean = ", m.out, ",   sd = ", s.out, sep="", "\n")
  if (brief) cat("\n")

  if (!brief) {

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
    if (!brief)
      cat("Distance of Sample Mean from Hypothesized: ", .fmt(m.dist), sep="", "\n")
    txt <- "Hypothesis Test of Mean:  t-value = "
    cat(txt, .fmt(tvalue,3), ",  df = ", df, ",  p-value = ", .fmt(pvalue,3), sep="", "\n\n")
  }
  if (!brief)
    cat("Margin of Error for ", clpct, " Confidence Level:  ", .fmt(E), sep="", "\n")
  txt <- " Confidence Interval for Mean:  "
  cat(clpct, txt, .fmt(lb), " to ", .fmt(ub), sep="", "\n\n")

}  # End One Group

