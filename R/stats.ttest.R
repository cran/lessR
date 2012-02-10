stats.ttest <-
function(n = NULL, m = NULL, s = NULL, mu0 = NULL, 
         n1 = NULL, n2 = NULL,  m1 = NULL, m2 = NULL, s1 = NULL, s2 = NULL, 
         Ynm = "Y", Xnm = "X", X1nm = "Group1", X2nm = "Group2", 
         conf.level = 0.95, digits.d = NULL, ...) {


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
    if (!is.null(m)) if (n.dec(m) > max.dd ) max.dd <- n.dec(m)   
    if (!is.null(s)) if (n.dec(s) > max.dd ) max.dd <- n.dec(s)   
    if (!is.null(m1)) if (n.dec(m1) > max.dd ) max.dd <- n.dec(m1)   
    if (!is.null(m2)) if (n.dec(m2) > max.dd ) max.dd <- n.dec(m2)   
    if (!is.null(s1)) if (n.dec(s1) > max.dd ) max.dd <- n.dec(s1)   
    if (!is.null(s2)) if (n.dec(s2) > max.dd ) max.dd <- n.dec(s2)   
    return(max.dd)
}


cat("\n")

two.group = FALSE
if (is.null(n) && !is.null(n1)) two.group = TRUE

if (!two.group) {
  if (is.null(mu0)) {
    cat("\nNo value for mu0, specified, so only do confidence interval. \n")
  }
}

# error conditions
if (is.null(n)) {
  if (is.null(n1) | is.null(n2)) {
  cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Specify a sample size for each group. \n\n")
  }
  if (is.null(m1) | is.null(m2)) {
  cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Specify a mean for each group. \n\n")
  }
  if (is.null(s1) | is.null(s2)) {
  cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Specify a standard deviation for each group. \n\n")
  }
}

# get max digits to right of and including the decimal pt 
if (is.null(digits.d)) digits.d <- max.dd(x) + 1
if (digits.d > 10) {
  cat("\nThese data contain", digits.d, "significant digits.\n")
  cat("Perhaps specify a smaller number to display with the  digits.d  parameter.\n")
}


if (two.group) { 
  cat("\n")
  cat("------------------------------------------------------------\n")
  cat("Compare", Ynm, "across", Xnm, "levels", X1nm, "and", X2nm, "\n")
  cat("------------------------------------------------------------\n\n")

  cat("------ Description ------\n\n")

  clpct <- paste(toString(round((conf.level)*100, 2)), "%", sep="")

  m1.out <- round(m1,digits.d)
  m2.out <- round(m2,digits.d)
  s1.out <- round(s1,digits.d)
  s2.out <- round(s2,digits.d)
  Xnmval <- paste(Xnm, X1nm)
  cat(Ynm, " for ", Xnmval, ":  n = ", n1,
      "    mean = ", m1.out, ",   sd = ", s1.out, sep="", "\n")
  Xnmval <- paste(Xnm, X2nm)
  cat(Ynm, " for ", Xnmval, ":  n = ", n2,
      "    mean = ", m2.out, ",   sd = ", s2.out, sep="", "\n\n")

  # sw
  v1 <- s1^2
  v2 <- s2^2
  df1 <- n1 - 1
  df2 <- n2 - 1
  swsq <- (df1*v1 + df2*v2) / (df1 + df2)
  sw <- sqrt(swsq)
  sw.out <- round(sw,digits.d)
  txt <- "Equal Group Variances Assumed, Within-group Standard Deviation:  "
  cat(txt, sw.out, "\n\n")

  # mean difference and standardized mean difference
  m.diff <- m1 - m2
  m.diff.out <- round(m.diff,digits.d)
  cat("Mean Difference of ", Ynm, ":  " , m.diff.out, sep="", "\n\n")
  
  # smd
  d <- m.diff/sw
  d.out <- round(d,digits.d)
  cat("Standardized Mean Difference of", Ynm)
    cat(", Cohen's d:  ", d.out, sep="", "\n")


  cat("\n\n------ Homogeneity of Variance------\n\n")

  cat("Note:  This hypothesis test performs poorly in non-normal samples and", "\n")
  cat("       the t-test is typically robust to violations of assumptions.", "\n")
  cat("       Use as a heuristic guide instead of interpreting literally.", "\n\n")


  # Homogeneity of Variance
  if (v1 >= v2) {
    vratio <- v1/v2
    vr <- paste(toString(round(v1,digits.d+1)), "/", toString(round(v2,digits.d+1)), sep="")
    df.num <- df1
    df.den <- df2
  }
  else {
    vratio <- v2/v1
    vr <- paste(toString(round(v2,digits.d+1)), "/", toString(round(v1,digits.d+1)), sep="")
    df.num <- df2
    df.den <- df1
  }

  v.out <- round(vratio,digits.d+1)

  p.var <- pf(vratio, df1=df.num, df2=df.den)
  p.var <- 2 * min(p.var, 1-p.var)  # adjust for two-sided test, results same as var.test{stats}
  pv.out <- round(p.var,min(4,digits.d+1))

  cat("Null hypothesis is equal variances of ", Ynm, ", i.e., homogeneous.", sep="", "\n")

  cat("Variance Ratio test:  F = ", vr, " = ", v.out, ",  df = ", df.num, ";", df.den, 
         ",  p-value = ", pv.out, sep="", "\n")


  cat("\n\n------ Inference ------\n\n")

  cat("Hypothesized Value of Mean Difference:", 0, "\n\n")

  # t-test
  df <- df1 + df2
  sterr <- sw * sqrt(1/n1 + 1/n2)
  t.cut <- qt((1-conf.level)/2, df=df, lower.tail=FALSE)
  E <- t.cut * sterr
  lb <- m.diff - E
  ub <- m.diff + E
  t.value <- m.diff/sterr
  p.value <- round(2 * pt(t.value, df=df, lower.tail=FALSE), min(4,(digits.d+1)))
  
  t.value <- round(t.value, min(2,digits.d+1))
  cat("Standard Error of Mean Difference: SE = ", round(sterr,digits.d), "\n\n")
  cat("Hypothesis Test of Mean Diff:  t-value =", t.value,
      "  df = ", df, ",  p-value = ", p.value, sep="", "\n\n")
  cat("Margin of Error for ", clpct, " Confidence Level:  ", round(E,digits.d), sep="", "\n")
  title <- " Confidence Interval for Mean Difference:  "
  cat(clpct, title, round(lb,digits.d), " to ", round(ub,digits.d), sep="", "\n\n")

  # smd confidence interval  
  check.MBESS <- suppressWarnings(require(MBESS, quietly=TRUE))
  if (check.MBESS) {
    cid <- ci.smd(smd=d, n.1=n1, n.2=n2, conf.level=conf.level)
    deltaL <- round(cid$Lower.Conf.Limit.smd,digits.d)
    deltaU <- round(cid$Upper.Conf.Limit.smd,digits.d)
    cat(clpct," Confidence Interval for smd:  ", deltaL, " to ", deltaU, sep="", "\n")
  }
  else {
    cat(">>> The confidence interval for smd requires package MBESS.", "\n")
    cat(">>> Confidence interval for smd not provided here, but all other output unaffected.", "\n")
    cat(">>> To get the MBESS package, run one time only: install.packages('MBESS')", "\n")
    cat(">>> IGNORE resulting 'Error in eval' error message below.", "\n")
  }
  cat("\n")

} #  end two-group



else {  # one-group

  cat("\n")
  cat("------------------------------------------------------------\n")
  cat("Analyze", Ynm) 
  if (!is.null(mu0)) cat(" for Hypothesized Value H0: mu =", mu0)
  cat("\n")
  cat("------------------------------------------------------------\n\n")

  cat("------ Description ------\n\n")

  clpct <- paste(toString(round((conf.level)*100, 2)), "%", sep="")

  m.out <- round(m,digits.d)
  s.out <- round(s,digits.d)
  cat(Ynm,  ":  n = ", n, ",   mean = ", m.out, ",   sd = ", s.out, sep="", "\n")

  # distance of mean from hypothesized value
  if (!is.null(mu0)) {
    m.dist <- m - mu0
    m.dist.out <- round(m.dist,digits.d)
    cat("\nDistance of Sample Mean from Hypothesized Value: ", m.dist.out, sep="", "\n")
  }

  cat("\n\n------ Inference ------\n\n")

  # t-test
  df <- n - 1
  sterr <- s * sqrt(1/n)
  t.cut <- qt((1-conf.level)/2, df=df, lower.tail=FALSE)
  E <- t.cut * sterr
  lb <- m - E
  ub <- m + E
  if (!is.null(mu0)) {
    t.value <- m.dist/sterr
    p.value <- round(2 * pt(t.value, df=df, lower.tail=FALSE), min(4,(digits.d+1)))
  }
 
  cat("Standard Error of Mean: SE = ", round(sterr,digits.d), "\n\n")
  if (!is.null(mu0)) {
    t.value <- round(t.value, min(2,digits.d+1))
    txt <- "Hypothesis Test of Mean:  t-value = "
    cat(txt, t.value, ",  df = ", df, ",  p-value = ", p.value, sep="", "\n\n")
  }
  cat("Margin of Error for ", clpct, " Confidence Level:  ", round(E,digits.d), sep="", "\n")
  txt <- " Confidence Interval for Mean:  "
  cat(clpct, txt, round(lb,digits.d), " to ", round(ub,digits.d), sep="", "\n\n")

}  # end one-group

}
