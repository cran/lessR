tt.default <-
function(x, y=null, 
         n = NULL, m = NULL, s = NULL, mu0 = NULL, 
         n1 = NULL, n2 = NULL,  m1 = NULL, m2 = NULL, s1 = NULL, s2 = NULL, 
         Ynm = "Y", Xnm = "X", X1nm = "Group1", X2nm = "Group2", 
         brief=FALSE, digits.d = NULL, 
         conf.level = 0.95, mmd = NULL, msmd = NULL, 
         bw1 = "nrd", bw2 = "nrd", ...) {


max.dd <- function(x) {

  n.dec <-function(xn) {
    xc <- as.character(xn)
    nchar(xc)
    ipos <- 0
    for (i in 1:nchar(xc)) if (substr(xc,i,i)==".") ipos <- i
    if (ipos > 0) n.dec <- nchar(xc)-ipos else n.dec <- 0
    return(n.dec)
  }
   
  max.dd <- 0
  if (from.data) {
    for (i in 1:length(x))
      if (!is.na(x[i])) if (n.dec(x[i]) > max.dd) max.dd <- n.dec(x[i])
  }
  else {
    if (!is.null(m)) if (n.dec(m) > max.dd) max.dd <- n.dec(m)   
    if (!is.null(s)) if (n.dec(s) > max.dd) max.dd <- n.dec(s)   
    if (!is.null(m1)) if (n.dec(m1) > max.dd) max.dd <- n.dec(m1)   
    if (!is.null(m2)) if (n.dec(m2) > max.dd) max.dd <- n.dec(m2)   
    if (!is.null(s1)) if (n.dec(s1) > max.dd) max.dd <- n.dec(s1)   
    if (!is.null(s2)) if (n.dec(s2) > max.dd) max.dd <- n.dec(s2)   
  }
  return(max.dd)

}


TwoGroup <-
function(YA, YB, n1=NULL, n2=NULL, m1=NULL, m2=NULL, s1=NULL, s2=NULL) {

  cat("------------------------------------------------------------\n")
  cat("Compare", Ynm, "across", Xnm, "levels", X1nm, "and", X2nm, "\n")
  cat("------------------------------------------------------------\n\n")

  # use variable label if it exists
  x.lbl <- ""
  y.lbl <- ""
  if (exists("mylabels")) {
    x.lbl <- as.character(mylabels[which(row.names(mylabels)==Xnm), "label"])
    if (length(x.lbl) == 0) x.lbl <- "" 
    y.lbl <- as.character(mylabels[which(row.names(mylabels)==Ynm), "label"])
    if (length(y.lbl) == 0) y.lbl <- ""
  }
  if ( (nchar(x.lbl) > 0) || (nchar(y.lbl) > 0) ) {
    cat("Response Variable:  ", Ynm, ", ", as.character(x.lbl), sep="", "\n")
    cat("Grouping Variable:  ", Xnm, ", ", as.character(y.lbl), sep="", "\n")
    cat("\n")
  }

  if (!brief) cat("\n------ Description ------\n\n")

  if (from.data) {
    n1 <- sum(!is.na(YA))
    n2 <- sum(!is.na(YB))
    n1.miss <- sum(is.na(YA))
    n2.miss <- sum(is.na(YB))
    YA <- na.omit(YA)
    YB <- na.omit(YB)
    m1 <- mean(YA)
    m2 <- mean(YB)
    s1 <- sd(YA)
    s2 <- sd(YB)
    v1 <- var(YA)
    v2 <- var(YB)
  }
  else {
    v1 <- s1^2
    v2 <- s2^2
  }

  clpct <- paste(toString(round((conf.level)*100, 2)), "%", sep="")
  m1.out <- round(m1,digits.d)
  m2.out <- round(m2,digits.d)
  s1.out <- round(s1,digits.d)
  s2.out <- round(s2,digits.d)
  Xnmval <- paste(Xnm, X1nm)
  cat(Ynm, " for ", Xnmval, ":  ", sep="")
  if (from.data) cat("n.miss = ", n1.miss, ",  ", sep="")
  cat("n1 = ", n1, sep="")
  cat(",  mean = ", m1.out, ",  sd = ", s1.out, sep="", "\n")
  Xnmval <- paste(Xnm, X2nm)
  cat(Ynm, " for ", Xnmval, ":  ", sep="")
  if (from.data) cat("n.miss = ", n2.miss, ",  ", sep="")
  cat("n2 = ", n2, sep="")
  cat(",  mean = ", m2.out, ",  sd = ", s2.out, sep="", "\n")
  cat("\n")

  # sw
  df1 <- n1 - 1
  df2 <- n2 - 1
  swsq <- (df1*v1 + df2*v2) / (df1 + df2)
  sw <- sqrt(swsq)
  sw.out <- round(sw,digits.d)
  if (!brief) cat("Equal Group Variances Assumed, Within-group",
                  "Standard Deviation:  ", sw.out, "\n\n")

  # mean difference and standardized mean difference
  mdiff <- m1 - m2
  mdiff.out <- round(mdiff,digits.d)
  cat("Mean Difference of ", Ynm, ":  " , mdiff.out, sep="", "\n\n")
  
  # smd
  smd <- mdiff/sw
  smd.out <- round(smd,digits.d)
  cat("Standardized Mean Difference of ", Ynm, ", ",
      "Cohen's d:  ", smd.out, sep="", "\n")


  if (!brief) {

    cat("\n\n------ Assumptions ------\n\n")

    cat("Note: These hypothesis tests can perform poorly, and the", "\n")
    cat("      t-test is typically robust to violations of assumptions.", "\n")
    cat("      Use as heuristic guides instead of interpreting literally.", "\n\n")

    if (from.data) {
      # Normality
      cat("Null hypothesis, for each group, is a normal distribution of ", sep="")
      cat(Ynm, ".", sep="", "\n")
      if (n1 > 30) {
        cat("Group " , X1nm, ": ", sep="")
        cat("Sample mean is normal because n>30, so no test needed.", sep="", "\n")
      }
      else {
        cat("Group", X1nm, " ")
        if (n1 > 2 && n1 < 5000) {
          nrm1 <- shapiro.test(YA)
          W.1 <- round(nrm1$statistic,min(4,digits.d+1))
          p.val1 <- round(nrm1$p.value,min(4,digits.d+1))
          cat(nrm1$method, ":  W = ", W.1, ",  p-value = ", p.val1, sep="", "\n")
        }
        else
          cat("Sample size out of range for Shapiro-Wilk normality test.", "\n")
      }  
      if (n2 > 30) {
        cat("Group " , X2nm, ": ", sep="")
        cat("Sample mean is normal because n>30, so no test needed.", sep="", "\n")
      }
      else {
        cat("Group", X2nm, " ")
        if (n2 > 2 && n2 < 5000) {
          nrm2 <- shapiro.test(YB)
          W.2 <- round(nrm2$statistic,min(4,digits.d+1))
          p.val2 <- round(nrm2$p.value,min(4,digits.d+1))
          cat(nrm2$method, ":  W = ", W.2, ",  p-value = ", p.val2, sep="", "\n")
        }
        else
          cat("Sample size out of range for Shapiro-Wilk normality test.", "\n")
      }  
      cat("\n")
    } 

    # Homogeneity of Variance
    # Var Ratio
    v1.out <- toString(round(v1,digits.d+1))
    v2.out <- toString(round(v2,digits.d+1))
    if (v1 >= v2) {
      vratio <- v1/v2
      vr <- paste(v1.out, "/", v2.out, sep="")
      df.num <- df1
      df.den <- df2
    }
    else {
      vratio <- v2/v1
      vr <- paste(v2.out, "/", v1.out, sep="")
      df.num <- df2
      df.den <- df1
    }
    v.out <- round(vratio,digits.d+1)

    p.var <- pf(vratio, df1=df.num, df2=df.den)
    # adjust for two-sided test, results same as var.test{stats}
    p.var <- 2 * min(p.var, 1-p.var)
    pv.out <- round(p.var,min(4,digits.d+1))

    cat("Null hypothesis is equal variances of ")
    cat(Ynm, ", i.e., homogeneous.", sep="", "\n")

    cat("Variance Ratio test:  F = ", vr, " = ", v.out, ",  df = ", df.num, ";", 
        df.den, ",  p-value = ",  pv.out, sep="", "\n")

    if (from.data) {
      # Levene
      YAm <- abs(YA - median(YA))
      YBm <- abs(YB - median(YB))
      t.bf <- t.test(YAm, YBm, var.equal=TRUE)
      tvalue.bf <- round(t.bf$statistic,min(4,digits.d+1))
      df.bf <- round(t.bf$parameter,min(4,digits.d+1))
      pvalue.bf <- round(t.bf$p.value,min(4,digits.d+1))
      cat("Levene's test, Brown-Forsythe:  t = ", tvalue.bf, ",  df = ", df.bf, sep="")
      cat(",  p-value = ", pvalue.bf, sep="", "\n")
    }
  }


  if (!brief) cat("\n\n------ Inference ------\n\n") else cat("\n---\n")

  # t-test
  sterr <- sw * sqrt(1/n1 + 1/n2)
  df <- df1 + df2
  tcut <- qt((1-conf.level)/2, df=df, lower.tail=FALSE)
  if (from.data) {
    ttest <- t.test(YA, YB, var.equal=TRUE, conf.level=conf.level)
    lb <- round(ttest$conf[1],digits.d)
    ub <- round(ttest$conf[2],digits.d)
    E <- round((ub-lb)/2,digits.d)
    tvalue <- round(ttest$statistic, min(2,digits.d))
    pvalue <- round(ttest$p.value, min(4,digits.d+1))
  }
  else {
    sterr <- sw * sqrt(1/n1 + 1/n2)
    E <- round(tcut*sterr, digits.d)
    lb <- round(mdiff-E, digits.d)
    ub <- round(mdiff+E, digits.d)
    tvalue <- round(mdiff/sterr, min(2,digits.d))
    pvalue <- round(2 * pt(abs(tvalue), df=df, lower.tail=FALSE), min(4,(digits.d+1)))
  }
  

  if (!brief) {
    cat("t-cutoff: tcut = ", round(tcut,3), "\n") 
    cat("Standard Error of Mean Difference: SE = ", round(sterr,digits.d), "\n\n")
  }
  mytitle <- "\nHypothesis Test of 0 Mean Diff:  t = "
  cat(mytitle, tvalue, ",  df = ", df, ",  p-value = ", pvalue, sep="", "\n\n")
  if (!brief) 
    cat("Margin of Error for ", clpct, " Confidence Level:  ", E, sep="", "\n")
  cat(clpct," Confidence Interval for Mean Difference:  ", lb, " to ", ub, 
      sep="", "\n\n")

  check.MBESS <- suppressWarnings(require(MBESS, quietly=TRUE))
  if (check.MBESS) {
    cid <- ci.smd(smd=smd, n.1=n1, n.2=n2, conf.level=conf.level)
    deltaL <- round(cid$Lower.Conf.Limit.smd,digits.d)
    deltaU <- round(cid$Upper.Conf.Limit.smd,digits.d)
    cat(clpct," Confidence Interval for smd:  ", deltaL, " to ", deltaU, sep="", "\n")
  }
  else {
    cat(">>> The confidence interval for smd requires package MBESS.", "\n")
    cat(">>> Confidence interval for smd not provided here, but all other output unaffected.", "\n")
    cat(">>> To get the MBESS package, run one time only: install.packages('MBESS')", "\n")
    cat(">>> If present, IGNORE resulting 'Error in eval' error message below.", "\n")
    deltaL <- NULL
    deltaU <- NULL
  }


  if (!brief) {

    cat("\n\n------ Practical Importance ------\n\n")
    cat("Minimum Mean Difference of practical importance: mmd\n")
    if ( !is.null(mmd) | !is.null(msmd) ) {
      if (!is.null(mmd)) msmd <- mmd / sw
      if (!is.null(msmd)) mmd <- msmd * sw
      cat("Compare mmd =", round(mmd,digits.d), " to the obtained value of md = ", mdiff.out, "\n")
      cat("Compare mmd to the confidence interval for md: ", lb, " to ", ub, "\n\n")
      cat("Minimum Standardized Mean Difference of practical importance: msmd\n")
      cat("Compare msmd = ", round(msmd,digits.d), " to the obtained value of smd = ", smd.out,"\n")
      if (!is.null(deltaL)) cat("Compare smd to the confidence interval for smd: ", 
          deltaL, " to ", deltaU, "\n")
    }
    else {
      cat("Minimum Standardized Mean Difference of practical importance: msmd\n")
      cat("Neither value specified, so no analysis\n")
    }

  }

  if (from.data) {
    # densities
    dYA <- density(YA, bw1)
    dYB <- density(YB, bw2)

    if (!brief) {
        cat("\n\n------ Graphics Smoothing Parameter ------\n\n")
        mytitle <- "Density bandwidth for "
        cat(mytitle, Xnm, " ", X1nm, ": ", round(dYA$bw,digits.d), sep="", "\n")
        cat(mytitle, Xnm, " ", X2nm, ": ", round(dYB$bw,digits.d), sep="", "\n\n")
    }

    cat("--------------------------------------------------\n")

    # values needed for graph
    min.x <- min(min(dYA$x),min(dYB$x))  # min x coordinate for graph
    max.x <- max(max(dYA$x),max(dYB$x))  # max x coordinate for graph
    max.y <- max(max(dYA$y),max(dYB$y))  # max y coordinate
    max.y <- max.y+.1*max.y  # allow room in graph region for d info

    # colors
    col.1 <- rgb(.63,.46,.15)
    col.1t <- rgb(.63,.46,.15, alpha=.5)
    col.2 <- rgb(.49,.56,.69)
    col.2t <- rgb(.49,.56,.69, alpha=.5)

    # set up coordinate system
    par(mar=c(3,3,8,.4), mgp=c(2,.6,0), cex.axis=1, cex.lab=1)
    plot.new()
    plot.window(xlim=c(min.x,max.x), ylim=c(0,max.y))
    axis(1); axis(2); box()
    title(xlab=Ynm, ylab="Density")

    xleft <- par("usr")[1]  # left side of graph
    xright <- par("usr")[2]  # right side of graph
    ybot <- par("usr")[3]  # bottom of graph
    ytop <- par("usr")[4]  # height of graph

    # vertical line for mean
    lines(c(m1,m1), c(0,ytop), lty="solid", lwd=2, col=col.1)
    lines(c(m2,m2), c(0,ytop), lty="twodash", lwd=2, col=col.2)
    
    # curve area
    polygon(c(min(dYA$x),dYA$x,max(dYA$x)), c(0,dYA$y,0), col=col.1t, border=NA, 
      density=10, angle=45)
    polygon(c(min(dYB$x),dYB$x,max(dYB$x)), c(0,dYB$y,0), col=col.2t, border=NA, 
      density=10, angle=-45)
      
    # bottom border of density curve  
    segments(min(dYA$x), 0, max(dYA$x), 0, col=col.1)
    segments(min(dYB$x), 0, max(dYB$x), 0, col=col.2)
    
    # density curve
    lines(dYA, col=col.1t, lty="solid", lwd=1.5)
    lines(dYB, col=col.2t, lty="twodash", lwd=1.5)
    
    # minimum mean difference of practical importance
    if ( !is.null(mmd) | !is.null(msmd) ) {
      col.e <- "gray50"  # color for effect
      mid <- (m1 + m2) / 2
      lr <- mid + .5*mmd  # line right
      ll <- mid - .5*mmd  # line left
      lines(c(lr,lr), c(ybot+.44*max.y,ytop-.44*max.y), lty="solid", lwd=2, col=col.e)
      lines(c(ll,ll), c(ybot+.44*max.y,ytop-.44*max.y), lty="solid", lwd=2, col=col.e)
      text(mid, ybot+.41*max.y, label=toString(round(mmd,2)), col=col.e)
      text(mid, ytop-.41*max.y, label=toString(round(msmd,2)), col=col.e)
      text(mid, ybot+.38*max.y, label="mmd", col=col.e)
      text(mid, ytop-.375*max.y, label="msmd", col=col.e)
    }

    # legends with descriptive stats (m1 > m2)
    textR <- paste(Xnm,X1nm);  nR <- n1;  mR <- m1.out;  sR <- s1.out;  col.R <- col.1;  aR <- 45
    textL <- paste(Xnm,X2nm);  nL <- n2;  mL <- m2.out;  sL <- s2.out;  col.L <- col.2;  aL <- -45

    col.lgnd <- "gray25"
    cex.lgnd <- .9

    radj <- xleft + .02*(max.x-min.x)
    legend("topleft", legend = textL, fill=col.L, density=20, angle=aL, bty="n",
      text.col=col.lgnd, cex=cex.lgnd)
    text(radj, ytop-.08*max.y, label=bquote(paste("n = ", .(nL))),  adj=0, col=col.lgnd, cex=cex.lgnd)
    text(radj, ytop-.12*max.y, label=bquote(paste("m = ", .(mL))),  adj=0, col=col.lgnd, cex=cex.lgnd)
    text(radj, ytop-.16*max.y, label=bquote(paste("s = ", .(sL))),  adj=0, col=col.lgnd, cex=cex.lgnd)
    
    ladj <- xright - .02*(xright-xleft)
    legend("topright", legend = textR, fill=col.R, density=20, angle=aR, bty="n",
      text.col=col.lgnd, cex=cex.lgnd)
    text(ladj, ytop-.08*max.y, label=bquote(paste("n = ", .(nR))), adj=1, col=col.lgnd, cex=cex.lgnd)
    text(ladj, ytop-.12*max.y, label=bquote(paste("m = ", .(mR))), adj=1, col=col.lgnd, cex=cex.lgnd)
    text(ladj, ytop-.16*max.y, label=bquote(paste("s = ", .(sR))), adj=1, col=col.lgnd, cex=cex.lgnd)

    # scale for s-pooled, d, mdiff at top of graph
    mlow <- min(m1, m2)
    mhi  <- max(m1, m2)
    col.d.unit <- "gray30"
    # connect first seg to top
    segments(mlow, max.y-.01*max.y, mlow, ytop, lwd=1, col=col.d.unit) 
    # provide at least 2 labeled d units on sd scale at top
    max.i <- max(ceiling(abs(smd)), 2)
    for (i in 0:max.i) {  # sd scale at top
      x.i <- mlow+i*sw
      # sd units
      segments(x.i, max.y+.025*max.y, x.i, ytop, col=col.d.unit, lwd=1)
      # d units counted
      text(x.i, max.y+.01*max.y, labels=i)
      # horiz bar connects endpoints
      segments(mlow, ytop, x.i, ytop, col=col.d.unit, lwd=4)
      last.coord.x <- x.i
    }
    # connect last seg to top
    segments(last.coord.x, max.y+.025*max.y, last.coord.x, ytop, lwd=1, col=col.d.unit)
    # print d value towards top
    text((m1+m2)/2, ytop-.07*max.y, label=smd.out)
    # horiz bar connects means
    segments(mlow, ytop-.09*max.y, mhi, ytop-.09*max.y, col=col.d.unit, lwd=2)
    # print d towards top
    text((m1+m2)/2, ytop-.11*max.y, label="smd")
    
    # print mdiff value towards bottom  
    text((m1+m2)/2, ybot+.11*max.y, label=mdiff.out)
    # horiz bar connects means
    segments(mlow, ybot+.09*max.y, mhi, ybot+.09*max.y, col=col.d.unit, lwd=2)
    # print diff towards bottom
    text((m1+m2)/2, ybot+.07*max.y, label="md")

    # title area, above graph
    mtext(paste("TwoGroup Plot"), side=3, line=6.6, font=2)
    mtext(paste("Compare",Ynm,"for",Xnm,X1nm,"and",X2nm), side=3, line=5.6, font=3)
    mtext(bquote(paste("    Classic t-test of 0 mean diff:   t = ", .(tvalue), 
      ",  df = ", .(df), ",   p-value = ", .(pvalue))), side=3, line=4.4, cex=1.08, adj=0)
    mtext(bquote(paste("    ",.(clpct), " Confidence Interval for Mean Difference:  ",
      .(lb), " to ", .(ub))), side=3, line=3.3, cex=1.08, adj=0)
    if (check.MBESS) {
      mtext(bquote(paste("    ",.(clpct), " Confidence Interval for Stnd Mean Diff:   ", 
        .(deltaL), " to ", .(deltaU))), side=3, line=2.1, cex=1.08, adj=0)
    }
    mtext(bquote(paste("s-within")), side=3, line=.9, 
          at=(mlow+(last.coord.x))/2, col="gray40")
    mtext(bquote(paste(.(round(sw,2)))), side=3, line=.2,
          at=(mlow+(last.coord.x))/2, col="gray40")
  }

} # TwoGroup


OneGroup  <-
function(Y, n=NULL, m=NULL, s=NULL, mu0=NULL) {
# one-group

  if (x.name != "NULL") Ynm <- x.name

  # use variable label if it exists
  y.lbl <- ""
  if (exists("mylabels")) {
    y.lbl <- as.character(mylabels[which(row.names(mylabels)==Ynm), "label"])
    if (length(y.lbl) == 0) y.lbl <- ""
  }
  if ( (nchar(y.lbl) > 0) ) {
    cat("Response Variable:  ", Ynm, ", ", as.character(x.lbl), sep="", "\n")
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
          nrm1 <- shapiro.test(x)
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
  if (!is.null(mu0)) {
    m.dist <- m - mu0
    m.dist.out <- round(m.dist,digits.d)
  }
  df <- n - 1
  tcut <- qt((1-conf.level)/2, df=df, lower.tail=FALSE)
  sterr <- s * sqrt(1/n)
  if (from.data) {
    if (!is.null(mu0)) mu.null <- mu0 else mu.null <- 0
    ttest <- t.test(Y, conf.level=conf.level, mu=mu.null)
    df <- ttest$parameter
    lb <- round(ttest$conf[1],digits.d)
    ub <- round(ttest$conf[2],digits.d)
    E <- round((ub-lb)/2,digits.d)
    if (!is.null(mu0)) {
      tvalue <- round(ttest$statistic, min(2,digits.d))
      pvalue <- round(ttest$p.value, min(4,digits.d+1))
    }
  }
  else {
    E <- tcut * sterr
    lb <- m - E
    ub <- m + E
    if (!is.null(mu0)) {
      tvalue <- m.dist/sterr
      pvalue <- round(2 * pt(abs(tvalue), df=df, lower.tail=FALSE), min(4,(digits.d+1)))
    }
  }     

  if (!brief) {
    cat("t-cutoff: tcut = ", round(tcut,3), "\n") 
    cat("Standard Error of Mean: SE = ", round(sterr,digits.d), "\n\n")
  }
  if (!is.null(mu0)) {
    cat("Hypothesized Value H0: mu =", mu0, "\n")
    if (!brief)
      cat("Distance of Sample Mean from Hypothesized Value: ", m.dist.out, sep="", "\n")
    tvalue <- round(tvalue, min(2,digits.d+1))
    txt <- "Hypothesis Test of Mean:  t-value = "
    cat(txt, tvalue, ",  df = ", df, ",  p-value = ", pvalue, sep="", "\n\n")
  }
  if (!brief)
    cat("Margin of Error for ", clpct, " Confidence Level:  ", round(E,digits.d), sep="", "\n")
  txt <- " Confidence Interval for Mean:  "
  cat(clpct, txt, round(lb,digits.d), " to ", round(ub,digits.d), sep="", "\n\n")

}  # one group


# ------------------------ tt ---------------------------------

  cat("\n")
  if (missing(y))  no.y <- TRUE
  else if (is.null(y)) no.y <- TRUE else no.y  <- FALSE
  if (is.null(n1) && no.y) two.gp <- FALSE else two.gp <- TRUE
  if (is.null(n) && is.null(n1)) from.data <- TRUE else from.data <- FALSE

  if (is.null(digits.d)) digits.d <- max.dd(x)
  if (from.data) digits.d <- digits.d + 1
  if (digits.d > 10) {
    cat("\nThese data contain", digits.d, "significant digits.\n")
    cat("Perhaps specify a smaller number to display with the  digits.d  parameter.\n")
  }

  if (two.gp) {
    if (from.data) { 

      if ( (length(x) < 2) || (length(y) < 2) )  {
       cat("\n"); stop(call.=FALSE, "\n","------\n",
         "Need at least two observations per sample.\n\n")
      }
     
      if ( !is.null(mmd) && !is.null(msmd) )  {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
         "Specify only one of mmd and msmd as one implies the other.\n\n")
      }
      orig.params <- par(no.readonly=TRUE)
      on.exit(par(orig.params))

      # Always put the group with the largest mean first
      if (mean(x, na.rm=TRUE) > mean(y, na.rm=TRUE))
        TwoGroup(x, y)
      else {  # switch
        Xtmp <- X2nm
        X2nm <- X1nm
        X1nm <- Xtmp
        TwoGroup(y, x)
      }

    }

    else {  # from stats
      TwoGroup(n1=n1, n2=n2, m1=m1, m2=m2, s1=s1, s2=s2)
    }

  }  # end two.gp 

  else { # one group
    if (from.data) {
      OneGroup(x, mu0=mu0)
    }
    else { # from stats
      OneGroup(n=n, m=m, s=s, mu0=mu0)
    }
  }

  cat("\n")


}
