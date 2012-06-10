.TwoGroup <-
function(YA, YB, n1=NULL, n2=NULL, m1=NULL, m2=NULL, s1=NULL, s2=NULL,
         Ynm, Xnm, X1nm, X2nm, 
         brief, digits.d, 
         conf.level=0.95, mmd=NULL, msmd=NULL, 
         bw1 = "nrd", bw2 = "nrd", from.data=TRUE) {
        
  if ( brief==TRUE  &&  (!is.null(mmd)  || !is.null(msmd)) ) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "mmd and msmd do not work with the brief version.\n\n")
  }

  cat("------------------------------------------------------------\n")
    cat("Compare", Ynm, "across", Xnm, "levels", X1nm, "and", X2nm, "\n")
    cat("------------------------------------------------------------\n\n")

# get variable labels if exist
    options(xname = Xnm)
    options(yname = Ynm)
    gl <- .getlabels()
    x.lbl <- gl$xl
    y.lbl <- gl$yl

    if ( (!is.null(x.lbl)) || (!is.null(y.lbl)) ) {
      cat("Response Variable:  ", Ynm, ", ", as.character(y.lbl), sep="", "\n")
      cat("Grouping Variable:  ", Xnm, ", ", as.character(x.lbl), sep="", "\n")
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
  if (!brief) cat("Equal Group Variances Assumed, Within-group",
      "Standard Deviation:  ", .fmt(sw), "\n\n")

# mean difference and standardized mean difference
  mdiff <- m1 - m2
  cat("Mean Difference of ", Ynm, ":  " , .fmt(mdiff), sep="", "\n\n")

# smd
  smd <- mdiff/sw
  cat("Standardized Mean Difference of ", Ynm, ", ",
      "Cohen's d:  ", .fmt(smd), sep="", "\n")


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
          W.1 <- nrm1$statistic
          p.val1 <- nrm1$p.value
          cat(nrm1$method, ":  W = ", .fmt(W.1,3), ",  p-value = ", .fmt(p.val1,3), sep="", "\n")
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
        W.2 <- nrm2$statistic
        p.val2 <- nrm2$p.value
        cat(nrm2$method, ":  W = ", .fmt(W.2,3), ",  p-value = ", .fmt(p.val2,3), sep="", "\n")
      }
      else
      cat("Sample size out of range for Shapiro-Wilk normality test.", "\n")
    }  
    cat("\n")
  } 

# Homogeneity of Variance
# Var Ratio
  if (v1 >= v2) {
    vratio <- v1/v2
      vr <- paste(.fmt(v1), "/", .fmt(v2), sep="")
      df.num <- df1
      df.den <- df2
  }
  else {
    vratio <- v2/v1
      vr <- paste(.fmt(v2), "/", .fmt(v1), sep="")
      df.num <- df2
      df.den <- df1
  }

  p.var <- pf(vratio, df1=df.num, df2=df.den)
# adjust for two-sided test, results same as var.test{stats}
  p.var <- 2 * min(p.var, 1-p.var)

  cat("Null hypothesis is equal variances of ")
  cat(Ynm, ", i.e., homogeneous.", sep="", "\n")

  cat("Variance Ratio test:  F = ", vr, " = ", .fmt(vratio),
      ",  df = ", df.num, ";", 
      df.den, ",  p-value = ",  .fmt(p.var,3), sep="", "\n")

  if (from.data) { # Levene
    YAm <- abs(YA - median(YA))
      YBm <- abs(YB - median(YB))
      t.bf <- t.test(YAm, YBm, var.equal=TRUE)
      tvalue.bf <- t.bf$statistic
      df.bf <- t.bf$parameter
      pvalue.bf <- t.bf$p.value
      cat("Levene's test, Brown-Forsythe:  t = ", .fmt(tvalue.bf,3),
          ",  df = ", df.bf, sep="")
      cat(",  p-value = ", .fmt(pvalue.bf,3), sep="", "\n")
  }
}


  if (!brief) cat("\n\n------ Inference ------\n\n") else cat("\n---\n")

# t-test
    sterr <- sw * sqrt(1/n1 + 1/n2)
      df <- df1 + df2
      tcut <- qt((1-conf.level)/2, df=df, lower.tail=FALSE)
      if (from.data) {
        ttest <- t.test(YA, YB, var.equal=TRUE, conf.level=conf.level)
          lb <- ttest$conf[1]
          ub <- ttest$conf[2]
          E <- (ub-lb) / 2
          tvalue <- ttest$statistic
          pvalue <- ttest$p.value
      }
      else {
        sterr <- sw * sqrt(1/n1 + 1/n2)
          E <- tcut*sterr
          lb <- mdiff-E
          ub <- mdiff+E
          tvalue <- mdiff/sterr
          pvalue <- 2 * pt(abs(tvalue), df=df, lower.tail=FALSE)
      }

  if (!brief) {
    cat("--- Assume equal population variances of", Ynm, "for each", Xnm, "\n\n")
    cat("t-cutoff: tcut = ", .fmt(tcut,3), "\n") 
    cat("Standard Error of Mean Difference: SE = ", .fmt(sterr), "\n")
  }
  mytitle <- "\nHypothesis Test of 0 Mean Diff:  t = "
    cat(mytitle, .fmt(tvalue,3), ",  df = ", df, ",  p-value = ", .fmt(pvalue,3), sep="", "\n\n")
    if (!brief) 
      cat("Margin of Error for ", clpct, " Confidence Level:  ", .fmt(E), sep="", "\n")
    cat(clpct," Confidence Interval for Mean Difference:  ", .fmt(lb), " to ", .fmt(ub), 
        sep="", "\n\n")

    cid <- ci.smd(smd=smd, n.1=n1, n.2=n2, conf.level=conf.level)  # MBESS function
    deltaL <-cid$Lower.Conf.Limit.smd
    deltaU <- cid$Upper.Conf.Limit.smd
    cat(clpct," Confidence Interval for smd:  ",
        .fmt(deltaL), " to ", .fmt(deltaU), sep="", "\n")

    if (!brief) {
      k1 <- v1/n1
      k2 <- v2/n2
      df.ne <- ((k1 + k2)^2) / ((k1^2)/(n1-1) + (k2^2)/(n2-1))
      tcut.ne <- qt((1-conf.level)/2, df=df.ne, lower.tail=FALSE)
      sterr.ne <- sqrt(k1 + k2)

      if (from.data) {
        ttne <- t.test(YA, YB, var.equal=FALSE, conf.level=conf.level)
        df.ne <- ttne$parameter
        lb.ne <- ttne$conf[1]
        ub.ne <- ttne$conf[2]
        E.ne <- (ub.ne-lb.ne)/2
        tvalue.ne <- ttne$statistic
        pvalue.ne <- ttne$p.value
      }
      else {
        E.ne <- tcut.ne*sterr.ne
        lb.ne <- mdiff-E.ne
        ub.ne <- mdiff+E.ne
        tvalue.ne <- mdiff / sterr.ne
        pvalue.ne <- 2 * pt(abs(tvalue.ne), df=df.ne, lower.tail=FALSE)
      }

      cat("\n--- Do not assume equal population variances of", Ynm, "for each", Xnm, "\n\n")
      cat("t-cutoff: tcut = ", .fmt(tcut.ne,3), "\n") 
      cat("Standard Error of Mean Difference: SE = ", .fmt(sterr.ne), "\n")
      mytitle <- "\nHypothesis Test of 0 Mean Diff:  t = "
      cat(mytitle, .fmt(tvalue.ne,3), ",  df = ", .fmt(df.ne,3), 
          ", p-value = ", .fmt(pvalue.ne,3), sep="", "\n\n")
      cat("Margin of Error for ", clpct, " Confidence Level:  ", .fmt(E.ne), sep="", "\n")
      cat(clpct," Confidence Interval for Mean Difference:  ", .fmt(lb.ne), " to ",
          .fmt(ub.ne), sep="", "\n")

      cat("\n\n------ Practical Importance ------\n\n")
      cat("Minimum Mean Difference of practical importance: mmd\n")
      if ( !is.null(mmd) | !is.null(msmd) ) {
        if (!is.null(mmd)) msmd <- mmd / sw
        if (!is.null(msmd)) mmd <- msmd * sw
        cat("Compare mmd =", round(mmd,digits.d), " to the obtained value of md = ",
          .fmt(mdiff), "\n")
        cat("Compare mmd to the confidence interval for md: ", .fmt(lb), " to ", 
            .fmt(ub), "\n\n")
        cat("Minimum Standardized Mean Difference of practical importance: msmd\n")
        cat("Compare msmd = ", round(msmd,digits.d), " to the obtained value of smd = ",
            .fmt(smd),"\n")
        if (!is.null(deltaL)) cat("Compare smd to the confidence interval for smd: ", 
            .fmt(deltaL), " to ", .fmt(deltaU), "\n")
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

# plot: set up coordinate system
    orig.params <- par(no.readonly=TRUE)
    on.exit(par(orig.params))
    .graphwin()
    par(mar=c(3,1.5,8,.4), mgp=c(2,.6,0), cex=.8, cex.axis=1, cex.lab=1.2)
    plot.new()
    plot.window(xlim=c(min.x,max.x), ylim=c(0,max.y))
    axis(1); box()
    title(xlab=Ynm)

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
    text(radj, ytop-.10*max.y, label=bquote(paste("n = ", .(nL))),
        adj=0, col=col.lgnd, cex=cex.lgnd)
    text(radj, ytop-.14*max.y, label=bquote(paste("m = ", .(.fmt(mL,digits.d)))),
        adj=0, col=col.lgnd, cex=cex.lgnd)
    text(radj, ytop-.18*max.y, label=bquote(paste("s = ", .(.fmt(sL,digits.d)))),
        adj=0, col=col.lgnd, cex=cex.lgnd)

    ladj <- xright - .02*(xright-xleft)
    legend("topright", legend = textR, fill=col.R, density=20, angle=aR, bty="n",
        text.col=col.lgnd, cex=cex.lgnd)
    text(ladj, ytop-.10*max.y, label=bquote(paste("n = ", .(nR))),
        adj=1, col=col.lgnd, cex=cex.lgnd)
    text(ladj, ytop-.14*max.y, label=bquote(paste("m = ", .(.fmt(mR,digits.d)))),
        adj=1, col=col.lgnd, cex=cex.lgnd)
    text(ladj, ytop-.18*max.y, label=bquote(paste("s = ", .(.fmt(sL,digits.d)))),
        adj=1, col=col.lgnd, cex=cex.lgnd)

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
      text((m1+m2)/2, ytop-.07*max.y, label=.fmt(smd))
# horiz bar connects means
      segments(mlow, ytop-.09*max.y, mhi, ytop-.09*max.y, col=col.d.unit, lwd=2)
# print d towards top
      text((m1+m2)/2, ytop-.11*max.y, label="smd")

# print mdiff value towards bottom  
      text((m1+m2)/2, ybot+.11*max.y, label=.fmt(mdiff))
# horiz bar connects means
      segments(mlow, ybot+.09*max.y, mhi, ybot+.09*max.y, col=col.d.unit, lwd=2)
# print diff towards bottom
      text((m1+m2)/2, ybot+.07*max.y, label="md")

    # title area, above graph
    mtext(paste("TwoGroup Plot"), side=3, line=6.6, font=2)
    mtext(paste("Compare",Ynm,"for",Xnm,X1nm,"and",X2nm), side=3, line=5.6, font=3, cex=.8)
    mtext(bquote(paste("  Classic t-test of 0 mean diff:   t = ", .(.fmt(tvalue,3)), 
      ",  df = ", .(df), ",   p-value = ", .(.fmt(pvalue,3)))), side=3, 
      line=4.4, cex=.8, adj=0)
    mtext(bquote(paste("  ",.(clpct), " Confidence Interval for Mean Difference:  ",
      .(.fmt(lb,3)), " to ", .(.fmt(ub,3)))), side=3, line=3.3, cex=.8, adj=0)
    mtext(bquote(paste("  ",.(clpct), " Confidence Interval for Stnd Mean Diff:   ", 
      .(.fmt(deltaL,3)), " to ", .(.fmt(deltaU,3)))), side=3, line=2.1, cex=.8, adj=0)
    mtext(bquote(paste("s-within")), side=3, line=.9, 
          at=(mlow+(last.coord.x))/2, col="gray40", cex=.8)
    mtext(bquote(paste(.(round(sw,2)))), side=3, line=.2,
          at=(mlow+(last.coord.x))/2, col="gray40", cex=.8)
  }


} # End Two Group


