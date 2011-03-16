powercurve.t.test <- 
function(n=NULL, s=NULL, n1=NULL, n2=NULL, s1=NULL, s2=NULL, 
         mmd=NULL, msmd=NULL, mdp=.8, mu0=NULL, ...) {
      
  cat("\n")
  
  # for all null arguments, pick up values from previous smd.t.test
  if (sum(sapply(list(s, n1, n2), is.null)) == 3) {  # all are NULL
    if (exists("n1", 1, inherits=FALSE) && exists("n2", 1, inherits=FALSE)) {
      n1 <- get("n1", 1, inherits=FALSE)
      n2 <- get("n2", 1, inherits=FALSE)
    } 
    else {
      if (is.null(n)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Need to specify sample size, either n or n1 and n2.\n\n")
      }
    }
    if (exists("sw", 1, inherits=FALSE))
      s <- sw
    else
      if (is.null(s1) && is.null(s2)) { 
            cat("\n"); stop(call.=FALSE, "\n","------\n",
            "Need to specify a sample standard deviation, s,\n",
            "or two standard deviations, s1 and s2.\n\n")
      }
    }
  
  if ( (is.null(n)) && (is.null(n1)) && (is.null(n2)) ) { 
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Need to specify either a common sample size for both groups, n,\n",
        "or specify both group sample sizes, n1 and n2, from which\n",
        "the within-group or pooled standard deviation, sw, is computed.\n\n")
  }

  if ( (is.null(s)) && (is.null(s1)) && (is.null(s2)) ) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Need to specify either a single standard deviation, s, \n",
        "or specify two group standard deviations, s1 and s2, plus a\n",
        "common sample size n or individual sample sizes n1 and n2, from which\n",
        "the within-group standard deviation is computed.\n\n")
  }

  if ( (mdp < 0)  || (mdp > 1) ) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Minimum desired power, mdp, must be between 0 and 1.\n\n")
  }

  if ( !is.null(mmd) && !is.null(msmd) ) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Specify only one of mmd and msmd as one implies the other.\n\n")
  }
  
  
  cat("------------------------------------------------------------\n")
  if (is.null(mu0)) {
    mytype <- "two.sample"
    cat("Power Curve Analysis for Independent Groups t-test\n")
  }
  else {
    mytype <- "one.sample"
    cat("Power Curve Analysis for One Sample t-test\n")
    cat("------------------------------------------------------------\n")
    cat("mu0 =", mu0, "\n")
  }
  
  # power curve for two groups, assuming mean diff of 0
  if (mytype == "two.sample") {
    cat("------------------------------------------------------------\n")
    
    # n1, n2 and n all should be set
    if (!is.null(n)) { 
      cat("\n", "Sample size: n = ", n, sep="")
      n1 <- n;  n2 <- n 
    }
    else {
      if ( !is.null(n1) && !is.null(n2) ) {
        n = 2 / ( 1/n1 + 1/n2 )  # harmonic mean
        cat("\n", "Sample sizes: n1 = ", n1, " and n2 = ", n2, sep="")
        cat("\n", "Harmonic mean of the two sample sizes: n = ", n, sep="", "\n")
      }
    }
        
    # within-group standard deviation	(need n1 and n2)
    if (is.null(s)) {
      df1 <- n1 - 1
      df2 <- n2 - 1
      ssq <- (df1*s1^2 + df2*s2^2) / (df1 + df2)
      s <- sqrt(ssq)
      cat("\n", "Equal Group Variances Assumed", sep="", "\n")
    }
    cat("\n", "Within-group (pooled) Standard Deviation:  sw = ", s, sep="", "\n")
    
    mytitle <- "Power Curve for Independent Groups t-test"
    myxlab <- bquote(paste("Alternative Values of ", mu[1] - mu[2]))
    H0 <- 0
  }
  
  # power values for a single sample, triggered by nonzero mu0
  else {
    mytitle <- "Power Curve for One Sample t-test"
    myxlab <- bquote(paste("Alternative Values of ", mu))
    H0 <- mu0
  }
  cat("------------------------------------------------------------\n")
  
  # get value of mmd if msmd supplied
  if ( !is.null(mmd) | !is.null(msmd) ) {
    if (!is.null(mmd)) msmd <- mmd / s   # null, because msmd only informs mmd
    if (!is.null(msmd)) mmd <- msmd * s
  }
  
  # configure range of deltas (need n)
  pp <- power.t.test(n=n, sd=s, power=.9999, type=mytype)
  if (!is.null(mmd)) { 
    xmax <- max(pp$delta,mmd)
    xmin <- min(-pp$delta,-mmd)
  }
  else {
    xmax <- pp$delta
    xmin <- -pp$delta
  }
  mydeltas <- seq(xmin,xmax,length=100)
  xmax <- H0 + xmax
  xmin <- H0 + xmin 
  rm(pp)
  
  # power curve
  mypower <- power.t.test(n=n, sd=s, delta=mydeltas, type=mytype)
  s.out <- toString(round(s,4))
  color.plot(H0+mydeltas, mypower$power, type="l", xlab=myxlab, ylab="Power",
    ylim=c(0,1.1), ...)
  abline(h=0, lwd=.5, col="gray50")
  mtext(mytitle, side=3, line=2.5, cex=1.1, font=2)
  n.out <- toString(round(n,3))
  mtext(paste("n=", n.out, ", s=", s.out, sep=""), side=3, line=1, font=3)
  
  # delta for a power of mdp, default is 0.8
  col80 <- "firebrick4"
  if (mdp != 0) {
    pp <- power.t.test(n=n, sd=s, power=mdp, type=mytype)
    del.hi <- H0 + pp$delta
    del.lo <- H0 - pp$delta
    lines(c(del.hi, del.hi), c(0,mdp), lwd=.5, col=col80)
    arrows(del.hi, mdp, xmax, mdp, lwd=.5, col=col80, length=.15, angle=20)
    lines(c(del.lo, del.lo), c(0,mdp), lwd=.5, col=col80)
    arrows(del.lo, mdp, xmin, mdp, lwd=.5, col=col80, length=.15, angle=20)
    text(del.hi, mdp+.033, labels="    Powerful", col=col80, cex=.85, adj=0)
    text(del.lo, mdp+.033, labels="Powerful    ", col=col80, cex=.85, adj=1)
  
  del.diff.out <- round(del.hi,3) - H0
  if (mytype == "two.sample") mytitle <- "\nMean difference "
  if (mytype == "one.sample") mytitle <- "\nDifference of mu from mu0 "
  cat(mytitle, "to achieve power of ", mdp, ": Diff = ", del.diff.out, sep="", "\n\n")
  rm(pp)
  cat("------------------------------------------------------------\n")
  }
  
  # for when the minimum meaningful difference, mmd, is provided
  if (!is.null(mmd)) {
    if (mytype == "two.sample") mytitle <- "the two means"
    if (mytype == "one.sample") mytitle <- "mu and mu0"
    cat("Minimum meaningful difference of ", mytitle, ": mmd\n", sep="")
    cat("------------------------------------------------------------\n")
    
    if (!is.null(msmd))
      cat("Provided standardized value is msmd = ", msmd, "\n")
  
    colmmd <- rgb(112,128,144,40, max=255)  # slategray base
    colbrd <- rgb(112,128,144,80, max=255)
    coltrv <- rgb(97,129,129, max=255)  # darkslategray
    # power for mmd
    d.hi <- H0 + mmd
    d.lo <- H0 - mmd
    pp <- power.t.test(n=n, sd=s, delta=mmd, type=mytype)
    rect(d.lo, 0, d.hi, pp$power, lwd=.25, col=colmmd, density=-10, border=colbrd)
    lines(c(d.hi, d.hi), c(0, 1.05), lwd=.5, col=coltrv, lty="longdash")
    lines(c(d.lo, d.lo), c(0, 1.05), lwd=.5, col=coltrv, lty="longdash")
    if (d.hi < xmax) {
      arrows(d.hi, 1.05, xmax, 1.05, lwd=.5, col=coltrv, length=.15, angle=20, lty=5)
      arrows(d.lo, 1.05, xmin, 1.05, lwd=.5, col=coltrv, length=.15, angle=20, lty=5)
    }
    text(d.hi, 1.08, labels="    Meaningful", col=coltrv, cex=.85, adj=0)
    text(H0, 1.02, labels="Trivial", col=coltrv, cex=.85)
    text(d.lo, 1.08, labels="Meaningful    ", col=coltrv, cex=.85, adj=1)
    
    p.out <- round(pp$power,3)
    mytitle <- "Given n1, n2 and sw, power for mmd of "
    cat(mytitle, mmd, ": Power = ", p.out, sep="", "\n")
    if (mdp != 0) {
      del.hi.out <- round(del.hi,3)
      if (del.hi > mmd+H0) {
        cat("------------------------------------------------------------\n")
        mytitle <- "Warning: Meaningful differences, from "
        cat(mytitle, d.hi, " to ", del.hi.out, ", have Power < ", mdp, "\n", sep="")
      }
      else {
        cat("------------------------------------------------------------\n")
        mytitle <- "Warning: Trivial differences, from "
        cat(mytitle, del.hi.out, " to ", mmd+H0, ", have Power > ", mdp, ".\n", sep="")
        cat("Note: All meaningful differences have Power > ", mdp, "\n", sep="")
      }
      cat("------------------------------------------------------------\n")
      rm(pp)
    
      # n needed to achieve a power of mdp=0.8 for mmd
      pp <- power.t.test(sd=s, delta=mmd, power=mdp, type=mytype)
      n.out <- ceiling(pp$n)
      mytitle <- "Given sw, needed n to achieve power= "
      cat(mytitle, mdp, " for mmd of ", mmd, ": n = ", n.out, sep="", "\n")
      if (mytype == "two.sample") cat("Sample size n applies to *each* group", "\n")
      rm(pp)
    }
    
    cat("------------------------------------------------------------\n")
    cat("\n")
    
  }

}
