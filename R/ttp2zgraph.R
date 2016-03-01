.ttp2graph <-
function(myxlab, mytitle, n, s, mdp, mmd, msmd, mytype, H0, ...) {

  
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

  #if 
    #(!is.null(getOption("colors"))) colors <- getOption("colors")
  #else
    #colors="dodgerblue"

  # power curve
  mypower <- power.t.test(n=n, sd=s, delta=mydeltas, type=mytype)
  x.values <- H0+mydeltas
  y.values <- mypower$power

  .plt.main(x.values, y.values, by=NULL, type="l",
         n.cat=getOption("n.cat"),
         col.fill=getOption("col.fill.pt"),
         col.stroke=getOption("col.stroke.pt"),
         col.bg=getOption("col.bg"), col.grid=getOption("col.grid"),
         shape.pts=21, col.area=NULL, col.box="black",
         cex.axis=0.75, col.axis="gray30", col.low=NULL, col.hi=NULL,
         xy.ticks=TRUE, xlab=myxlab, ylab="Power",
         main="", sub=NULL, cex=NULL,
         value.labels=NULL, rotate.values=0, offset=0.5,
         kind="default", means=TRUE,
         fit.line="none", col.fit.line="grey55",
         bubble.size=.25, bubble.counts=TRUE, 
         ellipse=FALSE,
         col.ellipse="transparent", fill.ellipse="transparent", 
         diag=FALSE, col.diag=par("fg"), lines.diag=FALSE,
         quiet=TRUE,
         ylim=c(0,1.1), want.labels=FALSE)
  abline(h=0, lwd=.5, col="gray50")
  # custom title (2 lines)
  mtext(mytitle, side=3, line=2.5, cex=1.1, font=2)
  if (abs(n - round(n)) > 0.000001)
    dgt.n <- 3
  else  # n is an integer
    dgt.n <- 0
  mtext(paste("n=", .fmt(n,dgt.n), ", s=", .fmt(s,4), sep=""),
               side=3, line=1, font=3)

  # delta for a power of mdp, default is 0.8
  clr <- getOption("colors")
  if (clr != "gray") col80 <- "firebrick4" else col80 <- "gray20"
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
  
  del.diff <- del.hi - H0
  if (mytype == "two.sample") mytitle <- "\nMean difference "
  if (mytype == "one.sample") mytitle <- "\nDifference of mu from mu0 "
  cat(mytitle, "to achieve power of ", mdp, ": Diff = ", .fmt(del.diff,3), 
      sep="", "\n\n")
  rm(pp)
  .dash(70)
  }
  
  # for when the minimum meaningful difference, mmd, is provided
  if (!is.null(mmd)) {
    if (mytype == "two.sample") mytitle <- "the two means"
    if (mytype == "one.sample") mytitle <- "mu and mu0"
    cat("Minimum meaningful difference of ", mytitle, ": mmd\n", sep="")
    .dash(70)
    
    if (mytype == "two.sample" && !is.null(msmd))
      cat("Provided standardized value is msmd = ", msmd, "\n")
  
    if (clr != "gray") {
      colmmd <- rgb(112,128,144,40, maxColorValue=255)  # slategray base
      colbrd <- rgb(112,128,144,80, maxColorValue=255)
      coltrv <- rgb(97,129,129, maxColorValue=255)  # darkslategray
    }
    else {
      colmmd <- rgb(128,128,128,60, maxColorValue=255) 
      colbrd <- rgb(128,128,128,80, maxColorValue=255) 
      coltrv <- rgb(115,115,115, maxColorValue=255)
    }    

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
    txt <- ", for mmd of "
    if (mytype == "two.sample") 
      mytitle <- paste("Given n = ", round(n,3), " and sw = ", signif(s), txt, sep="")
    if (mytype == "one.sample")
      mytitle <- paste("Given n = ", n, " and s = ", s, txt, sep="")
    cat(mytitle, mmd, ": Power = ", p.out, sep="", "\n")
    if (mdp != 0) {
      if (del.hi > mmd+H0) {
        .dash(70)
        mytitle <- ">>> Note: Meaningful differences, from "
        cat(mytitle, d.hi, " to ", .fmt(del.hi,3), ", have Power < ", mdp, "\n",
            sep="")
      }
      else {
        .dash(70)
        mytitle <- ">>> Note: Trivial differences, from "
        cat(mytitle, .fmt(del.hi,3), " to ", mmd+H0, ", have Power > ", mdp,
            ".\n", sep="")
        cat(">>> Note: All meaningful differences have Power > ", mdp, "\n", sep="")
      }
      .dash(70)
      rm(pp)
    
      # n needed to achieve a power of mdp=0.8 for mmd
      pp <- power.t.test(sd=s, delta=mmd, power=mdp, type=mytype)
      n.out <- ceiling(pp$n)
      txt <- ", needed n to achieve power= "
      if (mytype == "two.sample") mytitle <- paste("Given sw = ", signif(s), txt,
          sep="")
      if (mytype == "one.sample") mytitle <- paste("Given s = ", s, txt, sep="")
      cat(mytitle, mdp, " for mmd of ", mmd, ": n = ", n.out, sep="", "\n")
      if (mytype == "two.sample") cat("Sample size n applies to *each* group", "\n")
      rm(pp)
    }
    
    .dash(70)

  }

}
