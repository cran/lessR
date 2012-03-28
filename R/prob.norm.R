prob.norm <- 
function(lo=NULL, hi=NULL, mu=0, sigma=1, col.nrm="black", 
         col.fill.nrm="grey91", col.fill.int="slategray3", 
         ylab="", y.axis=FALSE, z=TRUE, mag=.9, ...) { 

  if (sigma <= 0) { 
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Sigma, the population standard deviation, must be larger than zero.\n\n")
  }
 
  if (mu==0  && sigma==1) z=FALSE
 
  if (is.null(lo)) {
    lo <- mu-sigma*10
    lo.lbl <- "..."
  }
  else lo.lbl <- as.character(lo)
  if (is.null(hi)) {
    hi <- mu+sigma*10
    hi.lbl <- "..."
  }
  else hi.lbl <- as.character(hi)  

  
  # normal density curve
  min.x <- mu-4*sigma
  max.x <- mu+4*sigma
  cuts <- seq(min.x,max.x,sigma)
  x <- seq(min.x, max.x, length=200)
  d.nrm <- dnorm(x,mu,sigma)
  plot(x, d.nrm, type="l", col=col.nrm, axes=FALSE, xlab="", ylab="", ...)
  polygon(c(min.x,x,max.x), c(0,d.nrm,0), col=col.fill.nrm)

  axis(side=1, at=cuts, cex.axis=mag)
  if (z) axis(side=1, at=cuts, cex.axis=mag, line=1.5, labels=-4:4, lwd=0, lwd.ticks=0)
  if (y.axis) {
    axis(side=2, cex.axis=mag)
    if (ylab == "") ylab="Normal Density"
    title(ylab=ylab)
  }

  # plot an interval
  y.lo <- dnorm(lo, mu, sigma)
  y.hi <- dnorm(hi, mu, sigma)
  xsub <- x[x>lo & x<hi]
  ysub <- d.nrm[x>lo & x<hi]
  polygon(c(lo,xsub,hi), c(0,ysub,0), col=col.fill.int)
  
  # prob of interval
  prob <- pnorm(hi, mean=mu, sd=sigma) - pnorm(lo, mean=mu, sd=sigma)
  
  # decorate
  lbl1 <- paste(" Prob =", toString(signif(prob, 4)))
  lbl2 <- paste(" for Y from", lo.lbl, "to", hi.lbl)
  title(main=paste(lbl1,lbl2), ...)
  lbl3 <- bquote(paste(mu, "=", .(mu), "  ", sigma, "=", .(sigma)))
  if (z) title(sub=lbl3, line=4, ...) else title(sub=lbl3, ...)

}
