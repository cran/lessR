prob.normal <- 
function(mu=0, sigma=1, lo=NULL, hi=NULL, col.nrm="black", 
         col.fill.nrm="grey85", col.fill.int="steelblue3") { 

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
  x <- seq(min.x, max.x, length=200)
  d.nrm <- dnorm(x,mu,sigma)
	plot(x, d.nrm, type="l", col=col.nrm, xlab="Y", ylab="Normal Density")
  polygon(c(min.x,x,max.x), c(0,d.nrm,0), col=col.fill.nrm)

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
  title(main=paste(lbl1,lbl2))
  lbl3 <- bquote(paste(mu, "=", .(mu), "  ", sigma, "=", .(sigma)))
  title(sub=lbl3)
}


