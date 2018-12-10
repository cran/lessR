prob.norm <- 
function(lo=NULL, hi=NULL, mu=0, sigma=1, nrm.color="black", 
         fill.nrm="grey91", fill.int="slategray3", 
         ylab="", y.axis=FALSE, z=TRUE, axis.size=.9,
         pdf.file=NULL, width=5, height=5, ...) { 

  if (sigma <= 0) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "Sigma, the population standard deviation, must be larger than zero.\n\n")
  }

  dots <- list(...)  # check for deprecated parameters
  if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (substr(names(dots)[i], 1, 4) == "col.") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "options that began with the abbreviation  col  now begin with  ",
          "color \n\n")
      if (names(dots)[i] == "mag")  axis.size <- dots[[i]]
      }
    }
  }

  if (!is.null(pdf.file))
    if (!grepl(".pdf", pdf.file)) pdf.file <- paste(pdf.file, ".pdf", sep="")
 
  if (mu==0  && sigma==1) z <- FALSE
 
  if (is.null(lo)) {
    lo <- mu - sigma*10
    lo.lbl <- "..."
  }
  else
    lo.lbl <- as.character(lo)
  if (is.null(hi)) {
    hi <- mu + sigma*10
    hi.lbl <- "..."
  }
  else hi.lbl <- as.character(hi)  

  
  # normal density curve
  .opendev(pdf.file, width, height)

  orig.params <- par(no.readonly=TRUE)
  par(mar=c(3,2,1.75,2), mgp=c(1,.5,0))


  min.x <- mu - 4*sigma
  max.x <- mu + 4*sigma
  cuts <- seq(min.x,max.x,sigma)
  x <- seq(min.x, max.x, length=200)
  d.nrm <- dnorm(x,mu,sigma)
  plot(x, d.nrm, type="l", col=nrm.color, axes=FALSE, xlab="", ylab="", ...)
  polygon(c(min.x,x,max.x), c(0,d.nrm,0), col=fill.nrm)

  axis(side=1, at=cuts, cex.axis=axis.size)
  if (z) axis(side=1, at=cuts, cex.axis=axis.size, line=1.5,
              labels=-4:4, lwd=0, lwd.ticks=0)
  if (y.axis) {
    axis(side=2, cex.axis=axis.size)
    if (ylab == "") ylab="Normal Density"
    title(ylab=ylab)
  }

  # plot an interval
  y.lo <- dnorm(lo, mu, sigma)
  y.hi <- dnorm(hi, mu, sigma)
  xsub <- x[x>lo & x<hi]
  ysub <- d.nrm[x>lo & x<hi]
  polygon(c(lo,xsub,hi), c(0,ysub,0), col=fill.int)
  
  # prob of interval
  prob <- pnorm(hi, mean=mu, sd=sigma) - pnorm(lo, mean=mu, sd=sigma)
  
  # decorate
  lbl1 <- paste(" Prob =", toString(signif(prob, 4)))
  lbl2 <- paste(" for Y from", lo.lbl, "to", hi.lbl)
  title(main=paste(lbl1,lbl2), ...)
  lbl3 <- bquote(paste(mu, "=", .(mu), "  ", sigma, "=", .(sigma)))
  if (z) title(sub=lbl3, line=4, ...) else title(sub=lbl3, ...)

  # terminate pdf graphics system
  par(orig.params)        
  if (!is.null(pdf.file)) {
    dev.off()
    .showfile(pdf.file, "Normal curve probability")
  }

  cat("Probability: ", prob, "\n")

}
