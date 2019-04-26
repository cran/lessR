prob_norm <- 
function(lo=NULL, hi=NULL, mu=0, sigma=1, nrm_color="black", 
         fill_nrm="grey91", fill_int="slategray3", 
         ylab="", y_axis=FALSE, z=TRUE, axis_size=.9,
         pdf_file=NULL, width=5, height=5, ...) { 


  # a dot in a parameter name to an underscore
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    change <- c("nrm.color", "fill.nrm", "fill.int",
                "y.axis", "axis.size", "pdf.file")
    for (i in 1:length(dots)) {
      if (names(dots)[i] %in% change) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }

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
      if (names(dots)[i] == "mag")  axis_size <- dots[[i]]
      }
    }
  }

  if (!is.null(pdf_file))
    if (!grepl(".pdf", pdf_file)) pdf_file <- paste(pdf_file, ".pdf", sep="")
 
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
  .opendev(pdf_file, width, height)

  orig.params <- par(no.readonly=TRUE)
  par(mar=c(3,2,1.75,2), mgp=c(1,.5,0))


  min_x <- mu - 4*sigma
  max.x <- mu + 4*sigma
  cuts <- seq(min_x,max.x,sigma)
  x <- seq(min_x, max.x, length=200)
  d.nrm <- dnorm(x,mu,sigma)
  plot(x, d.nrm, type="l", col=nrm_color, axes=FALSE, xlab="", ylab="", ...)
  polygon(c(min_x,x,max.x), c(0,d.nrm,0), col=fill_nrm)

  axis(side=1, at=cuts, cex.axis=axis_size)
  if (z) axis(side=1, at=cuts, cex.axis=axis_size, line=1.5,
              labels=-4:4, lwd=0, lwd.ticks=0)
  if (y_axis) {
    axis(side=2, cex.axis=axis_size)
    if (ylab == "") ylab="Normal Density"
    title(ylab=ylab)
  }

  # plot an interval
  y.lo <- dnorm(lo, mu, sigma)
  y.hi <- dnorm(hi, mu, sigma)
  xsub <- x[x>lo & x<hi]
  ysub <- d.nrm[x>lo & x<hi]
  polygon(c(lo,xsub,hi), c(0,ysub,0), col=fill_int)
  
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
  if (!is.null(pdf_file)) {
    dev.off()
    .showfile(pdf_file, "Normal curve probability")
  }

  cat("Probability: ", prob, "\n")

}
