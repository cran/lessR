prob_znorm <- 
function(mu=0, sigma=1, color_border="gray10",
         r=.10, g=.34, b=.94, a=.20,
         xlab="", ylab="", main="", 
         y_axis=FALSE, z=TRUE, axis_size=.9,
         pdf_file=NULL, width=5, height=5, ...) {


  # a dot in a parameter name to an underscore
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    change <- c("color.border", "y.axis", "axis.size", "pdf.file")
    for (i in 1:length(dots)) {
      if (names(dots)[i] %in% change) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }
# plot normal curve with integer SD lines
    
  if ( (r<0 || r>1)  ||  (g<0 || g>1)  || (b<0 || b>1)  ||  (a<0) || a>1) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Values of r, g, b and a must all be between 0 and 1, inclusive.\n\n")
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
   
  if (mu==0  && sigma==1) z <- FALSE

  xmin <- mu - 4*sigma
  xmax <- mu + 4*sigma

  cuts <- seq(xmin,xmax,sigma)

  color_sig <- rgb(r, g, b, a)
  #par(mar=c(3, 3, 1, 2), mgp=c(2,.6,0))

  # Normal Curve
  x <- seq(xmin, xmax, length=200)
  y <- dnorm(x ,mean=mu ,sd=sigma)

  orig.params <- par(no.readonly=TRUE)
  if (sys.nframe() == 1) { # no new window if called from sim.CLT
    .opendev(pdf_file, width, height)
#   par(mar=c(3,2,1.75,2), mgp=c(1,.5,0))
  }
  plot(x,y, type="l", lwd=2, col=color_border, axes=FALSE, xlab="", ylab="",
       main=main)
  if (z)
    title(xlab=xlab, line=3.5)
  else
    title(xlab=xlab)

  abline(h=0)
  axis(side=1, at=cuts, cex.axis=axis_size)
  if (z) axis(side=1, at=cuts, cex.axis=axis_size, line=1.5, labels=-4:4,
         lwd=0, lwd.ticks=0)
  if (y_axis) {
    axis(side=2, cex.axis=axis_size)
    if (ylab == "") ylab="Normal Density"
    title(ylab=ylab)
  }

  segments(mu, 0, mu, dnorm(mu, mean=mu, sd=sigma), col=color_border,
           lty="dotted")

  xsub <- x>(mu-3*sigma) & x<(mu+3*sigma)
  polygon(c(mu-3*sigma,x[xsub],mu+3*sigma), c(0,y[which(xsub)],0),
          col=color_sig, border=color_border, lty="dotted")

  xsub <- x>(mu-2*sigma) & x<(mu+2*sigma)
  polygon(c(mu-2*sigma,x[xsub],mu+2*sigma), c(0,y[which(xsub)],0),
          col=color_sig, border=color_border, lty="dotted")

  xsub <- x>(mu-sigma) & x<(mu+sigma)
  polygon(c(mu-sigma,x[xsub],mu+sigma), c(0,y[which(xsub)],0),
          col=color_sig, border=color_border, lty="dotted")

  # terminate pdf graphics system
  par(orig.params)        
  if (!is.null(pdf_file)) {
    dev.off()
    .showfile(pdf_file, "Normal curve probability")
  }

}
