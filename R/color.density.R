color.density <- 
function(x, col.nrm="darkblue", col.gen="blue", col.bg="seashell",  
         col.grid="grey90", col.hist="grey86",
         col.fill.nrm=rgb(80,150,200, alpha=70, max=255), 
         col.fill.gen=rgb(250,210,230, alpha=70, max=255),
         type=c("both", "general", "normal"),
         xlab=NULL, main=NULL, ...) {
		
	# produce actual argument, such as from an abbreviation, and flag if not exist
	type <- match.arg(type)

	# set the labels	
	if (is.null(xlab)) x.lbl <- deparse(substitute(x)) else x.lbl <- xlab
	if (is.null(main)) main.lbl <- "" else main.lbl <- main
	
  # histogram calculations, no plot
	h <- hist(x, plot=FALSE)
	
	# general density curve, no plot
	# suppress warnings about possible graphic parameters
	d.gen <- suppressWarnings(density(x, ...))
	
	mx <- mean(x)

	# min and max x coordinates for graph, make symmetric
	min.dev.x <- min(d.gen$x) - mx
	max.dev.x <- max(d.gen$x) - mx
  if (abs(min.dev.x) > abs(max.dev.x)) {
    min.x <- min(d.gen$x)
    max.x <- mx + abs(min.dev.x)
  }
  if (abs(max.dev.x) > abs(min.dev.x)) {
    min.x <- mx - abs(max.dev.x)
    max.x <- max(d.gen$x)
  }
  
  # normal density curve, no plot
  xx <- seq(min.x, max.x, length=200)
  d.nrm <- dnorm(xx,mean(x),sd(x))

  # max y coordinate for graph
  max.y <- max(max(d.nrm), max(d.gen$y), max(h$density))
	
	# set up plot area
	# bw if specified also gets passed to plot, so suppress warning
	suppressWarnings(plot(h, border="transparent", freq=FALSE, xlab=x.lbl, main=main.lbl, 
	  xlim=c(min.x,max.x), ylim=c(0,max.y), ...))
	
	# colored background for plotting area
	usr <- par("usr")
	rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="black")
	
	# grid lines computation and print
	vy <- pretty(h$density)
	abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid)

	# plot the histogram
	plot(h, add=TRUE, freq=FALSE, col=col.hist, border=col.hist)
	
	# plot the normal curve
	if (type == "normal" || type == "both") {
	  lines(xx, d.nrm, type="l", col=col.nrm)
    polygon(c(min.x,xx,max.x), c(0,d.nrm,0), col=col.fill.nrm)
	}
	
	# plot the general curve
	if (type == "general" || type == "both") {
	  lines(d.gen, col=col.gen)
 	  polygon(d.gen, col=col.fill.gen)
	  cat("\nDensity bandwidth for general curve: ", round(d.gen$bw,4), sep="", "\n")
	}
  cat("\n")
}