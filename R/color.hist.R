color.hist <- 
function(x, col="lightsteelblue", border="black", col.bg="seashell",
         col.grid="grey90", breaks="Sturges", over.grid=FALSE,
         show.values=FALSE, prop=FALSE, cumul=c("off", "on", "both"), 
         col.reg="seashell2", digits=5, xlab=NULL, main=NULL, ...) {
		
	# produce actual argument, such as from an abbreviation, and flag if not exist
	cumul <- match.arg(cumul)

	# set the labels	
	if (is.null(xlab)) x.lbl <- deparse(substitute(x)) else x.lbl <- xlab
	if (is.null(main)) main.lbl <- "" else main.lbl <- main
	
	# For user supplied bins, make sure entire data range is spanned
	if (is.numeric(breaks)) {
		cc <- cut(x, breaks, ...)	 # replace each data value with its bin
		labs <- levels(cc)  # get list of unique bins, ordered
		bins <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", labs) ),
					upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs) ))
		bin.min <- min(bins)
		bin.max <- max(bins)
		n.under <- length(x[which(x<bin.min)])
		n.over <- length(x[which(x>bin.max)])
		if (n.under+n.over > 0) {
			cat("\nRange of the data: ", min(x), " to ", max(x), "\n", sep="")
			if (length(breaks) > 3)
				cat("Bin Cutpoints: ", bin.min, breaks[2], "...", breaks[length(breaks)-1],
					bin.max, "\n\n")
		else
			cat("Range of the bins: ", bin.min, " to ", bin.max, "\n", sep="")
			if (n.under > 0) 
			  cat("Data values too small to fit in the bins: ", x[which(x<bin.min)], "\n")
			if (n.over > 0)
			  cat("Data values too large to fit in the bins: ", x[which(x>bin.max)], "\n")
			cat("\n")
			cat("Each data value must be in a bin.\n")
			txt <- "To fix this problem, extend the bin range "
			if (n.under > 0) cat(txt, "below ", bin.min, ".\n", sep="")
			if (n.over > 0) cat(txt, "above ", bin.max, ".\n", sep="")
			stop("Try again with an extended bin range", ".\n\n", call. = FALSE)
		}	
	}

  # calculate but do not plot the histogram
  # stop warnings about any unused parameters due to not plotting 
	h <- suppressWarnings(hist(x, plot=FALSE, breaks, ...))
	
	# summarize data
	cat("\n")
	cat("-------------------------------------------------", "\n")
	cat("Data Summary", "\n")
	cat("-------------------------------------------------", "\n")
	cat("Size   :  n =", length(x), "\n")
	cat("Min    :  min =", format(signif(range(x)[1],digits), scientific=FALSE), "\n")
	cat("Max    :  max =", format(signif(range(x)[2],digits), scientific=FALSE), "\n")
	cat("Mean   :  m =", format(signif(mean(x),digits), scientific=FALSE), "\n")
	cat("Std Dev:  s =", format(signif(sd(x),digits), scientific=FALSE), "\n\n")

	cat("Number of Bins:", length(h$breaks)-1, "\n")
	
	# relative frequency histogram option
	if (prop == TRUE) h$counts <- h$counts/length(x)
		
	# cumulative histogram option
	if (cumul != "off") {
	  old.counts <- h$counts
	  h$counts <- cumsum(h$counts)
	}
	
	# set up plot area
	if (prop == FALSE)
	  plot(h, border="transparent", xlab=x.lbl, main=main.lbl, freq=TRUE, ...)
	else
	  plot(h, border="transparent", xlab=x.lbl, ylab="Proportions", main=main.lbl, 
	    freq=TRUE, ...)
	
	# colored background for plotting area
	usr <- par("usr")
	rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="black")
	
	# grid lines computation
	vy <- pretty(h$counts)

	# plot the histogram and grid lines
	if (over.grid == FALSE) abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid)
	plot(h, add=TRUE, col=col, border=border, labels=show.values, freq=TRUE, ...)
	if (cumul == "both") {
    	h$counts <- old.counts
   	 plot(h, add=TRUE, col=col.reg, freq=TRUE)
	}
	if (over.grid == TRUE) abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid)

  # frequency table
  # j<17 condition is to stop the 0.99999... problem
  cum.c <- cumsum(h$counts)
  prop <- round(h$counts/length(x),2)
  cum.p <- cumsum(prop)
  max.dg <- 0
  for (i in 1:length(h$breaks)) {
    j <- nchar(as.character(h$breaks[i]))
    if (j>max.dg && j<17) max.dg <- j
  }
  max.dg.mid <- 0
  for (i in 1:length(h$mids)) {
    j <- nchar(as.character(h$mids[i]))
    if (j>max.dg.mid && j<19) max.dg.mid <- j
  }
  x.breaks <- format(h$breaks, width=max.dg, justify="right", scientific=FALSE)
  x.mids <- format(h$mids, width=max.dg.mid+3, justify="right", scientific=FALSE)
  
  cat("\n")
  buf1 <- format("", width=max.dg+5)
  strt <- 15 + 2*max.dg
  my.width <- strt-(nchar(buf1)+nchar("Bin"))-4
  buf2 <- format(" ", width=my.width)
  cat("------------------------------------------------------------------","\n")
  cat(buf1,"Bin",buf2,"  Midpoint","  Count"," Prop","  Cumul.c"," Cumul.p",sep="","\n")
  cat("------------------------------------------------------------------","\n")
  for (i in 2:length(h$breaks)-1)
    cat(sprintf("beyond %s to %s %s %6.0f %6.2f %6.0f %6.2f", x.breaks[i], 
      x.breaks[i+1], x.mids[i], h$counts[i], prop[i], cum.c[i], cum.p[i]), "\n") 
  cat("------------------------------------------------------------------","\n")
	cat("\n")
	
}
