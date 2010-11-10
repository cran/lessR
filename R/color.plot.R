color.plot <-
function(x, y=NULL, type=NULL, col.line="darkblue", col.area=NULL,  
           col.point="darkblue", col.fill=NULL, col.grid="grey90", 
           col.bg="seashell", col.box="black", xy.ticks=TRUE, 
           xlab=NULL, ylab=NULL, pch=NULL, cex=NULL, center.line=NULL,
           x.start=NULL, x.by=NULL, x.reverse=FALSE, ...) {        

# -------------------------
# Preliminaries
# -------------------------
	
	if (!is.null(type)) if (type != "p" && type != "l" && type != "b") 
		stop("Option 'type' can only be \"p\" for points, \"l\" for line 
			or \"b\" for both.")
		
	if (!is.null(center.line))
		if (center.line != "mean" && center.line != "median"&& center.line != "off") 
			stop ("Values of option 'center.line' are \"mean\", \"median\", or \"off\".")
			
	if (!is.null(y)) {
		s <- "ignored when two variables are specified.\n"
		if (!is.null(x.start)) cat("Warning: Option 'x.start'", s)
		if (!is.null(x.by)) cat("Warning: Option 'x.by'", s)
		if ((x.reverse == TRUE)) cat("Warning: Option 'x.reverse'", s)
	}
			
	#  pch=21 is a filled circle
	if (is.null(pch)) point.type <- 21 else point.type <- pch
	
	nrows <- length(x)
	if (is.null(cex)) pt.size <- 1 else pt.size <- cex
	

# -------------------------
# y only, so create x index
# -------------------------
	if (is.null(y)) {
	
		if (is.null(ylab)) y.lbl <- deparse(substitute(x)) else y.lbl <- ylab
		
		if (x.reverse == FALSE) y <- x
		else for (i in 1:nrows) y[i] <- x[nrows+1-i]
			
		if (is.null(x.start)) {
			if (is.null(xlab)) x.lbl <- "Index" else x.lbl <- xlab
			x <- seq(0,length(y)-1,1)  # ordinal position of each value on x-axis
		}
		else 	{  # x.start date specified
			if (is.null(xlab)) x.lbl <- "Date" else x.lbl <- xlab			
			date.seq <- seq(as.Date(x.start), by=x.by, length.out=nrows)
			x <- date.seq  # dates on x-axis
		}
		
		if (is.null(center.line)) {  # by default display center.line only if many runs
			m <- mean(y)
			n.change <- 0
			for (i in 1:(length(y)-1)) if ((y[i+1]>m) != (y[i]>m)) n.change <- n.change+1 
			if (n.change/(length(y)-1) < .15) center.line <- "off"
		}

		if (is.null(type)) 
		 if (is.null(col.area) || col.area == "transparent") type <- "b" else type <- "l"
			
		if (type == "b" && is.null(cex))  # set point size			
			if (nrows < 50) pt.size <- 1.0 else pt.size <- 1 - 0.002*nrows
		if (is.null(col.fill)) col.fill <- "plum"
		if (is.null(center.line) || center.line != "off") center.line <- "mean"
		
	}
	
# ----------------------------------------------------------
# x and y specified
# ----------------------------------------------------------
	else {
	
		if (is.null(col.fill)) col.fill <- "transparent"
		if (is.null(col.area)) col.area <- "transparent"
		
		if (is.null(type)) {  # when x values are sorted, plot a function
			if (sum(diff(x)>0) == length(x)-1) type <- "l" else type <- "p"
		}
		
		if (xy.ticks == TRUE) {  # assign axes labels with variable names as default
			if (is.null(xlab)) x.lbl <- deparse(substitute(x)) else x.lbl <- xlab
			if (is.null(ylab)) y.lbl <- deparse(substitute(y)) else y.lbl <- ylab
		}
		else {
			x.lbl <- ""
			y.lbl <- ""
		}
		
	}	

# -------------------------
# Plot
# -------------------------
	
	# plot setup
	plot(x, y, type="n", axes=FALSE, xlab=x.lbl, ylab=y.lbl, ...)
	if (xy.ticks == TRUE){
		if (is.null(x.start)) axis(1, ...)
			else axis.Date(1, at=seq(min(date.seq), max(date.seq), length.out=20), ...)
		axis(2, ...)
	}		

	# colored plotting area
	usr <- par("usr")
	rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border=col.box)
	
	# grid lines
	vx <- pretty(c(usr[1],usr[2]))
	vy <- pretty(c(usr[3],usr[4]))
	abline(v=seq(vx[1],vx[length(vx)],vx[2]-vx[1]), col=col.grid)
	abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid)

	# fill area under curve
	if (type != "p") col.border <- col.line else col.border <- "transparent"
	if (!is.null(col.area)) 
		polygon(c(x[1],x,x[length(x)]), c(min(y),y,min(y)), col=col.area, 
			border=col.border)
	
	# plot lines and/or points
	if (type == "l"| type == "b") {
		lines(as.numeric(x),y, col=col.line, ...)
	}
	if (type == "p" | type == "b") {
		points(x,y, col=col.point, pch=point.type, bg=col.fill, cex=pt.size, ...)
	}
	
	# plot center line
	if (!is.null(center.line) && center.line != "off") {
		cat("----------------------------------------------------\n")
		if (center.line == "mean") {
			m.y <- mean(y)
			lbl <- "mean"
			lbl.cat <- "Mean"
		}
		else if (center.line == "median") {
			m.y <- median(y)
			lbl <- "median"
			lbl.cat <- "Median"
		}
		abline(h=m.y, col="gray50", lty="dashed")
		mtext(lbl, side=4, cex=.9, col="gray50", las=2, at=c(m.y), line=0.1)
		cat(lbl.cat, m.y, "\n")
		cat("----------------------------------------------------\n")
	}
	
}
