.hst.main <- 
function(x, col.fill, col.stroke, col.bg, col.grid, col.reg,
       over.grid, cex.axis, col.axis, breaks, bin.start, bin.width,
       bin.end, prop, cumul, digits.d, xlab, ylab, main, quiet, ...) {


  if (!is.numeric(x)) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "A histogram is only computed from a numeric variable.\n",
      "For the analysis of a categorical variable use BarChart.\n\n")
  }
        
  if (is.numeric(breaks) && !is.null(bin.start)) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Choose only one option to specify a start value.\n",
      "Either choose the option  breaks  or the option  bin.start.\n\n")
  }

  # get variable labels if exist plus axes labels
  if (is.null(ylab)) if (!prop) ylab <- "Frequency" else ylab <- "Proportion"
  gl <- .getlabels(xlab, ylab, main)
  x.name <- gl$xn; x.lbl <- gl$xl; x.lab <- gl$xb
  y.lab <- gl$yb;
  main.lab <- gl$mb

  # get breaks from user supplied bin width and/or supplied start value
  if (!is.null(bin.width)  || !is.null(bin.start) || !is.null(bin.end)) {
    if (is.null(bin.start)) 
      bin.start <- pretty(min(x, na.rm = TRUE):max(x, na.rm = TRUE))[1]
    if (is.null(bin.width)) {
      h <- hist(x, plot=FALSE, breaks="Sturges")
      bin.width <- h$breaks[2]-h$breaks[1]
    }
    max.x <- max(x, na.rm=TRUE)
    if (is.null(bin.end)) bin.end <- max.x
    if (bin.end < bin.start) { 
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "bin.start: ", bin.start, "\n",
        "bin.end: ", bin.end, "\n",
        "bin.end is larger than bin.start, make bin.end larger.\n\n")
    }
    breaks <- seq(bin.start,bin.end,bin.width)
    seq.end <- bin.end
    while (max(breaks) < bin.end) {
      seq.end <- seq.end + bin.width
      breaks <- seq(bin.start,seq.end,bin.width)
    }
  }
  
  # for user supplied bins, from seq function or bin.start, 
  #  make sure entire data range is spanned
  if (is.numeric(breaks)) {
    cc <- cut(x, breaks, dig.lab=6, ...)   # replace each data value with its bin
    labs <- levels(cc)  # get list of unique bins, ordered
    bins <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", labs) ),
          upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs) ))
    bin.min <- min(bins)
    bin.max <- max(bins)
    n.under <- length(x[which(x<bin.min)])
    n.over <- length(x[which(x>bin.max)])
    if (n.under+n.over > 0) {
      cat("\nRange of the data: ", min(x, na.rm=TRUE), " to ",
          max(x, na.rm=TRUE), "\n", sep="")
      if (length(breaks) > 3)
        cat("Specified bin cutpoints: ", bin.min, breaks[2], "...", 
          breaks[length(breaks)-1], bin.max, "\n\n")
      else
        cat("Range of the specified bins: ", bin.min, " to ", bin.max, "\n", sep="")
      if (n.under > 0) 
        cat("Data values too small to fit in the bins: ", x[which(x<bin.min)], "\n")
      if (n.over > 0)
        cat("Data values too large to fit in the bins: ", x[which(x>bin.max)], "\n")
      cat("\n")
      cat("Each data value must be in a bin.\n")
      txt <- "To fix this problem, extend the bin range "
      if (n.under > 0) cat(txt, "below ", bin.min, ".\n", sep="")
      if (n.over > 0) cat(txt, "above ", bin.max, ".\n", sep="")
      stop("Extend the bin range and rerun.\n\n", call. = FALSE)
    }  
  }


  # calculate but do not plot the histogram
  # arguments in ... for plotting instructions generate warnings with no plot
  h <- suppressWarnings(hist(x, plot=FALSE, breaks, ...))
  
  # relative frequency histogram option
  if (prop) h$counts <- h$counts/length(x)
    
  # cumulative histogram option
  if (cumul != "off") {
    old.counts <- h$counts
    h$counts <- cumsum(h$counts)
  }
  
  # set up plot area
  suppressWarnings(plot(h, border="transparent", xlab=x.lab, ylab=y.lab,
        main=main.lab, freq=TRUE, axes=FALSE, ...))
  axis(1, cex.axis=cex.axis, col.axis=col.axis, ...) 
  axis(2, cex.axis=cex.axis, col.axis=col.axis, ...) 

  # colored background for plotting area
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="black")
  
  # grid lines computation
  vy <- pretty(h$counts)
  vx <- h$breaks

  # plot the histogram and grid lines
  if (!over.grid) {
    abline(v=seq(vx[1],vx[length(vx)],vx[2]-vx[1]), col=col.grid, lwd=.5)
    abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
  }

  suppressWarnings(plot(h, add=TRUE, col=col.fill,
                   border=col.stroke, freq=TRUE, ...))
  if (cumul == "both") {
    h$counts <- old.counts
    suppressWarnings(plot(h, add=TRUE, col=col.reg, freq=TRUE))
  }
  if (over.grid) {
    abline(v=seq(vx[1],vx[length(vx)],vx[2]-vx[1]), col=col.grid, lwd=.5)
    abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
  }

 
#------------
# text output
#------------
  if (!quiet) {
 
    # summarize data
    .ss.numeric(x, digits.d=digits.d, brief=TRUE)

    # frequency table
    # j<17 condition is to stop the 0.99999... problem
    cum.c <- cumsum(h$counts)
    prop <- h$counts / length(x)
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
    
    cat("\nBin Width:", h$breaks[2]-h$breaks[1], "\n")
    cat("Number of Bins:", length(h$breaks)-1, "\n")
    cat("\n")
    buf1 <- format("", width=max.dg+1)
    strt <- 9 + 2*max.dg
    my.width <- strt-(nchar(buf1)+nchar("Bin"))-7
    buf2 <- format(" ", width=my.width)
    .dash(56)
    cat(buf1,"Bin",buf2,"  Midpoint","  Count","  Prop","  Cumul.c"," Cumul.p",sep="","\n")
    .dash(56)
    for (i in 2:length(h$breaks)-1)
      cat(sprintf("%s > %s %s %6.0f %6.2f %6.0f %6.2f", x.breaks[i], 
        x.breaks[i+1], x.mids[i], h$counts[i], prop[i], cum.c[i], cum.p[i]), "\n") 
    .dash(56)
  
    cat("\n")
  }

  return(h)
}
