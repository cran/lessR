.hst.main <- 
function(x, fill=NULL, color=NULL, trans=NULL, col.reg=NULL,
       rotate.x=NULL, rotate.y=NULL, offset=NULL,
       breaks, bin.start, bin.width,
       bin.end, prop, hist.counts=NULL, cumul="off",
       xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
       xlab.adj=NULL, ylab.adj=NULL,
       bm.adj=NULL, lm.adj=NULL, tm.adj=NULL, rm.adj=NULL,
       add=NULL, x1=NULL, x2=NULL, y1=NULL, y2=NULL,
       scale.x=NULL, scale.y=NULL,
       quiet=FALSE, do.plot=TRUE, fun.call=NULL, ...) {


  # get variable labels if exist plus axes labels
  if (is.null(ylab)) {
    was.null <- TRUE
    ylab <- ifelse (!prop, "Count of", "Proportion of")
  }
  else
    was.null <- FALSE

  lab.cex <- getOption("lab.cex")
  lab.x.cex <- getOption("lab.x.cex")
  lab.y.cex <- getOption("lab.y.cex")
  lab.x.cex <- ifelse(is.null(lab.x.cex), lab.cex, lab.x.cex)
  adj <- .RSadj(lab.cex=lab.x.cex); lab.x.cex <- adj$lab.cex
  lab.y.cex <- ifelse(is.null(lab.y.cex), lab.cex, lab.y.cex)
  adj <- .RSadj(lab.cex=lab.y.cex); lab.y.cex <- adj$lab.cex


  gl <- .getlabels(xlab, ylab, main, lab.x.cex=lab.x.cex, 
                   lab.y.cex=lab.y.cex)
  x.name <- gl$xn; x.lbl <- gl$xl
  x.lab <- gl$xb
  y.lab <- ifelse (was.null, paste(gl$yb, x.name), gl$yb)
  main.lab <- gl$mb
  sub.lab <- gl$sb

  num.cat.x <- .is.num.cat(x, n.cat=getOption("n.cat"))
  if (num.cat.x) {
    if (is.null(bin.width)) bin.width <- 1
    if (is.null(bin.start)) bin.start <- min(x, na.rm=TRUE) - .5 
  } 

  # get breaks from user supplied bin width and/or supplied start value
  if (!is.null(bin.width)  || !is.null(bin.start) || !is.null(bin.end)) {
    if (is.null(bin.start)) 
      bin.start <- pretty(min(x, na.rm = TRUE):max(x, na.rm = TRUE))[1]
    if (is.null(bin.width)) {
      h <- suppressWarnings(hist(x, plot=FALSE, breaks="Sturges"))
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
  # make sure entire data range is spanned
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
      txt.u <- "";  txt.o <- "";  txt.nu <- "";  txt.no <- ""
      if (length(breaks) > 3)
        txt.c <- paste("Specified bin cut-points: ", bin.min, breaks[2], "...", 
          breaks[length(breaks)-1], bin.max, "\n\n")
      else
        txt.c <- paste("Range of the specified bins: ", bin.min,
          " to ", bin.max, "\n", sep="")
      if (n.under > 0) 
        txt.u <- paste("Data values too small to fit in the bins: ",
          x[which(x<bin.min)], "\n\n")
      if (n.over > 0)
        txt.o <- paste("Data values too large to fit in the bins: ",
          x[which(x>bin.max)], "\n\n")
      txt <- "To fix this problem, extend the bin range "
      if (n.under > 0)
        txt.nu <- paste(txt, "below ", bin.min, "\n", sep="")
      if (n.over > 0)
        txt.no <- paste(txt, "above ", bin.max, "\n", sep="")
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Range of the data for ", x.name, ": ", min(x, na.rm=TRUE), " to ",
            max(x, na.rm=TRUE), "\n",
        txt.c,
        txt.u,
        txt.o,
        "Each data value must be in a bin\n",
        txt.nu,
        txt.no, "\n",
        "Extend the bin range by setting bin.start and rerun\n\n")
    }
  }


  # calculate but do not plot the histogram
  # arguments in ... for plotting instructions generate warnings with no plot
  h <- suppressWarnings(hist(x, plot=FALSE, breaks, labels=hist.counts, ...))

  # relative frequency histogram option
  if (prop) h$counts <- h$counts/length(x)
    
  # cumulative histogram option
  if (cumul != "off") {
    old.counts <- h$counts
    h$counts <- cumsum(h$counts)
  }
  
  if (do.plot) {

    # set margins
    max.width <- strwidth(as.character(max(pretty(h$counts))), units="inches")
    
    margs <- .marg(max.width, y.lab, x.lab, main.lab, rotate.x)
    lm <- margs$lm
    tm <- margs$tm
    rm <- margs$rm
    bm <- margs$bm
    n.lab.x.ln <- margs$n.lab.x.ln
    n.lab.y.ln <- margs$n.lab.y.ln

#   if (lab.x.cex > 1.2) bm <- bm + (.15*lab.x.cex)
#   if (lab.y.cex > 1.2) lm <- lm + (.15*lab.y.cex)

    if (offset > 0.5) bm <- bm + (-0.05 + 0.2 * offset)  # offset kludge

    # user manual adjustment here
    bm <- bm + bm.adj
    lm <- lm + lm.adj
    tm <- tm + tm.adj
    rm <- rm + rm.adj
   
    orig.params <- par(no.readonly=TRUE)
    on.exit(par(orig.params))  
    
    par(bg=getOption("window.fill"))
    par(mai=c(bm, lm, tm, rm))

    # set up plot 
    plot(h, freq=TRUE, axes=FALSE, ann=FALSE, ...)
    
    # color plotting background color
    usr <- par("usr")          
    rect(usr[1], usr[3], usr[2], usr[4], col=getOption("panel.fill"),
         border="transparent")

    # plot grid lines
    if(is.null(scale.x))
      vx <- h$breaks
    else
      vx <- axTicks(1, axp=scale.x)
    .grid("v", seq(vx[1],vx[length(vx)],vx[2]-vx[1]))
    if(is.null(scale.y))
      vy <- pretty(h$counts)
    else
      vy <- axTicks(2, axp=scale.y)
    .grid("h", seq(vy[1],vy[length(vy)],vy[2]-vy[1]))

    # box around plot
    rect(usr[1], usr[3], usr[2], usr[4], col="transparent",
      border=getOption("panel.color"),
      lwd=getOption("panel.lwd"), lty=getOption("panel.lty"))

    # axis, axis ticks
    .axes(x.lvl=NULL, y.lvl=NULL,
          axTicks(1, axp=scale.x), axTicks(2, axp=scale.y),
          par("usr")[1], par("usr")[3], 
          rotate.x=rotate.x, rotate.y=rotate.y, offset=offset, ...)

    # axis labels
    max.lbl <- max(nchar(axTicks(2)))
    .axlabs(x.lab, y.lab, main.lab, sub.lab, max.lbl,
            xy.ticks=TRUE, offset=offset, 
            lab.x.cex=lab.x.cex, lab.y.cex=lab.y.cex, main.cex=NULL,
            n.lab.x.ln=n.lab.x.ln, n.lab.y.ln=n.lab.y.ln,
            xlab.adj=xlab.adj, ylab.adj=ylab.adj, ...) 


    # see if apply a pre-defined color range
    n.bins <- length(h$counts)
    if (length(fill) == 1) fill <- .color.range(fill, n.bins)

    # bar transparency
    n.clr <- length(fill)
cat("n.clr:", n.clr, "\n")
print(fill)
print(getOption("bar.fill.ordered"))
    if (!is.null(trans)) if (trans > 0)
      for (i in 1:n.clr) fill[i] <- .maketrans(fill[i], (1-trans)*256) 

    # plot the histogram
    plot(h, add=TRUE, col=fill, border=color, freq=TRUE,
         labels=hist.counts, ...)
    if (cumul == "both") {
      h$counts <- old.counts
      plot(h, add=TRUE, col=col.reg, freq=TRUE)
    }


    # annotations
    if (!is.null(add)) {

      add.cex <- getOption("add.cex")
      add.lwd <- getOption("add.lwd")
      add.lty <- getOption("add.lty")
      add.color <- getOption("add.color")
      add.fill <- getOption("add.fill")
      add.trans <- getOption("add.trans")

      .plt.add (add, x1, x2, y1, y2,
                add.cex, add.lwd, add.lty, add.color, add.fill, add.trans) 
    }
  }
 

#------------
# text output
#------------
  if (!quiet) {

    stats <- .hst.stats(h, length(x), fun.call)

    txsug=stats$txsug
    tx=stats$tx
    bin.width=stats$bin.width
    n.bins=stats$n.bins
    prop=stats$prop
    cum.c=stats$counts_cum
    cum.p=stats$prop_cum

    return(list(txsug=txsug, ttx=tx, bin.width=bin.width, n.bins=n.bins,
      breaks=h$breaks, mids=h$mids, counts=h$counts, prop=prop,
      counts_cum=cum.c, prop_cum=cum.p))
  }

  else {  # Plot needs binning and midpoints even if not displaying text output
    return(list(bin.width=bin.width,
      breaks=h$breaks, mids=h$mids, counts=h$counts))
  }

}
