.hst.main <- 
function(x, col.fill, col.stroke, col.bg, col.grid, col.reg,
       over.grid, cex.axis, col.axis, rotate.values, offset,
       breaks, bin.start, bin.width,
       bin.end, prop, hist.counts, cumul,
       xlab, ylab, main, sub, quiet, ...) {


  if (is.numeric(breaks) && !is.null(bin.start)) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Choose only one option to specify a start value.\n",
      "Either choose the option  breaks  or the option  bin.start.\n\n")
  }

  # get variable labels if exist plus axes labels
  if (is.null(ylab)) if (!prop) ylab <- "Frequency" else ylab <- "Proportion"
  gl <- .getlabels(xlab, ylab, main, sub, cex.lab=getOption("lab.size"))
  x.name <- gl$xn; x.lbl <- gl$xl
  x.lab <- gl$xb
  y.lab <- gl$yb;
  main.lab <- gl$mb
  sub.lab <- gl$sb
  cex.lab <- gl$cex.lab

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
      if (n.over > 0) cat(txt, "above ", bin.max, ".\n\n", sep="")
      stop("Extend the bin range and rerun.\n\n", call. = FALSE)
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

  if (is.null(main)) {
    orig.params <- par(no.readonly=TRUE)
    on.exit(par(orig.params))
    par(mar=c(4,4,2,2)+0.1)
  }
  
  # set up plot area
  plot(h, freq=TRUE, axes=FALSE, ann=FALSE, ...)

  # axis, axis ticks
  .axes(x.lvl=NULL, y.lvl=NULL, axTicks(1), axTicks(2),
        par("usr")[1], par("usr")[3], cex.axis, col.axis,
        rotate.values, offset, ...)

  # axis labels
  max.lbl <- max(nchar(axTicks(2)))
  .axlabs(x.lab, y.lab, main.lab, sub.lab, max.lbl, 
          xy.ticks=TRUE, offset=offset, cex.lab=cex.lab, ...) 

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

  plot(h, add=TRUE, col=col.fill, border=col.stroke, freq=TRUE, labels=hist.counts, ...)
  if (cumul == "both") {
    h$counts <- old.counts
    plot(h, add=TRUE, col=col.reg, freq=TRUE)
  }
  if (over.grid) {
    abline(v=seq(vx[1],vx[length(vx)],vx[2]-vx[1]), col=col.grid, lwd=.5)
    abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
  }

 
#------------
# text output
#------------
  if (!quiet) {

    tx <- character(length = 0)
    
    bin.width <- h$breaks[2]-h$breaks[1]
    n.bins <- length(h$breaks)-1
    tx[length(tx)+1] <- paste("Bin Width:", bin.width)
    tx[length(tx)+1] <- paste("Number of Bins:", n.bins)
    tx[length(tx)+1] <- ""

    # j<17 condition is to stop the 0.99999... problem
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
    x.mids <- format(h$mids, width=max.dg.mid, justify="right", scientific=FALSE)

    bn <- character(length=0)
    for (i in 1:(length(x.breaks)-1))
      bn[i] <- paste(x.breaks[i], ">", x.breaks[i+1])

    cum.c <- cumsum(h$counts)
    prop <- h$counts / length(x)
    cum.p <- cumsum(prop)

    out <- data.frame(bn, stringsAsFactors=FALSE)
    out$x.mids <- x.mids
    out$counts <- formatC(h$counts, digits=0, format="f")
    out$prop <- formatC(prop, digits=2, format="f")
    out$cum.c <- formatC(cum.c, digits=0, format="f")
    out$cum.p <- formatC(cum.p, digits=2, format="f")
    names(out) <- c("Bin", "Midpnt", "Count", "  Prop", "Cumul.c", "Cumul.p")

    # width of columns
    max.ln <- integer(length=0)
    for (i in 1:ncol(out)) {
      ln.nm <- nchar(colnames(out)[i]) + 1
      max.val <- max(nchar(out[,i]))
      max.ln[i] <- max(ln.nm, max.val) + 1
    }

    # write col labels
    tx[length(tx)+1] <- ""
    for (i in 1:ncol(out))
      tx[length(tx)] <- paste(tx[length(tx)], .fmtc(colnames(out)[i], w=max.ln[i]), sep="")
    tx[length(tx)+1] <- .dash2(sum(max.ln))

    # write values
    for (i in 1:nrow(out)) {
      tx[length(tx)+1] <- ""
       for (j in 1:ncol(out)) 
          tx[length(tx)] <- paste(tx[length(tx)], .fmtc(out[i,j], w=max.ln[j]), sep="")
    }

    return(list(tx=tx, bin.width=bin.width, n.bins=n.bins, breaks=h$breaks, 
      mids=h$mids, counts=h$counts, prop=prop, counts_cum=cum.c,
      prop_cum=cum.p))
  }

}
