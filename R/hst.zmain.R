.hst.main <- 
function(x, fill=NULL, color=NULL, trans=NULL, col.reg=NULL,
       rotate_x=NULL, rotate_y=NULL, offset=NULL,
       breaks, bin_start, bin_width, bin_end,
       prop, values=NULL, cumulate="off",
       xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
       xlab_adj=NULL, ylab_adj=NULL,
       bm.adj=NULL, lm.adj=NULL, tm.adj=NULL, rm.adj=NULL,
       add=NULL, x1=NULL, x2=NULL, y1=NULL, y2=NULL,
       scale_x=NULL, scale_y=NULL,
       quiet=FALSE, do_plot=TRUE, fun_call=NULL, ...) {


  # get variable labels if exist plus axes labels
  if (is.null(ylab)) {
    was.null <- TRUE
    ylab <- ifelse (!prop, "Count of", "Proportion of")
    if (cumulate != "off") ylab <- paste("Cumulative", ylab)
  }
  else
    was.null <- FALSE

  lab_cex <- getOption("lab_cex")
  lab_x_cex <- getOption("lab_x_cex")
  lab_y_cex <- getOption("lab_y_cex")
  lab_x_cex <- ifelse(is.null(lab_x_cex), lab_cex, lab_x_cex)
  adj <- .RSadj(lab_cex=lab_x_cex); lab_x_cex <- adj$lab_cex
  lab_y_cex <- ifelse(is.null(lab_y_cex), lab_cex, lab_y_cex)
  adj <- .RSadj(lab_cex=lab_y_cex); lab_y_cex <- adj$lab_cex


  gl <- .getlabels(xlab, ylab, main, lab_x_cex=lab_x_cex, 
                   lab_y_cex=lab_y_cex)
  x.name <- gl$xn; x.lbl <- gl$xl
  x.lab <- gl$xb
  y.lab <- ifelse (was.null, paste(gl$yb, x.name), gl$yb)
  main.lab <- gl$mb
  sub.lab <- gl$sb

  num.cat.x <- .is.num.cat(x, n_cat=getOption("n_cat"))
  if (num.cat.x) {
    if (is.null(bin_width)) bin_width <- 1
    if (is.null(bin_start)) bin_start <- min(x, na.rm=TRUE) - .5 
  } 

  # get breaks from user supplied bin width and/or supplied start value
  if (!is.null(bin_width)  || !is.null(bin_start) || !is.null(bin_end)) {
    if (is.null(bin_start)) 
      bin_start <- pretty(min(x, na.rm = TRUE):max(x, na.rm = TRUE))[1]
    if (is.null(bin_width)) {
      h <- suppressWarnings(hist(x, plot=FALSE, breaks="Sturges"))
      bin_width <- h$breaks[2]-h$breaks[1]
    }
    max.x <- max(x, na.rm=TRUE)
    if (is.null(bin_end)) bin_end <- max.x
    if (bin_end < bin_start) { 
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "bin_start: ", bin_start, "\n",
        "bin_end: ", bin_end, "\n",
        "bin_end is larger than bin_start, make bin_end larger.\n\n")
    }
    breaks <- seq(bin_start,bin_end,bin_width)
    seq.end <- bin_end
    while (max(breaks) < bin_end) {
      seq.end <- seq.end + bin_width
      breaks <- seq(bin_start,seq.end,bin_width)
    }
  }
  
  # for user supplied bins, from seq function or bin_start, 
  # make sure entire data range is spanned
  if (is.numeric(breaks)) {
    cc <- cut(x, breaks, dig.lab=6, ...)   # replace each data value with its bin
    labs <- levels(cc)  # get list of unique bins, ordered
    bins <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", labs) ),
          upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs) ))
    bin_min <- min(bins)
    bin_max <- max(bins)
    n.under <- length(x[which(x<bin_min)])
    n.over <- length(x[which(x>bin_max)])
    if (n.under+n.over > 0) {
      txt.u <- "";  txt.o <- "";  txt.nu <- "";  txt.no <- ""
      if (length(breaks) > 3)
        txt.c <- paste("Specified bin cut-points: ", bin_min, breaks[2], "...", 
          breaks[length(breaks)-1], bin_max, "\n\n")
      else
        txt.c <- paste("Range of the specified bins: ", bin_min,
          " to ", bin_max, "\n", sep="")
      if (n.under > 0) 
        txt.u <- paste("Data values too small to fit in the bins: ",
          x[which(x<bin_min)], "\n\n")
      if (n.over > 0)
        txt.o <- paste("Data values too large to fit in the bins: ",
          x[which(x>bin_max)], "\n\n")
      txt <- "To fix this problem, extend the bin range "
      if (n.under > 0)
        txt.nu <- paste(txt, "below ", bin_min, "\n", sep="")
      if (n.over > 0)
        txt.no <- paste(txt, "above ", bin_max, "\n", sep="")
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Range of the data for ", x.name, ": ", min(x, na.rm=TRUE), " to ",
            max(x, na.rm=TRUE), "\n",
        txt.c,
        txt.u,
        txt.o,
        "Each data value must be in a bin\n",
        txt.nu,
        txt.no, "\n",
        "Extend the bin range by setting bin_start and rerun\n\n")
    }
  }


  # calculate but do not plot the histogram
  # arguments in ... for plotting instructions generate warnings with no plot
  h <- suppressWarnings(hist(x, plot=FALSE, breaks, labels=values, ...))

  # relative frequency histogram option
  if (prop) h$counts <- h$counts/length(x)
    
  # cumulative histogram option
  if (cumulate != "off") {
    old.counts <- h$counts
    h$counts <- cumsum(h$counts)
  }
  
  if (do_plot) {

    # set margins
    max.width <- strwidth(as.character(max(pretty(h$counts))), units="inches")
    
    margs <- .plt.marg(max.width, y.lab, x.lab, main.lab, sub.lab, rotate_x)
    lm <- margs$lm
    tm <- margs$tm
    rm <- margs$rm
    bm <- margs$bm
    n.lab_x.ln <- margs$n.lab_x.ln
    n.lab_y.ln <- margs$n.lab_y.ln

    if (lab_x_cex > 1.1) bm <- bm + (.10*lab_x_cex)  # kludge
    if (lab_y_cex > 1.1) lm <- lm + (.10*lab_y_cex)

    if (offset > 0.5) bm <- bm + (-0.05 + 0.2 * offset)  # offset kludge

    # user manual adjustment here
    bm <- bm + bm.adj
    lm <- lm + lm.adj
    tm <- tm + tm.adj
    rm <- rm + rm.adj
   
    orig.params <- par(no.readonly=TRUE)
    on.exit(par(orig.params))  
    
    par(bg=getOption("window_fill"))
    par(mai=c(bm, lm, tm, rm))

    # set up plot 
    plot(h, freq=TRUE, axes=FALSE, ann=FALSE, ...)

    # plot background
    if (is.null(scale_x))
      vx <- h$breaks
    else
      vx <- axTicks(1, axp=scale_x)
    axT1 <- seq(vx[1], vx[length(vx)], vx[2]-vx[1])
    .plt.bck(par("usr"), axT1, NULL, do.h=FALSE)

    if (is.null(scale_y))
      vy <- pretty(h$counts)
    else
      vy <- axTicks(2, axp=scale_y)
    axT2 <- seq(vy[1], vy[length(vy)], vy[2]-vy[1])
    .plt.bck(par("usr"), NULL, axT2, do.v=FALSE)

    # axis, axis ticks
    .axes(x.lvl=NULL, y.lvl=NULL,
          axTicks(1, axp=scale_x), axTicks(2, axp=scale_y),
          rotate_x=rotate_x, rotate_y=rotate_y, offset=offset, ...)

    # axis labels
    .axlabs(x.lab, y.lab, main.lab, sub.lab, 
            xy_ticks=TRUE, offset=offset, 
            lab_x_cex=lab_x_cex, lab_y_cex=lab_y_cex, main_cex=NULL,
            n.lab_x.ln=n.lab_x.ln, n.lab_y.ln=n.lab_y.ln,
            xlab_adj=xlab_adj, ylab_adj=ylab_adj, ...) 


    # see if apply a pre-defined color range
    n.bins <- length(h$counts)
    clr <- NULL
    clr <- .color_range(fill, n.bins)
      
    # not a color range such as "hues" or "blues", so assign clr here
    if (is.null(clr)) {
        clr <- fill  # user provided the colors
    }

    # bar transparency
    if (!is.null(trans)) if (trans > 0)
      for (i in 1:length(clr)) clr[i] <- .maketrans(clr[i], (1-trans)*256)

    # plot the histogram
    plot(h, add=TRUE, col=clr, border=color, freq=TRUE,
         labels=values, ...)
    if (cumulate == "both") {
      h$counts <- old.counts
      plot(h, add=TRUE, col=col.reg, freq=TRUE)
    }


    # annotations
    if (!is.null(add)) {

      add_cex <- getOption("add_cex")
      add_lwd <- getOption("add_lwd")
      add_lty <- getOption("add_lty")
      add_color <- getOption("add_color")
      add_fill <- getOption("add_fill")
      add_trans <- getOption("add_trans")

      .plt.add (add, x1, x2, y1, y2,
                add_cex, add_lwd, add_lty, add_color, add_fill, add_trans) 
    }
  }
 

#------------
# text output
#------------

    stats <- .hst.stats(h, length(x), fun_call)

    txsug=stats$txsug
    tx=stats$tx
    bin_width=stats$bin_width
    n.bins=stats$n.bins
    prop=stats$prop
    cum.c=stats$counts_cum
    cum.p=stats$prop_cum

    return(list(txsug=txsug, ttx=tx, bin_width=bin_width, n.bins=n.bins,
      breaks=h$breaks, mids=h$mids, counts=h$counts, prop=prop,
      counts_cum=cum.c, prop_cum=cum.p))

}
