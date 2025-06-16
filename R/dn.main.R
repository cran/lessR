.dn.main <- 
function(x, 
         bw="nrd0", type="both",
         histo=TRUE, bin_start=NULL, bin_width=NULL,
         fill_hist, col.nrm, col.gen,
         fill_nrm, fill_gen, lwd=NULL,
         rotate_x=0, rotate_y=0, offset=0.5, 
         axis_fmt="K", axis_x_pre="", axis_y_pre="",
         x.pt=NULL, xlab=NULL, main=NULL, sub=NULL,
         y_axis=FALSE, x.min=NULL, x.max=NULL,
         band=FALSE, color_rug="gray40", size_rug=0.5, quiet,
         fncl=NULL, ...)  {
  

  if (!is.null(x.pt)) {
    y_axis <- TRUE
    type <- "general"
  }


# estimate the histogram and/or curves ------------------------------------

  # no missing data
  n <- sum(!is.na(x))
  n.miss <- sum(is.na(x))
  if (n.miss > 0) x <- na.omit(x)

  # get breaks from user supplied bin width and/or supplied start value
  # otherwise, breaks="Sturges by default
  # copied from histogram
  if (!is.null(bin_width)  || !is.null(bin_start)) {
    if (is.null(bin_start))
       bin_start <- pretty(min(x, na.rm=TRUE):max(x, na.rm=TRUE))[1]
    if (is.null(bin_width)) {
      h <- hist(x, plot=FALSE, breaks="Sturges")
      bin_width <- h$breaks[2]-h$breaks[1]
    }
    max.x <- max(x, na.rm = TRUE)
    seq.end <- max.x
    breaks <- seq(bin_start, seq.end, bin_width)
    while (max(breaks) < max.x) {
      seq.end <- seq.end + bin_width
      breaks <- seq(bin_start, seq.end, bin_width)
    }
  }
  else
    breaks="Sturges"

  if (is.null(main)) {
    orig.params <- par(no.readonly=TRUE)
    on.exit(par(orig.params))
    par(mar=c(4,4,2,2)+0.1)
  }

  # histogram calculations, no plot
  h <- hist(x, plot=FALSE, breaks)
 
  # general density curve, no plot
  # suppress warnings about possible graphic parameters
  d.gen <- suppressWarnings(density(x, bw, ...))
  
  # min and max x coordinates for graph
  x.min <- min(d.gen$x)
  x.max <- max(d.gen$x)
  
  # normal density curve, no plot
  xx <- seq(x.min, x.max, length=200)
  d.nrm <- dnorm(xx, mean(x), sd(x))

  # max y coordinate for graph
  # need to estimate all three objects in all cases: re-work
  y.max <- max(max(d.nrm), max(d.gen$y), max(h$density))


# set up plot area --------------------------------------------------------

  # plot parameters
  col.bg <- getOption("panel_fill")
  col.box <- getOption("panel_color")

  lab_cex <- getOption("lab_cex")
  axis_cex <- getOption("axis_cex")
  col.axis <- getOption("axis_text_color")

  # get lab_x_cex  lab_y_cex
  lab_cex <- getOption("lab_cex")
  lab_x_cex <- getOption("lab_x_cex")
  lab_x_cex <- ifelse(is.null(lab_x_cex), lab_cex, lab_x_cex)
  adj <- .RSadj(lab_cex=lab_x_cex); lab_x_cex <- adj$lab_cex

  # get variable labels if exist plus axes labels
  gl <- .getlabels(xlab, ylab=NULL, main, lab_x_cex=lab_x_cex) 
  x.name <- gl$xn; x.lbl <- gl$xl; x.lab <- gl$xb
  main.lab <- gl$mb
  sub.lab <- gl$sb
  # set margins
  if (band)  # rug, allow for more rug room
    y.min <- -0.02 * max(d.gen$y)  # make room for the rug
  else
    y.min <- 0
  margs <- .plt.marg(0, y.lab=NULL, x.lab, main, sub, lab_x_cex=lab_x_cex)
  lm <- margs$lm
  tm <- margs$tm
  rm <- margs$rm
  bm <- margs$bm
 
  if (y_axis) lm <- lm + 0.6  # allow extra room for densities

  orig.params <- par(no.readonly=TRUE)
  on.exit(par(orig.params))  

  par(bg=getOption("window_fill"))
  par(mai=c(bm, lm, tm, rm))
  
  # set up coordinate space
  plot(h, border="transparent", freq=FALSE,
     xlim=c(x.min,x.max), ylim=c(y.min,y.max),
     axes=FALSE, ann=FALSE, xlab=NULL, ylab=NULL, main=NULL, ...)

  # adjust axis label from tick with mgp[2]
  # mgp does not work with rotate_x, see .axes()
  my.mgp <- par("mgp")  # save to restore
  ax <- .axes_dim()  # get axis value parameters
  mgp2 <- -0.350 + (0.9 * ax$axis_x_cex)
  par(mgp = c(my.mgp[1], mgp2, my.mgp[3]))  # labels closer to axis
  adj <- .RSadj(axis_cex=ax$axis_x_cex); axis_x_cex <- adj$axis_cex

  # axis, axis ticks
  par(tcl=-0.28)  # axis tic length
  if (!y_axis)
    .axes(x.lvl=NULL, y.lvl=NULL, axTicks(1), NULL,
          rotate_x=rotate_x, rotate_y=rotate_y, offset=offset,
          axis_fmt=axis_fmt, axis_x_pre=axis_x_pre, axis_y_pre="no", ...)
  else
    .axes(x.lvl=NULL, y.lvl=NULL, axTicks(1), axTicks(2),
          rotate_x=rotate_x, rotate_y=rotate_y, offset=offset,
          axis_fmt=axis_fmt, axis_x_pre=axis_x_pre, axis_y_pre=axis_y_pre, ...)

  # axis value labels
  if (!y_axis) y.lab="" else y.lab="Density"
  .axlabs(x.lab, y.lab=NULL, main.lab, sub.lab,
          xy_ticks=TRUE, offset=offset, ...) 

  # colored background for plotting area
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border=col.box,
    lwd=getOption("panel_lwd"), lty=getOption("panel_lty"))
  

# plot the objects --------------------------------------------------------

  # plot the previously computed histogram
  if (histo)
    plot(h, add=TRUE, freq=FALSE, col=fill_hist, border="transparent")

  # plot the previously estimated normal curve
  if (type == "normal" || type == "both") {
    if (is.null(lwd)) lwd <- ifelse (fill_nrm == "transparent", 1.35, 1)
    polygon(c(x.min,xx,x.max), c(0,d.nrm,0), col=fill_nrm, 
            border=col.nrm, lwd=lwd)
  }

  # plot the previously computed general curve
  if (type == "general" || type == "both") {
    if (is.null(lwd)) lwd <- ifelse (fill_gen == "transparent", 1.35, 1)
    polygon(d.gen, col=fill_gen, border=col.gen, lwd=lwd)
  }

  # plot the rug option
  if (band) rug(x, col=color_rug, lwd=size_rug, ticksize=0.05)
 
  # plot the optional bar about a chosen point for general curve only
  if (!is.null(x.pt)  &&  type == "general") {
    d <- d.gen
    y.pt <- d$y[which.min(abs(x.pt-d$x))]
    xbeg <- x.pt - 0.5
    xend <- x.pt + 0.5
    xsub <- d$x[d$x > xbeg & d$x < xend]
    ysub <- d$y[d$x > xbeg & d$x < xend]
    txt <- paste("Density =", .fmt(y.pt,3), "at x =", .fmt(x.pt, 2))
    title(main=txt)
    polygon(c(xbeg, xsub, xend), c(0, ysub, 0), col="lightsteelblue")
    if (min(x) > 0) left <- -2*min(x) else left <- 2*min(x) 
    lines(c(left,x.pt), c(y.pt,y.pt), col="darkblue", lwd = 0.5)
    lines(c(x.pt,x.pt), c(0,y.pt), col="darkblue", lwd = 0.5)
  }


# text output -------------------------------------------------------------

  tx=""

  tx <- character(length = 0)
  if (getOption("suggest")) {

    # function call for suggestions
    fncl <- .fun_call.deparse(fncl) 
    fncl <- gsub(")$", "", fncl)  # get function call less closing ) 
    fncl <- gsub(" = ", "=", fncl)

    tx[length(tx)+1] <- ">>> Suggestions"
      tx[length(tx)+1] <- "bin_width: set the width of each bin"
    txt <- "  # histogram only"
    tx[length(tx)+1] <- paste("Histogram(", getOption("xname"),
       ")", txt, sep="")      
    txt <- "  # Violin/Box/Scatterplot (VBS) plot"
    tx[length(tx)+1] <- paste("Plot(", getOption("xname"), ")", txt,
       sep="")      
  }
  txsug <- tx
  if (length(txsug) == 0) txsug <- ""

  tx <- character(length = 0)

  tx[length(tx)+1] <- paste( "--- Bandwidth ---",
         "     for general curve:", .fmt(d.gen$bw,4), "\n")

  W <- NA; p.val <- NA
  if (type == "normal" || type == "both") {
    digits_d <- 3
    if (n > 2 && n < 5000) {
      nrm <- shapiro.test(x)
      W <- .fmt(nrm$statistic,min(4,digits_d+1))
      p.val <- .fmt(nrm$p.value,min(4,digits_d+1))
      tx[length(tx)+1] <- " "
      tx[length(tx)+1] <- paste("Null hypothesis is a normal population")
      tx[length(tx)+1] <- paste(nrm$method, ":  W = ", W, ",  p-value = ",
        p.val, sep="")
    }
    else 
      tx[length(tx)+1] <- paste("Sample size out of range for Shapiro-Wilk",
                          " normality test.")
  }

  return(list(tx=tx, txsug=txsug, 
              bw=d.gen$bw, n=n, n.miss=n.miss, W=W, pvalue=p.val))


} 
