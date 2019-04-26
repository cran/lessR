.dn.main <- 
function(x, 
         bw="nrd0", type="both",
         histogram=TRUE, bin_start=NULL, bin_width=NULL,
         col_fill, col.bg, col.box,
         col.nrm, col.gen, col_fill_nrm, col_fill_gen,
         lab_cex=1.0, axis_cex=0.75, col.axis="gray30",
         rotate_x=0, rotate_y=0, offset=0.5, 
         x.pt=NULL, xlab=NULL, main=NULL, sub=NULL,
         y_axis=FALSE, x.min=NULL, x.max=NULL,
         band=FALSE, color_rug="gray40", size.rug=0.5, quiet, ...)  {

  if (!is.null(x.pt)) {
    y_axis <- TRUE
    type <- "general"
  }

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

  # get breaks from user supplied bin width and/or supplied start value
  # otherwise, breaks="Sturges by default
  # copied from histogram
  if (!is.null(bin_width)  || !is.null(bin_start)) {
    if (is.null(bin_start)) bin_start <- pretty(min(x):max(x))[1]
    if (is.null(bin_width)) {
      h <- histogram(x, plot=FALSE, breaks="Sturges")
      bin_width <- h$breaks[2]-h$breaks[1]
    }
    max.x <- max(x, na.rm = TRUE)
    seq.end <- max.x
    breaks <- seq(bin_start,seq.end,bin_width)
    while (max(breaks) < max.x) {
      seq.end <- seq.end + bin_width
      breaks <- seq(bin_start,seq.end,bin_width)
    }
  }
  else breaks="Sturges"


  if (is.null(main)) {
    orig.params <- par(no.readonly=TRUE)
    on.exit(par(orig.params))
    par(mar=c(4,4,2,2)+0.1)
  }

  # histogram calculations, no plot
  h <- hist(x, plot=FALSE, breaks)

  # no missing data
  n <- sum(!is.na(x))
  n.miss <- sum(is.na(x))
  if (n.miss > 0) x <- na.omit(x)
 
  # general density curve, no plot
  # suppress warnings about possible graphic parameters
  d.gen <- suppressWarnings(density(x, bw, ...))
  
  mx <- mean(x)

  # min and max x coordinates for graph, make symmetric
  min_dev.x <- min(d.gen$x) - mx
  max.dev.x <- max(d.gen$x) - mx
  if (abs(min_dev.x) > abs(max.dev.x)) {
    if (is.null(x.min)) x.min <- min(d.gen$x)
    if (is.null(x.max)) x.max <- mx + abs(min_dev.x)
  }
  if (abs(max.dev.x) >= abs(min_dev.x)) {
    if (is.null(x.min)) x.min <- mx - abs(max.dev.x)
    if (is.null(x.max)) x.max <- max(d.gen$x)
  }
  
  # normal density curve, no plot
  xx <- seq(x.min, x.max, length=200)
  if (col_fill_nrm == "transparent") lw <- 2 else lw <- 1
  d.nrm <- dnorm(xx,mean(x),sd(x))

  # max y coordinate for graph
  max.y <- max(max(d.nrm), max(d.gen$y), max(h$density))
  
  # set margins
  margs <- .marg(0, y.lab=NULL, x.lab, main, lab_x_cex=lab_x_cex)
  lm <- margs$lm
  tm <- margs$tm
  rm <- margs$rm
  bm <- margs$bm

  orig.params <- par(no.readonly=TRUE)
  on.exit(par(orig.params))  
  
  par(bg=getOption("window_fill"))
  par(mai=c(bm, lm, tm, rm))

  
  # set up plot area
  plot(h, border="transparent", freq=FALSE,
     xlim=c(x.min,x.max), ylim=c(0,max.y),
     axes=FALSE, ann=FALSE, xlab=NULL, ylab=NULL, main=NULL, ...)

  # axis, axis ticks
  if (!y_axis)
    .axes(x.lvl=NULL, y.lvl=NULL, axTicks(1), NULL,
          rotate_x=rotate_x, rotate_y=rotate_y, offset=offset, ...)
  else
    .axes(x.lvl=NULL, y.lvl=NULL, axTicks(1), axTicks(2),
          rotate_x=rotate_x, rotate_y=rotate_y, offset=offset, ...)

  # axis value labels
  if (!y_axis) y.lab="" else y.lab="Density"
  max.lbl <- max(nchar(axTicks(2)))
  .axlabs(x.lab, y.lab=NULL, main.lab, sub.lab, max.lbl, 
          xy_ticks=TRUE, offset=offset, ...) 

  # colored background for plotting area
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border=col.box,
    lwd=getOption("panel_lwd"), lty=getOption("panel_lty"))
  
  # plot the histogram
  if (histogram)
    plot(h, add=TRUE, freq=FALSE, col=col_fill, border="transparent")

  # plot the normal curve
  if (type == "normal" || type == "both") {
    if (col_fill_nrm == "transparent") lw <- 1.35 else lw <- 1
    polygon(c(x.min,xx,x.max), c(0,d.nrm,0), col=col_fill_nrm, 
            border=col.nrm, lwd=lw)
  }

  # plot the general curve
  if (type == "general" || type == "both") {
    lw <- ifelse (col_fill_gen == "transparent", 1.35, 1)
    polygon(d.gen, col=col_fill_gen, border=col.gen, lwd=lw)
  }

  if (band) rug(x, col=color_rug, lwd=size.rug)
 
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

  # text output
  tx=""
  if (!quiet) {

    tx <- character(length = 0)

    tx[length(tx)+1] <- paste("Sample Size: ", n)
    tx[length(tx)+1] <- paste("Missing Values: ", n.miss)
 
    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste("Density bandwidth for general curve: ",
      .fmt(d.gen$bw,4), sep="")
    tx[length(tx)+1] <- "For a smoother curve, increase bandwidth with option: bw"

    tx[length(tx)+1] <- ""
    W <- NA; p.val <- NA
    if (type == "normal" || type == "both") {
      digits_d <- 3
      if (n > 2 && n < 5000) {
        nrm <- shapiro.test(x)
        W <- .fmt(nrm$statistic,min(4,digits_d+1))
        p.val <- .fmt(nrm$p.value,min(4,digits_d+1))
        tx[length(tx)+1] <- paste("Null hypothesis is a normal population")
        tx[length(tx)+1] <- paste(nrm$method, ":  W = ", W, ",  p-value = ",
          p.val, sep="")
      }
      else 
        tx[length(tx)+1] <- "Sample size out of range for Shapiro-Wilk normality test."
    }

  return(list(tx=tx, bw=d.gen$bw, n=n, n.miss=n.miss, W=W, pvalue=p.val))

  }

} 
