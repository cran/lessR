.den.main <- 
function(x, data, 
         bw, type, bin.start, bin.width, quiet,
         col.fill, col.bg, col.grid, col.nrm, col.gen,
         col.fill.nrm, col.fill.gen,
         cex.axis, col.axis, col.ticks,
         x.pt, xlab, main, y.axis, x.min, x.max, band, ...)  {

  if (!is.null(x.pt)) {
    y.axis <- TRUE
    type <- "general"
  }

  # get variable labels if exist plus axes labels
  gl <- .getlabels(xlab, main=main)
  x.name <- gl$xn; x.lbl <- gl$xl; x.lab <- gl$xb
  main.lab <- gl$mb

  # get breaks from user supplied bin width and/or supplied start value
  # otherwise, breaks="Sturges by default
  # copied from histogram
  if (!is.null(bin.width)  || !is.null(bin.start)) {
    if (is.null(bin.start)) bin.start <- pretty(min(x):max(x))[1]
    if (is.null(bin.width)) {
      h <- hist(x, plot=FALSE, breaks="Sturges")
      bin.width <- h$breaks[2]-h$breaks[1]
    }
    seq.end <- max(x)
    breaks <- seq(bin.start,seq.end,bin.width)
    while (max(breaks) < max(x)) {
      seq.end <- seq.end + bin.width
      breaks <- seq(bin.start,seq.end,bin.width)
    }
  }
  else breaks="Sturges"


  # histogram calculations, no plot
  h <- hist(x, plot=FALSE, breaks)

  n <- sum(!is.na(x))
  n.miss <- sum(is.na(x))
  if (n.miss > 0) x <- na.omit(x)
 
  # general density curve, no plot
  # suppress warnings about possible graphic parameters
  d.gen <- suppressWarnings(density(x, bw, ...))
  
  mx <- mean(x)

  # min and max x coordinates for graph, make symmetric
  min.dev.x <- min(d.gen$x) - mx
  max.dev.x <- max(d.gen$x) - mx
  if (abs(min.dev.x) > abs(max.dev.x)) {
    if (is.null(x.min)) x.min <- min(d.gen$x)
    if (is.null(x.max)) x.max <- mx + abs(min.dev.x)
  }
  if (abs(max.dev.x) >= abs(min.dev.x)) {
    if (is.null(x.min)) x.min <- mx - abs(max.dev.x)
    if (is.null(x.max)) x.max <- max(d.gen$x)
  }
  
  # normal density curve, no plot
  xx <- seq(x.min, x.max, length=200)
  if (col.fill.nrm == "transparent") lw <- 2 else lw <- 1
  d.nrm <- dnorm(xx,mean(x),sd(x))

  # max y coordinate for graph
  max.y <- max(max(d.nrm), max(d.gen$y), max(h$density))
  
  # set up plot area
  # bw if specified also gets passed to plot, so suppress warning
  if (!y.axis) y.lab="" else y.lab="Density"
  suppressWarnings(plot(h, border="transparent", freq=FALSE, xlab=x.lab,
     ylab=y.lab, main=main.lab, xlim=c(x.min,x.max), ylim=c(0,max.y), 
     axes=FALSE, ...))
  axis(1, cex.axis=cex.axis, col.axis=col.axis, col.ticks=col.ticks)
  if (y.axis) 
    axis(2, cex.axis=cex.axis, col.axis=col.axis, col.ticks=col.ticks)
  
  # colored background for plotting area
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="black")
  
  # plot the histogram
  plot(h, add=TRUE, freq=FALSE, col=col.fill, border="transparent")

  # plot the normal curve
  if (type == "normal" || type == "both") {
    if (col.fill.nrm == "transparent") lw <- 1.35 else lw <- 1
    polygon(c(x.min,xx,x.max), c(0,d.nrm,0), col=col.fill.nrm, 
            border=col.nrm, lwd=lw)
  }

  # plot the general curve
  if (type == "general" || type == "both") {
    if (col.fill.gen == "transparent") lw <- 1.35 else lw <- 1
    polygon(d.gen, col=col.fill.gen, border=col.gen, lwd=lw)
  }

  if (band) rug(x, col="gray70")
 
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
  if (!quiet) {
 
    # summarize data
    .ss.numeric(x, brief=TRUE)

    cat("\nDensity bandwidth for general curve: ", .fmt(d.gen$bw,4), sep="", "\n")
    cat("For a smoother curve, increase bandwidth with option: bw\n")

    cat("\nSample Size: ", n, "\n")
    cat("Missing Values: ", n.miss, "\n")

    digits.d <- 3
    if (n > 2 && n < 5000) {
      nrm <- shapiro.test(x)
      W <- .fmt(nrm$statistic,min(4,digits.d+1))
      p.val <- .fmt(nrm$p.value,min(4,digits.d+1))
      cat("\nNull hypothesis is a normal population\n")
      cat(nrm$method, ":  W = ", W, ",  p-value = ", p.val, sep="", "\n")
    }
    else
      cat("Sample size out of range for Shapiro-Wilk normality test.", "\n")
  }

  cat("\n")

  return(d.gen)

} 
