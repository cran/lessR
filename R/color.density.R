color.density <- 
function(x, col.nrm="darkblue", col.gen="blue", col.bg="ghostwhite",  
         col.grid="grey90", col.hist="grey86", 
         col.fill.nrm=rgb(80,150,200, alpha=70, max=255), 
         col.fill.gen=rgb(250,210,230, alpha=70, max=255),
         bin.start=NULL, bin.width=NULL, text.out=TRUE,
         type=c("both", "general", "normal"), x.pt=NULL,
         xlab=NULL, main=NULL, ...) {

  # produce actual argument, such as from an abbreviation, and flag if not exist
  type <- match.arg(type)
  
  if (type != "general"  &&  !is.null(x.pt)) { 
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        "x.pt only valid when type is equal to \"general\".\n\n")
  }

  # set the labels  
  if (is.null(xlab)) x.lbl <- deparse(substitute(x)) else x.lbl <- xlab
  if (is.null(main)) main.lbl <- "" else main.lbl <- main
      
  # get breaks from user supplied bin width and/or supplied start value
  # otherwise, breaks="Sturges by default
  # copied from color.hist
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
  abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)

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
    if (text.out)
      cat("\nDensity bandwidth for general curve: ", round(d.gen$bw,4), sep="", "\n")
  }
  
  # plot the optional bar about a chosen point for general curve only
  if (!is.null(x.pt)  &&  type == "general") {
    d <- d.gen 
    y.pt <- d$y[which.min(abs(x.pt-d$x))]
    xbeg <- x.pt - 0.5
    xend <- x.pt + 0.5
    xsub <- d$x[d$x > xbeg & d$x < xend]
    ysub <- d$y[d$x > xbeg & d$x < xend]
    txt.ypt <- toString(round(y.pt, 3))
    txt <- paste("Density =", txt.ypt, "at x =", toString(round(x.pt, 2)))
    title(main=txt)
    polygon(c(xbeg, xsub, xend), c(0, ysub, 0), col="lightsteelblue")
    if (min(x) > 0) left <- -2*min(x) else left <- 2*min(x) 
    lines(c(left,x.pt), c(y.pt,y.pt), col="darkblue", lwd = 0.5)
    lines(c(x.pt,x.pt), c(0,y.pt), col="darkblue", lwd = 0.5)
  }

  cat("\n")
}
