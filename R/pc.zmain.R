.pc.main <- 
function(x, y,
         random.col, col.fill, col.low, col.hi, colors,
         radius, hole, hole.fill, edges, 
         clockwise, init.angle, 
         density, angle, border, lty,
         cex, cex.main, quiet, main,
         pdf.file, width, height, ...)  {

  # get values for ... parameter values
  #stuff <- .getdots(...)
  #col.main <- stuff$col.main

  # set the labels
  # use variable label for main if it exists and main not specified
  gl <- .getlabels(main=main, lab.cex=getOption("lab.cex"))
  x.name <- gl$xn; x.lbl <- gl$xl
  lab.cex <- gl$lab.cex

  if (!is.null(main)) main.lbl <- main
  else if (length(x.lbl) == 0) main.lbl <- x.name else main.lbl <- x.lbl

  lab.cex <- 0.85
  if (strwidth(main.lbl, units="figure", cex=lab.cex) > .85) {
    brk <- nchar(main.lbl)
    while (strwidth(substr(main.lbl,1,brk), units="figure", cex=lab.cex) > .85)
      brk <- brk-1 
    while (substr(main.lbl,brk,brk) != " ") brk <- brk-1
    main.lbl <- paste(substr(main.lbl,1,brk), "\n",
                      substr(main.lbl,brk+1,nchar(main.lbl)))
    while (strwidth(main.lbl, units="figure", cex=lab.cex) > .85)
      lab.cex <- lab.cex-0.05
  }

  # entered counts typically integers as entered but stored as type double
  # if names(x) is null, likely data from sample and c functions
  if (!is.integer(x) && is.double(x) && !is.null(names(x)))  x <- as.table(x)
  if (!is.factor(x) && !is.table(x)) x <- factor(x)
  n.colors <- ifelse (!is.table(x), nlevels(x), length(x))

 
  # -------------
  # color palette
  # -------------

  # set some default colors in case not assigned below
  if (is.ordered(x)) {
    lowhi <- .ordcolors(colors, col.low, col.hi) 
    col.low <- lowhi$col.low
    col.hi <- lowhi$col.hi

    color.palette <- colorRampPalette(c(col.low, col.hi))
    clr <- color.palette(n.colors)
  } # end ordered

  else if (colors %in% c("lightbronze", "gray", "white")) {
    if (n.colors == 1 || length(col.fill) > 1)
      clr <- col.fill
    else {  # ordered grays
      if (n.colors == 2) { light <- "gray70"; dark <- "gray40" }
      else { light <- "gray84"; dark <- "gray30" }
      color.palette <- colorRampPalette(c(dark, light))
      clr <- color.palette(n.colors)
    }
  }

  else if (colors == "rainbow") clr <- rainbow(n.colors)
  else if (colors == "terrain") clr <- terrain.colors(n.colors)
  else if (colors == "heat") clr <- heat.colors(n.colors)

  else  {  # ordered color range not applicable here 
    if (length(col.fill) > 1)
      clr <- col.fill
    else
      if (n.colors > 1) clr <- .col.discrete("hcl")[1:n.colors]
  }

  if (!is.null(col.fill)) {
    for (i in 1:(min(length(col.fill),length(clr)))) clr[i] <- col.fill[i]
    n.colors <- min(length(col.fill),length(clr))
  }

  palette(clr)
  col <- 1:n.colors 


  # ----------------
  # prepare the data
  # ----------------

  if (is.null(y)) {  # x is categorical variable, convert to freq table
    if (!is.table(x)) x <- table(x)
  }
  else {  # y contains the values, x the names (categories)
    x.names <- x
    x <- y
    x <- as.table(x)
    names(x) <- x.names 
  }

  x.tbl <- x
  labels <- names(x)
  x <- as.numeric(x)


  # ------------------
  # plot the pie chart
  # ------------------

  # pie(x, col=col, main=main.lbl, cex=cex, cex.main=cex.main, col.main=col.main, ...)
  # modified pie function to add inner hole at the end, and plot two radiuses
  if (!is.numeric(x) || any(is.na(x) | x < 0)) 
      stop("'x' values must be positive.")

  if (is.null(labels)) 
    labels <- as.character(seq_along(x))
  else
    labels <- as.graphicsAnnot(labels)

  x <- c(0, cumsum(x)/sum(x))
  dx <- diff(x)
  nx <- length(dx)

  par(bg=getOption("panel.fill"))
  par(mai=c(.4, .5, .8, .5))
  plot.new()

  pin <- par("pin")  # plot dimensions in inches
  xlim <- c(-1, 1)
  ylim <- c(-1, 1)
  if (pin[1L] > pin[2L]) 
    xlim <- (pin[1L]/pin[2L]) * xlim
  else
    ylim <- (pin[2L]/pin[1L]) * ylim
  plot.window(xlim, ylim, "", asp=1)

  if (length(border) < nx) border <- rep_len(border, nx)
  if (length(lty) < nx) lty <- rep_len(lty, nx)
  angle <- rep(angle, nx)
  if (!is.null(density))
    if (length(density) < nx) density <- rep_len(density, nx)

  # get coordinates of a circle with specified radius
  twopi <- ifelse (clockwise, -2*pi, 2*pi)
  t2xy <- function(t, radius) {
      t2p <- twopi * t + init.angle * pi/180
      list(x = radius * cos(t2p), y = radius * sin(t2p))
  }

  # construct plot slice by slice
  for (i in 1L:nx) { 

    # plot slice
    n <- max(2, floor(edges * dx[i]))
    P <- t2xy(seq.int(x[i], x[i + 1], length.out=n), radius)
    polygon(c(P$x, 0), c(P$y, 0), density=density[i], angle=angle[i], 
        border=border[i], col=col[i], lty=lty[i])

    # plot label
    lab <- as.character(labels[i])
    P <- t2xy(mean(x[i + 0:1]), radius)
    if (!is.na(lab) && nzchar(lab)) {
        lines(c(1, 1.05) * P$x, c(1, 1.05) * P$y)
        text(1.1 * P$x, 1.1 * P$y, labels[i], xpd=TRUE, 
            adj=ifelse(P$x < 0, 1, 0), ...)
    }

  }  # end slice by slice

  # add centered hole over the top of the pie
  P <- t2xy(seq.int(0, 1, length.out=n*nx), hole)
  polygon(P$x, P$y, col=hole.fill, border=border)

  title(main=main.lbl, col.main=getOption("main.color"),
        line=par("mgp")[1]-.5, ...)

# legend("bottom", legend=unique(na.omit(x)), horiz=TRUE, cex=0.8, fill=col)


  # -----------
  # text output
  # -----------

  #if (length(dim(x)) == 1  && !quiet) {  # one variable
  if (!quiet) { 

    txsug <- ""
    if (getOption("suggest")) {
      txsug <- ">>> Suggestions"
        fc <- paste("PieChart(", x.name,
                    ", hole=0)  # traditional pie chart", sep="")
        txsug <- paste(txsug, "\n", fc, sep="")
        fc <- paste("BarChart(", x.name, ")  # bar chart", sep="")
        txsug <- paste(txsug, "\n", fc, sep="")
        fc <- paste("Plot(", x.name, ")  # bubble plot", sep="")
        txsug <- paste(txsug, "\n", fc, sep="")
        fc <- paste("Plot(", x.name,
                    ", values=\"count\")  # lollipop plot", sep="")
        txsug <- paste(txsug, "\n", fc, sep="")
    }
    class(txsug) <- "out_piece"

    if (.is.integer(x.tbl)) {
      stats <- .ss.factor(x.tbl, brief=TRUE, x.name=x.name)
      txttl <- stats$title
      counts <- stats$counts
      chi <- stats$chi
      class(txttl) <- "out_piece"
      class(counts) <- "out_piece"
      class(chi) <- "out_piece"
      output <- list(out_suggest=txsug, out_title=txttl,
                     out_counts=counts, out_chi=chi)
    }
    else {
      stats <- .ss.numeric(x.tbl, brief=TRUE, x.name=getOption("yname"))
      txout <- stats$tx
      output <- list(out_suggest=txsug, out_stats=txout)
    }

    class(output) <- "out_all"
    print(output)      
  }

 
  cat("\n")

}  #  end pc.main
