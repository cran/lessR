.plt.main <- 
function(x, y, by, dframe, type, n.cat,
         col.line, col.area, col.box, col.pts, col.fill,
         trans.pts, shape.pts, col.grid, col.bg, colors, 
         cex.axis, col.axis, col.ticks, 
         xy.ticks, xlab, ylab, main, cex,
         x.start, x.end, y.start, y.end, kind,
         fit.line, col.fit.line,
         col.bubble, bubble.size, col.flower,
         ellipse, col.ellipse, fill.ellipse, text.out, ...) {

  if (!is.null(type)) if (type != "p" && type != "l" && type != "b") { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Option 'type' can only be \"p\" for points,\n",
        "  \"l\" for line or \"b\" for both.\n\n")
  }

  nrows <- length(x)
  if (is.null(cex)) pt.size <- 0.8 else pt.size <- cex

  # color palette based on color theme colors
  cp <- .clr(colors, trans.pts)
  if (is.null(col.pts)) col.pts <- cp[5]
  if (is.null(col.fill)) col.fill <- cp[6]
  if (is.null(col.line)) col.line <- cp[5]
  if (is.null(col.bubble)) col.bubble <- cp[1]
  if (is.null(col.flower)) col.flower <- cp[2]
  if (is.null(col.grid)) col.grid <- cp[3]
  if (is.null(col.bg)) col.bg <- cp[4]
  col.symbol <- cp[2]

  if (is.null(col.fill)) col.fill <- "transparent"
  if (is.null(col.area)) col.area <- "transparent"

  # get variable labels if exist plus axes labels
  gl <- .getlabels(xlab, ylab, main)
  x.name <- gl$xn; x.lbl <- gl$xl; x.lab <- gl$xb
  y.name <- gl$yn; y.lbl <- gl$yl; y.lab <- gl$yb;
  main.lab <- gl$mb
  by.name <- getOption("byname")
   
  nu <- length(unique(na.omit(x)))
  if (is.numeric(x) && nu <= n.cat) {
    x <- as.factor(x)
    cat("\n")
    cat(">>> Variable is numeric, but only has", nu, "<= n.cat =", n.cat, "levels,",
        "so treat as categorical.\n",
        "   To treat as numeric, decrease  n.cat  to specify a",
        "lower number of unique values.\n",
        "   Suggest making this variable a factor with R factor function.\n")
   }

  if (!is.factor(x)) {
    if (is.null(type)) {  # if x is sorted with equal intervals, plot a line chart
      if (sum(is.na(x)) > 0) equal.int <- FALSE  # missing data in x present
      else {
        diff.x <- diff(x)
        for (i in 2:(length(x)-1)) 
          if (diff.x[i-1] != diff.x[i]) equal.int <- FALSE else equal.int <- TRUE
        rm(diff.x)
      }
      if (!is.unsorted(x) && equal.int  && sum(is.na(y))==0)  # also no y missing 
        type <- "l" else type <- "p"
    }
    if (kind == "default")  # set default
      if ( length(x)>10 && length(y)>10 && length(unique(x))<10 && length(unique(y))<10 )
        kind <- "bubble"
      else kind <- "regular"
    }
  else {  # x is a factor
    type <- "p"
    kind <- "xcat"
  }

  if ((kind == "bubble") || (kind == "sunflower")) {  
    if (is.null(x.start)) x.start=min(x, na.rm=TRUE)  # x.start, x.min for bubble plot
    if (is.null(x.end)) x.end=max(x, na.rm=TRUE)
    if (is.null(y.start)) y.start=min(y, na.rm=TRUE) 
    if (is.null(y.end)) y.end=max(y, na.rm=TRUE)
  }

  
  digits.d <- .max.dd(y) + 1
  options(digits.d=digits.d)

  # -------------------------
  # plot
  # -------------------------

  # plot setup
  if (kind == "regular") {

    if (!is.null(by)) par(omi=c(0,0,0,0.6))  # legend in right margin

    suppressWarnings(plot(x, y, type="n", axes=FALSE, xlab=x.lab, ylab=y.lab, 
             main=main.lab, ...))
    if (xy.ticks){
      suppressWarnings(axis(1, cex.axis=cex.axis, col.axis=col.axis, 
                         col.ticks=col.ticks, ...))
      suppressWarnings(axis(2, cex.axis=cex.axis, col.axis=col.axis, 
                       col.ticks=col.ticks, ...))
    }
  }

  else if (kind == "xcat") {
    plot.default(y ~ x, xlim=c(.5,nlevels(x)+.5), type="n", axes=FALSE, 
                 main=main.lab, xlab=x.lab, ylab=y.lab)
    axis(2, cex.axis=cex.axis, col.axis=col.axis, col.ticks=col.ticks)
    axis(1, labels=levels(x), at=1:nlevels(x), 
            cex.axis=cex.axis, col.axis=col.axis, col.ticks=col.ticks)
  }

  else if ((kind == "bubble") || (kind == "sunflower")) {
    mytbl <- table(x, y)  # get the counts
    x.lo <- x.start-.5
    x.hi <- x.end+.5
    y.lo <- y.start-.5
    y.hi <- y.end+.5

    # melt the table to a data frame
    k <- 0
    xx <- integer(length=0)
    yy <- integer(length=0)
    count <- integer(length=0)
    for (i in 1:nrow(mytbl)) {
      for (j in 1:ncol(mytbl)) {
        k <- k + 1
        count[k] <- mytbl[i,j]
        xx[k] <- as.integer(rownames(mytbl)[i])
        yy[k] <- as.integer(colnames(mytbl)[j])
      }
    }
    cords <- data.frame(xx, yy, count)
    # bubble plot
    if (kind == "bubble"  ||  kind == "sunflower")
      plot(x,y, type="n", xlab=x.lab, ylab=y.lab, main=main.lab,
           xlim=c(x.lo,x.hi), ylim=c(y.lo,y.hi), cex.axis=cex.axis, 
           col.axis=col.axis)
  }

  else {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "This type of plot not recognized: ", kind, "\n\n")
  }


  # colored plotting area
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border=col.box)

  # grid lines
  vx <- pretty(c(usr[1],usr[2]))
  vy <- pretty(c(usr[3],usr[4]))
  abline(v=seq(vx[1],vx[length(vx)],vx[2]-vx[1]), col=col.grid, lwd=.5)
  abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)

  # fill area under curve
  if (type != "p") col.border <- col.line else col.border <- "transparent"
    if (!is.null(col.area)) 
      polygon(c(x[1],x,x[length(x)]), c(min(y),y,min(y)),
              col=col.area, border=col.border)

  # the plot
  if (kind == "regular") {  # plot lines and/or points
    if (type == "l" | type == "b") {
      lines(as.numeric(x),y, col=col.line, ...)
    }
    if (type == "p" | type == "b") {

      if (is.null(by)) 
        suppressWarnings(points(x,y, pch=shape.pts, col=col.pts, bg=col.fill, 
                         cex=pt.size, ...))

      else {  # by grouping variable
        n.levels <- nlevels(by)

        clr <- character(length(n.levels))
        if (length(col.pts) == 1) 
          for (i in 1:n.levels) clr[i] <- col.pts
        else
          clr <- col.pts
        clr.tr <- clr

        shp <- integer(length(n.levels))
        if (length(shape.pts) == 1)
          for (i in 1:n.levels) shp[i] <- shape.pts
        else
           shp <- shape.pts
        shape.dft <- c(21,23,22,24,25,7:14)  # shape defaults
        if (length(col.pts)==1 && length(shape.pts)==1)  # both shape and color default
          for (i in 1:n.levels) shp[i] <- shape.dft[i]  # fill with default shapes

        for (i in 1:n.levels) {
          clr.tr[i] <- .maketrans(clr.tr[i], trans.pts)
          x.lv <- subset(x, by==levels(by)[i])
          y.lv <- subset(y, by==levels(by)[i])
          points(x.lv, y.lv, pch=shp[i], col=clr[i], bg=clr.tr[i], 
                 cex=pt.size, lwd=0.75, ...)
        }
        cat("\nTransparency level for plotted points: ", trans.pts, "\n\n")

      .plt.legend(levels(by), col.pts, clr, clr.tr, shp, trans.pts, col.bg, usr)

      }  # end by group

      if (ellipse) {
        n.del <- sum(is.na(x - y))  # number missing
        if (n.del == 0)
          dataEllipse(x, y, col=col.ellipse, levels=.95, lwd=1.5, 
          fill=fill.ellipse, fill.alpha=.06, center.cex=0, segments=100,
          plot.points=FALSE)  # car function
        else
          cat("\n>>>Note: Ellipse function does not work with missing data.\n")
      }
    }
  }  # kind = "regular"

  else if (kind == "xcat") {  # plot means
    for (i in (1:nlevels(x))) {
      m.lvl <- mean(y[x==levels(x)[i]], na.rm=TRUE)
      abline(h=m.lvl, col="gray75")
      points(rep(i,length(y[x==levels(x)[i]])), y[x==levels(x)[i]], col=col.pts)
      points(i, m.lvl, pch=23, bg=col.symbol)
    }
  }

  else if (kind == "bubble") {
    symbols(cords$xx, cords$yy, circles=cords$count, bg=col.bubble, 
            inches=bubble.size, add=TRUE, ...)
    zeros <- cords[cords$count==0, ] # 0 plots to a single pixel, so remove
    points(zeros$xx, zeros$yy, col=col.bg, bg=col.bg, pch=21, cex=.5)
    if (ellipse) {
      dataEllipse(x, y, col=col.ellipse, levels=.95, lwd=1.5, fill=fill.ellipse, 
        fill.alpha=.06, center.cex=0, segments=100, plot.points=FALSE)
    }
  }

  else if (kind == "sunflower") {
    sunflowerplot(cords$xx, cords$yy, number=cords$count, 
      seg.col=col.flower, col=col.flower, cex.axis=cex.axis, col.axis=col.axis,
      xlab=x.lab, ylab=y.lab, xlim=c(x.lo,x.hi), ylim=c(x.lo,x.hi))
  }

  # fit line option
  if (fit.line != "none") {  
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) {
      x.ok <- x[ok]
      y.ok <- y[ok]
      ord <- order(x.ok)
      x.ord <- x.ok[ord]
      y.ord <- y.ok[ord] 
      if (fit.line == "loess") {
        lines(x.ord, fitted(loess(y.ord~x.ord, ...)), col=col.fit.line)
      }
      if (fit.line == "ls") {
        if(!is.factor(x)) {
          model <- lm(y.ord ~ x.ord)
          abline(model$coef, col=col.fit.line)
        }
        else cat("\n>>> Note: Least squares line not permitted for a factor.\n")
      }
    }
  }

  # correlation
  if (text.out) {
    if (!is.null(y)  &&  !is.factor(x)  &&  type == "p") {
      n.pair <- sum(!is.na(x - y))  # number of points after listwise deletion
      n.del <- sum(is.na(x - y))  # number of pairwise deleted observations
      cat("\n")
      cat("Number of paired values with neither missing, n:", n.pair, "\n")
      cat("Number of observations (rows of data) deleted:", n.del, "\n")
      .cr.default(x, y, brief=TRUE)
    }
    if (!is.null(y) && is.factor(x)) {
      options(yname = x.name)
      options(xname = y.name)
      .ss.numeric(y, by=x, dframe=dframe, digits.d=digits.d, brief=TRUE)
    }
  }       

  cat("\n")

}  # end plt.main

