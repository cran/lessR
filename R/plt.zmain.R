.plt.main <- 
function(x, y, by, data, type, n.cat,
         col.fill, col.stroke, col.bg, col.grid,
         shape.pts, col.area, col.box, 
         cex.axis, col.axis,
         xy.ticks, xlab, ylab, main, cex, kind,
         fit.line, col.fit.line, bubble.size,
         ellipse, col.ellipse, fill.ellipse,
         diag, col.diag, lines.diag, quiet, ...) {

  if (!is.null(type)) if (type != "p" && type != "l" && type != "b") { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Option 'type' can only be \"p\" for points,\n",
        "  \"l\" for line or \"b\" for both.\n\n")
  }

  nrows <- length(x)
  pt.sz <- 0.8
  if (.Platform$OS == "windows") pt.sz <- 1
  if (is.null(cex)) pt.size <- pt.sz else pt.size <- cex

  if (is.null(col.fill)) col.fill <- "transparent"
  if (is.null(col.area)) col.area <- "transparent"

  # get variable labels if exist plus axes labels
  gl <- .getlabels(xlab, ylab, main)
  x.name <- gl$xn; x.lbl <- gl$xl; x.lab <- gl$xb
  y.name <- gl$yn; y.lbl <- gl$yl; y.lab <- gl$yb
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
        "   Suggest making this variable a factor with the R factor function.\n")
   }

  if (!is.factor(x)) {
    if (is.null(type)) {  # if x is sorted with equal intervals, plot a line chart
      if (sum(is.na(x)) > 0) equal.int <- FALSE  # missing data in x present
      else {
        diff.x <- diff(x)
        for (i in 2:(length(x)-1)) 
          if ((abs(diff.x[i-1] - diff.x[i]) > 0.0000000001)) 
            equal.int <- FALSE
          else
            equal.int <- TRUE
        rm(diff.x)
      }
      if (!is.unsorted(x) && equal.int  && sum(is.na(y))==0)  # also no y missing 
        type <- "l" else type <- "p"
    }
    if (kind == "default")  # set default
      if (length(x)>10 && length(y)>10 && length(unique(x))<10 && length(unique(y))<10)
        kind <- "bubble"
      else kind <- "regular"
    }
  else {  # x is a factor
    type <- "p"
    kind <- "xcat"
  }

  if ((kind == "bubble") || (kind == "sunflower")) {  

    x.start=min(x, na.rm=TRUE)  # for bubble plot
    x.end=max(x, na.rm=TRUE)
    y.start=min(y, na.rm=TRUE) 
    y.end=max(y, na.rm=TRUE)

    dots <- list(...)  # if xlim or ylim present, use these values
    if (length(dots) > 0) {
      for (i in 1:length(dots)) {
        if (names(dots)[i] == "xlim") {
          x.start <- dots[[i]][1]
          x.end <- dots[[i]][2]
        }
        if (names(dots)[i] == "ylim") {
          y.start <- dots[[i]][1]
          y.end <- dots[[i]][2]
        }
      }
    }
  }

  
  digits.d <- .max.dd(y) + 1
  options(digits.d=digits.d)

  # -------------------------
  # plot
  # -------------------------

  # plot setup
  if (kind == "regular") {

    if (!is.null(by)) par(omi=c(0,0,0,0.6))  # legend in right margin

    if (diag) {
      nums <- pretty(c(min(x,y),max(x,y)))
      l1 <- nums[1]
      l2 <- nums[length(nums)]
    }

    # non-graphical parameters in ... generate warnings when no plot
    if (!diag)
      plot(x, y, type="n", axes=FALSE, xlab=x.lab, ylab=y.lab, 
             main=main.lab, ...)
    else
      plot(x, y, type="n", axes=FALSE, xlab=x.lab, ylab=y.lab, 
             main=main.lab, xlim=c(l1,l2), ylim=c(l1,l2), ...)
    if (xy.ticks){
      axis(1, cex.axis=cex.axis, col.axis=col.axis, ...)
      axis(2, cex.axis=cex.axis, col.axis=col.axis, ...)
    }
  }

  else if (kind == "xcat") {
    plot.default(y ~ x, xlim=c(.5,nlevels(x)+.5), type="n", axes=FALSE, 
                xlab=x.lab, ylab=y.lab, main=main.lab, ...)
    axis(2, cex.axis=cex.axis, col.axis=col.axis, ...)
    axis(1, labels=levels(x), at=1:nlevels(x), 
            cex.axis=cex.axis, col.axis=col.axis, ...)
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

  # diagonal line
  if (diag) {
    segments(usr[1], usr[3], usr[2], usr[4], col=col.diag)
    if (lines.diag)
      for (i in 1:length(x)) segments(x[i],x[i], x[i], y[i])
    }

  # grid lines
  vx <- pretty(c(usr[1],usr[2]))
  vy <- pretty(c(usr[3],usr[4]))
  abline(v=seq(vx[1],vx[length(vx)],vx[2]-vx[1]), col=col.grid, lwd=.5)
  abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)

  # fill area under curve
  if (type != "p") col.border <- col.stroke else col.border <- "transparent"
    if (!is.null(col.area)) 
      polygon(c(x[1],x,x[length(x)]), c(min(y),y,min(y)),
              col=col.area, border=col.border)

  # the plot
  if (kind == "regular") {  # plot lines and/or points
    if (type == "l" | type == "b") {
      lines(as.numeric(x),y, col=col.stroke, ...)
    }
    if (type == "p" | type == "b") {

      if (is.null(by)) { 
        trans.pts <- getOption("trans.fill.pt")
        clr.trn <- .maketrans(col.fill, (1-trans.pts)*256)
        points(x,y, pch=shape.pts, col=col.stroke,
            bg=clr.trn, cex=pt.size, ...)
      }

      else {  # by grouping variable
        n.levels <- nlevels(by)

        clr <- character(length(n.levels))
        if (length(col.stroke) == 1) 
          for (i in 1:n.levels) clr[i] <- col.stroke
        else
          clr <- col.stroke
        clr.tr <- clr

        shp <- integer(length(n.levels))
        if (length(shape.pts) == 1)
          for (i in 1:n.levels) shp[i] <- shape.pts
        else
          shp <- shape.pts
        shape.dft <- c(21,23,22,24,25,7:14)  # shape defaults
        if (length(col.stroke)==1 && length(shape.pts)==1)  # both shape and color default
          for (i in 1:n.levels) shp[i] <- shape.dft[i]  # fill with default shapes

        trans.pts <- getOption("trans.fill.pt")
        for (i in 1:n.levels) {
          clr.tr[i] <- .maketrans(clr.tr[i], (1-trans.pts)*256)
          x.lv <- subset(x, by==levels(by)[i])
          y.lv <- subset(y, by==levels(by)[i])
          points(x.lv, y.lv, pch=shp[i], col=clr[i],
              bg=clr.tr[i], cex=pt.size, lwd=0.75, ...)
        }

      if (length(col.stroke) > 1) clr.tr <- clr
      .plt.legend(levels(by), col.stroke, clr, clr.tr, shp, trans.pts, col.bg, usr)

      }  # end by group
    }
  }  # kind = "regular"

  else if (kind == "xcat") {  # plot means
    for (i in (1:nlevels(x))) {
      m.lvl <- mean(y[x==levels(x)[i]], na.rm=TRUE)
      abline(h=m.lvl, col="gray75")
      points(rep(i,length(y[x==levels(x)[i]])), y[x==levels(x)[i]],
             pch=21, col=col.stroke, bg=col.fill)
      points(i, m.lvl, pch=23, bg=getOption("col.stroke"))
    }
  }

  else if (kind == "bubble") {
    symbols(cords$xx, cords$yy, circles=cords$count, bg=col.fill, 
            fg=col.stroke, inches=bubble.size, add=TRUE, ...)
    zeros <- cords[cords$count==0, ] # 0 plots to a single pixel, so remove
    points(zeros$xx, zeros$yy, col=col.bg, bg=col.bg, pch=21, cex=.5)
  }

  else if (kind == "sunflower") {
    sunflowerplot(cords$xx, cords$yy, number=cords$count, 
      seg.col=col.stroke, col=col.fill, cex.axis=cex.axis, col.axis=col.axis,
      xlab=x.lab, ylab=y.lab, xlim=c(x.lo,x.hi), ylim=c(x.lo,x.hi), add=TRUE)
  }

  if (ellipse) {  # car function
    n.pair <- sum(!is.na(x - y))  # number of points after listwise deletion
    n.del <- sum(is.na(x - y))  # number of pairwise deleted observations
    if (n.del != 0) {  # ellipse function cannot have missing data
      elp.dat <- na.omit(data.frame(x,y))
      x <- elp.dat$x
      y <- elp.dat$y 
    }
    dataEllipse(x, y, col=col.ellipse, levels=.95, lwd=1.5, fill=fill.ellipse,
                fill.alpha=.06, center.cex=0, segments=100, plot.points=FALSE)
    txt <- "[Ellipse with Monette, Fox, and Friendly's function dataEllipse"
    cat(txt, "from the car package]\n") 
  }


  # fit line option
  if (fit.line != "none") { 

    if (is.null(by)) {
      n.iter <- 1
    }
    else {
      n.iter <- n.levels
    }

    for (i in 1:n.iter) {

      if (is.null(by)) {
        x.lv <- x
        y.lv <- y
        clr <- col.fit.line
      }
      else {
        x.lv <- subset(x, by==levels(by)[i])
        y.lv <- subset(y, by==levels(by)[i])
        clr <- character(length(n.levels))
        if (length(col.stroke) == 1) 
          for (i in 1:n.levels) clr[i] <- col.stroke
        else
          clr <- col.stroke[i]
      }
      ok <- is.finite(x.lv) & is.finite(y.lv)
      if (any(ok)) {
        x.ok <- x.lv[ok]
        y.ok <- y.lv[ok]
        ord <- order(x.ok)
        x.ord <- x.ok[ord]
        y.ord <- y.ok[ord] 
        if (fit.line == "loess") {
          lines(x.ord, fitted(loess(y.ord~x.ord, ...)), col=clr, lwd=1.5)
        }
        if (fit.line == "ls") {
          if(!is.factor(x.lv)) {
            model <- lm(y.ord ~ x.ord)
            abline(model$coef, col=clr, lwd=1.5, xpd=FALSE)
          }
          else cat("\n>>> Note: Least squares line not permitted for a factor.\n")
        }
      }
    }
  }

  # correlation
  if (!quiet) {
    if (!is.null(y)  &&  !is.factor(x)  &&  type == "p") {
    if (!ellipse) { # ellipse option removes any missing data
      n.pair <- sum(!is.na(x - y))  # number of points after listwise deletion
      n.del <- sum(is.na(x - y))  # number of pairwise deleted observations
    }      
      .cr.main(x, y, brief=TRUE, ...)
    }
    if (!is.null(y) && is.factor(x)) {
      options(yname = x.name)
      options(xname = y.name)
      .ss.numeric(y, by=x, digits.d=digits.d, brief=TRUE)
    }

  cat("\n")
  }       

}  # end plt.main

