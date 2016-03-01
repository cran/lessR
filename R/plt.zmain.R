.plt.main <- 
function(x, y, by, type, n.cat,
         col.fill, col.stroke, col.bg, col.grid,
         shape.pts, col.area, col.box, 
         cex.axis, col.axis, col.low, col.hi,
         xy.ticks, xlab, ylab, main, sub, cex,
         value.labels, rotate.values, offset, kind, means,
         fit.line, col.fit.line, bubble.size, bubble.counts,
         ellipse, col.ellipse, fill.ellipse, 
         diag, col.diag, lines.diag, quiet, want.labels=TRUE, ...)  {
# want.labels set just for ttestPower, which provides its own labels

  # both x and y are plotted, even if only a single variable
  # for a 1-D bubble plot of a single factor var, y.call was set to 0's
  if (length(unique(y)) == 1) {
    bubble1 <- TRUE
    if (kind == "default") kind <- "bubble"
  }
  else
    bubble1 <- FALSE

  do.ellipse <- ifelse(as.logical(ellipse[1]), TRUE, FALSE) 

  if (!is.null(value.labels)) value.labels <- gsub(" ", "\n", value.labels) 

  # all processing in terms of numeric variables
  # convert factors, save levels
  x.lvl <- NULL; y.lvl <- NULL  # if null, then numeric
  if (is.factor(x)) {
    x.lvl <- levels(x)
    if (is.null(value.labels)) value.labels <- gsub(" ", "\n", x.lvl) 
    x <- as.numeric(x)
  }
  if (is.factor(y)) {
    y.lvl <- levels(y)
    y <- as.integer(y)
  }

  nrows <- length(x)
  pt.sz <- ifelse (.Platform$OS == "windows", 1, 0.8)
  pt.size <- ifelse (is.null(cex), pt.sz, cex)

  if (is.null(col.fill)) col.fill <- "transparent"
  if (is.null(col.area)) col.area <- "transparent"

  # get variable labels if exist plus axes labels
  if (want.labels) {
    gl <- .getlabels(xlab, ylab, main, cex.lab=0.98)
    x.name <- gl$xn; x.lbl <- gl$xl; x.lab <- gl$xb
    y.name <- gl$yn; y.lbl <- gl$yl; y.lab <- gl$yb
    main.lab <- gl$mb
    sub.lab <- gl$sb
    cex.lab <- gl$cex.lab
    by.name <- getOption("byname")
  }
  else {
    x.lab <- xlab
    y.lab <- ylab
    main.lab <- main
    sub.lab <- sub
    cex.lab <- 0.98
  }

  if (is.null(type)) {  # if x is sorted with equal intervals, plot a line chart
    if (sum(is.na(x)) > 0)
      equal.int <- FALSE  # missing data in x present
    else {
      diff.x <- diff(x)
      for (i in 2:(length(x)-1)) 
        if ((abs(diff.x[i-1] - diff.x[i]) > 0.0000000001)) 
          equal.int <- FALSE
        else
          equal.int <- TRUE
      rm(diff.x)
    }  # also no y missing
    type <- ifelse (!is.unsorted(x) && equal.int && sum(is.na(y))==0, "l", "p") 
  }

  # bubble plot for small number of unique values
  if (kind == "default")  # set default
    if (length(unique(x))<n.cat && length(unique(y))<n.cat)
      kind <- "bubble"
    else
      kind <- "regular"
  
  digits.d <- .max.dd(y) + 1
  options(digits.d=digits.d)


  # -------------------------
  # plot
  # -------------------------

  # -----------------------
  # setup coordinate system with plot and type="n"
  # non-graphical parameters in ... generate warnings when no plot
  
  if (!do.ellipse) {

    if (kind == "regular") {

      if (!is.null(by)) {  # make room for legend in right margin
        orig.params <- par(no.readonly=TRUE)
        on.exit(par(orig.params))
        par(omi=c(0,0,0,0.6))
      }

      if (!lines.diag) {
        if (length(unique(na.omit(x))) > 4)
          plot(x,y, type="n", axes=FALSE, ann=FALSE, ...)
        else {  # create a buffer on ends of x-axis 
          mn.x <- min(x, na.rm=TRUE) - .5
          mx.x <- max(x, na.rm=TRUE) + .5
          plot(x,y, type="n", axes=FALSE, ann=FALSE,
                                xlim=c(mn.x,mx.x), ...)
        }
      }
      else {  # need same x,y limits on both axes for lines.diag
        nums <- pretty(c(min(x,y, na.rm=TRUE), max(x,y, na.rm=TRUE)))
        l1 <- nums[1]
        l2 <- nums[length(nums)]
        plot(x,y, type="n", axes=FALSE, ann=FALSE,
                              xlim=c(l1,l2), ylim=c(l1,l2), ...)
      }
    }

    else if (kind %in% c("bubble", "sunflower")) {

      # get start and end values for each axis
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

      x.lo <- x.start-.5; x.hi <- x.end+.5
      y.lo <- y.start-.5; y.hi <- y.end+.5

      plot(x,y, type="n", axes=FALSE, ann=FALSE, 
           xlim=c(x.lo,x.hi), ylim=c(y.lo,y.hi))
    }

    else {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
      "This type of plot not recognized: ", kind, "\n\n")
    }
  }

  else {  # calculate ellipse(s), set plot with sufficient room
    cxy <- cor(x,y, use="complete.obs")
    s.x <- sd(x, na.rm=TRUE); s.y <- sd(y, na.rm=TRUE)
    m.x <- mean(x, na.rm=TRUE); m.y <- mean(y, na.rm=TRUE)
    lvl <- max(ellipse)
    lvl <- ifelse(lvl > 0.95, lvl, 0.95)  # set plot region for at least 0.95
    e <- ellipse(cxy, scale=c(s.x, s.y), centre=c(m.x, m.y), level=lvl)
    plot(e, type="n", axes=FALSE, ann=FALSE, main=main.lab, ...)
  }


  # ----
  # plot 
  # x.lvl and y.lvl are NULL unless x and y are factors

  #axis, axis ticks, value labels
  x.val <- NULL; y.val <- NULL
  if (!is.null(value.labels)) { 
    if (length(unique(y)) > 1)
      if (length(unique(x)) == length(unique(y)))  # see if set y axis values to those of x
        if (all(sort(unique(x)) == sort(unique(y)))) y.val <- value.labels
    x.val <- value.labels
  }
  else {
    x.val <- x.lvl  # x.val is NULL if x is numeric
    y.val <- y.lvl  # y.val is NULL if y is numeric 
  }

  if (!is.null(x.lvl) || length(unique(x)) <= n.cat) 
      axT1 <- sort(unique(x))  # just mark the category values
  else
    axT1 <- axTicks(1)  # else numeric, so all the ticks
  if (!is.null(y.lvl) || length(unique(y)) <= n.cat) 
    axT2 <- sort(unique(y)) 
  else
    axT2 <- axTicks(2) 

  if (xy.ticks)
    if (!bubble1)
      .axes(x.val, y.val, axT1, axT2,
            par("usr")[1], par("usr")[3], cex.axis, col.axis,
            rotate.values, offset=offset, ...)
    else  # 1-l scatter plot of categorical variable
      .axes(x.val, NULL, axT1, NULL,
            par("usr")[1], par("usr")[3], cex.axis, col.axis, 
            rotate.values, offset=offset, ...)
       
  # axis labels 
  if (!is.null(y.lvl) || !is.null(value.labels)) {
    if (!is.null(value.labels)) {
      v <- unlist(strsplit(value.labels, "\n", fixed=TRUE))
      max.lbl <- max(nchar(v))
    }
    else
      max.lbl <- max(nchar(y.lvl))
  }
  else
    max.lbl <- max(nchar(axTicks(2)))
  if (bubble1) {
    y.lab <- ""
    max.lbl <- 0
  }

  .axlabs(x.lab, y.lab, main.lab, sub.lab, max.lbl, 
          xy.ticks, offset=offset, cex.lab=cex.lab, ...) 

  # color plotting area
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border=col.box)

  # diagonal line
  if (diag) {
    segments(usr[1], usr[3], usr[2], usr[4], col=col.diag)  # diagonal line
    if (lines.diag)  # connect points in scatter plot to the line
      for (i in 1:length(x)) segments(x[i], x[i], x[i], y[i])
  }

  # grid lines
  if (!bubble1) {
    abline(v=axT1, col=col.grid, lwd=.75)
    abline(h=axT2, col=col.grid, lwd=.75)
  }
  else
    abline(v=axTicks(1), col=col.grid, lwd=.75)  # bubbles only

  # fill area under curve
  col.border <- ifelse (type != "p", col.stroke, "transparent")
  if (!is.null(col.area)) 
    polygon(c(x[1],x,x[length(x)]), c(min(y),y,min(y)),
            col=col.area, border=col.border)

  # plot the values
  if (kind == "regular") {  # plot lines and/or points
    if (type == "l" | type == "b")
      lines(as.numeric(x),y, col=col.stroke, ...)

    if (type == "p" | type == "b") {

      if (is.null(by)) { 

        trans.pts <- getOption("trans.fill.pt")  # plot points
        clr.trn <- .maketrans(col.fill, (1-trans.pts)*256)
        points(x,y, pch=shape.pts, col=col.stroke,
            bg=clr.trn, cex=pt.size, ...)

        if (!is.null(x.lvl) && means) {  # plot means
          for (i in (1:length(x.lvl))) {
            m.lvl <- mean(y[x==i], na.rm=TRUE)
            abline(h=m.lvl, col="gray50")
            bck.g <- ifelse(getOption("colors")!="gray", "gray50", "gray30")
            if (grepl(".black", getOption("colors"), fixed=TRUE)) bck.g <- "gray85"
            points(i, m.lvl, pch=23, bg=bck.g)
          }
        }
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

    }  # type == "p" | type == "b"
  }  # kind = "regular"


  else if (kind %in% c("bubble", "sunflower")) {

    mytbl <- table(x, y)  # get the counts

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

    # colors
    if (is.null(col.low) ||  is.null(col.hi))
      clr <- col.fill
    else {
      color.palette <- colorRampPalette(c(col.low, col.hi))
      clr <- color.palette(nrow(mytbl))
    }

    c <- cords$count  # 0 plots to a single pixel, so remove
    for (i in 1:length(c)) if (c[i]==0) c[i] <- NA

    if (kind == "bubble")
      symbols(cords$xx, cords$yy, circles=c, bg=clr, 
            fg=col.stroke, inches=bubble.size, add=TRUE, ...)

    else if (kind == "sunflower") {
      sunflowerplot(cords$xx, cords$yy, number=cords$count, 
        seg.col=col.stroke, col=col.fill, cex.axis=cex.axis, col.axis=col.axis,
        xlab=x.lab, ylab=y.lab, xlim=c(x.lo,x.hi), ylim=c(x.lo,x.hi), add=TRUE)
  }

    if (bubble.counts  &&  kind == "bubble") { 
      max.c <- max(c, na.rm=TRUE)  # do not display count if bubble is too small
      min.bubble <- 0.25 * max.c
      for (i in 1:length(c))
        if (!is.na(c[i])) if (c[i] <= min.bubble) c[i] <- NA
      text(cords$xx, cords$yy, c, cex=.7)
    }
  }  # end bubble/sunflower 


  # ellipse option
  if (do.ellipse) {
    for (i in 1:length(ellipse)) {
      e <- ellipse(cxy, scale=c(s.x, s.y), centre=c(m.x, m.y), level=ellipse[i])
      polygon(e, border=col.ellipse, col=fill.ellipse, lwd=1.5)
    }
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


  # text output
  # correlation
  if (!quiet) {

    nu <- length(unique(na.omit(x)))
    num.cat <- ifelse(is.null(x.lvl) && nu <= n.cat, TRUE, FALSE)
    cat.x <- ifelse (num.cat || !is.null(x.lvl), TRUE, FALSE)

    if (!bubble1) {
      nu <- length(unique(na.omit(y)))
      num.cat <- ifelse(is.null(y.lvl) && nu <= n.cat, TRUE, FALSE)
      cat.y <- ifelse (num.cat || !is.null(y.lvl), TRUE, FALSE)
    }
    else
      cat.y <- FALSE

    # traditional two-var numeric var scatter plot
    if (!cat.x  &&  !cat.y  &&  type == "p") {
      if (do.ellipse) { # ellipse option removes any missing data
        n.pair <- sum(!is.na(x - y))  # number of points after listwise deletion
        n.del <- sum(is.na(x - y))  # number of pairwise deleted observations
      }      

      stuff <- .cr.main(x, y, brief=TRUE, ...) 
      txbck <- stuff$txb
      txdsc <- stuff$txd
      txinf <- stuff$txi

      class(txbck) <- "out_piece"
      class(txdsc) <- "out_piece"
      class(txinf) <- "out_piece"

      output <- list(out_background=txbck, out_describe=txdsc, out_inference=txinf,
        r=stuff$r, tvalue=stuff$tvalue, df=stuff$df, pvalue=stuff$pvalue,
        lb=stuff$lb, ub=stuff$ub)

      class(output) <- "out_all"
      print(output)
    }

    # categorical x var, numeric y var
    else if (cat.x && !cat.y) {

      if (!is.null(x.lvl))
        x.by <- factor(x, labels=x.lvl)  # convert back to a factor
      else
        x.by <- x

      if (!bubble1) {
        options(yname = x.name)  # reverse order of x and y for .ss.numeric
        options(xname = y.name)
        stats <- .ss.numeric(y, by=x.by, digits.d=digits.d, brief=TRUE)
        txout <- stats$tx
        class(txout) <- "out_piece"

        output <- list(out_txt=txout)
        class(output) <- "out_all"
        print(output)
      }

      else {  # 1-D bubble plot of a factor var, y just a constant
        stats <- .ss.factor(x.by, by=NULL, brief=TRUE, digits.d=NULL,
                            x.name, y.name, x.lbl, y.lbl)

        txttl <- stats$title
        counts <- stats$count
        chi <- stats$chi

        class(txttl) <- "out_piece"
        class(counts) <- "out_piece"
        class(chi) <- "out_piece"
        output <- list(out_title=txttl, out_counts=counts, out_chi=chi)
        class(output) <- "out_all"
        print(output)      
      }
    }

    # categorical x and y vars
    else if (cat.x  &&  cat.y) {
      if (!is.null(x.lvl))
        x.fac <- factor(x, labels=x.lvl)
      else
        x.fac <- x
      if (!is.null(y.lvl))
        y.fac <- factor(y, labels=y.lvl)
      else
        y.fac <- y
      stats <- .ss.factor(x.fac, y.fac, brief=TRUE, digits.d=NULL,
                          x.name, y.name, x.lbl, y.lbl)

      txttl <- stats$txttl
      txfrq <- stats$txfrq
      txXV <- stats$txXV
      class(txttl) <- "out_piece"
      class(txfrq) <- "out_piece"
      class(txXV) <- "out_piece"
      output <- list(out_title=txttl, out_text=txfrq, out_XV=txXV)
      class(output) <- "out_all"
      print(output)
    }

  cat("\n")
  }       

}  # end plt.main

