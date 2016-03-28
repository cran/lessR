.plt.main <- 
function(x, y, by, type, n.cat,
         col.fill, col.stroke, col.bg, col.grid,
         shape.pts, col.area, col.box, col.trans, 
         cex.axis, col.axis, col.low, col.hi,
         xy.ticks, xlab, ylab, main, sub, cex,
         value.labels, rotate.values, offset, style, means, stat,
         fit.line, col.fit.line,
         bubble.size, bubble.power, bubble.counts,
         ellipse, col.ellipse, fill.ellipse, 
         diag, col.diag, lines.diag, sort.y, segments.y, segments.x,
         quiet, want.labels=TRUE, ...)  {

# want.labels set just for ttestPower, which provides its own labels
  # both x and y are plotted, even if only a single variable
  # for a 1-D bubble plot of a single factor var, y.call was set to 0's
  # numerical 1-D scatter plot done in .dp.main
  if (length(unique(y)) == 1) {
    bubble1 <- TRUE
    if (style == "default") style <- "bubble"
  }
  else
    bubble1 <- FALSE

  # sort y by x  (for Cleveland dot plot)
  if (sort.y) {
    ord <- order(x)
    y <- factor(y, levels=y[ord])
  }

  # for a Cleveland dot plot, do not do a default bubble plot
  unique.y <- ifelse(length(unique(y)) == length(y), TRUE, FALSE)
  if (!is.factor(x) && is.factor(y) && unique.y)
    if (n.cat == 10) n.cat <- 0

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

  # see if trans is customized for this analysis
  if (is.null(col.trans)) {  # no change, so no trans for Cleveland dp
    if (is.null(x.lvl) && !is.null(y.lvl) && unique.y) { 
      trans.pts <- 0 
      col.fill <- .maketrans(col.fill, (1-trans.pts)*256)
    }
  }
  else {  # trans has been changed from default, so change col.fill
    trans.pts <- col.trans
    col.fill <- .maketrans(col.fill, (1-trans.pts)*256)
  }

  num.cat.x <- ifelse(is.null(x.lvl) && .is.num.cat(x, n.cat), TRUE, FALSE)
  cat.x <- ifelse (num.cat.x || !is.null(x.lvl), TRUE, FALSE)

  if (!bubble1) {
    num.cat.y <- ifelse(is.null(y.lvl) && .is.num.cat(y, n.cat), TRUE, FALSE)
    cat.y <- ifelse (num.cat.y || !is.null(y.lvl), TRUE, FALSE)
  }
  else {
    num.cat.y <- FALSE
    cat.y <- FALSE
  }

  nrows <- length(x)
  pt.sz <- ifelse (.Platform$OS == "windows", 1, 0.80)
  pt.size <- ifelse (is.null(cex), pt.sz, cex)

  if (is.null(col.fill)) col.fill <- "transparent"
  if (is.null(col.area)) col.area <- "transparent"

  if (want.labels) {
    gl <- .getlabels(xlab, ylab, main, cex.lab=0.85)
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
    cex.lab <- 0.85
  }

  if (is.null(type)) {  # if x is sorted with equal intervals, plot a line chart
    if (sum(is.na(x)) > 0)
      eq.int <- FALSE  # missing data in x present
    else {
      eq.int <- TRUE
      d.x <- diff(x)
      for (i in 2:(length(x)-1)) 
        if ((abs(d.x[i-1] - d.x[i]) > 0.0000000001)) eq.int <- FALSE
      rm(d.x)
    }  # also no y missing

    type <- ifelse (!is.unsorted(x) && eq.int && sum(is.na(y))==0, "l", "p") 
  }

  # set bubble plot for small number of unique values of x and y
  if (style == "default")  # set default
    style <- ifelse (cat.x && cat.y, "bubble", "regular")
  
  digits.d <- .max.dd(y) + 1
  options(digits.d=digits.d)


  # -------------------------
  # plot
  # -------------------------
  x.val <- NULL
  y.val <- y.lvl  # if not reset to x value labels
  max.lbl.y <- NULL
  if (!is.null(value.labels)) { 
    x.val <- value.labels
    if (length(unique(y)) > 1) {  # see if set y axis values to those of x
      if (length(unique(na.omit(x))) == length(unique(na.omit(y)))) {
        if (all(sort(unique(x)) == sort(unique(y)))) {
          y.val <- value.labels
          v <- unlist(strsplit(value.labels, "\n", fixed=TRUE))
          max.lbl.y <- max(nchar(v))
        }
      }
    }
  }
  else {
    x.val <- x.lvl  # x.val is NULL if x is numeric, ignored
    y.val <- y.lvl  # y.val ignored if y is numeric 
  }

  plot.new()  # activate for strwidth

  if (!is.null(y.val)) {
    max.width <- 0
    for (i in (1:length(y.val))) {
      li <- ifelse(!is.na(y.val[i]), strwidth(y.val[i], units="inches"), 0)
      if (li > max.width)
        max.width <- strwidth(y.val[i], units="inches")
    }
  }
  else 
    max.width <- strwidth(as.character(max(pretty(y))), units="inches")

  lm <- max(max.width + 0.3, 0.67)
  if (!is.null(y.lab)) {
    if (!nzchar(y.lab)[1]) lm <- lm - 0.3
    if (grepl("\n", y.lab[1], fixed=TRUE)) lm <- lm + .15
  }
  else
    lm <- lm - 0.3

  rm <- ifelse (is.null(by), 0.25, 0.95)
  tm <- ifelse (is.null(main), 0.25, 0.75)
  bm <- 0.75

  orig.params <- par(no.readonly=TRUE)
  on.exit(par(orig.params))
  par(mai=c(bm, lm, tm, rm))


  # -----------------------
  # setup coordinate system with plot and type="n"
  # non-graphical parameters in ... generate warnings when no plot
  
  if (!do.ellipse) {

    if (style %in% c("regular", "off")) {

      if (!lines.diag) {
        unq.x <- length(unique(na.omit(x)))
        unq.y <- length(unique(na.omit(y)))
        if (unq.x > 5  &&  unq.y > 5) {
          if (stat != "count")
            plot(x,y, type="n", axes=FALSE, ann=FALSE, ...)
          else {  # plot counts
            mx.y <- max(pretty(y))
            plot(x,y, type="n", axes=FALSE, ann=FALSE, ylim=c(0, mx.y), ...)
          }
        }
        else {  # create a buffer on ends of an axis 

          mn.x <- min(x, na.rm=TRUE)
          mx.x <- max(x, na.rm=TRUE)
          mn.y <- min(y, na.rm=TRUE)
          mx.y <- max(y, na.rm=TRUE)
          if (unq.x <= 5) {
            buf.x <- 0.74 - (0.12 * unq.x)
            mn.x <- mn.x - buf.x
            mx.x <- mx.x + buf.x
          }
          if (unq.y <= 5) {
            buf.y <- 0.74 - (0.12 * unq.y)
            mn.y <- mn.y - buf.y
            mx.y <- mx.y + buf.y
          }
          plot(x,y, type="n", axes=FALSE, ann=FALSE,
               xlim=c(mn.x,mx.x), ylim=c(mn.y,mx.y), ...)
        }
      }  # no diag
      else {  # need same x,y limits on both axes for lines.diag
        nums <- pretty(c(min(x,y, na.rm=TRUE), max(x,y, na.rm=TRUE)))
        l1 <- nums[1]
        l2 <- nums[length(nums)]
        plot(x,y, type="n", axes=FALSE, ann=FALSE,
                              xlim=c(l1,l2), ylim=c(l1,l2), ...)
      }
    }

    else if (style %in% c("bubble", "sunflower", "off")) {

      # get start and end values for each axis
      x.start=min(x, na.rm=TRUE)  # for bubble plot
      x.end=max(x, na.rm=TRUE)
      y.start=min(y, na.rm=TRUE) 
      y.end=max(y, na.rm=TRUE)

      if (!is.null(x.lvl)) {  # categorical - factor
        x.start <- .5
        x.end <- length(x.lvl) + .5
      }
      else if (num.cat.x) {  # categorical - numerical
        x.start <- x.start-.5
        x.end <- x.end+.5
      }

      if (!is.null(y.lvl)) {
        y.start <- .5
        y.end <- length(y.lvl) + .5
      }
      else if (num.cat.y) {
        y.start <- y.start-.5
        y.end <- y.end+.5
      }

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

      plot(x,y, type="n", axes=FALSE, ann=FALSE, 
           xlim=c(x.start,x.end), ylim=c(y.start,y.end))
    }

  }

  else {  # set plot with sufficient room for ellipse and data
    cxy <- cor(x,y, use="complete.obs")
    s.x <- sd(x, na.rm=TRUE); s.y <- sd(y, na.rm=TRUE)
    m.x <- mean(x, na.rm=TRUE); m.y <- mean(y, na.rm=TRUE)
    lvl <- max(ellipse)
    f <- ellipse(cxy, scale=c(s.x, s.y), centre=c(m.x, m.y), level=lvl)
    f <- rbind(f, c(min(x, na.rm=TRUE), min(y, na.rm=TRUE)))
    f <- rbind(f, c(max(x, na.rm=TRUE), max(y, na.rm=TRUE)))
    plot(f, type="n", axes=FALSE, ann=FALSE, main=main.lab, ...)
    rm(f)
  }


  # ----
  # plot 
  # x.lvl and y.lvl are NULL unless x and y are factors

  #axis, axis ticks, value labels

  if (cat.x) {
    if (!is.null(x.lvl)) axT1 <- 1:length(x.lvl)   # mark category values
    if (num.cat.x) axT1 <- sort(unique(x))
    #if (num.cat.x) axT1 <- axTicks(1)
  }
  else
    axT1 <- axTicks(1)  # else numeric, so all the ticks

  if (cat.y) { 
    if (!is.null(y.lvl)) axT2 <- 1:length(y.lvl)
    if (num.cat.y) axT2 <- sort(unique(y))
    #if (num.cat.y) axT2 <- axTicks(2)
  }
  else
    axT2 <- axTicks(2) 

  if (xy.ticks) {
    if (!bubble1)
      .axes(x.val, y.val, axT1, axT2,
            par("usr")[1], par("usr")[3], cex.axis, col.axis,
            rotate.values, offset=offset, ...)
    else  # 1-l scatter plot of categorical variable
      .axes(x.val, NULL, axT1, NULL,
            par("usr")[1], par("usr")[3], cex.axis, col.axis, 
            rotate.values, offset=offset, ...)
  }
       
  # axis labels 
  if (is.null(max.lbl.y)) {  # could be set earlier when x.val = y.val
    if (!is.null(y.lvl)) {
      max.lbl.y <- max(nchar(y.lvl))
    }
    else
      max.lbl.y <- max(nchar(axTicks(2)))
  }

  if (bubble1) {
    y.lab <- ""
    max.lbl.y <- 0
  }

  .axlabs(x.lab, y.lab, main.lab, sub.lab, max.lbl.y, 
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
    abline(v=axT1, col=col.grid, lwd=.5)
    abline(h=axT2, col=col.grid, lwd=.5)
  }
  else
    abline(v=axT1, col=col.grid, lwd=.75)  # bubbles only

  # fill area under curve
  col.border <- ifelse (type != "p", col.stroke, "transparent")
  if (!is.null(col.area)) 
    polygon(c(x[1],x,x[length(x)]), c(min(y),y,min(y)),
            col=col.area, border=col.border)

  # plot the values
  if (style %in% c("regular", "off")) {  # plot lines and/or points
    if (type == "l" | type == "b")
      lines(as.numeric(x),y, col=col.stroke, ...)

    if (type == "p" | type == "b") {

      if (is.null(by)) { 
            
        if (style != "off") {
          points(x,y, pch=shape.pts, col=col.stroke, bg=col.fill, cex=pt.size, ...)

          if (segments.y)
            segments(x0=min(pretty(x)), y0=y, x1=x, y1=y, lty=1, lwd=.75,
                     col=col.stroke)
          if (segments.x)
            segments(y0=min(pretty(y)), x0=x, y1=y, x1=x, lty=1, lwd=.75,
                     col=col.stroke)
        }

        if (!is.null(x.lvl) && means) {  # plot means
          m.lvl <- numeric(length = 0)
          for (i in (1:length(x.lvl))) 
            m.lvl[i] <- mean(y[x==i], na.rm=TRUE)
          pch.avg <- ifelse(getOption("colors")!="gray", 21, 23)
          bck.g <- ifelse(getOption("colors")!="gray", "gray15", "gray30")
          if (grepl(".black", getOption("colors"), fixed=TRUE)) bck.g <- "gray85"
          abline(h=m.lvl, col="gray50", lwd=.5)
          points(m.lvl, pch=pch.avg, bg=bck.g)
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
        if (length(col.stroke)==1 && length(shape.pts)==1)  # shape, color 
          for (i in 1:n.levels) shp[i] <- shape.dft[i]  # fill is default shapes

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

      }  # end by

    }  # type == "p" | type == "b"
  }  # style = "regular"


  else if (style %in% c("bubble", "sunflower")) {

    mytbl <- table(x, y)  # get the counts

    # colors
    if (is.null(col.low) ||  is.null(col.hi))
      clr <- col.fill
    else {
      color.palette <- colorRampPalette(c(col.low, col.hi))
      clr <- color.palette(nrow(mytbl))
    }

    # melt the table to a data frame
    k <- 0
    xx <- integer(length=0)
    yy <- integer(length=0)
    count <- integer(length=0)
    for (i in 1:nrow(mytbl)) {
      for (j in 1:ncol(mytbl)) {
        if (mytbl[i,j] != 0) {  # 0 plots to a single pixel, so remove
          k <- k + 1
          count[k] <- mytbl[i,j]
          xx[k] <- as.numeric(rownames(mytbl)[i])  # rownames are factors
          yy[k] <- as.numeric(colnames(mytbl)[j])
        }
      }
    }
    cords <- data.frame(xx, yy, count)

    c <- cords$count
    if (style == "bubble")
      symbols(as.numeric(cords$xx), as.numeric(cords$yy),
            circles=c**bubble.power, inches=bubble.size,
            bg=clr, fg=col.stroke, add=TRUE, ...)

    else if (style == "sunflower") {
      sunflowerplot(cords$xx, cords$yy, number=c, 
        seg.col=col.stroke, col=col.fill, cex.axis=cex.axis, col.axis=col.axis,
        xlab=x.lab, ylab=y.lab,
        xlim=c(x.start,x.end), ylim=c(x.start,x.end), add=TRUE)
  }

    if (bubble.counts  &&  style == "bubble") { 
      max.c <- max(c, na.rm=TRUE)  # bubble is too small for count
      #min.bubble <- 0.25 * max.c  # radius, bubble.power=1
      #min.bubble <- 0.10 * max.c  # bubble.power=.6
      min.bubble <- (bubble.power/9) * max.c
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
  if (!quiet) {

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
    else if (cat.x && !cat.y && stat=="") {

      if (!is.null(x.lvl))
        x.by <- factor(x, levels=1:length(x.lvl), labels=x.lvl)  # convert back to a factor
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

    else if (!cat.x && cat.y && stat == "") {  # Cleveland dot plot
      if (!is.null(y.lvl))
        y.by <- factor(y, levels=1:length(y.lvl), labels=y.lvl)  # convert back to a factor
      else
        y.by <- y

        stats <- .ss.numeric(x, digits.d=digits.d, brief=TRUE)
        txout <- stats$tx
        class(txout) <- "out_piece"

        output <- list(out_txt=txout)
        class(output) <- "out_all"
        print(output)
    }

    # categorical x and y vars
    else if (cat.x  &&  cat.y) {
      if (!is.null(x.lvl))
        x.fac <- factor(x, levels=1:length(x.lvl), labels=x.lvl)
      else
        x.fac <- x
      if (!is.null(y.lvl))
        y.fac <- factor(y, levels=1:length(y.lvl), labels=y.lvl)
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

