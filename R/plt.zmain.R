.plt.main <-
function(x, y, by=NULL, n.cat=getOption("n.cat"),
         cat.x=FALSE, num.cat.x=FALSE, cat.y=FALSE, num.cat.y=FALSE,
         object="point", stat="data",

         col.fill=getOption("pt.fill"),
         area.fill=getOption("pt.fill"),
         col.color=getOption("pt.color"),

         col.trans=NULL, col.segment=getOption("segment.color"),

         xy.ticks=TRUE,
         xlab=NULL, ylab=NULL, main=NULL, main.cex=NULL, sub=NULL,
         value.labels=NULL, label.max=20,
         rotate.x=0, rotate.y=0, offset=0.5, prop=FALSE, origin.x=NULL,

         size=NULL, shape="circle", means=TRUE,
         segments.y=FALSE, segments.x=FALSE, ln.width=2,

         smooth=FALSE, smooth.points=100, smooth.size=1,
         smooth.exp=0.25, smooth.bins=128,

         radius=0.15, power=0.6, size.cut=TRUE,
         bubble.text=getOption("bubble.text.color"),
         col.low=NULL, col.hi=NULL,

         ID=NULL, ID.color="gray50", ID.size=0.75, out.ind,
         out.fill, out.color, out.shape.miss,

         fit.line="off", col.fit.line="gray55",
         fit.lwd=getOption("fit.lw"),
         fit.se=1, se.fill="gray80",

         ellipse=FALSE, col.ellipse="lightslategray",
         ellipse.fill="off", ellipse.lwd,

         center.line="default", show.runs=FALSE, stack=FALSE,

         freq.poly=FALSE, jitter.x=0, jitter.y=0,

         xlab.adj=0, ylab.adj=0, bm.adj=0, lm.adj=0, tm.adj=0, rm.adj=0,
         legend.title=NULL,

         add=NULL, x1=NULL, x2=NULL, y1=NULL, y2=NULL,
         add.cex=NULL, add.lwd=1, add.lty="solid",
         add.color=NULL, add.fill=NULL, add.trans=NULL,

         quiet=getOption("quiet"), want.labels=TRUE, ...)  {


  fill.bg <- getOption("panel.fill")
  date.ts <- ifelse (.is.date(x[,1]), TRUE, FALSE)
  size.pt <- size * .9  # size is set in Plot.R, reduce a bit for here

  if (center.line == "default") if (date.ts) center.line <- "off"

  # x and y come across here in their natural state, within each data frame
  # a time series has dates for x and numeric for y, factors are factors, etc

  # want labels set just for ttestPower, which provides its own labels
  # both x and y are plotted, even if only a single variable
  # for a 1-D bubble plot of a single factor var, y.call was set to 0's
  bubble1 <- ifelse (length(unique(y[,1])) == 1, TRUE, FALSE)

  unique.x <- ifelse(length(unique(x[,1])) == length(x[,1]), TRUE, FALSE)
  unique.y <- ifelse(length(unique(y[,1])) == length(y[,1]), TRUE, FALSE)

  do.ellipse <- ifelse(ellipse[1] > 0, TRUE, FALSE)

  # all processing in terms of numeric variables
  # convert factors to numeric, save levels, so x and y are always numeric
  # x will always be a matrix
  x.lvl <- NULL; y.lvl <- NULL  # if remain null, then not factors
  nm.x <- names(x)
  if (is.factor(x[,1])) {
    x.lvl <- levels(x[,1])
    if (!is.null(value.labels)) value.labels <- gsub(" ", "\n", x.lvl)
    x <- as.matrix(as.integer(x[,1]))
  }
  else if (!date.ts) {
    x <- as.matrix(x)
    colnames(x) <- nm.x
  }

  nm.y <- names(y)
  if (is.factor(y[,1])) {
    y.lvl <- levels(y[,1])  # gets put into alphabetical order
    y <- as.matrix(as.integer(y[,1]))
  }
  else if (!date.ts) {
    nm.y <- names(y)
    y <- as.matrix(y)
    colnames(y) <- nm.y
  }

  #if (bubble1) for (i in 1:length(y[,1])) y[i,1] <- x[i,1] %% 6

  # dimensions
  n.xcol <- ncol(x)
  n.ycol <- ncol(y)
  n.col <- max(n.xcol, n.ycol)
  nrows <- nrow(x)

  if (date.ts) {
    x.val <- x[,1]
    x <- as.matrix(x.val, ncol=1)
  }


  if (n.col > 1) center.line <- "off"   # no center.line for multiple plots

  if (is.null(x.lvl) && !is.null(y.lvl) && unique.y ||
      is.null(y.lvl) && !is.null(x.lvl) && unique.x) {
    cleveland <- TRUE
  }
  else
    cleveland <- FALSE

  # get lab.x.cex  lab.y.cex
  lab.cex <- getOption("lab.cex")
  lab.x.cex <- getOption("lab.x.cex")
  lab.y.cex <- getOption("lab.y.cex")
  lab.x.cex <- ifelse(is.null(lab.x.cex), lab.cex, lab.x.cex)
  # adj <- .RSadj(lab.cex=lab.x.cex); lab.x.cex <- adj$lab.cex
  lab.y.cex <- ifelse(is.null(lab.y.cex), lab.cex, lab.y.cex)
  # adj <- .RSadj(lab.cex=lab.y.cex); lab.y.cex <- adj$lab.cex

  if (date.ts) xx.lab <- xlab
# if (is.null(ylab))  # no ylab for Cleveland dot plot
#   if (is.null(x.lvl) && !is.null(y.lvl) && unique.y) ylab <- ""
  if (want.labels) {
    gl <- .getlabels(xlab, ylab, main, lab.x.cex=lab.x.cex,
                     lab.y.cex=lab.y.cex)
    x.name <- gl$xn; x.lbl <- gl$xl; x.lab <- gl$xb
    y.name <- gl$yn; y.lbl <- gl$yl; y.lab <- gl$yb
    main.lab <- gl$mb
    sub.lab <- gl$sb
    by.name <- getOption("byname")
  }
  else {
    x.lab <- xlab
    y.lab <- ylab
    main.lab <- main
    sub.lab <- sub
    x.name <- NULL
  }

  if (date.ts  &&  is.null(xx.lab)) x.lab <- ""

  if (!is.null(x.name)) if (x.name == "Index") {
    if (n.ycol > 1) y.lab <- ""
    if (!is.null(x.lbl)) y.lab <- paste(x.name, ": ", x.lbl, sep="")
  }

  if (is.null(col.fill)) col.fill <- "transparent"

  # by default display center.line only if runs, so detect if a run
  if (center.line == "default"  &&  !date.ts  &&  object == "both") {
    y.clean <- na.omit(y)
    m <- mean(y.clean)
    n.change <- 0
    for (i in 1:(length(y.clean)-1))
      if ((y.clean[i+1] > m) != (y.clean[i] > m)) n.change <- n.change+1
    if (n.change/(length(y.clean)-1) < .15)
      center.line <- "off"
    else
      center.line <- "median"
  }
  else  # default if not automatically assigned above
    if (!(center.line %in% c("off", "mean"))) center.line <- "median"

  # decimal digits
  digits.d <- .max.dd(y[,1]) + 1
  options(digits.d=digits.d)


  # -------------------------
  # plot
  # -------------------------
  # -------------------------

  # graphic system parameters

  # x.val is either any value.labels or x.lvl, or NULL if x is numeric
  mx.x.val.ln <- 1
  mx.y.val.ln <- 1
  if (!date.ts) {
    x.val <- NULL
    y.val <- y.lvl  # if not reset to x value labels
    max.lbl.y <- NULL
    if (!is.null(value.labels)) {
      x.val <- value.labels
      if (length(unique(y[,1])) > 1) {  # see if set y axis values to those of x
        if (length(unique(na.omit(x[,1]))) == length(unique(na.omit(y[,1])))) {
          if (all(sort(unique(x[,1])) == sort(unique(y[,1])))) {
            y.val <- value.labels
            v <- unlist(strsplit(value.labels, "\n", fixed=TRUE))
            max.lbl.y <- max(nchar(v))
          }
        }
      }
    }
    else {  # is null value.labels
      x.val <- x.lvl  # x.val is NULL if x is numeric, ignored
      y.val <- y.lvl  # y.val ignored if y is numeric
    }
  }
  else {  # time series
    max.lbl.y <- NULL
    y.val <- NULL
  }

  # get max number of lines in x value labels
  if (!is.null(x.val)) {
    stuff <- .get.val.ln(x.val, x.name)
    x.val <- stuff$val.lab
    mx.x.val.ln <- stuff$mx.val.ln
  }

  # get max number of lines in y value labels
  if (!is.null(y.val)  &&  y.name != "row.names"  && !cleveland) {
    stuff <- .get.val.ln(y.val, y.name)
    y.val <- stuff$val.lab
    mx.y.val.ln <- stuff$mx.val.ln
  }

  axis.y.cex <- ifelse(is.null(getOption("axis.y.cex")),
    getOption("axis.cex"), getOption("axis.y.cex"))

  if (!is.null(y.val)) {  # y-axis labels are characters
    yv <- unlist(strsplit(y.val, "\n", fixed=TRUE))
    max.y.width <- max(strwidth(yv, units="inches", cex=axis.y.cex))
    if (options("device") != "RStudioGD")  # not work in R, only RStudio
      max.y.width <- .09 * axis.y.cex * max(nchar(yv))
  }
  else {  # y-axis labels are numeric
    prety <- max(pretty(c(min(y, na.rm=TRUE), max(y, na.rm=TRUE))))
    mx.num <-  ifelse (!prop, as.character(prety), .fmt(prety, 2))
    max.y.width <- max(strwidth(mx.num, cex=axis.y.cex, units="inches"))
  }


  # set title for bubble plot if proportions
  if (object == "bubble"  &&  prop  &&  is.null(main)  &&  cat.y) {
    main.lab <- paste("Percentage of", y.name, "\nwithin each level of", x.name)
    main <- "not null"
  }

# if (!date.ts) if (is.null(x.lab)) if (n.xcol > 1) x.lab <- NULL
  if (!is.null(x.lab)) if (n.xcol > 1  &&  substr(x.lab, 1, 2) == "c(")
      x.lab <- NULL   # e.g., get rid of x.lab == "c(Female,Male)" 

  if (length(size) > 1) {  # size is a variable
    sz.nm <- getOption("sizename")
    main.lab <- bquote(paste(italic(.(sz.nm)), ": Bubble size from ",
      .(min(size)), " to ", .(max(size)), sep=""))
  }

  # set margins
  margs <- .marg(max.y.width, y.lab, x.lab, main.lab, rotate.x,
                mx.x.val.ln, mx.y.val.ln,
                lab.x.cex=lab.x.cex, lab.y.cex=lab.y.cex)
  mm <- margs$lm  # left margin, lm is linear model
  tm <- margs$tm
  rm <- margs$rm
  bm <- margs$bm
  n.lab.x.ln <- margs$n.lab.x.ln
  n.lab.y.ln <- margs$n.lab.y.ln

  if (n.xcol > 1  ||  n.ycol > 1  ||  !is.null(by)) {  # vertical legend room
    new.adj <- .22  + (.65 * axis.y.cex)
    rm <- rm + new.adj 
    if (axis.y.cex > 1) if (!is.null(by)) rm <- rm + .1  # kludge
  }
  if (object == "both")
    if (center.line != "off") rm <- rm + .4  # room for center.line label

  if (offset > 0.5) bm <- bm + (-0.05 + 0.2 * offset)  # offset kludge

  rm <- rm + 0.10  # make room when the last axis date > last data value

  # user manual adjustment
  bm <- bm + bm.adj
  mm <- mm + lm.adj
  tm <- tm + tm.adj
  rm <- rm + rm.adj

  orig.params <- par(no.readonly=TRUE)
  on.exit(par(orig.params))

  par(bg=getOption("window.fill"))
  par(mai=c(bm, mm, tm, rm))


  # -----------------------
  # setup coordinate system only with plot and type="n"
  # non-graphical parameters in ... Generate warnings when no plot
  # -----------------------

  mn.x <- ifelse(is.null(x.lvl), min(x, na.rm=TRUE), 1)
  mx.x <- ifelse(is.null(x.lvl), max(x, na.rm=TRUE), length(x.lvl))
  mn.y <- ifelse(is.null(y.lvl), min(y, na.rm=TRUE), 1)
  mx.y <- ifelse(is.null(y.lvl), max(y, na.rm=TRUE), length(y.lvl))

  # re-calibrate y=axis if stacking
  n.by <- ifelse (is.null(by), 0, nlevels(by))
  if (stack  &&  n.ycol > 1) {  # data in wide format, one col for each y
    y.tot <- apply(y, 1, sum, na.rm=TRUE)
    mx.y <- max(y.tot)
  }
  if (stack  &&  (n.ycol == 1 &&  n.by > 1)) {  # data in tidy format
    for (i in 1:n.by) {
      if (i > 1) yo <- yl
      yl <- subset(y, by==levels(by)[i])
      if (i > 1) yl[,1] <- yl[,1] + yo[,1]
    }
    mx.y <- max(yl)
  }


  if (!do.ellipse) {

    if (cat.x) {
      mn.x <- mn.x - .4
      mx.x <- mx.x + .4
    }
    if (cat.y) {
      mn.y <- mn.y - .4
      mx.y <- mx.y + .4
    }

    if (is.null(origin.x)) {  # set default for minimum x-value displayed
      if (stat %in% c("count", "proportion", "%"))
        origin.x <- 0
      else
        origin.x <- mn.x
    }
    if (stat != "data" && (!all(y == 0))) mx.y <- mx.y + (.08 * (mx.y-mn.y))

    region <- matrix(c(origin.x, mx.x, mn.y, mx.y), nrow=2, ncol=2)


  }  # no ellipse

  else {  # set plot with sufficient room for ellipse and data
    cxy <- cor(x[,1],y[,1], use="complete.obs")
    s.x <- sd(x[,1], na.rm=TRUE); s.y <- sd(y[,1], na.rm=TRUE)
    m.x <- mean(x[,1], na.rm=TRUE); m.y <- mean(y[,1], na.rm=TRUE)
    lvl <- max(ellipse)
    region <- ellipse(cxy, scale=c(s.x, s.y), centre=c(m.x, m.y), level=lvl)
    region <- rbind(region, c(mn.x, mn.y), c(mx.x, mx.y))
  }

  # plot: setup the coordinate system
  plot(region, type="n", axes=FALSE, ann=FALSE, ...)
  rm(region)

  usr <- par("usr")


  # set up plot background
  # ----------------------

  # axis ticks and values
  if (cat.x) {
    if (!is.null(x.lvl)) axT1 <- 1:length(x.lvl)   # mark category values
    if (!is.null(x.val)) x.val <- .abbrev(x.val, label.max)
    if (num.cat.x) axT1 <- sort(unique(x))  # x.lvl or x.val are NULL
  }
  else {
    if (!date.ts) {
      if (stat == "count") {
        if (mx.x <= 3)  # just want integers for count axis
          n.best <- 3
        else if (mx.x > 3  &&  mx.x < 100)
          n.best <- 4
        else
          n.best <- 5
        axT1 <- pretty(origin.x:mx.x, n=n.best)
      }
      else
        axT1 <- pretty(c(origin.x, x))  # else numeric, so all the ticks
    }
    else
      axT1 <-axTicks(1)  
  }

  if (cat.y) {
    if (!is.null(y.lvl)) axT2 <- 1:length(y.lvl)
    if (!is.null(y.val)) y.val <- .abbrev(y.val, label.max)
    if (num.cat.y) axT2 <- sort(unique(y))
  }
  else {
    if (!date.ts)
      axT2 <- pretty(y)
    else
      axT2 <- axTicks(2)
  }

  # background color
  rect(usr[1], usr[3], usr[2], usr[4], col=fill.bg, border="transparent")

  # grid lines (put before box color around plot)
  .grid("v", axT1)
  if (!bubble1) .grid("h", axT2)

  # box around plot
  rect(usr[1], usr[3], usr[2], usr[4],
    col="transparent", border=getOption("panel.color"),
    lwd=getOption("panel.lwd"), lty=getOption("panel.lty"))

  # axes
  if (xy.ticks) {
    if (!bubble1) {
      if (!date.ts) {  # get ticks for both axes
        .axes(x.val, y.val, axT1, axT2,
              rotate.x=rotate.x, rotate.y=rotate.y, offset=offset, ...)
      }
      else {  # time
        axis.x.color <- ifelse(is.null(getOption("axis.x.color")),
          getOption("axis.color"), getOption("axis.x.color"))
        axis.x.text.color <- ifelse(is.null(getOption("axis.x.text.color")),
          getOption("axis.text.color"), getOption("axis.x.text.color"))
        axis.x.cex <- ifelse(is.null(getOption("axis.x.cex")),
            getOption("axis.cex"), getOption("axis.x.cex"))
        adj <- .RSadj(axis.cex=axis.x.cex); axis.x.cex <- adj$axis.cex
        my.mgp <- par("mgp")  # save to restore
        mgp2 <- -0.275 + (0.9 * axis.x.cex)  # adjust label to axis distance
        par(mgp = c(my.mgp[1], mgp2, my.mgp[3]))  # labels closer to axis
        axis.Date(1, x.val, col=axis.x.color, cex.axis=axis.x.cex,
                  col.axis=axis.x.text.color, tck=-.02, ...)  # x-axis
        par(mgp = my.mgp)  # restore back to previous value
        .axes(NULL, y.val, axT1, axT2,
              rotate.x=rotate.x, rotate.y=rotate.y,
              offset=offset, y.only=TRUE, ...)  # y-axis
      }
    }
    else  # bubble1: 1-D scatter plot of categorical variable
      .axes(x.val, NULL, axT1, NULL,
            rotate.x=rotate.x, rotate.y=rotate.y, offset=offset, ...)

  }  # end xy.ticks

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
          x.val, xy.ticks, offset=offset,
          lab.x.cex=lab.x.cex, lab.y.cex=lab.y.cex, main.cex,
          n.lab.x.ln, n.lab.y.ln, xlab.adj, ylab.adj, ...)


  # ---------------
  # plot the values
  # ---------------

    # ------
    # colors

    n.clrs <- max(n.col, n.by)

  # prepare col.fill and col.color, values that enter into the analysis
  # convert any name of a color range to the colors, otherwise leave unchanged
  #   set NULL value to default

  if (!is.null(col.fill)) {
    if (length(col.fill) == 1) col.fill <- .color.range(col.fill, n.clrs)
  }
  else 
    col.fill <- getOption("pt.fill")  # default

  color.miss <- ifelse (is.null(col.color), TRUE, FALSE)
  if (!color.miss) {
    if (length(col.color) == 1) col.color <- .color.range(col.color, n.clrs)
  }
  else {
    if (stack  &&  col.fill != "transparent") {
      for (i in 1:n.clrs) col.color[i] <- "black"
    }
    else
      col.color <- getOption("pt.color")  # default
  }




  if (object %in% c("point", "both")) {

    color <- character(length=length(n.clrs))
    fill <- character(length=length(n.clrs))
    ltype <- character(length=n.clrs)
    for (i in 1:length(ltype)) ltype[i] <- "solid"

    if (n.clrs == 1) {
      color[1] <- col.color[1]
      fill[1] <- col.fill[1]
    }

#   else if (n.clrs == 2) {
#     color[1] <- col.color[1]
#     color[2] <- ifelse (length(col.color) > 1, col.color[2], col.color[1])
#     if (getOption("sub.theme") == "black") {
#       if (getOption("theme") != "gray")
#         color[2] <- getOption("bar.color")
#       else
#         color[2] <- getOption("lab.color")
#     }
#     if (col.fill == "transparent") fill[1] <- color[1]
#     if (object == "both") {
#       ltype[2] <- "dotted"
#       if (!(getOption("theme") %in% c("gray")))
#         fill[2] <- fill[1]
#       else {
#         fill[2] <- fill[1]
#         color[2] <- rgb(.15,.15,.15)
#       }
#     }  # end object is both
#     else { 
#       if (length(col.fill) == 1)
#         fill[2] <- "transparent"
#       else
#         fill[2] <- col.fill[2]
#     }
#   }  # end n.clrs=2

    else { # n.clrs > 1 
      if (length(col.fill) > 1) {
        fill <- col.fill  # use given color vector
      }
      else if (!is.ordered(by)) {
        if (getOption("theme") %in% c("gray", "white")) {
          if (col.fill != "transparent") fill <- getColors("grays", n=n.clrs)
        }
        else {
          if (col.fill != "transparent")
            fill <- getColors("hues", n=n.clrs)
          else
            fill <- "transparent"
          if (object == "both"  &&  color.miss  &&  fill != "transparent")
            color <- fill
        }
      }
      else {  # ordered default
        fill <- .color.range(.get.fill(), n.clrs)  # see if range
      }
    } # end n.clrs > 1

    
    #  kludge needed for Plot(c(x1,x2), y)
    if (length(col.color) < length(fill) &&  # more fill than color colors
        col.color == getOption("pt.color"))  # default
      color <- fill 
    else {
      color <- col.color  # color different than fill
    }

    if (length(col.color) == 1  &&  n.clrs > 1)
      if (!color.miss)
        for (i in 1:n.clrs) color[i] <- col.color
      else {
        if (col.color != "transparent")
          color <- getColors("hues", n=n.clrs)
        else
          color <- "transparent"
      }

    if (!stack  &&  color.miss  &&  fill != "transparent"  &&  n.clrs > 1)
      color <- fill


    trans.pts <- ifelse(is.null(col.trans),
                        getOption("trans.pt.fill"), col.trans)  # default

    # see if trans is customized for this analysis
    if (is.null(col.trans))  # no change, so no trans for Cleveland dp
      trans.pts <- ifelse (cleveland, 0, getOption("trans.pt.fill")) 
    else
      trans.pts <- col.trans
    if (trans.pts > 0  &&  fill[1] != "transparent")
      fill <- .maketrans(fill, (1-trans.pts)*256)

    # if (object != "point"  ||  size == 0) fill <- color  # no fill in legend
    fill[which(fill == "off")] <- "transparent"
    color[which(color == "off")] <- "transparent"

     if (area.fill == "on") area.fill <- getOption("violin.fill")
     if (object == "both") if (area.fill == "transparent") area.fill <- fill
     if (ln.width == 0) area.fill <- "transparent"



    # ----------
    # plot lines (and area.fill)
    if (object == "both") {

      if (n.xcol == 1  &&  n.ycol == 1) {
        if (n.by <= 1) {  # pure single panel, data in wide format

          if (ln.width > 0)  # plot line(s)
            if (date.ts || freq.poly || object == "both")  # object == "both"
                #lines(as.numeric(x[,1]), y[,1], col=col.segment, lwd=ln.width,
                lines(as.numeric(x[,1]), y[,1], col=color[1], lwd=ln.width,
                      ...)

          if (fill[1] != "transparent") # fill area
            polygon(c(x[1],x,x[length(x)]), c(min(y[,1]),y[,1],min(y[,1])),
                col=area.fill, border="transparent")
        }  # n.by is 1

        else {  # n.by > 1,  tidy format, all y data in same column
          for (i in 1:n.by) {  # only stack=TRUE makes sense
            xl <- subset(x, by==levels(by)[i])
            if (i > 1) yo <- yl
            yl <- subset(y, by==levels(by)[i])

            if (stack) {
              if (i == 1) {
                xx <- c(xl[1],xl,xl[length(xl)])
                yy <- c(min(yl[,1]),yl[,1],min(yl[,1]))
                if (fill[1] != "transparent")
                  polygon(xx, yy, col=area.fill[1], border="transparent")
              }
              if (i > 1) {
                yl[,1] <- yl[,1] + yo[,1]
                xx <- c(c(xl[1],xl,xl[length(xl)]),
                      rev(c(xl[1],xl,xl[length(xl)])))
                yy <- c(c(min(yl[,1]),yl[,1],min(yl[,1])),
                           rev(c(min(yo[,1]),yo[,1],min(yo[,1]))))
                if (fill[1] != "transparent")
                 polygon(xx, yy, col=area.fill[i], border="transparent")
              }
            }

            if (ln.width > 0) {
              if (stack)  # set line properties here, no option for user
                ln.width <- 1
              lines(xl, yl[,1], col=color[i], lty=ltype[i], lwd=ln.width, ...)
            }  # end ln.width > 0
          }
        }
      }  # end n.xcol and n.ycol = 1

      if (n.ycol > 1) {
        for (i in 1:n.ycol) {

          if (stack) {
            if (i == 1) {
              xx <- c(x[1],x,x[length(x)])
              yy <- c(min(y[,1]),y[,1],min(y[,1]))
              if (fill[1] != "transparent")
                polygon(xx, yy, col=area.fill[1], border="transparent")
            }
            if (i > 1) {
              y[,i] <- apply(y[,(i-1):i], 1, sum, na.rm=TRUE)  # sum for stacking
              xx <- c( c(x[1],x,x[length(x)]), rev(c(x[1],x,x[length(x)])) )
              yy <- c( c(min(y[,i]),y[,i],min(y[,i])),
                         rev(c(min(y[,i-1]),y[,i-1],min(y[,i-1]))) )
              if (fill[1] != "transparent")
                polygon(xx, yy, col=area.fill[i], border="transparent")
            }
          }

          if (ln.width > 0) {
              lines(x[,1], y[,i], col=color[i], lty=ltype[i], lwd=ln.width, ...)
          }  # end ln.width > 0

        }  # end i loop

      }  # end n.ycol > 1

      if (n.xcol > 1) {
        if (ln.width > 0) for (i in 1:n.xcol)
          lines(as.numeric(x.val),y[,1], col=fill[i], lwd=ln.width, ...)
      }

      # plot center line
      if (center.line != "off") {
        if (center.line == "mean") {
          m.y <- mean(y[,1], na.rm=TRUE)
          lbl <- " mean"
          lbl.cat <- "mean:"
        }
        else if (center.line == "median") {
          m.y <- median(y[,1], na.rm=TRUE)
          lbl <- " medn"
          lbl.cat <- "median:"
        }
        else if (center.line == "zero") {
          m.y <- 0
          lbl <- ""
          lbl.cat <- "zero:"
        }

        abline(h=m.y, col="gray50", lty="dashed")  # draw center line
        mtext(lbl, side=4, cex=.9, col="gray50", las=2, at=m.y, line=0.1)

        if (center.line == "zero") m.y <- median(y[,1], na.rm=TRUE)  # for runs

      }  # end center.line

      else {
        lbl.cat <- "median: "
        m.y <- median(y[,1], na.rm=TRUE)
      }

    }  # end both


    # -----------
    # plot points
    if (object %in% c("point", "both")) {

      if (is.null(by)) {

        if (smooth) {  # 2-D kernel density plot
          # grid lines only plot after the sp, so no effect here
         #abline(v=axT1, col=getOption("grid.x.color"),
         #       lwd=getOption("grid.lwd"), lty=getOption("grid.lty"))
         #if (!bubble1)
         #  abline(h=axT2, col=getOption("grid.y.color"),
         #         lwd=getOption("grid.lwd"), lty=getOption("grid.lty"))
          clr.den <- colorRampPalette(c(getOption("window.fill"),
                                         getOption("bar.fill.ordered")))
          smoothScatter(x, y, nrpoints=smooth.points, nbin=smooth.bins,
                        transformation=function(x) x^(smooth.exp),
                        colramp=clr.den, cex=smooth.size, add=TRUE)
        }

        else if (size > 0) {  # plot the individual points, plus means, segments, etc.

          if (jitter.x > 0)
            x[,1] <- jitter(x[,1], factor=jitter.x)
          if (jitter.y > 0)
            y[,1] <- jitter(y[,1], factor=jitter.y)

          if (n.xcol == 1  &&  n.ycol == 1) {

             if (length(out.ind) == 0)  # not outliers
                points(x[,1], y[,1], pch=shape, col=color[1], bg=fill[1],
                       cex=size.pt, ...)

             else {  # display outliers separately

                if (getOption("theme") == "gray")
                  if (any(size.pt > 0.9)) if (out.shape.miss) out.shape <- 23

                points(x[-out.ind,1], y[-out.ind,1],
                   pch=shape, col=color[1], bg=fill[1], cex=size.pt, ...)
                points(x[out.ind,1], y[out.ind,1],
                   pch=shape, col=out.color, bg=out.fill, cex=size.pt, ...)
                text(x[out.ind], y[out.ind], labels=ID[out.ind],
                   pos=1, offset=0.4, col=ID.color, cex=ID.size)
             }
          }

          else if (n.ycol == 1) {  # one y
            for (i in 1:n.xcol) {  # one to many x's
                if (getOption("theme") %in% c("gray", "white"))
                  if (n.clrs == 2  &&  i == 2) fill[i] <- "transparent"
                points(x[,i], y[,1], pch=shape, col=color[i], bg=fill[i],
                       cex=size.pt, ...)
            }
          }

          else  if (n.xcol == 1) {  # one x
            for (i in 1:n.ycol) {  # one to many y's
                if (getOption("theme") %in% c("gray", "white"))
                  if (n.clrs == 2  &&  i == 2) fill[i] <- "transparent"
                points(x[,1],y[,i], pch=shape, col=color[i], bg=fill[i],
                       cex=size.pt, ...)  # one x
            }
          }

          if (segments.y) {
            if (n.xcol == 1) # line segments from points to axis
              segments(x0=0, y0=y, x1=x, y1=y,
                       lty=1, lwd=.75, col=col.segment)
              #segments(x0=min(pretty(x)), y0=y, x1=x, y1=y,
                       #lty=1, lwd=.75, col=col.color)
            else if (n.xcol == 2)  # line segments between points
              segments(x0=x[,1], y0=y[,1], x1=x[,2], y1=y[,1],
                       lty=1, lwd=.75, col=col.segment)
          }

          if (!(stat %in% c("count", "prop", "%"))) {
            if (segments.x)
              segments(y0=par("usr")[3], x0=x, y1=y, x1=x, lty=1, lwd=.75,
                       col=col.segment)
          }
          else {
            if (segments.x)
              if (n.xcol == 1)
                 segments(y0=0, x0=x, y1=y, x1=x, lty=1, lwd=1, col=col.segment)
          }

          if (means  &&  stat == "data") {
            pch.avg <- ifelse(getOption("theme")!="gray", 21, 23)
            bck.g <- ifelse(getOption("theme")!="gray", "gray15", "gray30")
            if (grepl(".black", getOption("theme"), fixed=TRUE))
              bck.g <- "gray85"

            m.lvl <- numeric(length = 0)

            # plot means for factor x, num y
            if (!is.null(x.lvl) && is.null(y.lvl) && !unique.x) {
              for (i in (1:length(x.lvl)))
                m.lvl[i] <- mean(y[x==i], na.rm=TRUE)
              abline(h=m.lvl, col="gray50", lwd=.5)
              points(m.lvl, pch=pch.avg, bg=bck.g)
            }

            # plot means for num x, factor y
            if (is.null(x.lvl) && !is.null(y.lvl) && !unique.y) {
              for (i in (1:length(y.lvl)))
                m.lvl[i] <- mean(x[y==i], na.rm=TRUE)
              abline(v=m.lvl, col="gray50", lwd=.5)
              points(m.lvl, 1:length(y.lvl), pch=pch.avg, bg=bck.g)
            }
          }  # means
        }  # end not smooth


      }  # end is null by


      else {  # by grouping variable

        clr <- character(length(n.by))
        clr.tr <- character(length(n.by))  # translucent version of clr
        if (length(color) == 1)
          for (i in 1:n.by) clr[i] <- color  # all levels get same color
        else
          clr <- color

        shp <- integer(length(n.by))
        if (length(shape) == 1)
          for (i in 1:n.by) shp[i] <- shape
        else
          shp <- shape

        shape.dft <- c(21,23,22,24,25,7:14)  # shape defaults
        if (length(color)==1 && length(fill) == 1 && length(shape)==1)
          for (i in 1:n.by) shp[i] <- shape.dft[i]  #  default shapes

        for (i in 1:n.by) {
          x.lv <- subset(x, by==levels(by)[i])
          y.lv <- subset(y, by==levels(by)[i])
          points(x.lv, y.lv[,1], pch=shp[i], col=clr[i], bg=fill[i], cex=size.pt,
                 lwd=0.75, ...)
        }
        if (fill[1] == "transparent") fill <- color
        if (stack) {
          point.size <- 2.5 * axis.x.cex
          .plt.by.legend(levels(by), color, fill, shp=22, trans.pts, fill.bg,
                       usr, pt.size=point.size)
        }
        else
          .plt.by.legend(levels(by), color, fill, shp, trans.pts, fill.bg, usr)

        }  # end by

      }  # end plot points object is point or both


#     if (stack) fill <- color  # fill can be very translucent
      if (fill[1] == "transparent") fill <- color
      if (n.xcol > 1)  # horizontal legend, on x-axis
        .plt.legend(colnames(x), FALSE, color, fill, shape, fill.bg, usr,
                    lab.cex=lab.x.cex, pt.size=1.25, legend.title)
      if (n.ycol > 1)  # vertical legend, on y-axis
        .plt.legend(colnames(y), FALSE, color, fill, shape, fill.bg, usr,
                    lab.cex=lab.y.cex, pt.size=1.25, legend.title)

    }  # object is point, line, both


  else if (object %in% c("bubble", "sunflower")) {

    n.clrs <- 1  # for fit.line

    # colors
    if (is.null(col.low) || is.null(col.hi) || !bubble1) {
      if (col.fill[1] != "#46505A")  # default
        clr <- col.fill
      else
        clr <- getOption("bar.fill.ordered")
      clr.color <- col.color
    }
    else {  # 1-var bubble plot and BPFM can have a color gradient
      color.palette <- colorRampPalette(c(col.low, col.hi))
      clr <- color.palette(length(unique(x)))
      clr.color <- "gray70"
    }

    if (length(size) == 1) {  # no value for size specified, do counts
      mytbl <- table(x, y)  # get the counts, all x-y combinations
      n.count <- nrow(mytbl) * ncol(mytbl)
      if (prop) {
        count <- numeric(length=n.count)
        if (!is.null(y.lvl))
          mytbl <- prop.table(mytbl, 1)
        else
          mytbl <- mytbl / sum(mytbl)
      }
      else
        count <- integer(length=n.count)

      # melt the table of counts to a data frame with xx, yy, count
      xx <- integer(length=n.count)
      yy <- integer(length=n.count)
      k <- 0
      for (i in 1:nrow(mytbl)) {
        for (j in 1:ncol(mytbl)) {
          if (mytbl[i,j] != 0) {  # 0 plots to a single pixel, so remove
            k <- k + 1
            count[k] <- mytbl[i,j]
            xx[k] <- as.numeric(rownames(mytbl)[i])  # row names are factors
            yy[k] <- as.numeric(colnames(mytbl)[j])
          }
        }
      }
      if (prop) count <- round(count, 2)
      cords <- data.frame(xx, yy, count)


      if (is.null(radius)) radius <- .22

  # scale for regular R or RStudio
  adj <- .RSadj(radius=radius)  # reg R multiply by 1.6
  radius <- adj$radius

  # can use xyTable instead, though does not have coordinates for count=0
  #cnt <- xyTable(x,y)
  #symbols(cnt$x, cnt$y, circles=sz, inches=radius,
          #bg=clr, fg=clr.color, add=TRUE, ...)

      if (object == "bubble") {
        sz <- cords[,3]**power  # radius unscaled
        symbols(cords$xx, cords$yy, circles=sz, inches=radius,
            bg=clr, fg=clr.color, add=TRUE, ...)
        mxru <- max(sz)
        sz <- 2 * (sz/mxru) * radius  # scaled diameter (symbols does)
      }

      else if (object == "sunflower") {
        sunflowerplot(cords$xx, cords$yy, number=cords$count,
            seg.col=col.color, col=col.fill,
            col.axis=getOption("axis.x.color"),
            xlab=x.lab, ylab=y.lab, add=TRUE)
      }
    }  # end length(size) == 1

    else {  # size is a variable (unless size is constant and bubble specified)

      if (is.null(radius)) radius <- .1

      # scale for regular R or RStudio
      adj <- .RSadj(radius=radius)  # reg R multiply by 1.6
      radius <- adj$radius

      cords <- data.frame(x, y, size)

      cords <- na.omit(cords)
      sz <- cords[,3]**power  # radius unscaled
      symbols(cords[,1], cords[,2], circles=sz,
        inches=radius, bg=clr, fg=clr.color, add=TRUE, ...)
      mxru <- max(sz)
      sz <- 2 * (sz/mxru) * radius  # scaled diameter
    }


    if (size.cut  &&  object == "bubble") {

      # get q.ind before setting too small bubbles for text at NA
      if (size.cut > 1) {
        by.prob <- 1 / (size.cut - 1)
        bub.probs <- seq(0, 1, by.prob)
        qnt <- quantile(cords[,3], probs=bub.probs, type=3, na.rm=TRUE)
        qnt.TF <- logical(length(cords))
        for (i in 1:nrow(cords))
            qnt.TF[i] <- ifelse(cords[i,3] %in% qnt, TRUE, FALSE)
        q.ind <- which(qnt.TF)
      }
      else
        q.ind <- 1:nrow(cords)  # all bubbles get text

      # should get size of each amount just for those displayed (q.ind)
      sz.cex <- numeric(length=nrow(cords))
      for (i in 1:nrow(cords)) {
        sz.cex[i] <- getOption("axis.cex")  # cex target for text size
       # target for text size
        sz.txt <- strwidth(cords[i,3], units="inches", cex=sz.cex[i])
        while ((sz.txt - sz[i]) > -.03) {
          if (sz.cex[i] > 0.5) {  # need cex larger than 0.5
            sz.cex[i] <- sz.cex[i] - 0.05
            # actual
            sz.txt <- strwidth(cords[i,3], units="inches", cex=sz.cex[i])
          }
          else {
            cords[i,3] <- NA
            break;
          }
        }
        if (options("device") != "RStudioGD")
          sz.cex[i] <- sz.cex[i] * 1.3
      }

      # write bubble text
      if (!prop) {
        if (bubble.text != "transparent")
          text(cords[q.ind,1], cords[q.ind,2], cords[q.ind,3],
            cex=sz.cex[q.ind], col=bubble.text)
      }
      else {
        crd <- .fmt0(cords[,3],2)
        for (j in 1:length(crd))
          if (grepl("NA", crd[j], fixed=TRUE)) crd[j] <- " "
        if (bubble.text != "transparent")
          text(cords[,1], cords[,2], crd, cex=sz.cex, col=bubble.text)
      }
    }  # end amount

  }  # end bubble/sunflower


  # ellipse option
  if (do.ellipse) {

    for (i in 1:n.clrs) {
      if (n.clrs == 1) {  # one plot, all the data
          x.lv <- x[,1]
          y.lv <- y[,1]
      }

      else {  # multiple, pull out subset

        if (!is.null(by)) {  # multiple by plots
          x.lv <- subset(x, by==levels(by)[i])
          y.lv <- subset(y, by==levels(by)[i])
        }

        if (n.col > 1) {  # multiple variable plots
          if (n.xcol > 1) {
            x.lv <- x[,i]
            y.lv <- y[,1]
          }
          else {
            x.lv <- x[,1]
            y.lv <- y[,i]
          }
        }

        clr <- ifelse (length(color) == 1, color, color[i])

      }  # end multiple

      for (j in 1:length(ellipse)) {
        cxy <- cor(x.lv, y.lv, use="complete.obs")
        m.x <- mean(x.lv, na.rm=TRUE)
        m.y <- mean(y.lv, na.rm=TRUE)
        s.x <- sd(x.lv, na.rm=TRUE)
        s.y <- sd(y.lv, na.rm=TRUE)

        ln.type <- ifelse (n.clrs == 2 && i == 2, "dashed", "solid")
        corr <- ifelse (n.clrs == 1, cxy, cxy[1,1])
        col.border <- ifelse (n.clrs == 1, col.ellipse, clr)

        e <- ellipse(corr, scale=c(s.x, s.y), centre=c(m.x, m.y),
                     level=ellipse[j])
        polygon(e, border=col.border, col=ellipse.fill,
                lwd=ellipse.lwd, lty=ln.type)
      }  # jth ellipse
    }  # ith pattern
  }  # do.ellipse



  # fit line option
  if (fit.line != "off") {

    fit.remove <- ifelse (!is.null(out.ind), TRUE, FALSE)
    do.remove <- FALSE
    for (i.rem in 1:(as.numeric(fit.remove)+1)) {
      if (fit.remove) if (i.rem == 2) do.remove <- TRUE  # 2nd pass
      for (i in 1:n.clrs) {
        if (n.clrs == 1) {  # one plot, all the data
          if (!date.ts) {
            x.lv <- x[,1]
            y.lv <- y[,1]
          }
          else {
            x.lv <- as.numeric(x.val)
            y.lv <- as.numeric(y[,i])
          }
          clr <- col.fit.line
        }

        else {  # multiple, pull out subset

          if (!is.null(by)) {  # multiple by plots
            x.lv <- subset(x, by==levels(by)[i])
            y.lv <- subset(y, by==levels(by)[i])
          }

          if (n.col > 1) {  # multiple variable plots
            if (n.xcol > 1) {
              x.lv <- x[,i]
              y.lv <- y[,1]
            }
            else {
              x.lv <- x[,1]
              y.lv <- y[,i]
            }
          }

          clr <- ifelse (length(color) == 1, color, color[i])
        }  # end multiple

        ln.type <- "solid"
        if (n.clrs == 2 && i == 2) ln.type <- "dashed"

        if (!is.null(out.ind)) if (do.remove) {
          x.lv <- x.lv[-out.ind]
          y.lv <- y.lv[-out.ind]
          ln.type <- ifelse (ln.type != "dashed", "dashed", "solid")
          fit.se[1] <- 0  # for line w/o outliers, no se band
        }

        ok <- is.finite(x.lv) & is.finite(y.lv)
        if (any(ok)) {
          x.lv <- x.lv[ok]
          y.lv <- y.lv[ok]
          od <- order(x.lv)
          x.lv <- x.lv[od]
          y.lv <- y.lv[od]

          # fit line
          if (fit.line == "loess")
            l.ln <- loess(y.lv ~ x.lv)
          else if (fit.line == "lm")
            l.ln <- lm(y.lv ~ x.lv)
          f.ln <- fitted(l.ln, ...)
          lines(x.lv, f.ln, col=clr, lwd=fit.lwd, lty=ln.type)

          # se bands about fit line
          if (fit.se[1] != 0) {
            for (j in 1:length(fit.se)) {
              p.ln <- predict(l.ln, se=TRUE)
              prb <- (1 - fit.se[j]) / 2
              up.ln <- f.ln + (qt(prb,nrows-1) * p.ln$se.fit)
              dn.ln <- f.ln - (qt(prb,nrows-1) * p.ln$se.fit)
              # lines(x.lv, up.ln, col=clr, lwd=0.5, lty=ln.type)
              # lines(x.lv, dn.ln, col=clr, lwd=0.5, lty=ln.type)
              polygon(c(x.lv, rev(x.lv)), c(up.ln, rev(dn.ln)),
                      col=getOption("se.fill"), border="transparent")
            }  # end for each se plot
          }
        }

      }  # ith pattern
    }  # fit.remove
  }  # fit.line


  # add enhancements

  if (!is.null(add)) if (add[1] == "means") {
    add[1] <- "v.line"
    add[2] <- "h.line"
    x1 <- "mean.x"
    y1 <- "mean.y"
  }

  if (!is.null(x1)) {
    if (date.ts) x1 <- julian(x1)  # dates to date serial numbers
    if (length(which(x1 == "mean.x")) > 0) {
      x1[which(x1 == "mean.x")] <- mean(x, na.rm=TRUE)
      x1 <- as.numeric(x1)
    }
  }
  if (!is.null(y1)) {
    if (length(which(y1 == "mean.y")) > 0) {
      y1[which(y1 == "mean.y")] <- mean(y, na.rm=TRUE)
      y1 <- as.numeric(y1)
    }
  }

  if (!is.null(add)) {
      if (add[1] == "labels")
        text(x, y, labels=ID, pos=1, offset=0.4,
           pch=shape, col=getOption("add.color"), cex=getOption("add.cex"))
      else
        .plt.add (add, x1, x2, y1, y2,
             add.cex, add.lwd, add.lty, add.color, add.fill, add.trans)
  }


}  # end plt.main

