.plt.main <-
function(x, y, by=NULL, n_cat=getOption("n_cat"),
         cat.x=FALSE, num.cat.x=FALSE, cat.y=FALSE, num.cat.y=FALSE,
         object="point", stat="data",

         col_fill=getOption("pt_fill"),
         area_fill=getOption("pt_fill"),
         col_color=getOption("pt_color"),

         col.trans=NULL, col.segment=getOption("segment_color"),

         xy_ticks=TRUE,
         xlab=NULL, ylab=NULL, main=NULL, main_cex=NULL, sub=NULL,
         value_labels=NULL, label_max=20,
         rotate_x=0, rotate_y=0, offset=0.5, prop=FALSE, origin_x=NULL,

         size=NULL, shape="circle", means=TRUE,
         segments_y=FALSE, segments_x=FALSE, ln.width=2,

         smooth=FALSE, smooth_points=100, smooth_size=1,
         smooth_exp=0.25, smooth_bins=128,

         radius=0.15, power=0.6, size_cut=TRUE,
         bubble_text=getOption("bubble_text_color"),
         col.low=NULL, col.hi=NULL,

         ID=NULL, ID_color="gray50", ID_size=0.75, out_ind=NULL,
         out_fill, out_color, out_shape.miss,

         fit.line="off", col.fit.line="gray55",
         fit_lwd=getOption("fit.lw"),
         fit_se=1, se_fill="gray80",

         ellipse=FALSE, col.ellipse="lightslategray",
         ellipse_fill="off", ellipse_lwd,

         center_line="default", show_runs=FALSE, stack=FALSE,

         freq.poly=FALSE, jitter_x=0, jitter_y=0,

         xlab_adj=0, ylab_adj=0, bm.adj=0, lm.adj=0, tm.adj=0, rm.adj=0,
         legend_title=NULL, scale_x=NULL, scale_y=NULL,

         add=NULL, x1=NULL, x2=NULL, y1=NULL, y2=NULL,
         add_cex=NULL, add_lwd=1, add_lty="solid",
         add_color=NULL, add_fill=NULL, add_trans=NULL,

         quiet=getOption("quiet"), want.labels=TRUE, ...)  {


  fill_bg <- getOption("panel_fill")
  date.ts <- ifelse (.is.date(x[,1]), TRUE, FALSE)
  size.pt <- size * .9  # size is set in Plot.R, reduce a bit for here

  if (center_line == "default") if (date.ts) center_line <- "off"

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
    if (!is.null(value_labels)) value_labels <- gsub(" ", "\n", x.lvl)
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
  n_col <- max(n.xcol, n.ycol)
  nrows <- nrow(x)

  if (date.ts) {
    x.val <- x[,1]
    x <- as.matrix(x.val, ncol=1)
  }


  if (n_col > 1) center_line <- "off"   # no center_line for multiple plots

  if (is.null(x.lvl) && !is.null(y.lvl) && unique.y ||
      is.null(y.lvl) && !is.null(x.lvl) && unique.x) {
    cleveland <- TRUE
  }
  else
    cleveland <- FALSE

  # get lab_x_cex  lab_y_cex
  lab_cex <- getOption("lab_cex")
  lab_x_cex <- getOption("lab_x_cex")
  lab_y_cex <- getOption("lab_y_cex")
  lab_x_cex <- ifelse(is.null(lab_x_cex), lab_cex, lab_x_cex)
  # adj <- .RSadj(lab_cex=lab_x_cex); lab_x_cex <- adj$lab_cex
  lab_y_cex <- ifelse(is.null(lab_y_cex), lab_cex, lab_y_cex)
  # adj <- .RSadj(lab_cex=lab_y_cex); lab_y_cex <- adj$lab_cex

  if (date.ts) xx.lab <- xlab
# if (is.null(ylab))  # no ylab for Cleveland dot plot
#   if (is.null(x.lvl) && !is.null(y.lvl) && unique.y) ylab <- ""
  if (want.labels) {
    gl <- .getlabels(xlab, ylab, main, lab_x_cex=lab_x_cex,
                     lab_y_cex=lab_y_cex)
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

  if (is.null(col_fill)) col_fill <- "transparent"

  # by default display center_line only if runs, so detect if a run
  if (center_line == "default"  &&  !date.ts  &&  object == "both") {
    y.clean <- na.omit(y)
    m <- mean(y.clean)
    n.change <- 0
    for (i in 1:(length(y.clean)-1))
      if ((y.clean[i+1] > m) != (y.clean[i] > m)) n.change <- n.change+1
    if (n.change/(length(y.clean)-1) < .15)
      center_line <- "off"
    else
      center_line <- "median"
  }
  else  # default if not automatically assigned above
    if (!(center_line %in% c("off", "mean"))) center_line <- "median"

  # decimal digits
  digits_d <- .max.dd(y[,1]) + 1
  options(digits_d=digits_d)


  # -------------------------
  # plot
  # -------------------------
  # -------------------------

  # graphic system parameters

  # x.val is either any value_labels or x.lvl, or NULL if x is numeric
  mx.x.val.ln <- 1
  mx.y.val.ln <- 1
  if (!date.ts) {
    x.val <- NULL
    y.val <- y.lvl  # if not reset to x value labels
    max.lbl.y <- NULL
    if (!is.null(value_labels)) {
      x.val <- value_labels
      if (length(unique(y[,1])) > 1) {  # see if set y axis values to those of x
        if (length(unique(na.omit(x[,1]))) == length(unique(na.omit(y[,1])))) {
          if (all(sort(unique(x[,1])) == sort(unique(y[,1])))) {
            y.val <- value_labels
            v <- unlist(strsplit(value_labels, "\n", fixed=TRUE))
            max.lbl.y <- max(nchar(v))
          }
        }
      }
    }
    else {  # is null value_labels
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

  axis_y_cex <- ifelse(is.null(getOption("axis_y_cex")),
    getOption("axis_cex"), getOption("axis_y_cex"))

  if (!is.null(y.val)) {  # y-axis labels are characters
    yv <- unlist(strsplit(y.val, "\n", fixed=TRUE))
    max.y.width <- max(strwidth(yv, units="inches", cex=axis_y_cex))
    if (options("device") != "RStudioGD")  # not work in R, only RStudio
      max.y.width <- .09 * axis_y_cex * max(nchar(yv))
  }
  else {  # y-axis labels are numeric
    mn.y <- min(y, na.rm=TRUE)
    mx.y <- max(y, na.rm=TRUE)
    prety <- pretty(c(mn.y, mx.y))
    prety <- max(pretty(c(min(y, na.rm=TRUE), max(y, na.rm=TRUE))))
    mx.num <-  ifelse (!prop, as.character(prety), .fmt(prety, 2))
    max.y.width <- max(strwidth(mx.num, cex=axis_y_cex, units="inches"))
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
  margs <- .marg(max.y.width, y.lab, x.lab, main.lab, rotate_x,
                mx.x.val.ln, mx.y.val.ln,
                lab_x_cex=lab_x_cex, lab_y_cex=lab_y_cex)
  mm <- margs$lm  # left margin, lm is linear model
  tm <- margs$tm
  rm <- margs$rm
  bm <- margs$bm
  n.lab_x.ln <- margs$n.lab_x.ln
  n.lab_y.ln <- margs$n.lab_y.ln

  if (n.xcol > 1  ||  n.ycol > 1  ||  !is.null(by)) {  # vertical legend room
    if (n.xcol > 1) nm.v <- nm.x
    if (n.ycol > 1) nm.v <- nm.y
    if (!is.null(by)) nm.v <- by.name
    big.nm <- max(nchar(nm.v))
    if (big.nm > 6) rm <- rm + (.05 * (big.nm - 6))
    rm <- rm + .25  + (.65 * axis_y_cex)
    if (axis_y_cex > 1) if (!is.null(by)) rm <- rm + .1  # kludge
  }
  if (object == "both")
    if (center_line != "off") rm <- rm + .4  # room for center_line label

  if (offset > 0.5) bm <- bm + (-0.05 + 0.2 * offset)  # offset kludge

  rm <- rm + 0.10  # make room when the last axis date > last data value

  if ((options("device") != "RStudioGD")  &&  !is.null(by)) rm <- rm + .3

  # user manual adjustment
  bm <- bm + bm.adj
  mm <- mm + lm.adj
  tm <- tm + tm.adj
  rm <- rm + rm.adj

  orig.params <- par(no.readonly=TRUE)
  on.exit(par(orig.params))

  par(bg=getOption("window_fill"))
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
  if (!is.null(scale_x)) {
     mn.x <- min(mn.x, scale_x[1])
     mx.x <- max(mx.x, scale_x[2])
  }
  if (!is.null(scale_y)) {
     mn.y <- min(mn.y, scale_y[1])
     mx.y <- max(mx.y, scale_y[2])
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

    if (is.null(origin_x)) {  # set default for minimum x-value displayed
      if (stat %in% c("count", "proportion", "%"))
        origin_x <- 0
      else
        origin_x <- mn.x
    }
    if (stat != "data" && (!all(y == 0))) mx.y <- mx.y + (.08 * (mx.y-mn.y))

    region <- matrix(c(origin_x, mx.x, mn.y, mx.y), nrow=2, ncol=2)


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
    if (!is.null(x.val)) x.val <- .abbrev(x.val, label_max)
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
        axT1 <- pretty(origin_x:mx.x, n=n.best)
      }
      else {
        if (is.null(scale_x))
          axT1 <- pretty(c(origin_x, x))  # else numeric, so all the ticks
        else
          axT1 <- axTicks(1, axp=scale_x)
      }
    }
    else
      axT1 <-axTicks(1)  
  }

  if (cat.y) {
    if (!is.null(y.lvl)) axT2 <- 1:length(y.lvl)
    if (!is.null(y.val)) y.val <- .abbrev(y.val, label_max)
    if (num.cat.y) axT2 <- sort(unique(y))
  }
  else {
    if (is.null(scale_y))
      axT2 <- pretty(c(mn.y, mx.y))  # more extreme values than axTicks
    else
      axT2 <- axTicks(2, axp=scale_y)
  }

  # background color
  rect(usr[1], usr[3], usr[2], usr[4], col=fill_bg, border="transparent")

  # grid lines (put before box color around plot)
  .grid("v", axT1)
  if (!bubble1) .grid("h", axT2)

  # box around plot
  rect(usr[1], usr[3], usr[2], usr[4],
    col="transparent", border=getOption("panel_color"),
    lwd=getOption("panel_lwd"), lty=getOption("panel_lty"))

  # axes
  if (xy_ticks) {
    if (!bubble1) {
      if (!date.ts) {  # get ticks for both axes
        .axes(x.val, y.val, axT1, axT2,
              rotate_x=rotate_x, rotate_y=rotate_y, offset=offset, ...)
      }
      else {  # time
        axis_x_color <- ifelse(is.null(getOption("axis_x_color")),
          getOption("axis_color"), getOption("axis_x_color"))
        axis_x_text_color <- ifelse(is.null(getOption("axis_x_text_color")),
          getOption("axis_text_color"), getOption("axis_x_text_color"))
        axis_x_cex <- ifelse(is.null(getOption("axis_x_cex")),
            getOption("axis_cex"), getOption("axis_x_cex"))
        adj <- .RSadj(axis_cex=axis_x_cex); axis_x_cex <- adj$axis_cex
        my.mgp <- par("mgp")  # save to restore
        mgp2 <- -0.275 + (0.9 * axis_x_cex)  # adjust label to axis distance
        par(mgp = c(my.mgp[1], mgp2, my.mgp[3]))  # labels closer to axis
        axis.Date(1, x.val, col=axis_x_color, cex.axis=axis_x_cex,
                  col.axis=axis_x_text_color, tck=-.02, ...)  # x-axis
        par(mgp = my.mgp)  # restore back to previous value
        .axes(NULL, y.val, axT1, axT2,
              rotate_x=rotate_x, rotate_y=rotate_y,
              offset=offset, y.only=TRUE, ...)  # y-axis
      }
    }
    else  # bubble1: 1-D scatter plot of categorical variable
      .axes(x.val, NULL, axT1, NULL,
            rotate_x=rotate_x, rotate_y=rotate_y, offset=offset, ...)

  }  # end xy_ticks

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
          x.val, xy_ticks, offset=offset,
          lab_x_cex=lab_x_cex, lab_y_cex=lab_y_cex, main_cex,
          n.lab_x.ln, n.lab_y.ln, xlab_adj, ylab_adj, ...)


  # ---------------
  # plot the values
  # ---------------

    # ------
    # colors

    n.clrs <- max(n_col, n.by)

  # prepare col_fill and col_color, values that enter into the analysis
  # convert any name of a color range to the colors, otherwise leave unchanged
  #   set NULL value to default

  if (!is.null(col_fill)) {
    if (length(col_fill) == 1) col_fill <- .color_range(col_fill, n.clrs)
  }
  else 
    col_fill <- getOption("pt_fill")  # default

  color_miss <- ifelse (is.null(col_color), TRUE, FALSE)
  if (!color_miss) {
    if (length(col_color) == 1) col_color <- .color_range(col_color, n.clrs)
  }
  else {
    if (stack  &&  col_fill != "transparent") {
      for (i in 1:n.clrs) col_color[i] <- "black"
    }
    else
      col_color <- getOption("pt_color")  # default
  }

  if (object %in% c("point", "both")) {

    color <- character(length=length(n.clrs))
    fill <- character(length=length(n.clrs))
    ltype <- character(length=n.clrs)
    for (i in 1:length(ltype)) ltype[i] <- "solid"

    if (n.clrs == 1) {
      color[1] <- col_color[1]
      fill[1] <- col_fill[1]
    }

#   else if (n.clrs == 2) {
#     color[1] <- col_color[1]
#     color[2] <- ifelse (length(col_color) > 1, col_color[2], col_color[1])
#     if (getOption("sub_theme") == "black") {
#       if (getOption("theme") != "gray")
#         color[2] <- getOption("bar_color")
#       else
#         color[2] <- getOption("lab_color")
#     }
#     if (col_fill == "transparent") fill[1] <- color[1]
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
#       if (length(col_fill) == 1)
#         fill[2] <- "transparent"
#       else
#         fill[2] <- col_fill[2]
#     }
#   }  # end n.clrs=2

    else { # n.clrs > 1 
      if (length(col_fill) > 1) {
        fill <- col_fill  # use given color vector
      }
      else if (!is.ordered(by)) {
        if (getOption("theme") %in% c("gray", "white")) {
          if (col_fill != "transparent") fill <- getColors("grays", n=n.clrs)
        }
        else {
          if (col_fill != "transparent")
            fill <- getColors("hues", n=n.clrs)
          else
            fill <- "transparent"
          if (object == "both"  &&  color_miss  &&  fill != "transparent")
            color <- fill
        }
      }
      else {  # ordered default
        fill <- .color_range(.get_fill(), n.clrs)  # see if range
      }
    } # end n.clrs > 1
    
    #  kludge needed for Plot(c(x1,x2), y)
    if (length(col_color) < length(fill) &&  # more fill than color colors
        col_color == getOption("pt_color"))  # default
      color <- fill 
    else {
      color <- col_color  # color different than fill
    }

    if (length(col_color) == 1  &&  n.clrs > 1)
      if (!color_miss)
        for (i in 1:n.clrs) color[i] <- col_color
      else {
        if (col_color != "transparent")
          color <- getColors("hues", n=n.clrs)
        else
          color <- "transparent"
      }

    if (!stack  &&  color_miss  &&  fill[1] != "transparent"  &&  n.clrs > 1)
      color <- fill

    trans_pts <- ifelse(is.null(col.trans),
                        getOption("trans_pt_fill"), col.trans)  # default

    # see if trans is customized for this analysis
    if (is.null(col.trans))  # no change, so no trans for Cleveland dp
      trans_pts <- ifelse (cleveland, 0, getOption("trans_pt_fill")) 
    else
      trans_pts <- col.trans
    if (trans_pts > 0  &&  fill[1] != "transparent")
      fill <- .maketrans(fill, (1-trans_pts)*256)

    # if (object != "point"  ||  size == 0) fill <- color  # no fill in legend
    fill[which(fill == "off")] <- "transparent"
    color[which(color == "off")] <- "transparent"

     if (area_fill == "on") area_fill <- getOption("violin_fill")
     if (object == "both") if (area_fill == "transparent") area_fill <- fill
     if (ln.width == 0) area_fill <- "transparent"


    # ----------
    # plot lines (and area_fill)
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
                col=area_fill, border="transparent")
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
                  polygon(xx, yy, col=area_fill[1], border="transparent")
              }
              if (i > 1) {
                yl[,1] <- yl[,1] + yo[,1]
                xx <- c(c(xl[1],xl,xl[length(xl)]),
                      rev(c(xl[1],xl,xl[length(xl)])))
                yy <- c(c(min(yl[,1]),yl[,1],min(yl[,1])),
                           rev(c(min(yo[,1]),yo[,1],min(yo[,1]))))
                if (fill[1] != "transparent")
                 polygon(xx, yy, col=area_fill[i], border="transparent")
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
                polygon(xx, yy, col=area_fill[1], border="transparent")
            }
            if (i > 1) {
              y[,i] <- apply(y[,(i-1):i], 1, sum, na.rm=TRUE)  # sum for stacking
              xx <- c( c(x[1],x,x[length(x)]), rev(c(x[1],x,x[length(x)])) )
              yy <- c( c(min(y[,i]),y[,i],min(y[,i])),
                         rev(c(min(y[,i-1]),y[,i-1],min(y[,i-1]))) )
              if (fill[1] != "transparent")
                polygon(xx, yy, col=area_fill[i], border="transparent")
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
      if (center_line != "off") {
        if (center_line == "mean") {
          m.y <- mean(y[,1], na.rm=TRUE)
          lbl <- " mean"
          lbl.cat <- "mean:"
        }
        else if (center_line == "median") {
          m.y <- median(y[,1], na.rm=TRUE)
          lbl <- " medn"
          lbl.cat <- "median:"
        }
        else if (center_line == "zero") {
          m.y <- 0
          lbl <- ""
          lbl.cat <- "zero:"
        }

        abline(h=m.y, col="gray50", lty="dashed")  # draw center line
        mtext(lbl, side=4, cex=.9, col="gray50", las=2, at=m.y, line=0.1)

        if (center_line == "zero") m.y <- median(y[,1], na.rm=TRUE)  # for runs

      }  # end center_line

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
         #abline(v=axT1, col=getOption("grid_x_color"),
         #       lwd=getOption("grid_lwd"), lty=getOption("grid_lty"))
         #if (!bubble1)
         #  abline(h=axT2, col=getOption("grid_y_color"),
         #         lwd=getOption("grid_lwd"), lty=getOption("grid_lty"))
          clr.den <- colorRampPalette(c(getOption("window_fill"),
                                         getOption("bar_fill_ordered")))
          smoothScatter(x, y, nrpoints=smooth_points, nbin=smooth_bins,
                        transformation=function(x) x^(smooth_exp),
                        colramp=clr.den, cex=smooth_size, add=TRUE)
        }

        else if (size > 0) {  # plot the individual points, plus means, segments, etc.

          if (jitter_x > 0)
            x[,1] <- jitter(x[,1], factor=jitter_x)
          if (jitter_y > 0)
            y[,1] <- jitter(y[,1], factor=jitter_y)

          if (n.xcol == 1  &&  n.ycol == 1) {

             if (length(out_ind) == 0)  # not outliers
                points(x[,1], y[,1], pch=shape, col=color[1], bg=fill[1],
                       cex=size.pt, ...)

             else {  # display outliers separately

                if (getOption("theme") == "gray")
                  if (any(size.pt > 0.9)) if (out_shape.miss) out_shape <- 23

                points(x[-out_ind,1], y[-out_ind,1],
                   pch=shape, col=color[1], bg=fill[1], cex=size.pt, ...)
                points(x[out_ind,1], y[out_ind,1],
                   pch=shape, col=out_color, bg=out_fill, cex=size.pt, ...)
                text(x[out_ind], y[out_ind], labels=ID[out_ind],
                   pos=1, offset=0.4, col=ID_color, cex=ID_size)
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

          if (segments_y) {
            if (n.xcol == 1) # line segments from points to axis
              segments(x0=0, y0=y, x1=x, y1=y,
                       lty=1, lwd=.75, col=col.segment)
              #segments(x0=min(pretty(x)), y0=y, x1=x, y1=y,
                       #lty=1, lwd=.75, col=col_color)
            else if (n.xcol == 2)  # line segments between points
              segments(x0=x[,1], y0=y[,1], x1=x[,2], y1=y[,1],
                       lty=1, lwd=.75, col=col.segment)
          }

          if (!(stat %in% c("count", "prop", "%"))) {
            if (segments_x)
              segments(y0=par("usr")[3], x0=x, y1=y, x1=x, lty=1, lwd=.75,
                       col=col.segment)
          }
          else {
            if (segments_x)
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
          point.size <- 2.5 * axis_x_cex
          .plt.by.legend(levels(by), color, fill, shp=22, trans_pts, fill_bg,
                       usr, pt.size=point.size)
        }
        else
          .plt.by.legend(levels(by), color, fill, shp, trans_pts, fill_bg, usr)

        }  # end by

      }  # end plot points object is point or both


#     if (stack) fill <- color  # fill can be very translucent
      if (fill[1] == "transparent") fill <- color
      if (n.xcol > 1)  # horizontal legend, on x-axis
        .plt.legend(colnames(x), FALSE, color, fill, shape, fill_bg, usr,
                    lab_cex=lab_x_cex, pt.size=1.25, legend_title)
      if (n.ycol > 1)  # vertical legend, on y-axis
        .plt.legend(colnames(y), FALSE, color, fill, shape, fill_bg, usr,
                    lab_cex=lab_y_cex, pt.size=1.25, legend_title)

    }  # object is point, line, both


  else if (object %in% c("bubble", "sunflower")) {

    n.clrs <- 1  # for fit.line

    # colors
    if (is.null(col.low) || is.null(col.hi) || !bubble1) {
      if (col_fill[1] != "#46505A")  # default
        clr <- col_fill
      else
        clr <- getOption("bar_fill_ordered")
      clr_color <- col_color
    }
    else {  # 1-var bubble plot and BPFM can have a color gradient
      color_palette <- colorRampPalette(c(col.low, col.hi))
      clr <- color_palette(length(unique(x)))
      clr_color <- "gray70"
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
          #bg=clr, fg=clr_color, add=TRUE, ...)

      if (object == "bubble") {
        sz <- cords[,3]**power  # radius unscaled
        symbols(cords$xx, cords$yy, circles=sz, inches=radius,
            bg=clr, fg=clr_color, add=TRUE, ...)
        mxru <- max(sz)
        sz <- 2 * (sz/mxru) * radius  # scaled diameter (symbols does)
      }

      else if (object == "sunflower") {
        sunflowerplot(cords$xx, cords$yy, number=cords$count,
            seg.col=col_color, col=col_fill,
            col.axis=getOption("axis_x_color"),
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
        inches=radius, bg=clr, fg=clr_color, add=TRUE, ...)
      mxru <- max(sz)
      sz <- 2 * (sz/mxru) * radius  # scaled diameter
    }


    if (size_cut  &&  object == "bubble") {

      # get q.ind before setting too small bubbles for text at NA
      if (size_cut > 1) {
        by.prob <- 1 / (size_cut - 1)
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
        sz.cex[i] <- getOption("axis_cex")  # cex target for text size
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
        if (bubble_text != "transparent")
          text(cords[q.ind,1], cords[q.ind,2], cords[q.ind,3],
            cex=sz.cex[q.ind], col=bubble_text)
      }
      else {
        crd <- .fmt0(cords[,3],2)
        for (j in 1:length(crd))
          if (grepl("NA", crd[j], fixed=TRUE)) crd[j] <- " "
        if (bubble_text != "transparent")
          text(cords[,1], cords[,2], crd, cex=sz.cex, col=bubble_text)
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

        if (n_col > 1) {  # multiple variable plots
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
        polygon(e, border=col.border, col=ellipse_fill,
                lwd=ellipse_lwd, lty=ln.type)
      }  # jth ellipse
    }  # ith pattern
  }  # do.ellipse



  # fit line option
  if (fit.line != "off") {

    fit.remove <- ifelse (!is.null(out_ind), TRUE, FALSE)
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

          if (n_col > 1) {  # multiple variable plots
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

        if (!is.null(out_ind)) if (do.remove) {
          x.lv <- x.lv[-out_ind]
          y.lv <- y.lv[-out_ind]
          ln.type <- ifelse (ln.type != "dashed", "dashed", "solid")
          fit_se[1] <- 0  # for line w/o outliers, no se band
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
          lines(x.lv, f.ln, col=clr, lwd=fit_lwd, lty=ln.type)

          # se bands about fit line
          if (fit_se[1] != 0) {
            for (j in 1:length(fit_se)) {
              p.ln <- predict(l.ln, se=TRUE)
              prb <- (1 - fit_se[j]) / 2
              up.ln <- f.ln + (qt(prb,nrows-1) * p.ln$se.fit)
              dn.ln <- f.ln - (qt(prb,nrows-1) * p.ln$se.fit)
              # lines(x.lv, up.ln, col=clr, lwd=0.5, lty=ln.type)
              # lines(x.lv, dn.ln, col=clr, lwd=0.5, lty=ln.type)
              polygon(c(x.lv, rev(x.lv)), c(up.ln, rev(dn.ln)),
                      col=getOption("se_fill"), border="transparent")
            }  # end for each se plot
          }
        }

      }  # ith pattern
    }  # fit.remove
  }  # fit.line


  # annotations 

  if (!is.null(add)) if (add[1] == "means") {
    add[1] <- "v_line"
    add[2] <- "h_line"
    x1 <- "mean_x"
    y1 <- "mean_y"
  }

  if (!is.null(x1)) {
    if (date.ts) x1 <- julian(x1)  # dates to date serial numbers
    if (length(which(x1 == "mean_x")) > 0) {
      x1[which(x1 == "mean_x")] <- mean(x, na.rm=TRUE)
      x1 <- as.numeric(x1)
    }
  }
  if (!is.null(y1)) {
    if (length(which(y1 == "mean_y")) > 0) {
      y1[which(y1 == "mean_y")] <- mean(y, na.rm=TRUE)
      y1 <- as.numeric(y1)
    }
  }

  if (!is.null(add)) {
      if (add[1] == "labels")
        text(x, y, labels=ID, pos=1, offset=0.4,
           pch=shape, col=getOption("add_color"), cex=getOption("add_cex"))
      else
        .plt.add (add, x1, x2, y1, y2,
             add_cex, add_lwd, add_lty, add_color, add_fill, add_trans)
  }


}  # end plt.main

