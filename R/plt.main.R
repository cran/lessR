.plt.main <-
function(x, y, by=NULL,
         cat.x=FALSE, cat.y=FALSE,
         object="point", stat="data",

         fill=getOption("pt_fill"),
         area_fill="transparent",
         color=getOption("pt_color"),

         pts_trans=0, col.segment=getOption("segment_color"),

         xy_tics=TRUE,
         xlab=NULL, ylab=NULL, main=NULL, main_cex=NULL, sub=NULL,
         rotate_x=0, rotate_y=0, offset=0.5, prop=FALSE,
         origin_x=NULL, origin_y=NULL,

         size=NULL, ln.width=1, shape=21, means=TRUE,
         segments=FALSE, segments_y=FALSE, segments_x=FALSE,

         smooth=FALSE, smooth_points=100, smooth_size=1,
         smooth_exp=0.25, smooth_bins=128,

         radius=0.15, power=0.6, size_cut=TRUE,
         bubble_text=getOption("bubble_text_color"),
         col.low=NULL, col.hi=NULL,

         ID=NULL, ID_color="gray50", ID_size=0.60, out_ind=NULL,
         out_fill, out_color, out_shape, out_shape.miss,

         fit.line="off", fit_power=1, fit_color="gray55",
         fit_lwd=getOption("fit.lw"), fit_new=NULL,
         fit_se=1, se_fill="gray80", plot_errors=FALSE,

         ellipse=FALSE, ellipse_color="lightslategray",
         ellipse_fill="off", ellipse_lwd,

         run=FALSE, center_line="off", stack=FALSE,
         ts_unit=NULL, ts_agg=FALSE, do.agg="sum", ts_ahead=0,
         ts_fit=FALSE, n_date_tics=NULL,
         y.fit, y.hat, x.fit, x.hat, y.upr, y.lwr,
         mxf.x, mnf.y, mxf.y,

         freq.poly=FALSE, jitter_x=NULL, jitter_y=NULL,

         xlab_adj=0, ylab_adj=0, bm.adj=0, lm.adj=0,
         tm.adj=0, rm.adj=0,
         scale_x=NULL, scale_y=NULL, pad_x=c(0,0), pad_y=c(0,0),
         legend_title=NULL,

         add=NULL, x1=NULL, x2=NULL, y1=NULL, y2=NULL,
         add_cex=NULL, add_lwd=1, add_lty="solid",
         add_color=NULL, add_fill=NULL, add_trans=NULL,

         quiet=FALSE, want.labels=TRUE, bubble.title=TRUE, ...)  {


  # -------------------------
  # preliminaries
  # -------------------------

  fill_bg <- getOption("panel_fill")
  date.var <- ifelse (.is.date(x[,1]), TRUE, FALSE)
  if (is.null(size)) size <- 1
  size.pt <- size * .9  # size is set in Plot.R, reduce a bit for here
  theme <- getOption("theme")

  # by.bub means that size is a variable to be plotted in by levels
  if (length(size) > 1  &&  !is.null(by)) {
    by.bub <- TRUE
    object <- "point"  # change from bubble to plot to get to by code
  }
  else
    by.bub <- FALSE


  # --------------------------
  # data structures dimensions
  # --------------------------

  # x and y come across here in their natural state, within each data frame
  # a time series has dates for x and numeric for y, factors are factors, etc

  # want labels set just for ttestPower, which provides its own labels
  # both x and y are plotted, even if only a single variable
  # for a 1-D bubble plot of a single factor var, y was set to 0's
  bubble1 <- ifelse (length(unique(y[,1])) == 1, TRUE, FALSE)

  unique.x <- ifelse (length(unique(x[,1])) == length(x[,1]), TRUE, FALSE)
  unique.y <- ifelse (length(unique(y[,1])) == length(y[,1]), TRUE, FALSE)
  n.by <- ifelse (is.null(by), 0, nlevels(by))

  do.ellipse <- ifelse (ellipse[1] > 0, TRUE, FALSE)

  # dimensions
  n.xcol <- ncol(x)
  n.ycol <- ncol(y)
  n.col <- max(n.xcol, n.ycol)
  nrows <- nrow(x)

  # ---------------------
  # axis labels and title
  # ---------------------

  # size is a variable, these values passed to .plt.main
  # bubble.title=FALSE passed from .plt.bins to suppress title
  # bubble plot can be directly from object="point"
  if (length(size) > 1  &&  bubble.title) {
    sz.nm <- getOption("sizename")
    main.lab <- bquote(paste(italic(.(sz.nm)), ": Bubble size from ",
      .(min(size)), " to ", .(max(size)), sep=""))
  }
  else
    main.lab <- NULL

  # get lab_x_cex  lab_y_cex    Synchronize with Plot() code, put in own sub
  lab_cex <- getOption("lab_cex")
  lab_x_cex <- getOption("lab_x_cex")
  lab_y_cex <- getOption("lab_y_cex")
  lab_x_cex <- ifelse (is.null(lab_x_cex), lab_cex, lab_x_cex)
  lab_y_cex <- ifelse (is.null(lab_y_cex), lab_cex, lab_y_cex)

  # get x.lab and y.lab
  if (date.var) xx.lab <- xlab
  if (want.labels) {
    gl <- .getlabels(xlab, ylab, main, lab_x_cex=lab_x_cex,
                     lab_y_cex=lab_y_cex)
    x.name <- gl$xn; x.lbl <- gl$xl; x.lab <- gl$xb
    y.name <- gl$yn; y.lab <- gl$yb

    if (is.null(main.lab)) main.lab <- gl$mb
    sub.lab <- gl$sb
    by.name <- getOption("byname")
  }
  else {
    x.lab <- xlab
    y.lab <- ylab
    if (is.null(main.lab)) main.lab <- main
    sub.lab <- sub
    x.name <- NULL
  }
  if (!is.null(x.lab)) if (n.xcol > 1  &&  substr(x.lab, 1, 2) == "c(")
      x.lab <- NULL  # e.g., get rid of == "c(Female, Male)"
  if (date.var && is.null(xx.lab)) x.lab <- ""

  # set title for bubble plot if proportions
  if (object == "bubble"  &&  prop  &&  is.null(main)  &&  cat.y) {
    main.lab <- paste("Percentage of", y.name, "\nwithin each level of", x.name)
    main <- "not null"
  }

  if (!is.null(x.name)) if (x.name == "Index") {
    if (n.ycol > 1) y.lab <- ""  # labels are on the legend
    if (!is.null(x.lbl)) y.lab <- paste(x.name, ": ", x.lbl, sep="")
  }
  # ---------------------


  # all processing in terms of numeric variables
  # categorical variables x and/or y are converted to factors in Plot()
  # convert factors to numeric, save levels, so x and y are always numeric
  # x will always be a matrix
  x.lvl <- NULL; y.lvl <- NULL  # if remain null, then not factors
  y.frcst <- NULL
  nm.x <- names(x)
  if (cat.x) {
    x.lvl <- levels(x[,1])
    x <- as.matrix(as.integer(unclass(x[,1])))
  }
  else if (!date.var) {
    x <- as.matrix(x)
    colnames(x) <- nm.x
  }
  else {  # date.var
    if (n.by == 0)
      x.dates <- x[,1]  # save actual dates for later
    else {  # x-axis tics just for one level of by
      cnt <- sum(by == levels(by)[1])
      x.dates <- x[1:cnt,1]  # assumes by is sorted by level
    }
  if (ts_ahead > 0) x.dates <- c(x.dates, x.hat[,1])

   x <- as.matrix(x[,1], ncol=1)  # x to numeric version of dates
  }  # end if date.var

  # y to a matrix
  nm.y <- names(y)
  if (cat.y) {
    y.lvl <- levels(y[,1])  #  put into alphabetical order
    y <- as.matrix(as.integer(unclass(y[,1])))
  }
  else if (!date.var) {
    nm.y <- names(y)
    y <- as.matrix(y)
    colnames(y) <- nm.y
  }
  else {
    y <- as.matrix(y)
  }

  # y decimal digits
  digits_d <- .max.dd(y[,1]) + 1
  options(digits_d=digits_d)

  if (n.col > 1) center_line <- "off"   # no center_line for multiple plots

  cleveland <- FALSE
  if (!cat.x && cat.y && unique.y  ||  !cat.y && cat.x && unique.x)
    cleveland <- TRUE


  # ----------------------
  # set up the plot region
  # ----------------------

  # graphic system parameters
  # x.val is either x.lvl, or NULL if x is numeric
  mx.x.val.ln <- 1
  mx.y.val.ln <- 1
  if (!date.var) {
    x.val <- x.lvl  # x.val is NULL if x is numeric, ignored
    y.val <- y.lvl  # y.val ignored if y is numeric
  }
  else {  # date.var
    x.val <- x[,1]
    y.val <- NULL
  }

  mn.x <- ifelse (cat.x, 1, min(x, na.rm=TRUE))
  if (ts_ahead > 0) {  # forecast margin adjustments
    mx.x <- mxf.x
    mx.y <- mxf.y
    mn.y <- mnf.y
  }
  else {
    mx.x <- ifelse (cat.x, length(x.lvl), max(x, na.rm=TRUE))
    mn.y <- ifelse (cat.y, 1, min(y, na.rm=TRUE))
    mx.y <- ifelse (cat.y, length(y.lvl), max(y, na.rm=TRUE))
  }


  # -----------
  # set margins

  # get max number of lines in x value labels
  if (cat.x) {
    stuff <- .get.val.ln(x.lvl, x.name)
    mx.x.val.ln <- stuff$mx.val.ln
  }

  # get max number of lines in y value labels
  if (cat.y  &&  y.name != "row.names"  && !cleveland) {
    stuff <- .get.val.ln(y.val, y.name)
    mx.y.val.ln <- stuff$mx.val.ln
  }

  # --------- get size of y-axis labels
  axis_y_cex <- ifelse (is.null(getOption("axis_y_cex")),
    getOption("axis_cex"), getOption("axis_y_cex"))

  if (cat.y) {  # y-axis labels are characters
    yv <- unlist(strsplit(y.val, "\n", fixed=TRUE))
    max.y.width <- max(strwidth(yv, units="inches", cex=axis_y_cex))
    if (options("device") != "RStudioGD")  # not work in R, only RStudio
      max.y.width <- .09 * axis_y_cex * max(nchar(yv))
  }
  else {  # y-axis labels are numeric
    prety <- pretty(c(mn.y, mx.y))
    ind <- which(nchar(prety) == max(nchar(prety)))[1]  # get largest nchar
    mx.num <-  ifelse (!prop, as.character(prety[ind]), .fmt(prety, 2))
    max.y.width <- max(strwidth(mx.num, cex=axis_y_cex, units="inches"))
  }
  # ---------

  margs <- .plt.marg(max.y.width, y.lab, x.lab, main.lab, sub.lab,
                rotate_x, mx.x.val.ln, mx.y.val.ln,
                lab_x_cex=lab_x_cex, lab_y_cex=lab_y_cex)
  mm <- margs$lm  # left margin, lm is linear model
  tm <- margs$tm
  rm <- margs$rm
  bm <- margs$bm
  n.lab_x.ln <- margs$n.lab_x.ln
  n.lab_y.ln <- margs$n.lab_y.ln

  # room in rm for the vertical legend
  if (n.xcol > 1  ||  n.ycol > 1  ||  !is.null(by)) {
    if (n.xcol > 1) nm.v <- nm.x
    if (n.ycol > 1) nm.v <- nm.y
    if (!is.null(by)) nm.v <- by.name
    big.nm <- max(nchar(nm.v))
    if (big.nm > 6) rm <- rm + (.05 * (big.nm - 6))
    if (n.ycol == 1) rm <- rm + .30  + (.65 * axis_y_cex)
    if (axis_y_cex > 1) if (!is.null(by)) rm <- rm + .1  # kludge
  }

  if (center_line != "off") rm <- rm + .4  # room for center_line label
  rm <- rm + 0.10  # make room when the last axis date > last data value
  if ((options("device") != "RStudioGD")  &&  !is.null(by)) rm <- rm + .3

  if (offset > 0.5) bm <- bm + (-0.05 + 0.2 * offset)  # offset kludge

  if (ts_ahead > 0) rm <- rm + 0.75  # make room for vertical legend

  if (date.var && n.by>1) rm <- rm - 0.05

  if (n.ycol>1 && date.var) {  # if not a time series, then no y label
    mm <- mm + 0.25  # reduce size of plot in the left to make room for label
    ylab_adj <- ylab_adj + 0.2  # move label right when calling .axlabs()
  }
  if (n.ycol > 1)
    rm <- rm + 0.80  # add right margin for legend

  if (n.xcol>1  && !date.var) {
    bm <- bm + 0.20  # allow for the bottom legend
    rm <- rm - 0.60  # remove superfluous right margin
  }

  # user manual adjustment
  bm <- bm + bm.adj
  mm <- mm + lm.adj
  tm <- tm + tm.adj
  rm <- rm + rm.adj

  orig.params <- par(no.readonly=TRUE)
  on.exit(par(orig.params))

  par(bg=getOption("window_fill"))
  par(mai=c(bm, mm, tm, rm))
  par(tcl=-0.28)  # axis tic length

  # -----------------------
  # setup region for coordinate system only with plot and type="n"
  # non-graphical parameters in ... Generate warnings when no plot

  # re-calibrate maximum of y-axis if stacking
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
      mn.x <- mn.x - .2
      mx.x <- mx.x + .2
    }
    if (cat.y) {
      mn.y <- mn.y - .2
      mx.y <- mx.y + .2
    }

    if (is.null(origin_x)) {  # set default for minimum x-value displayed
      if (stat %in% c("count", "proportion", "%"))
        origin_x <- 0
      else
        origin_x <- mn.x
    }
    if (stat != "data" && (!all(y == 0))) mx.y <- mx.y + (.08 * (mx.y-mn.y))
    region <- matrix(c(origin_x, mx.x, mn.y, mx.y), nrow=2, ncol=2)
  }  # end no ellipse

  else {  # set plot with sufficient room for ellipse and data
    cxy <- cor(x[,1],y[,1], use="complete.obs")
    s.x <- sd(x[,1], na.rm=TRUE); s.y <- sd(y[,1], na.rm=TRUE)
    m.x <- mean(x[,1], na.rm=TRUE); m.y <- mean(y[,1], na.rm=TRUE)
    lvl <- max(ellipse)
    region <- ellipse(cxy, scale=c(s.x, s.y), centre=c(m.x, m.y), level=lvl)
    region <- rbind(region, c(mn.x, mn.y), c(mx.x, mx.y))
  }

  # revise min and max for x and y axes considering the ellipse
  mx.x <- max(region[,1])
  mn.x <- min(region[,1])
  mx.y <- max(region[,2])
  mn.y <- min(region[,2])

  # set y-axis origin for numeric variables
  if (!cat.y) {
    if (!is.null(origin_y)) {
      if (all(y, na.rm=TRUE) > 0) mn.y <- origin_y
      if (all(y, na.rm=TRUE) < 0) mx.y <- origin_y
    }
  }

  # add padding to x and y axes
  add.pad_lab <- FALSE
  if (all(pad_x == 0)) {  # pad extra for labels in 2-D plot
    if (!is.null(add))
      add.pad_lab <- ifelse ("labels" %in% add, TRUE, FALSE)
    if (add.pad_lab || (length(out_ind) > 0)) {
      if (all(pad_x == 0)) pad_x <- c(0.05, 0.05)
      if (all(pad_y == 0)) pad_y <- c(0.03, 0.03)
    }
  }

  # get plot region
  xp <- pretty(c(mn.x, mx.x))
  xP <- pad_x[2] * (xp[length(xp)] - xp[1])
  yp <- pretty(c(mn.y, mx.y))
  xN <- pad_x[1] * (xp[length(xp)] - xp[1])
  yP <- pad_y[2] * (yp[length(yp)] - yp[1])
  yN <- pad_y[1] * (yp[length(yp)] - yp[1])
  region <- rbind(region, c(mn.x-xN, mn.y-yN), c(mx.x+xP, mx.y+yP))


  # plot: setup the coordinate system
  # ---------------------------------
  plot(region, type="n", axes=FALSE, ann=FALSE, ...)
  rm(region)
  usr <- par("usr")
  # ----------------------


  # ----------------------
  # set up plot background

  # set axT1: x-axis tics and values, Date var uses x.zoo.dates, defined later
  if (cat.x)
    axT1 <- seq_along(x.lvl)   # mark category values
  else {  # numerical
    if (!date.var) {
      if (stat == "count") {
        if (mx.x <= 3)  # just want integers for count axis
          n.best <- 3
        else if (mx.x > 3  &&  mx.x < 100)
          n.best <- 4
        else
          n.best <- 5
        axT1 <- pretty(origin_x:mx.x, n=n.best)
      }
      else {  # stat != "count"
        if (is.null(scale_x))
          axT1 <- pretty(c(origin_x, x), eps.correct=0)  # numeric, so all tics
        else {
          if (!run)
            axT1 <- axTicks(1, axp=scale_x)
          else
            axT1 <- seq(scale_x[1], scale_x[2], by=scale_x[3])
        }
      }
    }  # end !date.var
  }  # end numerical

  # set axT2: y-axis tics and values
  if (cat.y)
    axT2 <- seq_along(y.lvl)
  else {
    if (is.null(scale_y))
      axT2 <- pretty(c(mn.y, mx.y))  # more extreme values than axTicks
    else
      axT2 <- axTicks(2, axp=scale_y)
  }

  if (bubble1) {  # 1-D scatter plot of categorical variable
    axT2 <- NULL
    y.val <- NULL
    y.lab <- ""
  }

  # axes value labels
  if (xy_tics) {

    # adjust axis label from tick with mgp[2]
    # mgp does not work with rotate_x, see .axes()
    my.mgp <- par("mgp")  # save to restore
    ax <- .axes_dim()  # get axis value parameters
    mgp2 <- -0.350 + (0.9 * ax$axis_x_cex)
    par(mgp = c(my.mgp[1], mgp2, my.mgp[3]))  # labels closer to axis
    adj <- .RSadj(axis_cex=ax$axis_x_cex); axis_x_cex <- adj$axis_cex

    if (!date.var) {  # call axis() for both axes
      .axes(x.lvl, y.lvl, axT1, axT2,
            rotate_x=rotate_x, rotate_y=rotate_y, offset=offset, ...)
    }
    else {  # date.var, y-axis
      .axes(NULL, y.val, NULL, axT2, rotate_x=rotate_x, rotate_y=rotate_y,
            offset=offset, y.only=TRUE, ...)  # y-axis values

      # date.var, x-axis
      len.xtics <- length(x.dates)
      indices <- seq_along(x.dates)  # retain if num of data pts < n.tics
      step_range <- integer(length(50))

      if (ts_unit == "years") {
        x.zoo.dates <- format(x.dates, "%Y")
        n.tics <- ifelse (is.null(n_date_tics), 10, n_date_tics)
      }
      else if (ts_unit == "quarters") {
        x.zoo.dates <- as.character(zoo::as.yearqtr(x.dates))
        n.tics <- ifelse (is.null(n_date_tics), 9, n_date_tics)
        if (len.xtics >= 4*n.tics) {
          for (i in 1:50) step_range[i] <- 4*i
          tics_range <- ceiling(len.xtics / step_range)
          step <- step_range[which.min(abs(tics_range-n.tics))]
          indices <- seq(1, len.xtics, by=step)
        }
      }
      else if (ts_unit == "months") {
        x.zoo.dates <- as.character(zoo::as.yearmon(x.dates))
        n.tics <- ifelse (is.null(n_date_tics), 7, n_date_tics)
        if (len.xtics >= 12*n.tics) {
          for (i in 1:50) step_range[i] <- 12*i
          tics_range <- ceiling(len.xtics / step_range)
          step <- step_range[which.min(abs(tics_range-n.tics))]
          indices <- seq(1, length(x.dates), by=step)
        }
      }
      else {  # e.g., days
         x.zoo.dates <- as.character(x.dates)
         n.tics <- ifelse (is.null(n_date_tics), 7, n_date_tics)
      }

      # subset the indices to display if needed
      if (len.xtics >= n.tics) {
        step <- ceiling(len.xtics / n.tics)
        indices <- seq(1, len.xtics, by=step)
      }

      # x.zoo.dates is the xts customized for char ts_unit date, Aug 2023
      # x.dates are the dates in Date format, e.g., 2023-08-01
      axis(1, x.zoo.dates, at=x.dates[indices], labels=x.zoo.dates[indices],
                col=ax$axis_x_color, cex.axis=axis_x_cex,
                col.axis=ax$axis_x_text_color, ...)  # x-axis
    }  # end date.var

    par(mgp = my.mgp)  # restore back to previous value
  }  # end xy_tics

  if (date.var) {  # create y.lab
    if (ts_unit != "unknown") {
      tu <- gsub("s$", "", ts_unit)
      if (tu == "days7") tu <- "days"
      substr(tu, 1, 1) <- toupper(substr(tu, 1, 1))
      if (do.agg) {
        if (ts_agg == "sum") show.agg <- "Total"
        if (ts_agg == "mean") show.agg <- "Mean"
      }
      else
        show.agg <- ""
      ynm <- ifelse (n.ycol==1, y.name, "")  # get rid of c("Sales", "Profit")
      y.lab <- paste(show.agg, ynm, "by", tu)
    }

    .plt.bck(usr, x.dates[indices], axT2, do.h=!bubble1)
  }
  else
    .plt.bck(usr, axT1, axT2, do.h=!bubble1)

  # axes labels
  .axlabs(x.lab, y.lab, main.lab, sub.lab,
          x.val, xy_tics, offset=offset,
          lab_x_cex=lab_x_cex, lab_y_cex=lab_y_cex, main_cex,
          n.lab_x.ln, n.lab_y.ln, xlab_adj, ylab_adj, ...)


  # ---------------
  # plot the values
  # ---------------

  # colors
  # ------

  n.clrs <- max(n.col, n.by)

  ltype <- character(length=n.clrs)
  for (i in seq_along(ltype)) ltype[i] <- "solid"


  # plot lines (and area_fill)
  # --------------------------

  if (object %in% c("point", "both")) {

    if (object == "both") {

      if (n.xcol == 1  &&  n.ycol == 1) {
        if (n.by <= 1) {  # pure single panel, data in wide format

          if (ln.width > 0) {  # plot data line segments
            if (date.var || freq.poly)  # object == "both"
              lines(x[,1], y[,1], col=color[1], lwd=ln.width, ...)
          }

          if (area_fill[1] != "transparent") # fill area
            polygon(c(x[1],x,x[length(x)]), c(mn.y,y[,1],mn.y),
                col=area_fill, border="transparent")

          if (ts_ahead > 0) {  # forecast
            trns.red <- .maketrans("darkred", 0.25*256)
            lines(x.fit[,1], y.fit[,1], col=trns.red, lwd=ln.width)
            lines(c(x.fit[nrow(x.fit),1], x.hat[1,1]),  # connect fit with hat
                  c(y.fit[nrow(y.fit),1], y.hat[1,1]),
                  col=trns.red, lwd=ln.width)
            f.size.pt <- ifelse (size.pt<0.5, .65, size.pt)
            points(x.hat[,1], y.hat[,1], pch=shape, bg="darkred", cex=f.size.pt)
            lines(x.hat[,1], y.hat[,1], col="darkred", lwd=ln.width)
            lines(x.hat[,1], y.upr[,1], col=trns.red, lwd=ln.width)
            lines(x.hat[,1], y.lwr[,1], col=trns.red, lwd=ln.width)
            xx <- c(x.hat[,1], rev(x.hat[,1]))
            yy <- c(y.upr[,1], rev(y.lwr[,1]))
            polygon(xx, yy, border=NA, col=rgb(.6,0,0,.15))

            lgn.clr <- c("black", trns.red, "darkred")
            lgn.nm <- ifelse (nchar(y.name) < 6, y.name, "data")
            .plt.by.legend(c(lgn.nm, "model\nfit", "fore-\ncast"),
                           lgn.clr, lgn.clr, shp="lines", pts_trans, fill_bg,
                           usr, pt.size=size, legend_title=legend_title)
          }
        }  # n.by is 1

        else {  # n.by > 1,  tidy format, all y data in same column
          for (i in 1:n.by) {  # only stack=TRUE makes sense
            xl <- x[by==levels(by)[i], , drop=FALSE]
            if (i > 1) yo <- yl
            yl <- y[by==levels(by)[i], , drop=FALSE]

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
            }  # end stack

            if (ln.width > 0) {
              if (stack)  # set line properties here, no option for user
                ln.width <- 1
              lines(xl, yl[,1], col=color[i], lty=ltype[i], lwd=ln.width, ...)
            }  # end ln.width > 0
          }
        }  # end n.by > 1
      }  # end n.xcol and n.ycol = 1

      if (n.ycol > 1) {
        for (i in 1:n.ycol) {

          if (stack) {
            # polygons
            if (i == 1) {
              xx <- c(x[1],x,x[length(x)])
              yy <- c(mn.y,y[,1],mn.y)  # starts at the scale_y setting
              if (fill[1] != "transparent")
                polygon(xx, yy, col=area_fill[1], border="transparent")
            }
            if (i > 1) {
              y[,i] <- apply(y[,(i-1):i], 1, sum, na.rm=TRUE)  # sum to stack
              xx <- c(c(x[1],x,x[length(x)]), rev(c(x[1],x,x[length(x)])))
              yy <- c(c(min(y[,i]),y[,i],min(y[,i])),
                         rev(c(min(y[,i-1]),y[,i-1],min(y[,i-1]))))
              if (fill[1] != "transparent")
                polygon(xx, yy, col=area_fill[i], border="transparent")
            }
          }  # end stack polygons

          if (ln.width > 0) {  # lines
            lines(x[,1], y[,i], col=color[i], lty=ltype[i], lwd=ln.width, ...)
          }  # end ln.width > 0

        }  # end i loop
      }  # end n.ycol > 1

      if (n.xcol > 1) {
        if (ln.width > 0) for (i in 1:n.xcol)
          lines(as.numeric(x.val),y[,1], col=fill[i], lwd=ln.width, ...)
      }

    }  # end both


    # -----------
    # plot points

    if (object %in% c("point", "both")) {

      # -----------------------------------
      # set jitter for scatterplot with discrete levels
#     do.jit <- FALSE
#     if (nrows < 100) {
#       if (max(table(x,y) > 1)) do.jit <- TRUE  # duplication
#     }

      # multiple sizes if n_bin>0
      if (size[1]>0 && n.xcol==1 && stat=="data") {
        if (is.null(jitter_x))  {  # ifelse() does not work
          if (length(unique(x[,1])) <= 14  &&  nrows > 14)
            jitter_x <- (diff(range(x[,1], na.rm=TRUE))) / 32
        }
        if (is.null(jitter_y))  {  # ifelse() does not work
          if (length(unique(y[,1])) <= 14  &&  nrows > 14)
            jitter_y <- (diff(range(y[,1], na.rm=TRUE))) / 32
        }
      }

      # for VBS need to set jitter, here only set if jitter is NULL
      if (is.null(jitter_x)) jitter_x <- 0
      if (is.null(jitter_y)) jitter_y <- 0

      # --- process jitter ---
      if (jitter_x > 0) {
        x.temp <- x[,1]
        x[,1] <- x + runif(length(x[,1]), -jitter_x, jitter_x)
      }
      if (jitter_y > 0) {
        y.temp <- y[,1]
        y[,1] <- y + runif(length(y[,1]), -jitter_y, jitter_y)
      }
      # ----------------------


      # no grouping variable
      # --------------------
      if (is.null(by)) {

        if (smooth) {  # 2-D kernel density plot
          # grid lines only plot after the sp
          clr.den <- colorRampPalette(c(getOption("window_fill"),
                                         getOption("bar_fill_cont")))
          smoothScatter(x, y, nrpoints=smooth_points, nbin=smooth_bins,
                        transformation=function(x) x^(smooth_exp),
                        colramp=clr.den, cex=smooth_size, add=TRUE)
        }

        else if (size.pt[1] > 0) {  # plot points, and means, segments, etc.

          # --- plot points, segments, means with no by ---
          # -----------------------------------------------

          # plot points
          if (n.xcol == 1  &&  n.ycol == 1) {  # one x and one y variable
            if (length(out_ind) == 0)  # no outliers
              points(x[,1], y[,1], pch=shape, col=color[1], bg=fill[1],
                       cex=size.pt, ...)
            else {  # display outliers separately
              points(x[-out_ind,1], y[-out_ind,1],
                 pch=shape, col=color[1], bg=fill[1], cex=size.pt, ...)
              points(x[out_ind,1], y[out_ind,1],
                 pch=out_shape, col=out_color, bg=out_fill, cex=size.pt, ...)
              text(x[out_ind], y[out_ind], labels=ID[out_ind],
                 pos=1, offset=0.4, col=ID_color, cex=ID_size)
            }
          }

          else if (n.ycol == 1) {  # one y
            for (i in 1:n.xcol) {  # one to many x's
                if (theme %in% c("gray", "white"))
                  if (n.clrs == 2  &&  i == 2) fill[i] <- "transparent"
                points(x[,i], y[,1], pch=shape, col=color[i], bg=fill[i],
                       cex=size.pt, ...)
            }
          }

          else  if (n.xcol == 1) {  # one x
            for (i in 1:n.ycol) {  # one to many y's
                if (theme %in% c("gray", "white"))
                  if (n.clrs == 2  &&  i == 2) fill[i] <- "transparent"
                points(x[,1],y[,i], pch=shape, col=color[i], bg=fill[i],
                       cex=size.pt, ...)  # one x
            }
          }

          # plot segments to axis
          if (segments_y) {
            if (n.xcol == 1) # line segments from points to axis
              segments(x0=0, y0=y, x1=x, y1=y,
                       lty=1, lwd=.75, col=col.segment)
              #segments(x0=min(pretty(x)), y0=y, x1=x, y1=y,
                       #lty=1, lwd=.75, col=color)
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


          # plot means
          # ---------

          if (means  &&  stat == "data") {

          # get colors
            pch.avg <- ifelse (theme!="gray", 21, 23)
            bck.g <- ifelse (theme!="gray", rgb(130,90,70,
                            maxColorValue=255), "gray40")
            if (grepl(".black", theme, fixed=TRUE))
              bck.g <- "gray85"

            # restore un-jittered data
            if (jitter_x > 0) {
              x[,1] <- x.temp
              rm(x.temp)
            }
            if (jitter_y > 0) {
              y[,1] <- y.temp
              rm(y.temp)
            }

            m.lvl <- numeric(length = 0)

            # plot means for num y across levels of factor x
            if (cat.x && !cat.y && !unique.x) {
              for (i in seq_along(x.lvl))
                m.lvl[i] <- mean(y[x==i], na.rm=TRUE)
              abline(h=m.lvl, col="gray50", lwd=.5)
              points(m.lvl, pch=pch.avg, bg=bck.g, col=bck.g, cex=size.pt*1.5)
            }

            # plot means for num x across levels of factor y
            if (!cat.x && cat.y && !unique.y) {
              for (i in seq_along(y.lvl))
                m.lvl[i] <- mean(x[y==i], na.rm=TRUE)
              abline(v=m.lvl, col="gray50", lwd=.5)
              points(m.lvl, seq_len(length(y.lvl)), pch=pch.avg, bg=bck.g,
                     col=bck.g, cex=size.pt*1.5)
            }
          }  # end means

        }  # end not smooth, plot points with no by
        # -----------------------------------------

        # plot center line
        # ----------------

        if (center_line != "off") {
          if (center_line == "mean") {
            m.y <- mean(y[,1], na.rm=TRUE)
            lbl <- " mean"
          }
          else if (center_line == "median") {
            m.y <- median(y[,1], na.rm=TRUE)
            lbl <- " median"
          }
          else if (center_line == "zero") {
            m.y <- 0
            lbl <- ""
          }

          abline(h=m.y, col="gray50", lty="dashed")  # draw center line
          mtext(lbl, side=4, cex=.7, col="gray50", las=2, at=m.y, line=0.1)

          if (center_line == "zero") m.y <- median(y[,1], na.rm=TRUE)  # runs

        }  # end center_line

        else
          m.y <- median(y[,1], na.rm=TRUE)

        if (segments) {  # interaction means plot, ts with non-Date x var
          if (!is.null(ylab))  # thin line for ANOVA interaction plot
            if (grepl("Cell Means of", ylab, fixed=TRUE)) ln.width <- 0.75
          for (j in 1:(nrow(x)-1)) {
            segments(x0=x[j,1], y0=y[j,1],
                     x1=x[j+1,1], y1=y[j+1,1],
                     lty="solid", lwd=ln.width, col=fill[i])
          }
        }  # end segments

      }  # end is.null by


      # --------------------
      # by grouping variable

      else {
        clr <- character(length(n.by))

        if (length(color) == 1)
          for (i in 1:n.by) clr[i] <- color  # all levels get same color
        else
          clr <- color

        shp <- integer(length(n.by))
        if (length(shape) == 1)
          for (i in 1:n.by) shp[i] <- shape
        else
          shp <- shape
#       if (length(color)==1 && length(fill)==1 && length(shape)==1) {
#         if (n.by <= 5)  # R presents only five filled points
#           shape.dft <- c(21,23,22,24,25)  # shape defaults
#         else
#           shape.dft <- c(1,0,5,2,6,8,7,9,10,12:14,11)  # shape defaults
#         for (i in 1:n.by) shp[i] <- shape.dft[i]  #  default shapes
#       }
        # plot points and segments for each group
        for (i in 1:n.by) {
          x.lv <- subset(x, by==levels(by)[i])
          y.lv <- subset(y, by==levels(by)[i])
          if (!by.bub)  {
            if (size.pt > 0)
              points(x.lv, y.lv[,1], pch=shp[i], col=clr[i], bg=fill[i],
                     cex=size.pt, lwd=0.75, ...)
          }
          else {  # size is a variable
            size.lv <- subset(size, by==levels(by)[i])
            fill[i] <- .maketrans(fill[i], (1-pts_trans)*256)
            # size is a var and a by var
            .plt.bubble(x.lv, y.lv, size.lv, radius, power, fill[i], clr[i],
                        size_cut, prop, bubble_text, object)
          }

          # interaction means plot, ts without a date var and segments=TRUE
          if (segments) {
            if (!is.null(ylab))  # thin line for ANOVA interaction plot
              if (grepl("Cell Means of", ylab, fixed=TRUE)) ln.width <- 0.75
            for (j in 1:(nrow(x.lv)-1)) {
              segments(x0=x.lv[j,1], y0=y.lv[j,1],
                       x1=x.lv[j+1,1], y1=y.lv[j+1,1],
                       lty="solid", lwd=ln.width, col=fill[i])
            }
          }  # end segments

        }  # end 1:n.by

        # legend
        if (fill[1] == "transparent") fill <- color
        if (stack) {  # default pt.size is 0
          point.size <- ifelse (size > 0, 2.5 * axis_x_cex, 0)
          .plt.by.legend(levels(by), color, area_fill, shp=22, pts_trans,
                         fill_bg, usr, pt.size=point.size,  # 22 is a rectangle
                         legend_title=legend_title)
        }
        else {  # not stack
          if (n.by > 1) {
#           for (i in 1:length(shp)) shp[i] <- 21  # circle
            .plt.by.legend(levels(by), color, fill, shp[1], pts_trans,
                           fill_bg, usr, legend_title=legend_title)
          }
        }

      }  # end by
    }  # end plot points object is point or both


    # legend
    # ------

    if (fill[1] == "transparent") fill <- color

    if (n.xcol > 1)  # horizontal legend, on x-axis
      .plt.legend(colnms=colnames(x), horiz=TRUE,
                  color, fill, shape, fill_bg, usr,
                  lab_cex=lab_x_cex, pt.size=1.25, legend_title)

    if (n.ycol > 1) {  # vertical legend, on y-axis
      if (stack)  # ifelse form only returns the first value of the vector
        the.fill <- area_fill
      else
        the.fill <- fill
      .plt.legend(colnms=colnames(y), horiz=FALSE,
                  color, the.fill, shape, fill_bg, usr,
                  lab_cex=lab_y_cex, pt.size=1.25, legend_title)
    }

  }  # object is point, line, both


  # ----------------------------
  # --- bubble or sunflower plot -  no by var

  else if ((object %in% c("bubble", "sunflower"))) {
    if (!is.null(by)) {
      cat("\n"); stop(call.=FALSE, "\n------\n",
        "Parameter  by  not valid for bubble plot\n\n")
    }

    n.clrs <- 1  # for fit.line

    # colors
    if (is.null(col.low) || is.null(col.hi) || !bubble1) {
      if (fill[1] != "#46505A")  # default
        clr <- fill
      else
        clr <- getOption("bar_fill_cont")
      clr_color <- color
    }
    else {  # 1-var bubble plot and BPFM can have a color gradient
      color_palette <- colorRampPalette(c(col.low, col.hi))
      clr <- color_palette(length(unique(x)))
      clr_color <- "gray70"
    }

    # no value for size specified, do counts

    if (length(size) == 1) {
      mytbl <- table(x, y)  # get the counts, all x-y combinations
      n.count <- nrow(mytbl) * ncol(mytbl)
      if (prop) {
        count <- numeric(length=n.count)
        if (cat.y)
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
      for (i in seq_len(nrow(mytbl))) {
        for (j in seq_len(ncol(mytbl))) {
          k <- k + 1
          count[k] <- mytbl[i,j]
          if (count[k] == 0) count[k] <- NA  # 0 count not plotted
          xx[k] <- as.numeric(rownames(mytbl)[i])  # row names are factors
          yy[k] <- as.numeric(colnames(mytbl)[j])
        }
      }
      if (prop) count <- round(count, 2)

      # for categorical vars, size not a var
      if (object == "bubble") {
        .plt.bubble(xx, yy, count, radius, power, clr, clr_color,
                    size_cut, prop, bubble_text, object)
      }
      else if (object == "sunflower") {
        cords <- data.frame(xx, yy, count, stringsAsFactors=TRUE)
        cords <- na.omit(cords)
        sunflowerplot(cords$xx, cords$yy, number=cords$count,
            seg.col=clr_color, col=clr,
            col.axis=getOption("axis_x_color"), add=TRUE)
      }

    }  # end length(size) == 1

    # size is a variable (unless size is constant and bubble specified)
    # no by var
    else {
      .plt.bubble(x, y, size, radius, power, clr, clr_color,
                  size_cut, prop, bubble_text, object)
    }
  }  # end bubble/sunflower
  # -----------------------


  # --------------
  # ellipse option

  if (do.ellipse) {

    for (i in 1:n.clrs) {

      if (n.clrs == 1) {  # one plot, all the data
          x.lv <- x[,1]
          y.lv <- y[,1]
      }

      else {  # multiple, pull out subset

        if (!is.null(by)) {  # multiple by plots
          ind <- which(by == levels(by)[i])
          x.lv <- x[ind]
          y.lv <- y[ind]
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

      # ellipse stats
      cxy <- cor(x.lv, y.lv, use="pairwise.complete.obs")
      m.x <- mean(x.lv, na.rm=TRUE)
      m.y <- mean(y.lv, na.rm=TRUE)
      s.x <- sd(x.lv, na.rm=TRUE)
      s.y <- sd(y.lv, na.rm=TRUE)

      for (j in seq_along(ellipse)) { # for each ellipse for this by group
        e <- ellipse::ellipse(cxy, scale=c(s.x, s.y), centre=c(m.x, m.y),
                     level=ellipse[j])
        ln.type <- "solid"
        col.border <- ifelse (n.clrs == 1, ellipse_color, clr)
        polygon(e, border=col.border, col=ellipse_fill,
                lwd=ellipse_lwd, lty=ln.type)
      }  # jth ellipse
    }  # ith pattern
  }  # do.ellipse


  # ---------------
  # fit line option

  if (fit.line != "off") {

    # if outliers noted then also do line w/o outliers
    fit.remove <- ifelse (length(out_ind) > 0, TRUE, FALSE)
    do.remove <- FALSE
    n.loops <- ifelse (n.clrs > 1, 1, as.numeric(fit.remove)+1)
    for (i.remv in 1:n.loops) {
      if (fit.remove) {
        fit_se[1] <- 0  # for line w/o outliers, no se band
        fit_lwd <- 1.5
        if (i.remv == 2) do.remove <- TRUE  # 2nd pass
      }

      mse.ln <- double(length=n.clrs)  # .ln is linear
      mse.nl <- double(length=n.clrs)  # .nl is nonlinear
      b0 <- double(length=n.clrs)
      b1 <- double(length=n.clrs)
      Rsq <- double(length=n.clrs)
      by.cat <- character(length=n.clrs)

      for (i.clr in 1:n.clrs) {  # note: double looping with i.remv and i

        if (n.clrs == 1) {  # one plot, all the data
          if (!date.var) {
            x.lv <- x[,1]
            y.lv <- y[,1]
          }
          else {  # date.var
            x.lv <- as.numeric(x.val)
            y.lv <- as.numeric(y[,i.clr])
          }
          clr <- fit_color
        }  # end n.clrs == 1

        else {  # multiple clrs, pull out subset

          if (!is.null(by)) {  # multiple by plots
            x.lv <- subset(x, by==levels(by)[i.clr])
            y.lv <- subset(y, by==levels(by)[i.clr])
          }

          if (n.col > 1) {  # multiple variable plots
            if (n.xcol > 1) {
              x.lv <- x[,i.clr]
              y.lv <- y[,1]
            }
            else {
              x.lv <- x[,1]
              y.lv <- y[,i.clr]
            }
          }

          clr <- ifelse (length(color) == 1, color, color[i.clr])
        }  # end multiple

        ln.type <- "solid"

        if (!is.null(out_ind)) if (do.remove) {
          x.lv <- x.lv[-out_ind]
          y.lv <- y.lv[-out_ind]
          ln.type <- ifelse (ln.type != "dashed", "dashed", "solid")
        }

        col.ln <- ifelse (n.clrs==1, fit_color, fill[i.clr])

        pf <- .plt.fit(x.lv, y.lv, fit.line, fit_power, fit_new)  # fit line

        x.lv <- pf$x.lv  # x and y get reduced in .plt.fit if NA
        y.lv <- pf$y.lv
        f.ln <- pf$f.ln  # fitted
        l.ln <- pf$l.ln  # linearized
        y.new <- pf$y.new  # computed from fitted function 

        if (i.remv == 1) {  # if outlier removal, only outlier line reported
          mse.ln[i.clr] <- pf$mse.ln
          mse.nl[i.clr] <- pf$mse.nl
          b0[i.clr] <- pf$b0
          b1[i.clr] <- pf$b1
          Rsq[i.clr] <- pf$Rsq
        }

        if (n.by > 0)
          by.cat[i.clr] <- levels(by)[i.clr]

        if (fit.line %in% c("exp", "quad", "log", "null"))
          fit_se[1] <- 0

        # se bands about each eligible fit line
        if (fit_se[1] != 0) {
          for (j in seq_along(fit_se)) {
            p.ln <- predict(l.ln, se=TRUE)
            prb <- (1 - fit_se[j]) / 2
            nrows <- length(y.lv)
            up.ln <- f.ln + (qt(prb,nrows-1) * p.ln$se.fit)
            dn.ln <- f.ln - (qt(prb,nrows-1) * p.ln$se.fit)
            polygon(c(x.lv, rev(x.lv)), c(up.ln, rev(dn.ln)),
                    col=se_fill, border="transparent")

          }  # end for each se plot
        }

        # plot fit line(s) on top of se bands
        if (!("transparent" %in% clr)) {
          if (n.clrs ==2  &&  (color[1] == color[2]))
            ln.type <- ifelse (i == 2, "dashed", "solid")
          lines(x.lv, f.ln, col=clr, lwd=fit_lwd, lty=ln.type)
        }
        else {
          lines(x.lv, f.ln, col=col.ln, lwd=fit_lwd, lty=ln.type)
        }

        # plot residuals option
        if (plot_errors) {
          red <- rgb(130,40,35, maxColorValue=255)
          pe.clr <- ifelse (theme %in% c("gray", "white"), "gray58", red)
          segments(y0=f.ln, y1=y.lv, x0=x.lv, x1=x.lv,
                   col=pe.clr, lwd=1)
        }
      }  # ith pattern (clr)
    }  # fit.remove

    if (!quiet) cat ("\n")
  }  # end fit.line
  else
    y.new <- NULL


  # -----------
  # annotations

  if (!is.null(add)) if (add[1] == "means") {
    add[1] <- "v_line"
    add[2] <- "h_line"
    x1 <- "mean_x"
    y1 <- "mean_y"
  }

  if (!is.null(x1)) {
    if (date.var) x1 <- julian(x1)  # dates to date serial numbers
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
        .plt.add(add, x1, x2, y1, y2,
             add_cex, add_lwd, add_lty, add_color, add_fill, add_trans)
  }
  # end annotations


  # -----------
  # end, return

  if (fit.line == "off") {
    mse.ln <- NULL;  mse.nl <- NULL;  b0 <- NULL;  b1 <- NULL;  Rsq <- NULL
    by.cat <- NULL
  }

  return(list(mse.ln=mse.ln, mse.nl=mse.nl, b0=b0, b1=b1, Rsq=Rsq,
              by.cat=by.cat, jitter_x=jitter_x, jitter_y=jitter_y,
              y.new=y.new))

}  # end plt.main
