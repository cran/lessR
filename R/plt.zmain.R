.plt.main <- 
function(x, y, by=NULL, n.cat=getOption("n.cat"),
         object="point", values="data",

         col.fill=getOption("fill.pt"),
         col.stroke=getOption("stroke.pt"),
         col.bg=getOption("bg"),
         col.grid=getOption("grid"),
         col.box=getOption("box"),

         col.trans=NULL, col.segments=col.stroke,
         col.area="transparent",

         cex.axis=0.76, col.axis="gray30", xy.ticks=TRUE,
         xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
         value.labels=NULL, label.max=20,
         rotate.values=0, offset=0.5, prop=FALSE,

         size=NULL, shape="circle", means=TRUE, 
         sort.yx=FALSE,
         segments.y=FALSE, segments.x=FALSE, line.width=2,

         smooth=FALSE, smooth.points=100, smooth.trans=0.25,
         smooth.bins=128,

         bubble.scale=0.25, bubble.power=0.6, bubble.text=TRUE,
         col.low=NULL, col.hi=NULL,

         fit.line="off", col.fit.line="gray55", se.fit.line=1,

         ellipse=FALSE, col.ellipse="lightslategray",
         fill.ellipse="off",
         
         center.line="default", show.runs=FALSE, stack=FALSE,

         method="overplot", pt.reg="circle", pt.out="circle", 
         col.out30="firebrick2", col.out15="firebrick4", 

         freq.poly=FALSE, quiet=getOption("quiet"),
         fun.call=NULL, want.labels=TRUE, ...)  {


  date.ts <- ifelse (.is.date(x[,1]), TRUE, FALSE)
  if (center.line == "default") if (date.ts) center.line <- "off"

  # x and y come across here in their natural state, within each data frame
  # a time series has dates for x and numeric for y, factors are factors, etc
  
  # want labels set just for ttestPower, which provides its own labels
  # both x and y are plotted, even if only a single variable
  # for a 1-D bubble plot of a single factor var, y.call was set to 0's
  # numerical 1-D scatter plot done in .dp.main
  bubble1 <- ifelse (length(unique(y[,1])) == 1, TRUE, FALSE)

  unique.x <- ifelse(length(unique(x[,1])) == length(x[,1]), TRUE, FALSE)
  unique.y <- ifelse(length(unique(y[,1])) == length(y[,1]), TRUE, FALSE)

  do.ellipse <- ifelse(as.logical(ellipse[1]), TRUE, FALSE)
  
  # windows line too thin at 1, but no increments allowed, and 2 is too thick
  # cannot test Mac at this time
  if (fit.line != "off") 
    fit.line.lwd <- ifelse(.Platform$OS == "windows", 2, 1.75)

  if (!is.null(value.labels)) value.labels <- gsub(" ", "\n", value.labels) 

  # all processing in terms of numeric variables
  # convert factors to numeric, save levels, so x and y are always numeric
  # x will always be a matrix
  x.lvl <- NULL; y.lvl <- NULL  # if remain null, then not factors
    nm.x <- names(x)
  if (is.factor(x[,1])) {
    x.lvl <- levels(x[,1])
    if (is.null(value.labels)) value.labels <- gsub(" ", "\n", x.lvl) 
    x <- as.matrix(as.integer(x[,1]))
  }
  else if (!date.ts) {
    x <- as.matrix(x)
    colnames(x) <- nm.x
  }

  nm.y <- names(y)
  if (is.factor(y[,1])) {
    y.lvl <- levels(y[,1])
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


  # see if trans is customized for this analysis
  if (is.null(col.trans)) {  # no change, so no trans for Cleveland dp
    if (cleveland) {
      trans.pts <- 0 
      col.fill <- .maketrans(col.fill, (1-trans.pts)*256)
    }
  }
  else {  # trans has been changed from default, so change col.fill
    trans.pts <- col.trans
    col.fill <- .maketrans(col.fill, (1-trans.pts)*256)
  }
           
  # scale for regular R or RStudio
  adj <- .RSadj(bubble.scale, cex.axis)
  bubble.scale <- adj$bubble.scale
  size.axis <- adj$size.axis
  size.lab <- adj$size.lab
  cex.txt <- adj$size.txt

  if (date.ts) xx.lab <- xlab
  if (want.labels) {
    gl <- .getlabels(xlab, ylab, main, cex.lab=size.lab)
    x.name <- gl$xn; x.lbl <- gl$xl; x.lab <- gl$xb
    y.name <- gl$yn; y.lbl <- gl$yl; y.lab <- gl$yb
    main.lab <- gl$mb
    sub.lab <- gl$sb
    size.lab <- gl$cex.lab
    by.name <- getOption("byname")
  }
  else {
    x.lab <- xlab
    y.lab <- ylab
    main.lab <- main
    sub.lab <- sub
    size.lab <- getOption("lab.size")
  }

  if (date.ts  &&  is.null(xx.lab)) x.lab <- ""
  
  if (!is.null(x.name)) if (x.name == "Index") {
    if (n.ycol > 1) y.lab <- ""
    if (!is.null(x.lbl)) y.lab <- paste(x.name, ": ", x.lbl, sep="")
  }

  # size of lines for line chart
  xlb <- "xxx"  # dummy argument
  if (!is.null(x.lab)) xlb <- x.lab
  if (object == "both")
    size.ln <- line.width  # size of lines
  

  # size of points
  scale.pt <- ifelse (.Platform$OS == "windows", 1.00, 0.80)
  if (is.null(size)) {  # size.pt not set yet
    size.pt <- scale.pt
    if (options("device") == "RStudioGD")
      size.pt <- size.pt*1.20
      #size.pt <- ifelse (.Platform$OS == "windows", size.pt*1.20, size.pt*1.20)

    if (object == "both") {
      size.pt <- 0.77 * size.pt  # default pt size for lines
      if (col.area != "transparent")  # default no points if area shown
        size.pt <- 0
      else if (nrows > 50) {
        size.pt <- .9 - 0.002*nrows
        if (size.pt < 0) size.pt <- 0
      }
    }
  }
  else  # size had been set
    size.pt <- size * scale.pt
  
  if (is.null(col.fill)) col.fill <- "transparent"  

  if (!date.ts) {
    num.cat.x <- is.null(x.lvl)  &&  .is.num.cat(x[,1], n.cat)
    cat.x <- ifelse (num.cat.x || !is.null(x.lvl), TRUE, FALSE)
  }
  else {
    num.cat.x <- FALSE
    cat.x <- FALSE
  }
  if (!bubble1  &&  !date.ts) {
    num.cat.y <- is.null(y.lvl) && .is.num.cat(y[,1], n.cat)
    cat.y <- ifelse (num.cat.y || !is.null(y.lvl), TRUE, FALSE)
  }
  else {
    num.cat.y <- FALSE
    cat.y <- FALSE
  }
    
  # by default display center.line only if runs, so detect if a run
  if (center.line == "default"  &&  !date.ts  &&  object == "both") {
    m <- mean(y, na.rm=TRUE)
    n.change <- 0
    for (i in 1:(length(y)-1))
      if ((y[i+1] > m) != (y[i] > m)) n.change <- n.change+1 
    if (n.change/(length(y)-1) < .15)
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

  # x.val is either any value.labels or x.lvl, or NULL if x is numeric
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
    else {
      x.val <- x.lvl  # x.val is NULL if x is numeric, ignored
      y.val <- y.lvl  # y.val ignored if y is numeric 
    }
  }
  else {
    max.lbl.y <- NULL
    y.val <- NULL
  }

  # -------------------------
  # graphic system parameters
  # -------------------------
  if (!is.null(y.val)) {
    max.width <- 0
    for (i in (1:length(y.val))) {
      li <- ifelse(!is.na(y.val[i]), strwidth(y.val[i], units="inches"), 0)
      if (li > max.width)
        max.width <- strwidth(y.val[i], units="inches")
    }
  }
  else 
    max.width <- strwidth(as.character(max(pretty(y[,1]))), units="inches")

  # set title for bubble plot if proportions
  if (object == "bubble"  &&  prop  &&  is.null(main)  &&  cat.y) {
    main.lab <- paste("Percentage of", y.name, "\nwithin each level of", x.name)
    main <- "not null"
  }

  if (!date.ts) if (n.xcol > 1) x.lab <- NULL
  
  if (length(size) > 1) {  # size is a variable
    sz.nm <- getOption("sizename")
    main.lab <- bquote(paste(italic(.(sz.nm)), ": Bubble size from ",
      .(min(size)), " to ", .(max(size)), sep=""))
  }

  # set margins
  margs <- .marg(max.width, y.lab, x.lab, main.lab, x.val, prop, rotate.values)
  lm <- margs$lm
  tm <- margs$tm
  rm <- margs$rm
  bm <- margs$bm

  if (n.ycol > 1) rm <- rm + 0.85  # room for legend
  if (!is.null(by)) rm <- rm + 0.85  # room for legend
  if (object == "both")
    if (center.line != "off") rm <- rm + .4  # room for center.line label
  if (n.xcol > 1) bm <- bm + .2
  
  orig.params <- par(no.readonly=TRUE)
  on.exit(par(orig.params))
  
  par(mai=c(bm, lm, tm, rm))


  # -----------------------
  # setup coordinate system only with plot and type="n"
  # non-graphical parameters in ... generate warnings when no plot
  # -----------------------

  mn.x <- ifelse(is.null(x.lvl), min(x, na.rm=TRUE), 1)
  mx.x <- ifelse(is.null(x.lvl), max(x, na.rm=TRUE), length(x.lvl))
  mn.y <- ifelse(is.null(y.lvl), min(y, na.rm=TRUE), 1)
  mx.y <- ifelse(is.null(y.lvl), max(y, na.rm=TRUE), length(y.lvl))

  if (stack  &&  n.ycol > 1) {  # re-calibrate y=axis if stacking
    y.tot <- apply(y, 1, sum, na.rm=TRUE)
    mx.y <- max(y.tot)
  }


  if (!do.ellipse) {

    if (cat.x) {
      mn.x <- mn.x - .5
      mx.x <- mx.x + .5
    }
    if (cat.y) {
      mn.y <- mn.y - .5
      mx.y <- mx.y + .5
    }
    #if (bubble1) {
      #mn.y <- mn.y - 1
      #mx.y <- mx.y + 1
    #}

    if (values %in% c("count", "prop")) mn.y <- 0
    if (values != "data" && (!all(y == 0))) mx.y <- mx.y + (.08 * (mx.y-mn.y))

    region <- matrix(c(mn.x, mx.x, mn.y, mx.y), nrow=2, ncol=2)

  }  # no ellipse

  else {  # set plot with sufficient room for ellipse and data
    cxy <- cor(x[,1],y[,1], use="complete.obs")
    s.x <- sd(x[,1], na.rm=TRUE); s.y <- sd(y[,1], na.rm=TRUE)
    m.x <- mean(x[,1], na.rm=TRUE); m.y <- mean(y[,1], na.rm=TRUE)
    lvl <- max(ellipse)
    region <- ellipse(cxy, scale=c(s.x, s.y), centre=c(m.x, m.y), level=lvl)
    region <- rbind(region, c(mn.x, mn.y))
  }

  # plot coordinate system
  plot(region, type="n", axes=FALSE, ann=FALSE, ...)
  rm(region)

  usr <- par("usr")          
  rect(usr[1], usr[3], usr[2], usr[4], col="transparent", border="blue")

  #
  # set up plot background
  # ----------------------

  # axis ticks and values
  if (cat.x) {
    if (!is.null(x.lvl)) axT1 <- 1:length(x.lvl)   # mark category values
    if (!is.null(x.val)) x.val <- .abbrev(x.val, label.max)
    if (num.cat.x) axT1 <- sort(unique(x))  # x.lvl or x.val are NULL
  }
  else
    axT1 <- axTicks(1)  # else numeric, so all the ticks
  
  if (cat.y) {
    if (!is.null(y.lvl)) axT2 <- 1:length(y.lvl)
    if (!is.null(y.val)) y.val <- .abbrev(y.val, label.max)
    if (num.cat.y) axT2 <- sort(unique(y))
  }
  else
    axT2 <- axTicks(2) 

  if (xy.ticks) {
    if (!bubble1) {
      if (!date.ts) {  # get ticks for both axes
        .axes(x.val, y.val, axT1, axT2,
              par("usr")[1], par("usr")[3], size.axis, col.axis,
              rotate.values, offset=offset, ...)
      }
      else {  # time 
        axis.Date(1, x.val, cex.axis=size.axis, col.axis=col.axis, ...)  # strptime
        .axes(NULL, y.val, axT1, axT2,
              par("usr")[1], par("usr")[3], size.axis, col.axis,
              rotate.values, offset=offset, y.only=TRUE, ...)  # only do y-axis
      }
    }
    else  # bubble1: 1-D scatter plot of categorical variable
      .axes(x.val, NULL, axT1, NULL,
            par("usr")[1], par("usr")[3], size.axis, col.axis, 
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
          x.val, xy.ticks, offset=offset, cex.lab=size.lab, ...) 

  # color plotting background color
  usr <- par("usr")          
  rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="transparent")

  # grid lines (put before box edge of plot)
  abline(v=axT1, col=col.grid, lwd=.5)
  if (!bubble1)
    abline(h=axT2, col=col.grid, lwd=.5)
  
  # box around plot
  rect(usr[1], usr[3], usr[2], usr[4], col="transparent", border=col.box)

  # ---------------
  # plot the values
  # ---------------

  # plot lines and/or points
  if (object %in% c("point", "both")) {
    trans.pts <- ifelse(is.null(col.trans), 
                        getOption("trans.fill.pt"), col.trans)

    n.by <- ifelse (is.null(by), 0, nlevels(by))


    # colors
    n.patterns <- max(n.col, n.by)

    stroke <- character(length=length(col.stroke))
    fill <- character(length=length(col.fill))
    if (n.patterns == 1) {
      stroke[1] <- col.stroke[1]
      fill[1] <- col.fill[1]
    }
    else if (n.patterns == 2) {
      stroke[1] <- col.stroke[1]
      stroke[2] <- ifelse (length(col.stroke) > 1, col.stroke[2], col.stroke[1])
      fill[1] <- col.fill[1]
      if (object == "both") {
        if (!(getOption("colors") %in% c("gray", "gray.black")))
          fill[2] <- .col.discrete(bright=TRUE)[2]
        else {
          fill[2] <- rgb(.15,.15,.15)
          stroke[2] <- fill[2]
        }
      }
      else { 
        if (length(col.fill) == 1)
          fill[2] <- "transparent"
        else
          fill[2] <- col.fill[2]
      }
    }  # n.patterns=2
    else  # n.patterns > 2
      stroke <- .col.discrete(bright=TRUE)[1:n.patterns]

    if (n.patterns > 2)
      for (i in 1:length(stroke)) fill[i] <- .maketrans(stroke[i], (1-trans.pts)*256)
  
       
    # -----
    # lines
    if (object == "both") {
 
      if (n.xcol == 1  &&  n.ycol == 1) { 

        if (line.width > 0) {  # plot line(s)
         
          if (date.ts || freq.poly || object == "both") {
              lines(as.numeric(x[,1]),y[,1], col=col.segments, lwd=size.ln, ...)
          }
        }  # line.width > 0

        if (col.area != "transparent") # fill area
          polygon(c(x[1],x,x[length(x)]), c(min(y[,1]),y[,1],min(y[,1])),
              col=col.area, border="transparent")
      }  # end n.xcol and n.ycol = 1 
          
      if (n.ycol > 1) {
        for (i in 1:n.ycol) {

          if (stack) {
            if (i == 1) {
              xx <- c(x[1],x,x[length(x)])
              yy <- c(min(y[,1]),y[,1],min(y[,1]))
              if (col.area != "transparent") 
                polygon(xx, yy, col=fill[1], border="transparent")
            }
            if (i > 1) { 
              y[,i] <- apply(y[,(i-1):i], 1, sum, na.rm=TRUE)  # sum for stacking
              xx <- c( c(x[1],x,x[length(x)]), rev(c(x[1],x,x[length(x)])) )
              yy <- c( c(min(y[,i]),y[,i],min(y[,i])),
                         rev(c(min(y[,i-1]),y[,i-1],min(y[,i-1]))) )
              if (col.area != "transparent") 
                polygon(xx, yy, col=fill[i], border="transparent")
            }
          }

          if (line.width > 0) {
            if (!date.ts)
              lines((x[,1]),y[,i], col=fill[i], lwd=size.ln, ...)
            else
              lines(x[,1], y[,i], col=fill[i], lwd=size.ln, ...)
          }  # end line.width > 0

          .plt.legend(colnames(y), FALSE, stroke, fill, shape, col.bg, usr, 
                    cex.lab=size.lab)  # y-axis

        }  # end i loop
    }  # end n.ycol > 1

      if (n.xcol > 1) {
        if (line.width > 0) for (i in 1:n.xcol)
          lines(as.numeric(x.val),y[,1], col=fill[i], lwd=size.ln, ...)
        .plt.legend(colnames(x), TRUE, stroke, fill, shape, col.bg, usr,
                    cex.lab=size.lab)  # x-axis
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
  
    }  # end lines


    # ------
    # points
    if (object %in% c("point", "both")) {

      if (is.null(by)) {
      
        if (smooth) {  # 2-D kernel density plot
          clr.den <- colorRampPalette(c("white", getOption("fill.bar")))
          smoothScatter(x, y, nrpoints=smooth.points, nbin=smooth.bins,
                        transformation=function(x) x^(smooth.trans),
                        colramp = clr.den, add=TRUE)
          abline(v=axT1, col=col.grid, lwd=.5) # grid lines (off by default)
          if (!bubble1) abline(h=axT2, col=col.grid, lwd=.5)
        }
    
        else {  # plot the individual points, plus means, segments, etc.

          if (n.ycol == 1) {
            for (i in 1:n.xcol) {
                points(x[,i], y[,1], pch=shape, col=stroke[i], bg=fill[i],
                              cex=size.pt, ...)
            }
          }
          else {
            for (i in 1:n.ycol)
                points(x[,1],y[,i], pch=shape, col=stroke[i], bg=fill[i],
                              cex=size.pt, ...)
          }
      
          if (n.xcol > 1)  # horizontal legend, on x-axis
            .plt.legend(colnames(x), TRUE, stroke, fill, shape, col.bg, usr, 
                        cex.lab=size.lab)
          if (n.ycol > 1)  # vertical legend, on y-axis
            .plt.legend(colnames(y), FALSE, stroke, fill, shape, col.bg, usr, 
                        cex.lab=size.lab)


          if (segments.y) { 
            if (n.xcol == 1) # line segments from points to axis
              segments(x0=min(pretty(x)), y0=y, x1=x, y1=y, 
                       lty=1, lwd=.75, col=col.stroke)
            else if (n.xcol == 2)  # line segments between points
              segments(x0=x[,1], y0=y[,1], x1=x[,2], y1=y[,1], 
                       lty=1, lwd=.75, col=col.segments)
          }

          if (!(values %in% c("count", "prop"))) {
            if (segments.x)
              segments(y0=par("usr")[3], x0=x, y1=y, x1=x, lty=1, lwd=.75,
                       col=col.segments)
          }
          else {
            if (segments.x) 
              if (n.xcol == 1)
                 segments(y0=0, x0=x, y1=y, x1=x, lty=1, lwd=1, col=col.segments)
          }

          if (means  &&  values == "data") {
            pch.avg <- ifelse(getOption("colors")!="gray", 21, 23)
            bck.g <- ifelse(getOption("colors")!="gray", "gray15", "gray30")
            if (grepl(".black", getOption("colors"), fixed=TRUE))
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
        }  # null by
        
      }

      else {  # by grouping variable

        clr <- character(length(n.by))
        clr.tr <- character(length(n.by))  # translucent version of clr
        if (length(stroke) == 1) 
          for (i in 1:n.by) clr[i] <- stroke  # all levels get same color
        else
          clr <- stroke

        shp <- integer(length(n.by))
        if (length(shape) == 1)
          for (i in 1:n.by) shp[i] <- shape
        else
          shp <- shape

        shape.dft <- c(21,23,22,24,25,7:14)  # shape defaults
        if (length(stroke)==1 && length(shape)==1)  #  color, shape 
          for (i in 1:n.by) shp[i] <- shape.dft[i]  # fill is default shapes

        for (i in 1:n.by) {
          x.lv <- subset(x, by==levels(by)[i])
          y.lv <- subset(y, by==levels(by)[i])
          points(x.lv, y.lv, pch=shp[i], col=clr[i], bg=fill[i], cex=size.pt,
                 lwd=0.75, ...)
        }

        .plt.by.legend(levels(by), stroke, fill, shp, trans.pts, col.bg, usr)

        }  # end by

      }  # object is point or both
    }  # object is point, line, both


  else if (object %in% c("bubble", "sunflower")) {

    n.patterns <- 1  # for fit.line

    # colors
    if (is.null(col.low) || is.null(col.hi) || !bubble1) {
      clr <- col.fill
      clr.stroke <- col.stroke
    }
    else {  # 1-var bubble plot and BPFM can have a color gradient
      color.palette <- colorRampPalette(c(col.low, col.hi))
      clr <- color.palette(length(unique(x)))
      clr.stroke <- "gray70"
    }

    if (is.null(size)) {  # no value for size specified, do counts
      mytbl <- table(x, y)  # get the counts, all x-y combinations
      n.count <- nrow(mytbl) * ncol(mytbl)
      if (prop) { 
        count <- numeric(length=n.count)
        if (!is.null(y.lvl))
          mytbl <- prop.table(mytbl, 1)
        else
          mytbl <- mytbl/sum(mytbl)
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
            xx[k] <- as.numeric(rownames(mytbl)[i])  # rownames are factors
            yy[k] <- as.numeric(colnames(mytbl)[j])
          }
        }
      }
      if (prop) count <- round(count, 2)
      cords <- data.frame(xx, yy, count)

      if (object == "bubble") {
        sz <- cords[,3]**bubble.power  # radius unscaled 
        symbols(cords$xx, cords$yy,
            circles=sz, inches=bubble.scale,
            bg=clr, fg=clr.stroke, add=TRUE, ...)
        mxru <- max(sz)
        sz <- 2 * (sz/mxru) * bubble.scale  # scaled diameter (symbols does)
      }

      else if (object == "sunflower") {
        sunflowerplot(cords$xx, cords$yy, number=cords$count, 
            seg.col=col.stroke, col=col.fill, cex.axis=size.axis,
            col.axis=col.axis, xlab=x.lab, ylab=y.lab, add=TRUE)
      }
    }

    else {  # size is a variable (unless size is constant and bubble specified)

      cords <- data.frame(x, y, size)
      cords <- na.omit(cords)
      sz <- cords[,3]**bubble.power  # radius unscaled 
      symbols(cords[,1], cords[,2], circles=sz,
        inches=bubble.scale, bg=clr, fg=clr.stroke, add=TRUE, ...)  
      mxru <- max(sz)
      sz <- 2 * (sz/mxru) * bubble.scale  # scaled diameter
    }

    if (bubble.text  &&  object == "bubble") { 

      # get q.ind before setting too small bubbles at NA
      if (bubble.text > 1) { 
        by.prob <- 1 / (bubble.text - 1)
        bub.probs <- seq(0, 1, by.prob)
        qnt <- quantile(cords[,3], probs=bub.probs, type=3, na.rm=TRUE)
        qnt.TF <- logical(length(cords))
        for (i in 1:nrow(cords))
            qnt.TF[i] <- ifelse(cords[i,3] %in% qnt, TRUE, FALSE) 
        q.ind <- which(qnt.TF)
      }
      else
        q.ind <- 1:nrow(cords)  # all bubbles get text

      # should get size of each bubble.text just for those displayed (q.ind)
      sz.cex <- numeric(length=nrow(cords))
      for (i in 1:nrow(cords)) {
        sz.cex[i] <- cex.txt  # cex target for text size
       # target for text size
        sz.txt <- strwidth(cords[i,3], units="inches", cex=sz.cex[i])
        while ((sz.txt - sz[i]) > -.03) {
          if (sz.cex[i] > 0.45) {  # need cex larger than 0.45
            sz.cex[i] <- sz.cex[i] - 0.05
            # actual
            sz.txt <- strwidth(cords[i,3], units="inches", cex=sz.cex[i])
          }
          else {
            cords[i,3] <- NA
            break;
          }
        }
      }

      if (!prop)
        text(cords[q.ind,1], cords[q.ind,2], cords[q.ind,3], cex=sz.cex[q.ind])
      else {
        crd <- .fmt0(cords[,3],2)
        for (j in 1:length(crd))
          if (grepl("NA", crd[j], fixed=TRUE)) crd[j] <- " "
        text(cords[,1], cords[,2], crd, cex=sz.cex)
      }
    }  # end bubble text

  }  # end bubble/sunflower 


  # ellipse option
  if (do.ellipse) {
    for (i in 1:length(ellipse)) {
      e <- ellipse(cxy, scale=c(s.x, s.y), centre=c(m.x, m.y),  
                   level=ellipse[i], npoints=200)
      polygon(e, border=col.ellipse, col=fill.ellipse, lwd=1)
    }
  }


  # fit line option
  if (fit.line != "off") { 

    for (i in 1:n.patterns) {
      if (n.patterns == 1) {  # one plot
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

      else {  # multiple

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

        clr <- ifelse (length(stroke) == 1, stroke, stroke[i])
      }  # multiple

      ln.type <- "solid"
      if (n.patterns == 2 && i == 2) ln.type <- "dashed"

      ok <- is.finite(x.lv) & is.finite(y.lv)
      if (any(ok)) {
        x.ok <- x.lv[ok]
        y.ok <- y.lv[ok]
        od <- order(x.ok)
        x.od <- x.ok[od]
        y.od <- y.ok[od] 

        if (fit.line == "loess")
          l.ln <- loess(y.od ~ x.od)
        else if (fit.line == "ls")
          l.ln <- lm(y.od ~ x.od)

        # fit line
        f.ln <- fitted(l.ln, ...)
        lines(x.od, f.ln, col=clr, lwd=fit.line.lwd, lty=ln.type)

        # se bands about fit line
        for (j in 1:length(se.fit.line)) {
          p.ln <- predict(l.ln, se=TRUE)
          up.ln <- fitted(l.ln, ...) + (se.fit.line[j] * p.ln$se.fit)
          dn.ln <- fitted(l.ln, ...) - (se.fit.line[j] * p.ln$se.fit)
          lines(x.od, up.ln, col=clr, lwd=0.5, lty=ln.type)
          lines(x.od, dn.ln, col=clr, lwd=0.5, lty=ln.type)
          len <- length(x.od)
          xx <- c( c(x.od[1],x.od,x.od[len]), rev(c(x.od[1],x.od,x.od[len])) )
          yy <- c( c(min(up.ln),up.ln,min(up.ln)),
                     rev(c(min(dn.ln),dn.ln,min(dn.ln))) )
          polygon(xx, yy, col=getOption("se.fill"), border="transparent")
        }  # end for each se plot
      }

    }  # ith pattern
  }  # fit.line



  # -----------
  # text output
  # -----------

  if (getOption("suggest")) {
    # function call for suggestions
    fncl <- .fun.call.deparse(fun.call) 
    fncl <- gsub(")$", "", fncl)  # get function call less closing ) 
    fncl <- gsub(" = ", "=", fncl)
  }
  
  if (!quiet  &&  values == "data") {
  
    if (!(object %in% c("line", "both"))) {
    
      # traditional two-var numeric var scatter plot
      if (!cat.x  &&  !cat.y  &&  object %in% c("point", "bubble")) {
        txsug <- ""
        
        if (getOption("suggest")) {
          txsug <- ">>> Suggestions"
            
          fc <- ""
          if (!grepl("ellipse", fncl)  &&  n.col == 1)
            fc <- paste(fc, ", ellipse=TRUE", sep="")
          if (grepl("ellipse", fncl)  &&  n.col == 1)
            fc <- paste(fc, ", fill.ellipse=\"off\"", sep="")
          if (!grepl("fit.line", fncl)) 
            fc <- paste(fc, ", fit.line=TRUE", sep="")
          if (grepl("fit.line=TRUE", fncl))
            fncl <- sub("fit.line=TRUE", "fit.line=\"ls\"", fncl)
          if (nzchar(fc)) {
            fc <- paste(fncl, fc, ") ", sep="")
            txsug <- paste(txsug, "\n", fc, sep="")
          }
         
          fc <- ""
          if (!grepl("se.fit.line", fncl))
            fc <- paste(fc, ", se.fit.line=1:3", sep="")
          if (nzchar(fc)) {
            fc <- gsub(" = ", "=", fc)
            txt <- ")   # fit line with standard errors of 1, 2 and 3"
            fc <- paste(fncl, fc, txt, sep="")
            txsug <- paste(txsug, "\n", fc, sep="")
          } 
            
          fc <- ""
          if (!grepl("ellipse", fncl)  &&  n.col == 1)
            fc <- paste(fc, ", ellipse=seq(.25,.95,.10)", sep="")
          if (!grepl("size", fncl))
            fc <- paste(fc, ", size=0", sep="")
          if (nzchar(fc)) {
            fc <- gsub(" = ", "=", fc)
            fc <- paste(fncl, fc, ")   # no points, shaded ellipses", sep="")
            txsug <- paste(txsug, "\n", fc, sep="")
          } 
          fc <- ""
          if (!grepl("smooth", fncl)  &&  smooth)
            fc <- paste(fc, ", smooth=FALSE", sep="")
          if (grepl("ellipse", fncl)) fncl <- .rm.arg.l("ellipse", fncl)
          if (nzchar(fc)) {
            fc <- paste(fncl, fc, 
                    ")  # smooth default for > 2500 rows of data", sep="")
            txsug <- paste(txsug, "\n", fc, sep="")
          }
          
          if (!smooth) {
            fc <- ""
            if (!grepl("size", fncl)  &&  n.col == 1)
              fc <- paste(fc, ", size=2", sep="")
            if (!grepl("shape", fncl)  &&  n.col == 1)
              fc <- paste(fc, ", shape=\"diamond\"", sep="")
            if (!grepl("bg", fncl)) 
              fc <- paste(fc, ", bg=\"off\"", sep="")
            if (!grepl("grid", fncl)) 
              fc <- paste(fc, ", grid=\"off\"", sep="")
            if (nzchar(fc)) {
              fc <- paste(fncl, fc, ") ", sep="")
              txsug <- paste(txsug, "\n", fc, sep="")
            }
          }

          if (!smooth) {
            fc <- ""
            if (!grepl("smooth", fncl))
              fc <- paste(fc, ", smooth=TRUE", sep="")
            if (!grepl("fit.line", fncl))
              fc <- paste(fc, ", fit.line=\"ls\"", sep="")
            if (grepl("ellipse", fncl)) fncl <- .rm.arg.l("ellipse", fncl)
            if (nzchar(fc)) {
              txt <- ")  # smoothed plot, least-squares fit line"
              fc <- paste(fncl, fc, txt, sep="")
              txsug <- paste(txsug, "\n", fc, sep="")
            }
          }

          if (object == "bubble") {
            fc <- ""
            smaller <- as.character(bubble.scale / 1.5)
            larger <- as.character(bubble.scale * 1.5)
            if (!grepl("bubble", fncl)) {
              if (bubble.scale >= 0.25) {
                fc <- paste(fc, ", bubble.scale=", smaller, sep="")
                txt <- "# smaller bubbles"
              }
              else {
                fc <- paste(fc, ", bubble.scale=", larger, sep="")
                txt <- "# larger bubbles"
              }
              if (nzchar(fc)) {
                fc <- paste(fncl, fc, ") ", sep="")
                fc <- paste(fc, txt, sep="")
                txsug <- paste(txsug, "\n", fc, sep="")
              }
          }
        }

        txsug <- .rm.arg.2(" x=", txsug) 
        txsug <- .rm.arg.2("(x=", txsug)
        txsug <- .rm.arg.2(" y=", txsug) 

        for (i in 1:n.col) {

          if (n.xcol > 1) {
            options(xname = colnames(x)[i])
            stuff <- .cr.main(x[,i], y[,1], brief=TRUE, ...) 
          }
          else {
            options(yname = colnames(y)[i])
            stuff <- .cr.main(x[,1], y[,i], brief=TRUE, ...) 

            txbck <- stuff$txb
            txdsc <- stuff$txd
            txinf <- stuff$txi

            class(txsug) <- "out_piece"
            class(txbck) <- "out_piece"
            class(txdsc) <- "out_piece"
            class(txinf) <- "out_piece"

            if (nzchar(txsug)  &&  i == 1)
              output <- list(out_suggest=txsug, out_background=txbck,
                out_describe=txdsc, out_inference=txinf,
                r=stuff$r, tvalue=stuff$tvalue, df=stuff$df, pvalue=stuff$pvalue,
                lb=stuff$lb, ub=stuff$ub)
            else
              output <- list(out_background=txbck,
                out_describe=txdsc, out_inference=txinf,
                r=stuff$r, tvalue=stuff$tvalue, df=stuff$df, pvalue=stuff$pvalue,
                lb=stuff$lb, ub=stuff$ub)

            class(output) <- "out_all"
            print(output)
          }
        }
      }
    }  # end traditional 2-way scatter plot


      # categorical var with numeric var for means plot or bubble-1D plot
      else if ((cat.x && !cat.y && !unique.x) || (!cat.x && cat.y && !unique.y)) {
   
        if (!bubble1) {  # means plot

        txsug <- ""
        if (getOption("suggest")) {
          txsug <- ">>> Suggestions"

          fc <- ""
          if (!grepl("means", fncl))
            fc <- paste(fc, ", means=FALSE", sep="")
          if (nzchar(fc)) {
            fc <- paste(fncl, fc, ") ", sep="")
            txsug <- paste(txsug, "\n", fc, "  # do not plot means", sep="")
          }
          
          fc <- ""
          if (!grepl("values", fncl)) {
            fc <- paste(fc, ", values=\"mean\"", sep="")
            if (grepl("means", fncl)) fncl <- .rm.arg.l("means", fncl) 
          }
          if (nzchar(fc)) {
            fc <- paste(fncl, fc, ") ", sep="")
            txsug <- paste(txsug, "\n", fc, "  # only plot means", sep="")
          }
   
          if (cat.x) {
            rv <- y.name
            pv <- x.name
            n.lvl <- length(unique(x))
          }
          else {
            rv <- x.name
            pv <- y.name
            n.lvl <- length(unique(y))
          }
          fnct <- ifelse(n.lvl == 2, "ttest", "ANOVA")
          fc <- paste("\n", fnct, "(", rv, " ~ ", pv,
                      ")  # inferential analysis", sep="")
          txsug <- paste(txsug, fc, sep="")
                   
          txsug <- .rm.arg.2(" x=", txsug) 
          txsug <- .rm.arg.2("(x=", txsug) 
          txsug <- .rm.arg.2(" y=", txsug) 

        }

          if (cat.x && !cat.y) {
            if (!is.null(x.lvl))  # convert back to a factor if was one
              x.by <- factor(x, levels=1:length(x.lvl), labels=x.lvl)
            else
              x.by <- x
            options(yname = x.name)  # reverse order of x and y for .ss.numeric
            options(xname = y.name)
            stats <- .ss.numeric(y, by=x.by, digits.d=digits.d, brief=TRUE)
          }
          else if (!cat.x && cat.y) {
            if (!is.null(y.lvl))  # convert back to a factor if was one
              y.by <- factor(y, levels=1:length(y.lvl), labels=y.lvl)
            else
              y.by <- y
            stats <- .ss.numeric(x, by=y.by, digits.d=digits.d, brief=TRUE)
          }

          txout <- stats$tx

          class(txout) <- "out_piece"

          output <- list(out_suggest=txsug, out_txt=txout)
          class(output) <- "out_all"
          print(output)
        }

        else {  # 1-D bubble plot of a factor var, y just a constant

        txsug <- ""
        if (getOption("suggest")) {
          txsug <- ">>> Suggestions"
          
          fc <- ""
          if (!grepl("color.low", fncl))
            fc <- paste(fc, ", color.low=\"lemonchiffon2\"", sep="")
          if (!grepl("color.hi", fncl))
            fc <- paste(fc, ", color.hi=\"maroon3\"", sep="")
          if (nzchar(fc)) {
            fc <- paste(fncl, fc, ") ", sep="")
            txsug <- paste(txsug, "\n", fc, sep="")
          }

          fc <- paste("Plot(", x.name,
                 ", values=\"count\")  # scatter plot of counts", sep="")
          txsug <- paste(txsug, "\n", fc, sep="")

          fc <- paste("Plot(", x.name, 
                 ", bar=TRUE)  # bar chart of counts", sep="")
          txsug <- paste(txsug, "\n", fc, sep="")
          
          txsug <- .rm.arg.2(" x=", txsug) 
          txsug <- .rm.arg.2("(x=", txsug) 

        }
        
          if (!is.null(x.lvl))
            x.by <- factor(x, levels=1:length(x.lvl), labels=x.lvl)
          else
            x.by <- factor(x)

          stats <- .ss.factor(x.by, by=NULL, brief=TRUE, digits.d=NULL,
                              x.name, y.name, x.lbl, y.lbl)

          txttl <- stats$title
          counts <- stats$count
          chi <- stats$chi

          class(txsug) <- "out_piece"
          class(txttl) <- "out_piece"
          class(counts) <- "out_piece"
          class(chi) <- "out_piece"
          output <- list(out_suggest=txsug, out_title=txttl,
                         out_counts=counts, out_chi=chi)
          class(output) <- "out_all"
          print(output)      
        }
        
      }


      # Cleveland dot plot
      else if (cleveland) { 

        txsug <- ""
        if (getOption("suggest")) {
          txsug <- ">>> Suggestions"
          fc <- ""
          if (!grepl("sort.yx", fncl))
            fc <- paste(fc, ", sort.yx=FALSE", sep="")
          if (!grepl("segments.y", fncl)) 
            fc <- paste(fc, ", segments.y=FALSE", sep="")
          if (!grepl("grid", fncl)) 
            fc <- paste(fc, ", grid=\"on\"", sep="")
          if (nzchar(fc)) {
            fncl <- .fun.call.deparse(fun.call) 
            fncl <- gsub(")$", "", fncl)  # get function call less closing
            fncl <- gsub(" = ", "=", fncl)
            fc <- paste(fncl, fc, ") ", sep="")
            txsug <- paste(txsug, "\n", fc, sep="")
            
            txsug <- .rm.arg.2(" x=", txsug) 
            txsug <- .rm.arg.2("(x=", txsug) 
         }
        }

        if (!is.null(y.lvl))
          # convert back to a factor if was one originally
          y.by <- factor(y, levels=1:length(y.lvl), labels=y.lvl)
        else
          y.by <- y

          txout <- ""
          for (i in 1:n.xcol) {
            stats <- .ss.numeric(x[,i], digits.d=digits.d, brief=TRUE)
            txout[length(txout)+1] <- paste("---", colnames(x)[i], "---")
            for (j in 2:length(stats$tx)) txout[length(txout)+1] <- stats$tx[j]
            if (i < n.xcol) {
              txout[length(txout)+1] <- ""
              txout[length(txout)+1] <- ""
            }
          }
      
      txotl <- ""
      txotl <- .outliers(x)
      if (txotl[1] == "") txotl <- "No (Box plot) outliers"

          class(txsug) <- "out_piece"
          class(txout) <- "out_piece"
          class(txotl) <- "out_piece"

          if (nzchar(txsug))
            output <- list(out_suggest=txsug, out_txt=txout, out_outliers=txotl)
          else
            output <- list(out_txt=txout, out_outliers=txotl)
          class(output) <- "out_all"
          print(output)
      }


      # categorical x and y vars
      else if (cat.x  &&  cat.y) {

        txsug <- ""
        if (getOption("suggest")) {
          txsug <- ">>> Suggestions"
          
          fc <- ""
          if (!grepl("bubble.text", fncl))
            fc <- paste(fc, ", bubble.text=FALSE", sep="")
          if (nzchar(fc)) {
            fncl <- .fun.call.deparse(fun.call) 
            fncl <- gsub(")$", "", fncl)  # get function call less closing )
            fncl <- gsub(" = ", "=", fncl)
            fc <- paste(fncl, fc, ") ", sep="")
            txsug <- paste(txsug, "\n", fc, sep="")
          }
           
          fc <- ""
          if (!grepl("color.trans", fncl))
            fc <- paste(fc, ", color.trans=.8", sep="")
          if (!grepl("bg", fncl))
            fc <- paste(fc, ", bg=\"off\"", sep="")
          if (!grepl("grid", fncl))
            fc <- paste(fc, ", grid=\"off\"", sep="")
          if (nzchar(fc)) {
            fncl <- .fun.call.deparse(fun.call) 
            fncl <- gsub(")$", "", fncl)  # get function call less closing )
            fncl <- gsub(" = ", "=", fncl)
            fc <- paste(fncl, fc, ") ", sep="")
            fc <- sub(",,", ",", fc, fixed=TRUE)  # hack
            txsug <- paste(txsug, "\n", fc, sep="")

          }
   
          fc <- paste("\nPlot(", x.name, ", ", y.name, 
                      ", bar=TRUE)  # bar chart", sep="")
          txsug <- paste(txsug, fc, sep="")
 
          fc <- paste("\nSummaryStats(", x.name, ", ", y.name, 
                      ")  # or ss", sep="")
                      
          txsug <- paste(txsug, fc, sep="")
          txsug <- .rm.arg.2(" x=", txsug) 
          txsug <- .rm.arg.2("(x=", txsug) 
          txsug <- .rm.arg.2(" y=", txsug) 
      
        }

        if (!is.null(x.lvl))
          x.fac <- factor(x, levels=1:length(x.lvl), labels=x.lvl)
        else
          x.fac <- x[,1]
        if (!is.null(y.lvl))
          y.fac <- factor(y, levels=1:length(y.lvl), labels=y.lvl)
        else
          y.fac <- y

        stats <- .ss.factor(x.fac, y.fac, brief=FALSE, digits.d=NULL,
                            x.name, y.name, x.lbl, y.lbl)

        txttl <- stats$txttl
        txfrq <- stats$txfrq
        txXV <- stats$txXV

        class(txsug) <- "out_piece"
        class(txttl) <- "out_piece"
        class(txfrq) <- "out_piece"
        class(txXV) <- "out_piece"
        if (!prop)
          output <- list(out_suggest=txsug, out_title=txttl, out_text=txfrq,
                         out_XV=txXV)
        else {
          txrow <- stats$txrow
          class(txrow) <- "out_piece"
          output <- list(out_title=txttl, out_text=txfrq,
                         out_row=txrow, out_XV=txXV)   }

        class(output) <- "out_all"
        print(output)
      }
      
    }
    
    else {  # line chart (object is "both")
  
      txsug <- ""
      if (getOption("suggest")) {
        txsug <- ">>> Suggestions"

        fc <- ""
        if (!grepl("size", fncl)  &&  size.pt > 0)
          fc <- paste(fc, ", size=0", sep="")
        if (nzchar(fc)) {
          fc <- gsub(" = ", "=", fc)
          fc <- paste(fncl, fc, ")   # just line segments, no points", sep="")
          txsug <- paste(txsug, "\n", fc, sep="")
        }
          
        fc <- ""
        if (!grepl("line.width", fncl))
          fc <- paste(fc, ", line.width=0", sep="")
        if (nzchar(fc)) {
          fc <- gsub(" = ", "=", fc)
          if (size.pt > 0)
            txt <- "just points, no line segments"
          else
            txt <- "just area"
          fc <- paste(fncl, fc, ")   # ", txt, sep="")
          txsug <- paste(txsug, "\n", fc, sep="")
        }
          
        fc <- ""
        if (!grepl("area", fncl)  &&  (!grepl("stack", fncl)))
          fc <- paste(fc, ", area=TRUE", sep="")
        if (nzchar(fc)) {
          fc <- gsub(" = ", "=", fc)
          fc <- paste(fncl, fc, ")   # default color fill", sep="")
          txsug <- paste(txsug, "\n", fc, sep="")
        }        

        txsug <- .rm.arg.2(" x=", txsug) 
        txsug <- .rm.arg.2("(x=", txsug) 
        txsug <- .rm.arg.2(" y=", txsug)   
      }
      
      class(txsug) <- "out_piece"
      output <- list(out_suggest=txsug)
      class(output) <- "out_all"
      print(output)
      
      # analyze runs
      if (!date.ts  &&  center.line != "off") {
        cat("n:", nrows, "\n")
        n.miss <- sum(is.na(y))
        cat("missing:", n.miss, "\n")
        cat(lbl.cat, round(m.y,digits.d), "\n")
        cat("\n")
        .dash(12); cat("Run Analysis\n"); .dash(12)
        run <- integer(length=0)  # length of ith run in run[i]
        n.runs <- 1  # total number of runs
        run[n.runs] <- 1
        line.out <- "    1"
        cat("\n")
        for (i in 2:length(y)) {
          if (y[i] != m.y) {  # throw out values that equal m.y
            if (sign(y[i]-m.y) != sign(y[i-1]-m.y)) {  # new run
              if (show.runs) {
                if (n.runs < 10) buf <- "  " else buf <- " "
                cat("size=", run[n.runs], "  Run", buf, n.runs, ":",
                    line.out, "\n", sep="")
              }
              line.out <- ""
              n.runs <- n.runs + 1
              run[n.runs] <- 0
            }
            run[n.runs] <- run[n.runs] + 1
            buf <- ifelse (i < 10, "  ", " ")
            line.out <- paste(line.out, buf, i)
          }
        }
        cat("size=", run[n.runs], "  Run", buf, n.runs, ":", line.out,
            "\n", sep="")
        eq.ctr <- which(y==m.y)
        cat("\nTotal number of runs:", n.runs, "\n")
        txt <- "Total number of values that do not equal the "
        cat(txt, lbl.cat, " ", length(y)-length(eq.ctr), "\n", sep="")
        if (length(eq.ctr) != 0) {
          if (show.runs) {
            cat("\nValues ignored that equal the", lbl.cat, "\n")
            for (i in 1:length(eq.ctr))
              cat("    #", eq.ctr[i], " ", y[eq.ctr[i]], sep="", "\n")
            cat("Total number of values ignored:", length(eq.ctr), "\n")
          }
        }
        else 
          cat("Total number of values ignored that equal the", lbl.cat, 
              length(eq.ctr), "\n")
      }  # end analyze runs
     
    }  # end line chart
    
  }  # end if (!quiet  &&  values == "data")

  else if (!quiet) {  # values not data

      if (cat.x  &&  !cat.y  &&  object %in% c("point", "bubble")) {
        txsug <- ""
        
        if (getOption("suggest")) {
          txsug <- ">>> Suggestions"
            
          fc <- ""
          if (!grepl("segments.x", fncl))
            fc <- paste(fc, ", segments.x=FALSE", sep="")
          if (nzchar(fc)) {
            fc <- paste(fncl, fc, ")  # just points", sep="")
            txsug <- paste(txsug, "\n", fc, sep="")
          }
         
          fc <- ""
          if (!grepl("bar", fncl))
            fc <- paste(fc, ", bar=TRUE", sep="")
          if (nzchar(fc)) {
            fc <- gsub(" = ", "=", fc)
            fc <- paste(fncl, fc, ")   # bar chart", sep="")
            txsug <- paste(txsug, "\n", fc, sep="")
          } 

        txsug <- .rm.arg.2(" x=", txsug) 
        txsug <- .rm.arg.2("(x=", txsug)
        txsug <- .rm.arg.2(" y=", txsug) 

        class(txsug) <- "out_piece"

        if (nzchar(txsug)) {
          output <- list(out_suggest=txsug)
          class(output) <- "out_all"
          print(output)
        }
      }
    }  # end values not data
    
  }
    
  cat("\n")

}  # end plt.main

