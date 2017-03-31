.plt.main <- 
function(x, y, by=NULL, n.cat=getOption("n.cat"),
         object="point", values="data",

         col.fill=getOption("fill.pt"),
         col.stroke=getOption("stroke.pt"),
         col.bg=getOption("bg"),
         col.grid=getOption("grid"),
         col.box=getOption("box"),

         col.trans=NULL, col.segment=col.stroke,
         col.area="transparent",

         cex.axis=0.76, col.axis="gray30", xy.ticks=TRUE,
         xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
         value.labels=NULL, label.max=20,
         rotate.x=0, rotate.y=0, offset=0.5, prop=FALSE,

         size=NULL, shape="circle", means=TRUE, 
         sort.yx=FALSE,
         segments.y=FALSE, segments.x=FALSE, size.ln=2,

         smooth=FALSE, points.smooth=100, trans.smooth=0.25,
         bins.smooth=128,

         radius=0.25, power=0.6, size.cut=TRUE, bubble.text="black",
         col.low=NULL, col.hi=NULL,

         ID=NULL, ID.cut=0, ID.color="gray50", ID.size=0.75,

         fit.line="off", col.fit.line="gray55", lwd.fit=1.5, se.fit=1,

         ellipse=FALSE, col.ellipse="lightslategray",
         fill.ellipse="off", lwd.ellipse,
         
         center.line="default", show.runs=FALSE, stack=FALSE,

         method="overplot", pt.reg="circle", pt.out="circle", 
         col.out30="firebrick2", col.out15="firebrick4", 

         freq.poly=FALSE,

         add=NULL, x1=NULL, x2=NULL, y1=NULL, y2=NULL,
         add.cex=NULL, add.lwd=1, add.lty="solid",
         add.stroke=NULL, add.fill=NULL, add.trans=NULL,

         quiet=getOption("quiet"), want.labels=TRUE, ...)  {

  date.ts <- ifelse (.is.date(x[,1]), TRUE, FALSE)

  size.pt <- size  # size is set in Plot.R

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

  do.ellipse <- ifelse(ellipse[1] > 0, TRUE, FALSE)

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
    if (col.area != "transparent")
      col.area <- .maketrans(col.area, (1-trans.pts)*256)
  }
           
  # scale for regular R or RStudio
  adj <- .RSadj(radius, cex.axis)
  radius <- adj$radius
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
  margs <- .marg(max.width, y.lab, x.lab, main.lab, x.val, prop, rotate.x)
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
      mn.x <- mn.x - .5
      mx.x <- mx.x + .5
    }
    if (cat.y) {
      mn.y <- mn.y - .5
      mx.y <- mx.y + .5
    }

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
    region <- rbind(region, c(mn.x, mn.y), c(mx.x, mx.y))
  }

  # plot: setup the coordinate system
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
    axT1 <-axTicks(1)  # else numeric, so all the ticks
  
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
              rotate.x, rotate.y, offset=offset, ...)
      }
      else {  # time 
        axis.Date(1, x.val, cex.axis=size.axis, col.axis=col.axis, ...)  # strptime
        .axes(NULL, y.val, axT1, axT2,
              par("usr")[1], par("usr")[3], size.axis, col.axis,
              rotate.x, rotate.y, offset=offset, y.only=TRUE, ...)  # only do y-axis
      }
    }
    else  # bubble1: 1-D scatter plot of categorical variable
      .axes(x.val, NULL, axT1, NULL,
            par("usr")[1], par("usr")[3], size.axis, col.axis, 
            rotate.x, rotate.y, offset=offset, ...)
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
  rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="transparent")

  # grid lines (put before box stroke of plot)
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

    # colors
    n.patterns <- max(n.col, n.by)

    stroke <- character(length=length(col.stroke))
    fill <- character(length=length(col.fill))
    ltype <- character(length=n.patterns)
    for (i in 1:length(ltype)) ltype[i] <- "solid"
    if (n.patterns == 1) {
      stroke[1] <- col.stroke[1]
      fill[1] <- col.fill[1]
    }
    else if (n.patterns == 2) {
      stroke[1] <- col.stroke[1]
      stroke[2] <- ifelse (length(col.stroke) > 1, col.stroke[2], col.stroke[1])
      fill[1] <- col.fill[1]
      if (object == "both") {
        ltype[2] <- "dotted"
        if (!(getOption("colors") %in% c("gray", "gray.black")))
          #fill[2] <- .col.discrete(bright=TRUE)[2]
          fill[2] <- fill[1]
        else {
          fill[2] <- fill[1]
          #fill[2] <- rgb(.15,.15,.15)
          #stroke[2] <- fill[2]
          stroke[2] <- rgb(.15,.15,.15)
        }
      }  # end object is both
      else { 
        if (length(col.fill) == 1)
          fill[2] <- "transparent"
        else
          fill[2] <- col.fill[2]
      }
    }  # n.patterns=2
    else  {  # n.patterns > 2
      stroke <- .col.discrete(bright=TRUE)[1:n.patterns]
      for (i in 1:length(stroke))
        fill[i] <- .maketrans(stroke[i], (1-trans.pts)*256)
    }
  
       
    # -----
    # lines
    if (object == "both") {
 
      if (n.xcol == 1  &&  n.ycol == 1) {
        if (n.by <= 1) {  # pure single panel, data in wide format 

          if (size.ln > 0)  # plot line(s)
            if (date.ts || freq.poly || object == "both") {
                lines(as.numeric(x[,1]), y[,1], col=col.segment, lwd=size.ln, ...)
          }  # size.ln > 0

          if (col.area != "transparent") # fill area
            polygon(c(x[1],x,x[length(x)]), c(min(y[,1]),y[,1],min(y[,1])),
                col=col.area, border="transparent")
        }  # n.by is 1

        else {  # n.by > 1,  tidy format, all y data in same column
          for (i in 1:n.by) {
            xl <- subset(x, by==levels(by)[i])
            if (i > 1) yo <- yl
            yl <- subset(y, by==levels(by)[i])

            if (stack) {
              if (i == 1) {
                xx <- c(xl[1],xl,xl[length(xl)])
                yy <- c(min(yl[,1]),yl[,1],min(yl[,1]))
                if (col.area != "transparent") 
                  polygon(xx, yy, col=fill[1], border="transparent")
              }
              if (i > 1) { 
                yl[,1] <- yl[,1] + yo[,1]
                xx <- c( c(xl[1],xl,xl[length(xl)]), rev(c(xl[1],xl,xl[length(xl)])) )
                yy <- c( c(min(yl[,1]),yl[,1],min(yl[,1])),
                           rev(c(min(yo[,1]),yo[,1],min(yo[,1]))) )
                if (col.area != "transparent") 
                  polygon(xx, yy, col=fill[i], border="transparent")
              }
            }

            if (size.ln > 0) { 
                lines(xl, yl[,1], col=stroke[i], lty=ltype[i], lwd=size.ln, ...)
            }  # end size.ln > 0
          }
        }
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

          if (size.ln > 0) { 
              lines(x[,1], y[,i], col=stroke[i], lty=ltype[i], lwd=size.ln, ...)
          }  # end size.ln > 0

        }  # end i loop

      }  # end n.ycol > 1

      if (n.xcol > 1) {
        if (size.ln > 0) for (i in 1:n.xcol)
          lines(as.numeric(x.val),y[,1], col=fill[i], lwd=size.ln, ...)
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
          smoothScatter(x, y, nrpoints=points.smooth, nbin=bins.smooth,
                        transformation=function(x) x^(trans.smooth),
                        colramp = clr.den, add=TRUE)
          abline(v=axT1, col=col.grid, lwd=.5) # grid lines (off by default)
          if (!bubble1) abline(h=axT2, col=col.grid, lwd=.5)
        }
    
        else {  # plot the individual points, plus means, segments, etc.

          if (n.ycol == 1) {  # one y
            for (i in 1:n.xcol) {  # one to many x's
                points(x[,i], y[,1], pch=shape, col=stroke[i], bg=fill[i],
                       cex=size.pt, ...)
            }
          }
          else {
            for (i in 1:n.ycol) {  # one to many y's 
                if (n.patterns == 2  &&  i == 2) fill[i] <- "transparent"
                points(x[,1],y[,i], pch=shape, col=stroke[i], bg=fill[i],
                       cex=size.pt, ...)  # one x
            }
          }

          # label points with ID
          do.pts <- TRUE
          if (!is.null(size)) if (size == 0) do.pts <- FALSE

          if (ID.cut > 0 && do.pts) if (n.xcol == 1  &&  n.ycol == 1) {

            m.x <- mean(x, na.rm=TRUE)
            m.y <- mean(y, na.rm=TRUE)
            center <- c(m.x, m.y)

            mat <- matrix(c(x,y), ncol=2)
            cov.mat <- cov(mat, use="complete.obs")

            dst <- numeric(length=nrow(mat))
            for (i in 1:nrow(mat))
              dst[i] <- mahalanobis(c(mat[i,1], mat[i,2]), center, cov.mat)
            ord <- order(dst, decreasing=TRUE)

            df <- data.frame(x, y, dst, ID)
            df <- df[ord,]
            df <- na.omit(df)
            df <- subset(df, dst > quantile(dst, 1-ID.cut))
            text(df[,1], df[,2], labels=df[,4],
                 pos=1, offset=0.4, col=ID.color, cex=ID.size)
          }  # end label points

          if (segments.y) { 
            if (n.xcol == 1) # line segments from points to axis
              segments(x0=min(pretty(x)), y0=y, x1=x, y1=y, 
                       lty=1, lwd=.75, col=col.stroke)
            else if (n.xcol == 2)  # line segments between points
              segments(x0=x[,1], y0=y[,1], x1=x[,2], y1=y[,1], 
                       lty=1, lwd=.75, col=col.segment)
          }

          if (!(values %in% c("count", "prop"))) {
            if (segments.x)
              segments(y0=par("usr")[3], x0=x, y1=y, x1=x, lty=1, lwd=.75,
                       col=col.segment)
          }
          else {
            if (segments.x) 
              if (n.xcol == 1)
                 segments(y0=0, x0=x, y1=y, x1=x, lty=1, lwd=1, col=col.segment)
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
          points(x.lv, y.lv[,1], pch=shp[i], col=clr[i], bg=fill[i], cex=size.pt,
                 lwd=0.75, ...)
        }

        .plt.by.legend(levels(by), stroke, fill, shp, trans.pts, col.bg, usr)

        }  # end by

      }  # end object is point or both
      
      # plot legend as needed
      if (n.xcol > 1)  # horizontal legend, on x-axis
        .plt.legend(colnames(x), TRUE, stroke, fill, shape, col.bg, usr, 
                    cex.lab=size.lab)
      if (n.ycol > 1)  # vertical legend, on y-axis
        .plt.legend(colnames(y), FALSE, stroke, fill, shape, col.bg, usr, 
                    cex.lab=size.lab)

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
            xx[k] <- as.numeric(rownames(mytbl)[i])  # rownames are factors
            yy[k] <- as.numeric(colnames(mytbl)[j])
          }
        }
      }
      if (prop) count <- round(count, 2)
      cords <- data.frame(xx, yy, count)

      if (object == "bubble") {
        sz <- cords[,3]**power  # radius unscaled 
        symbols(cords$xx, cords$yy,
            circles=sz, inches=radius,
            bg=clr, fg=clr.stroke, add=TRUE, ...)
        mxru <- max(sz)
        sz <- 2 * (sz/mxru) * radius  # scaled diameter (symbols does)
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
      sz <- cords[,3]**power  # radius unscaled 
      symbols(cords[,1], cords[,2], circles=sz,
        inches=radius, bg=clr, fg=clr.stroke, add=TRUE, ...)  
      mxru <- max(sz)
      sz <- 2 * (sz/mxru) * radius  # scaled diameter
    }


    if (size.cut  &&  object == "bubble") { 

      # get q.ind before setting too small bubbles at NA
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


  #ellipse option
  #if (do.ellipse) {
    #for (i in 1:length(ellipse)) {
      #e <- ellipse(cxy, scale=c(s.x, s.y), centre=c(m.x, m.y),  
                   #level=ellipse[i], npoints=200)
      #polygon(e, border=col.ellipse, col=fill.ellipse, lwd=1)
    #}
  #}


  # ellipse option
  if (do.ellipse) { 

    for (i in 1:n.patterns) {
      if (n.patterns == 1) {  # one plot, all the data
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

        clr <- ifelse (length(stroke) == 1, stroke, stroke[i])

      }  # end multiple

      for (j in 1:length(ellipse)) {
        cxy <- cor(x.lv, y.lv, use="complete.obs")
        m.x <- mean(x.lv, na.rm=TRUE) 
        m.y <- mean(y.lv, na.rm=TRUE) 
        s.x <- sd(x.lv, na.rm=TRUE) 
        s.y <- sd(y.lv, na.rm=TRUE) 

        ln.type <- ifelse (n.patterns == 2 && i == 2, "dashed", "solid")
        corr <- ifelse (n.patterns == 1, cxy, cxy[1,1])
        col.border <- ifelse (n.patterns == 1, col.ellipse, clr)

        e <- ellipse(corr, scale=c(s.x, s.y), centre=c(m.x, m.y),
                     level=ellipse[j])
        polygon(e, border=col.border, col=fill.ellipse,
                lwd=lwd.ellipse, lty=ln.type)
      }  # jth ellipse
    }  # ith pattern
  }  # do.ellipse



  # fit line option
  if (fit.line != "off") { 

    for (i in 1:n.patterns) {
      if (n.patterns == 1) {  # one plot, all the data
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

        clr <- ifelse (length(stroke) == 1, stroke, stroke[i])
      }  # end multiple

      ln.type <- "solid"
      if (n.patterns == 2 && i == 2) ln.type <- "dashed"

      ok <- is.finite(x.lv) & is.finite(y.lv)
      if (any(ok)) {
        x.lv <- x.lv[ok]
        y.lv <- y.lv[ok]
        od <- order(x.lv)
        x.lv <- x.lv[od]
        y.lv <- y.lv[od] 

        if (fit.line == "loess")
          l.ln <- loess(y.lv ~ x.lv)
        else if (fit.line == "ls")
          l.ln <- lm(y.lv ~ x.lv)

        # fit line
        f.ln <- fitted(l.ln, ...)
        lines(x.lv, f.ln, col=clr, lwd=lwd.fit, lty=ln.type)

        # se bands about fit line
        for (j in 1:length(se.fit)) {
          p.ln <- predict(l.ln, se=TRUE)
          up.ln <- fitted(l.ln, ...) + (se.fit[j] * p.ln$se.fit)
          dn.ln <- fitted(l.ln, ...) - (se.fit[j] * p.ln$se.fit)
          lines(x.lv, up.ln, col=clr, lwd=0.5, lty=ln.type)
          lines(x.lv, dn.ln, col=clr, lwd=0.5, lty=ln.type)
          xx <- c(x.lv, rev(x.lv))
          yy <- c(up.ln, rev(dn.ln))
          polygon(xx, yy, col=getOption("se.fill"), border="transparent")
        }  # end for each se plot
      }

    }  # ith pattern
  }  # fit.line


  # add enhancements

  if (!is.null(add)) if (add[1] == "means") {
    add[1] <- "v.line"
    add[2] <- "h.line"
    x1 <- "mean.x"
    y1 <- "mean.y"
  }
 
  if (!is.null(x1)) {
    x1[which(x1 == "mean.x")] <- mean(x, na.rm=TRUE)
    x1 <- as.numeric(x1)
  }
  if (!is.null(y1)) {
    y1[which(y1 == "mean.y")] <- mean(y, na.rm=TRUE)
    y1 <- as.numeric(y1)
  }

  if (!is.null(add))
    .plt.add (add, x1, x2, y1, y2,
         add.cex, add.lwd, add.lty, add.stroke, add.fill, add.trans)

}  # end plt.main

