.bc.main <- 
function(x, y, by, 
         fill, col.color, col.trans, theme,
         horiz, addtop, gap, prop, scale.y,
         xlab, ylab, main,
         value.labels, label.max, beside,
         rotate.x, offset, break.x, sort.x,
         values, values.color, values.cex, values.digits, values.pos,
         xlab.adj, ylab.adj, bm.adj, lm.adj, tm.adj, rm.adj,
         legend.title, legend.loc, legend.labels, legend.horiz,
         add, x1, x2, y1, y2, out.size, quiet, ...) {

  # if x is integer, not labeled correctly
  if (length(unique(x)) != length(x)) if (!is.factor(x)) x <- factor(x)
  
  if ( (is.table(x) || is.matrix(x)) && is.null(legend.title) ) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "Need to specify a value for:  legend.title\n\n")
  }

  # --------------------------------------
  # axis values, axis labels, legend setup

  # get axis.x.cex, axis.y.cex
  axis.x.cex <- ifelse(is.null(getOption("axis.x.cex")), 
    getOption("axis.cex"), getOption("axis.x.cex"))
  adj <- .RSadj(axis.cex=axis.x.cex); axis.x.cex <- adj$axis.cex
  axis.y.cex <- ifelse(is.null(getOption("axis.y.cex")), 
    getOption("axis.cex"), getOption("axis.y.cex"))
  adj <- .RSadj(axis.cex=axis.y.cex); axis.y.cex <- adj$axis.cex

  # get lab.x.cex  lab.y.cex
  lab.cex <- getOption("lab.cex")
  lab.x.cex <- getOption("lab.x.cex")
  lab.y.cex <- getOption("lab.y.cex")
  lab.x.cex <- ifelse(is.null(lab.x.cex), lab.cex, lab.x.cex)
  adj <- .RSadj(lab.cex=lab.x.cex); lab.x.cex <- adj$lab.cex
  lab.y.cex <- ifelse(is.null(lab.y.cex), lab.cex, lab.y.cex)
  adj <- .RSadj(lab.cex=lab.y.cex); lab.y.cex <- adj$lab.cex

  gl <- .getlabels(xlab, ylab, main, by.nm=TRUE, lab.x.cex=lab.x.cex, 
                   lab.y.cex=lab.y.cex, flip=horiz)
  x.name <- gl$xn;  x.lbl <- gl$xl;  y.lbl <- gl$yl
  x.lab <- ifelse (horiz, gl$yb, gl$xb)
  main.lab <- gl$mb
  by.name <- y.lbl
  if (!is.null(by)) by.name <- getOption("byname")

  ylab.keep <- ifelse (is.null(ylab), FALSE, TRUE)

  if (ylab.keep) {
      y.lab <- ylab
  }
  else {  # First part of y-axis label
    if (!is.vector(x)) {
      txt <- "Proportion"
      if (!is.null(by) && prop && !beside)
        txt <- paste("Cell Proportion within")
      if (is.null(y))
         if (!prop) ylab <- "Count" else ylab <- txt
    }
    else {
      y.lab <- x.name
      if (length(unique(x)) != length(x)) x.lab <- ""
    }
  }

  if (is.null(ylab))
    done <- FALSE
  else
    done <- ifelse (grepl("of", ylab, fixed=TRUE), TRUE, FALSE) 

  if (!ylab.keep) {
    if ((!prop || is.null(by)) && is.null(y) && !done && !is.vector(x))
      y.lab <- paste(ylab, "of", x.name)
    if (!is.null(by)) {
      if (!beside) {
        txt <- paste(ylab, "of", x.name)
        y.lab <- ifelse (!prop, txt, paste(ylab, x.name, "by", by.name))
      }
      else
        y.lab <- ifelse(prop, "Proportion", "Count")
    }
    if (!is.null(y)) y.lab <- getOption("yname")
  }

  if (is.matrix(x)) {  # get the variable names as counts entered directly
    options(xname = x.lab)
    options(byname = legend.title)
  }

  # get legend title, l.lab
  if (!is.null(legend.title))
    l.lab <- legend.title 
  else
    if (!is.null(by)) if (exists("y.lbl"))
      l.lab <- ifelse (length(y.lbl) == 0, by.name, y.lbl)

  # title
  main.lbl <- ifelse (!is.null(main), main, "")


  # --------------------------
  # get missing, data entered? 

  x.miss <- sum(is.na(x))
  by.miss <- NULL
  by.miss <- if (!is.null(by)) sum(is.na(by))

  # entered counts typically integers as entered but stored as type double
  # if names(x) or rownames(x) is null, likely data from sample and c functions
  # y is getting data values directly from a data frame with counts entered
  entered.pre <- FALSE
  if (!is.matrix(x) && !is.null(names(x))) entered.pre <- TRUE
  if (is.matrix(x) && !is.null(rownames(x))) entered.pre <- TRUE
  entered <- ifelse (!is.integer(x) && is.double(x) && entered.pre, TRUE, FALSE)

  if (is.null(by) && beside && !entered) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "beside=TRUE  is not valid for analysis of only one variable.\n\n")
  }


  # -------------------------------------------
  # get Y variable, either directly or tabulate

  # do not tabulate counts, y is provided
  if (!is.null(y)) {
    entered <- TRUE
	
    if (is.null(by)) {  # no by variable
      yn <- getOption("yname")
      if (!is.numeric(y) > 0) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "y-values, those from the 2nd unlabeled variable, ", yn, ",",
          " must be\n", " numeric\n\n",
          "A  by  variable is categorical, with a small number of values\n",
          "It appears that ", yn, " is a by variable\n",
          "As of lessR 3.5.6 to specify a  by  variable in the function call\n",
          "  precede its name with:  by=", "\n\n")
      } 

      if (anyDuplicated(names(x)) > 0) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "The data contain duplicated values of variable: ", x.name, "\n\n")
      } 

      if (length(y) > 20) {
        warning(call.=FALSE, "There are more than 20 categories to plot\n\n",
            "Perhaps you mean for the 2nd variable, ", yn, ", to be a",
            "by  variable even though it is numeric\n\n",
            "A by variable only has a small number of values\n\n",
            "Explicitly precede the variable's name with:  by=", "\n\n")
     }

      x.temp <- x
      x <- y
      names(x) <- x.temp
      x <- as.table(x)
      if (prop) {
        x.count <- x  # save table of counts for possible bar display
        x <- x/sum(x)
      }
    }

    else {  # a by variable
      x.temp <- x
      do.row <- ifelse (x[1] == x[2], FALSE, TRUE)  # x is outer loop
      m <- matrix(y, nrow=length(levels(by)), byrow=do.row)
      colnames(m) <- unique(x) 
      rownames(m) <- unique(by) 
      m <- as.table(m, dnn=c(by.name, x.name))
      names(dimnames(m)) <- c(by.name, x.name) 
      x <- m
      if (prop) {
        x.count <- x  # save table of counts for possible bar display
        x <- prop.table(x, 2)
      }
    }
  }  # end !is.null(y)


  # tabulate for counts

  if (!entered) {  
    if (is.null(by)) {
      x.temp <- x  # save counts, to be restored for text output
      x <- table(x, dnn=NULL)
      if (prop) {
        x.count <- x  # save table of counts for possible bar display
        x <- x/sum(x)
      }
    }
    else {  # a by variable
      if (length(x) == length(by))
        x.temp <- table(by,x, dnn=c(by.name, x.name))
      else {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        x.name, " and ", by.name, " must be of the same size\n\n",
        "Size of ", x.name, ": ", length(x), "\n", 
        "Size of ", by.name, ": ", length(by), "\n\n", sep="")
      }
      x <- table(by, x, dnn=c(by.name, x.name))
      if (prop) {
        x.count <- x  # save table of counts for possible bar display
        x <-prop.table(x, 2)
      }
    }
  }  # end !entered so tabulate

  if (prop) {
    if (any(is.nan(x))) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Some cells (frequencies) are zero\n",
        "Division to calculate proportions not possible\n",
        "Run analysis without  prop  to identify the 0 cells\n\n")
    }
  }

  # ------------
  # sort options

  if (sort.x != "off") {
    srt.dwn <- ifelse (sort.x == "down", TRUE, FALSE)
    if (is.null(by)) {
      x <- x[order(x, decreasing=srt.dwn)]
    }
    else {
      x.df <- as.data.frame.matrix(x, nrow=2)
      x.df <- x.df[order(apply(x.df, 2, sum), decreasing=srt.dwn)]
      x <- as.table(as.matrix(x.df))
    }
  }


  # ------
  # colors

  # if use getColors to generate a range of colors
  if (is.null(by) && !is.matrix(x))
    n.cat <- length(x)  
  else
    n.cat <- nrow(x)
  if (!is.null(by)) n.cat <- nrow(x)  # num cat for by variable
  # see if apply a pre-defined color range
  if (length(fill) == 1) fill <- .color.range(fill, n.cat)  # range

  # get n.colors (does not indicate fill multiple colors)
  if (is.null(by) && !is.matrix(x))
    n.colors <- length(fill)
  else
    n.colors <- nrow(x)
  if (!is.null(by)) n.colors <- nrow(x)
  
  if (length(fill) < n.colors)  # two-variable bar chart
    fill <- getColors(n=length(unique(by)))

  if (!is.null(col.trans)) 
    for (i in 1:n.colors) fill[i] <- .maketrans(fill[i], (1-col.trans)*256) 

  if (n.colors > 1) {
    palette(fill)
    colr <- 1:n.colors   # colr is a sequence of integers
  }
  else  # colr is a color
    colr <- ifelse (is.null(fill), getOption("bar.fill"), fill)


  # ----------------------------------------------------------------------------
  # preliminaries

  if (values.pos == "out") addtop <- addtop + .02  # kludge
  max.y <- ifelse (is.matrix(x) && !beside, max(colSums(x)), max(x))
  max.y <- max.y + (addtop * max.y)

  if (any(x < 0)) {
    min.y <- ifelse (is.matrix(x) && !beside, min(colSums(x)), min(x))
    min.y <- min.y - abs(addtop * min.y)
  }
  else
    min.y <- 0

  if (is.null(legend.labels)) legend.labels <- row.names(x)
  if (beside) legend.horiz <- FALSE
  if ((!is.null(by) || is.matrix(x)) && !beside) legend.horiz <- TRUE

  if (is.null(gap)) {  # ifelse does not work here when gap is a vector
    if (!is.null(by) && beside)
      gap <- c(0.1,1)
    else
      gap <- 0.2
  }

  # get the names
  the.names <- integer(length=0)
  if (length(dim(x)) == 0)
    the.names <- names(x)
  else {
    if (is.null(by))  # ifelse does not work
      the.names <- rownames(x)
    else
      the.names <- colnames(x)
  }

  # labels horiz or vertical
  las.value <- ifelse (horiz  && max(nchar(the.names)) > 5, 0, 1)

  
  # ------------
  # value labels

  if (!is.null(value.labels)) {
    val.lab <- value.labels
  }
  else {
    if (is.null(by))
      val.lab <- names(x)
    else
      val.lab <- colnames(x)
    if (length(val.lab) == 0) val.lab <- colnames(x)  # read matrix directly
    if (!is.null(names(y))) val.lab <- names(y)
  }

  # for each value label, partition into mx.x.val.ln lines if (break.x)
  mx.x.val.ln <- 1
  ln.val <- integer(length=length(val.lab))
  if (break.x) {
    stuff <- .get.val.ln(val.lab, x.name)
  }
  val.lab <- stuff$val.lab 
  mx.x.val.ln <- stuff$mx.val.ln

  mx.y.val.ln <- 1

  if (is.null(y)) if (horiz) {  # switch
    temp <- x.lab;  x.lab <- y.lab;  y.lab <- temp
    temp <- mx.x.val.ln;  mx.x.val.ln <- mx.y.val.ln;  mx.y.val.ln <- temp
  }

  # set max.val.width to get max width of y-axis labels for lm adjustment
  lblval.y <- character(length=0)
  if (is.null(scale.y)) {
    prety <- max(pretty(c(min.y, max.y)))
    mx.num <-  ifelse (!prop, as.character(prety), .fmt(prety, 2))
    max.y.width <- max(strwidth(mx.num, cex=axis.y.cex, units="inches"))

  }
  else {  # need scale to be defined to get y.coords, not done till later
    ax.num <- ifelse(horiz, 1, 2)  # location of numerical axis
    y.coords <- axTicks(ax.num, axp=scale.y)
    nd <- 0
    for (i in 1:length(y.coords))
      if (.num.dec(y.coords[i]) > nd) nd <- .num.dec(y.coords[i])
    if (nd > 2) nd <- 2  # only allow a few decimal digits
    for (i in 1: length(y.coords))
      lblval.y[i] <- as.character(.fmt(y.coords[i], nd))
    j <- which(nchar(lblval.y) == max(nchar(lblval.y)))[1]
    max.y.width <- max(strwidth(lblval.y[j], cex=axis.y.cex, units="inches"))
  }

  max.x.width <- NULL
  if (horiz  ||  rotate.x == 90) {  # "y"-axis is categorical (i.e., x-axis)
    val.split <- unlist(strsplit(val.lab, "\n"))  # break into separate words
    if (horiz)
      max.y.width <- max(strwidth(val.split, cex=axis.x.cex, units="inches"))
    else
      max.x.width <- max(strwidth(val.split, cex=axis.x.cex, units="inches"))
  }

  
  # ----------------
  # set up plot area

  margs <- .marg(max.y.width, y.lab, x.lab, main.lab,
                rotate.x, mx.x.val.ln, mx.y.val.ln,
                lab.x.cex=lab.x.cex, lab.y.cex=lab.y.cex, max.x.width)
  lm <- margs$lm
  tm <- margs$tm
  rm <- margs$rm
  bm <- margs$bm
  n.lab.x.ln <- margs$n.lab.x.ln
  n.lab.y.ln <- margs$n.lab.y.ln

  if (horiz)
    rm <- rm + 0.1  # sometimes the max axis value goes off the plot
  else 
    if (offset > 0.5) bm <- bm + (-0.05 + 0.2 * offset)  # offset kludge

  if (legend.loc == "right.margin"  &&  (!is.null(by) || is.matrix(x)))
    rm <- rm + .82

  # user manual adjustment
  bm <- bm + bm.adj
  lm <- lm + lm.adj
  tm <- tm + tm.adj
  rm <- rm + rm.adj

  orig.params <- par(no.readonly=TRUE)
  on.exit(par(orig.params))


  # rescale to control bar width for small number of bars
  # set rescale
  if (class(x) == "numeric"  &&  entered) x <- as.table(x)
  rescale <- 0
  if (is.null(by)) if (nrow(x) <= 4) rescale <- nrow(x)
  if (!is.null(by) && !beside) if (ncol(x) <= 4) rescale <- ncol(x)
  if (class(x) == "matrix" && entered) rescale <- 0  # turned off for now
  # set width.bars, gap
  if (rescale == 4) width.bars <- .17
  if (rescale == 3) width.bars <- .22
  if (rescale == 2) width.bars <- .28
  if (rescale > 0) gap <- 0.246 + (0.687 * width.bars)

  par(bg=getOption("window.fill"))
  par(mai=c(bm, lm, tm, rm))

  # barplot run here only to establish usr coordinates, axTick values
  #  otherwise usr is just 0,1 for both axes
  # the barplot itself is not retained
  if (rescale == 0) {
    if (!horiz)
      barplot(x, col="transparent", ylim=c(min.y,max.y), axisnames=FALSE,
        beside=beside, space=gap, axes=FALSE, ...)
    else
      barplot(x, col="transparent", horiz=TRUE, axisnames=FALSE,
        beside=beside, space=gap, axes=FALSE, xlim=c(min.y, max.y), ...)
  }
  else { # rescale, need (0,1) limit on cat axis for re-scale to work
    if (!horiz)
      barplot(x, col="transparent", ylim=c(min.y,max.y), axisnames=FALSE,
        beside=beside, space=gap, width=width.bars, xlim=c(0,1),
        axes=FALSE, ...)
    else
      barplot(x, col="transparent", horiz=TRUE, axisnames=FALSE,
        beside=beside, space=gap, width=width.bars, xlim=c(min.y, max.y),
        ylim=c(0,1), axes=FALSE, ...)
  }

  ax.num <- ifelse(horiz, 1, 2)  # location of numerical axis
  y.coords <- axTicks(ax.num, axp=scale.y)


  ## ----
  ## PLOT

  usr <- par("usr")

  # panel fill
  col.bg <- getOption("panel.fill")
  rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="transparent")

  # grid lines for y-axis only
  .grid(ifelse(horiz, "v", "h"), y.coords) 

  # box around plot
  rect(usr[1], usr[3], usr[2], usr[4], col="transparent",
       border=getOption("panel.color"),
       lwd=getOption("panel.lwd"), lty=getOption("panel.lty"))

  # the bars
  if (rescale == 0)
    x.coords <- barplot(x, add=TRUE, col=fill, beside=beside, horiz=horiz,
          axes=FALSE, ann=FALSE, border=col.color, las=las.value, 
          space=gap, axisnames=FALSE, ...)
  else
    x.coords <- barplot(x, add=TRUE, col=fill, beside=beside, horiz=horiz,
          axes=FALSE, ann=FALSE, border=col.color, las=las.value, 
          space=gap, width=width.bars, xlim=c(0,1), axisnames=FALSE, ...)

  # text labels on or above the bars
  if (values != "off") {

    if (!prop) {
      if (values == "input")
        x.txt <- as.character(x)
      else if (values == "%")
        x.txt <- paste(.fmt(x/sum(x) * 100, values.digits), "%", sep="")
      else if (values == "prop")
        x.txt <- .fmt(x/sum(x), values.digits)
    }
    else {  # prop
      if (values == "input")
        x.txt <- as.character(x.count)
      else if (values == "%")
        x.txt <- paste(.fmt(x * 100, values.digits), "%", sep="")
      else if (values == "prop")
        x.txt <- .fmt(x, values.digits)
    }

    # as.numeric & as.character convert table to vector: re-convert
    if (!is.null(by)) x.txt <- matrix(x.txt, nrow=nrow(x))

    if (!horiz) {
      if (is.null(by)) {
        if (values.pos == "out") {
           usr.y.in <- diff(grconvertY(0:1, 'inches', 'user'))
           ycrd <- x + .1 * usr.y.in 
        } 
        else
          ycrd <- x/2
        text(x.coords, ycrd, labels=x.txt, col=values.color, cex=values.cex)
      }  # no by
      else {
        for (i in 1:ncol(x)) {
          mid.x <- cumsum(x[,i]) - (x[,i] / 2)
          text(x.coords[i], mid.x, labels=x.txt[,i],
             col=values.color, cex=values.cex) 
        }
      }
    }
    else {  # horiz
      if (is.null(by)) {
        if (values.pos == "out") {
           usr.x.in <- diff(grconvertX(0:1, 'inches', 'user')) 
           ycrd <- x + .17 * usr.x.in 
        } 
        else
          ycrd <- x/2
        text(ycrd, x.coords, labels=x.txt, col=values.color, cex=values.cex)
      }  # no by
      else {
        for (i in 1:ncol(x)) {
          mid.x <- cumsum(x[,i]) - (x[,i] / 2)
          text(mid.x, x.coords[i], labels=x.txt[,i],
             col=values.color, cex=values.cex) 
        }
      }  # end !horiz
    }
  }  # end values

  # y-axis
  if (!horiz) las.value <- 1
  axis.y.color <- ifelse(is.null(getOption("axis.y.color")), 
    getOption("axis.color"), getOption("axis.y.color"))
  axis.y.text.color <- ifelse(is.null(getOption("axis.y.text.color")), 
    getOption("axis.text.color"), getOption("axis.y.text.color"))
  adj1 <- ifelse (!horiz, 0.5, -1.0)
  if (is.null(scale.y))  # if scale.y defined, can evaluate earlier
    lblval.y <- as.character(y.coords)
  axis(ax.num, col=axis.y.color,   # maybe split off the text like with x axis?
       col.axis=axis.y.text.color, cex.axis=axis.y.cex, las=las.value,
       tck=-.03, padj=adj1,  at=y.coords, labels=lblval.y, ...) 
       #tck=-.03, padj=adj1,  at=axTicks(ax.num, axp=scale.y), ...) 
  
  # x-axis is the category value axis
  axis.x.color <- ifelse(is.null(getOption("axis.x.color")), 
    getOption("axis.color"), getOption("axis.x.color"))
  axis.x.text.color <- ifelse(is.null(getOption("axis.x.text.color")), 
    getOption("axis.text.color"), getOption("axis.x.text.color"))


  if (beside) x.coords <- apply(x.coords, 2, mean)  # one label per group
  if (!horiz) {
    usr.y.in <- diff(grconvertY(0:1, 'inches', 'user'))  # y user x.coords inch
    ax.value <- 1;  xx <- x.coords;  yy <- par("usr")[3] - (.15 * usr.y.in)
  }
  else {
    usr.x.in <- diff(grconvertX(0:1, 'inches', 'user'))  # x user x.coords inch
    ax.value <- 2;  xx <- par("usr")[1] - (.15 * usr.x.in);  yy <- x.coords
  }

  adj.x <- ifelse(!horiz, 0.5, 1.0)
  adj.y <- ifelse(!horiz, 1.0, 0.5)
  if (rotate.x == 0) {
    axis(ax.value, at=x.coords, labels=FALSE, tck=-.02, col=axis.x.color, ...)
    text(x=xx, y=yy, labels=val.lab, adj=c(adj.x,adj.y),
         xpd=TRUE, cex=axis.x.cex, col=axis.x.text.color, ...)
  }
  else if (rotate.x > 0  && rotate.x < 90) {
    axis(ax.value, at=x.coords, labels=FALSE, tck=-.02, col=axis.x.color, ...)
    text(x=xx, y=yy, labels=val.lab, pos=1,  # pos needed for offset
         xpd=TRUE, cex=axis.x.cex, col=axis.x.text.color,
         srt=rotate.x, offset=offset, ...)
  }
  else if (rotate.x == 90)  # 90 degrees rotate 
    axis(ax.value, at=x.coords, labels=val.lab, tck=-.02, col=axis.x.color,
         cex.axis=axis.x.cex, las=2, ...)

  # title
  title(main=main.lab, cex.main=getOption("main.cex"),
        col.main=getOption("main.color"))

  lab.x.color <- ifelse(is.null(getOption("lab.x.color")), 
    getOption("lab.color"), getOption("lab.x.color"))

  # xlab positioning
  ln.ht.x <- par('cin')[2] * lab.x.cex * par('lheight')  # line ht inches
  xlab.adj <- xlab.adj / ln.ht.x
  lblx.lns <- par("mar")[1] - 1.3   # par("mar")[1] is bm in lines
  title(xlab=x.lab, line=lblx.lns-xlab.adj, cex.lab=lab.x.cex,
        col.lab=lab.x.color)

  # ylab positioning (based on .axlabs function)
  lab.y.color <- ifelse(is.null(getOption("lab.y.color")), 
  getOption("lab.color"), getOption("lab.y.color"))
  ln.ht.y <- par('cin')[2] * lab.y.cex * par('lheight')  # line ht inches
  ylab.adj <- ylab.adj / ln.ht.y
  lm <- par("mar")[2]  # get current left margin in lines
  lbly.lns <- lm - (0.3 + 0.9*n.lab.y.ln)
  title(ylab=y.lab, line=lbly.lns-ylab.adj,
        cex.lab=lab.y.cex, col.lab=lab.y.color)


  # ------------------------------------------------------
  # legend for two variable plot including variable labels

  if ( (!is.null(by) || is.matrix(x)) && !is.null(legend.loc))  {

    col.txt <- ifelse (sum(col2rgb(col.bg))/3 > 80, "black", rgb(.97,.97,.97))

    # default right.margin option
    if (legend.loc == "right.margin") {

      options(byname = getOption("byname"))
      trans.pts <- .6  # dummy value
      .plt.by.legend(legend.labels, col.color, fill, shp=22, trans.pts,
                     col.bg, usr, pt.size=1.6, pt.lwd=0)

    }  # right margin

    else
      legend(legend.loc, legend=legend.labels, title=l.lab, fill=colr, 
             horiz=legend.horiz, cex=.7, bty="n", text.col=col.txt)
  }


  # ----------------------------------------------------------------------------
  # text output

  if (prop) {
    if (is.null(y)) {
      if (!is.null(by) || is.matrix(x))
        x  <- x.temp 
      else
        x <- table(x.temp)
    }
  }

  dd <- .getdigits(x, min.digits=0) - 1
  n.dim <- length(dim(x))
  stats <- ""

  # x is a table, could be real values or integers <0 if y is specified
  # no by variable, dim == 0 if x<-x.temp 
  if (n.dim == 1) {
    if (.is.integer(x)  &&  all(x >= 0)) {  # x is table of count-like values

      txsug <- ""
      if (getOption("suggest")) {
        txsug <- ">>> Suggestions"
        fc <- paste("Plot(", x.name, ")  # bubble plot", sep="")
        txsug <- paste(txsug, "\n", fc, sep="")
        fc <- paste("Plot(", x.name,
                    ", values=\"count\")  # lollipop plot", sep="")
        txsug <- paste(txsug, "\n", fc, sep="")
        fc <- paste("BarChart(", x.name,
                    ", horiz=TRUE)  # horizontal bar chart", sep="")
        txsug <- paste(txsug, "\n", fc, sep="")
        fc <- paste("BarChart(", x.name,
                 ", fill=\"colors\", color=\"off\")  # hcl bar colors", sep="")
        txsug <- paste(txsug, "\n", fc, sep="")
        fc <- paste("PieChart(", x.name, ")  # doughnut chart", sep="")
        txsug <- paste(txsug, "\n", fc, sep="")
      }

      stats <- .ss.factor(x, by=NULL, brief=TRUE, digits.d=NULL,
                          x.name, by.name, x.lbl, y.lbl, label.max,
                          x.miss, by.miss, out.size)

      if (!is.null(stats)) {
        txttl <- stats$title
        counts <- stats$count
        miss <- stats$miss
        chi <- stats$chi
        lbl <- stats$lbl

        class(txsug) <- "out_piece"
        class(txttl) <- "out_piece"
        class(counts) <- "out_piece"
        class(miss) <- "out_piece"
        class(chi) <- "out_piece"
        class(lbl) <- "out_piece"
        output <- list(out_suggest=txsug, out_title=txttl, out_miss=miss,
                       out_lbl=lbl, out_counts=counts, out_chi=chi)
        class(output) <- "out_all"
        if (!quiet) print(output)      
      }
    }  # end counts or count-like

    else {  # x (table from y) not count-like: not integer or values < 0

      txsug <- ""
      if (getOption("suggest")) {
        y.name <- getOption("yname")
        txsug <- ">>> Suggestions"
        fc <- paste("Plot(", y.name, ", ", x.name, ") # lollipop plot", sep="")
        txsug <- paste(txsug, "\n", fc, sep="")
      }

      stats <- .ss.real(x, by, digits.d=dd,
                   x.name, getOption("yname"), by.name, x.lbl, y.lbl, label.max) 
      txtbl <- stats$txtbl 
      class(txsug) <- "out_piece"
      class(txtbl) <- "out_piece"
      output <- list(out_suggest=txsug, out_txt=txtbl)

      class(output) <- "out_all"
      if (!quiet) print(output)         
    }
  }  # if (n.dim == 1)

  # a by variable
  else {

    txsug <- ""
    if (getOption("suggest")) {
      txsug <- ">>> Suggestions"
      fc <- paste("Plot(", x.name, ", ", by.name, ")  # bubble plot", sep="")
      txsug <- paste(txsug, "\n", fc, sep="")
      fc <- paste("BarChart(", x.name, ", by=", by.name,
                  ", horiz=TRUE)  # horizontal bar chart", sep="")
      txsug <- paste(txsug, "\n", fc, sep="")
      fc <- paste("BarChart(", x.name,
                  ", fill=\"colors\")  # different bar colors", sep="")
      txsug <- paste(txsug, "\n", fc, sep="")
    }

    if (is.null(y) && .is.integer(x)) {

      # need brief=FALSE for row proportions
      stats <- .ss.factor(x, by, brief=FALSE, digits.d=NULL,
                        x.name, by.name, x.lbl, y.lbl, label.max) 
      txttl <- stats$txttl
      txfrq <- stats$txfrq
      txXV <- stats$txXV
      txlbl <- stats$txlbl

      class(txsug) <- "out_piece"
      class(txttl) <- "out_piece"
      class(txfrq) <- "out_piece"
      class(txXV) <- "out_piece"
      class(txlbl) <- "out_piece"
      if (!prop)
        output <- list(out_suggest=txsug, out_title=txttl, out_lbl=txlbl,
                       out_text=txfrq, out_XV=txXV)
      else {
        txrow <- stats$txrow
        class(txrow) <- "out_piece"
        output <- list(out_title=txttl, out_text=txfrq, out_row=txrow, out_XV=txXV)
      }
      class(output) <- "out_all"
      if (!quiet) print(output)
    }  # end is.null(y)

    else {  # y is present
      stats <- .ss.real(x, y, by, digits.d=3, x.name,
                        getOption("yname"), by.name, x.lbl, y.lbl, label.max) 
      txtbl <- stats$txtbl 
      class(txtbl) <- "out_piece"
      output <- list(out_txt=txtbl)

      class(output) <- "out_all"
      if (quiet) print(output)
    }
  
  }

  if (!is.null(add)) {

    add.cex <- getOption("add.cex")
    add.lwd <- getOption("add.lwd")
    add.lty <- getOption("add.lty")
    add.color <- getOption("add.color")
    add.fill <- getOption("add.fill")
    add.trans <- getOption("add.trans")

    .plt.add (add, x1, x2, y1, y2,
              add.cex, add.lwd, add.lty, add.color, add.fill, add.trans) 
  }
  cat("\n")

  return(stats)

}

