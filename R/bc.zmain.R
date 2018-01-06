.bc.main <- 
function(x, y, by, 
         col.fill, col.color, col.bg, col.box, col.trans, colors,
         horiz, addtop, gap, prop,
         xlab, ylab, main, lab.cex,
         value.labels, label.max,
         axis.cex, cex.names, rotate.x, rotate.y, offset, beside,
         col.low, col.hi,
         legend.title, legend.loc, legend.labels, legend.horiz, quiet, ...) {

  # if x is integer, not labeled correctly
  if (length(unique(x)) != length(x)) if (!is.factor(x)) x <- factor(x)

  # scale for regular R or RStudio
  #adj <- .RSadj(radius=NULL, axis.cex, cex.names, lab.cex)
  #size.axis <- adj$size.axis
  #size.lab <- adj$size.lab
  #size.txt <- adj$size.txt

  if ( (is.table(x) || is.matrix(x)) && is.null(legend.title) ) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "Need to specify a value for:  legend.title\n\n")
  }

  # get values for ... parameter values
  #stuff <- .getdots(...)
  #col.main <- stuff$col.main
  #col.lab <- stuff$col.lab
  #col.sub <- stuff$col.sub
  #cex.main <- stuff$cex.main

  # get lab.x.cex  lab.y.cex
  lab.cex <- getOption("lab.cex")
  lab.x.cex <- getOption("lab.x.cex")
  lab.y.cex <- getOption("lab.y.cex")
  lab.x.cex <- ifelse(is.null(lab.x.cex), lab.cex, lab.x.cex)
  adj <- .RSadj(lab.cex=lab.x.cex); lab.x.cex <- adj$lab.cex
  lab.y.cex <- ifelse(is.null(lab.y.cex), lab.cex, lab.y.cex)
  adj <- .RSadj(lab.cex=lab.y.cex); lab.y.cex <- adj$lab.cex

  gl <- .getlabels(xlab, ylab, main, by.nm=TRUE, lab.x.cex=lab.x.cex, 
                   lab.y.cex=lab.y.cex)
  x.name <- gl$xn; x.lab <- gl$xb; x.lbl <- gl$xl
  y.lbl <- gl$yl
  by.name <- y.lbl
  main.lab <- gl$mb
  lab.x.cex <- gl$lab.x.cex
  lab.y.cex <- gl$lab.y.cex
  if (!is.null(by)) by.name <- getOption("byname")

  ylab.keep <- ifelse(is.null(ylab), FALSE, TRUE)

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

  # if a table, convert to a matrix
  #if (is.table(x)) {
    #xr.nm <- rownames(x)
    #xc.nm <- colnames(x)  # as.numeric makes equivalent to matrix input
    #x <- matrix(as.numeric(x), nrow=nrow(x), ncol=ncol(x))
    #rownames(x) <- xr.nm
    #colnames(x) <- xc.nm
  #}

  # get legend title, l.lab
  if (!is.null(legend.title))
    l.lab <- legend.title 
  else
    if (!is.null(by)) if (exists("y.lbl"))
      l.lab <- ifelse (length(y.lbl) == 0, by.name, y.lbl)

  # title
  main.lbl <- ifelse (!is.null(main), main, "")

  # entered counts typically integers as entered but stored as type double
  # if names(x) or rownames(x) is null, likely data from sample and c functions
  # y is getting data values directly from a data frame with counts entered
  entered.pre <- FALSE
  if (!is.matrix(x) && !is.null(names(x))) entered.pre <- TRUE
  if (is.matrix(x) && !is.null(rownames(x))) entered.pre <- TRUE
  entered <- ifelse (!is.integer(x) && is.double(x) && entered.pre, TRUE, FALSE)


  # -------------------------------
  # do not do counts, y is provided

  if (!is.null(y)) {
    entered <- TRUE
    if (is.null(by)) {  # no by variable
      yn <- getOption("yname")
      if (!is.numeric(y) > 0) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "y-values, those from the 2nd unlabeled variable, ", yn, ",",
          " must be\n", " numeric\n\n",
          "A by variable is categorical, with a small number of potentially\n",
          "  non-numeric values\n",
          "It appears that ", yn, " is a by variable\n",
          "As of lessR 3.5.6, explicitly indicate the by variable if it\n",
          "  second in the list for the function call\n\n",
          "To specify a by variable as the 2nd variable in the function call,\n",
          "  explicitly precede its name with:  by=", "\n\n")
      } 

      if (anyDuplicated(names(x)) > 0) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "The data contain duplicated values of variable: ", x.name, "\n\n")
      } 

      if (length(y) > 20) {
        warning(call.=FALSE, "There are more than 20 categories to plot\n\n",
            "Perhaps you mean for the 2nd variable, ", yn, ", to be a",
            " by variable\n",
            "  even though it is numeric\n\n",
            "A by variable only has a small number of values\n\n",
            "To specify a by variable as the 2nd variable in the",
            " function call,\n",
            "  explicitly precede its name with:  by=", "\n\n")
     }

      x.temp <- x
      x <- y
      names(x) <- x.temp
      x <- as.table(x)
      if (prop) x <- x/sum(x)
    }

    else {  # a by variable
      do.row <- ifelse (x[1] == x[2], FALSE, TRUE)  # x is outer loop
      m <- matrix(y, nrow=length(levels(by)), byrow=do.row)
      colnames(m) <- levels(x) 
      rownames(m) <- levels(by) 
      m <- as.table(m, dnn=c(by.name, x.name))
      names(dimnames(m)) <- c(by.name, x.name) 
      x <- m
      if (prop) x <- prop.table(x, 2)
    }
  }  # end !is.null(y)

  
  # save ordered status before converting x to a table
  order.x <- ifelse (is.ordered(x) && is.null(by), TRUE, FALSE)
  order.y <- ifelse (is.ordered(by), TRUE, FALSE)

  # tabulate for counts
  if (!entered) {  # x.temp allows frequencies to be restored for text output
    if (!is.null(by)) {
      if (length(x) == length(by))
        x.temp <- table(by,x, dnn=c(by.name, x.name))
      else {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        x.name, " and ", by.name, " must be of the same size\n\n",
        "Size of ", x.name, ": ", length(x), "\n", 
        "Size of ", by.name, ": ", length(by), "\n\n", sep="")
      }
      x <- table(by,x, dnn=c(by.name, x.name))
      if (prop) x <- prop.table(x, 2)
    }
    else {  # one variable 
      x.temp <- x  # save counts
      x <- table(x, dnn=NULL)
      if (prop) x <- x/sum(x)
    }
  }  # end !entered


  if (is.null(by) && beside && !entered) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "beside=TRUE  is not valid for analysis of only one variable.\n\n")
  }


  # ------
  # colors

  # get n.colors (does not indicate col.fill multiple colors)
  if (is.null(by) && !order.x && !is.matrix(x))
    n.colors <- length(col.fill)
  else
    n.colors <- nrow(x)
  if (!is.null(by) && order.y) n.colors <- nrow(x)

  if ( (colors == "rainbow"  ||  colors=="terrain"  || colors=="heat") ) {
    n.colors <- nrow(x)
    nogo <- FALSE
    if (is.ordered(x) && is.null(by)) nogo <- TRUE
    if (is.ordered(by)) nogo <- TRUE
    if (nogo) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Can only do an R color range when there is more than one color. \n\n")
    }
  }

  # color palette

  if ((order.x && is.null(by)) || order.y) {  # one var, an ordered factor
    lowhi <- .ordcolors(colors, col.low, col.hi) 
    col.low <- lowhi$col.low
    col.hi <- lowhi$col.hi

    color.palette <- colorRampPalette(c(col.low, col.hi))
    clr <- color.palette(n.colors)
  } # end ordered

  else if (colors %in% c("lightbronze", "gray", "white")) {
    if (n.colors == 1 || length(col.fill) > 1)
      clr <- col.fill
    else {
      if (n.colors == 2) { light <- "gray70"; dark <- "gray40" }
      else { light <- "gray84"; dark <- "gray30" }
      color.palette <- colorRampPalette(c(dark, light))
      clr <- color.palette(n.colors)
    }
  }

  else if ((colors %in% c("darkred", "gray", "blue", "rose",
      "green", "gold", "red", "dodgerblue", "darkgreen", "purple", "sienna",
      "brown", "orange")
          && (is.null(by) && !is.matrix(x)))) {
      if (n.colors == 1 || length(col.fill) > 1)
        clr <- col.fill
      else {
        color.palette <- colorRampPalette(getOption("col.bar.fill"))
        clr <- color.palette(nrow(x))
      }
  }

  else if (colors == "rainbow") clr <- rainbow(n.colors)
  else if (colors == "terrain") clr <- terrain.colors(n.colors)
  else if (colors == "heat") clr <- heat.colors(n.colors)

  else  {  # ordered color range does not make sense here 
    if (length(col.fill) > 1)
      clr <- col.fill
    else
      if (n.colors > 1) clr <- .col.discrete()[1:n.colors]
    # lighten some default background colors
    if (col.bg == "#EEF0F2") col.bg <- rgb(245,245,245, maxColorValue=255)
    if (col.bg == "#E5DB8E") col.bg <- rgb(251,245,220, maxColorValue=255)
  }


  if (!is.null(col.trans)) 
    for (i in 1:n.colors) clr[i] <- .maketrans(clr[i], (1-col.trans)*256) 

  if (n.colors > 1) {
    palette(clr)
    colr <- 1:n.colors   # colr is a sequence of integers
  }
  else  # colr is a color
    colr <- ifelse (is.null(col.fill), getOption("bar.fill"), col.fill)


  # ----------------------------------------------------------------------------
  # preliminaries
 
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

  # get max label size
  the.names <- integer(length=0)
  if (length(dim(x)) == 0)
    the.names <- names(x)
  else
    if (is.null(by))
      the.names <- rownames(x)
    else
      the.names <- colnames(x)

  max.nm <- 0
  for (i in (1:length(the.names))) {
    li <- ifelse (!is.na(the.names[i]), nchar(the.names[i]), 0)
    if (li > max.nm) max.nm <- li
  }
  
  # labels horiz or vertical
  las.value <- ifelse (horiz  && max.nm > 5, 0, 1)

  
  # ----------------------------------------------------------------------------
  # set up plot area

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

  for (i in 1:length(val.lab))  # for each value label, partition into lines
    val.lab[i] <- .makelines(val.lab[i], mx.ch=11)
  if (!is.null(val.lab)) val.lab <- .abbrev(val.lab, label.max)
 
  if(is.null(y)) if (horiz) {  # switch
    temp <- x.lab
    x.lab <- y.lab
    y.lab <- temp  # FIX: label size processed as x.lab, maybe too large
  }

  ly <- length(y.lab)
  if (ly > 1) {  # not perfect, but some line break better than none
    if (nchar(y.lab[ly]) > 38) {
      brk <- nchar(y.lab[ly]) %/% 2  # break label down the middle
      while (substr(y.lab[ly],brk,brk) != " ") brk <- brk-1  # break at a word
      line1 <- substr(y.lab[ly], 1, brk)
      line2 <- substr(y.lab[ly], brk+1, nchar(y.lab[ly]))
      y.lab[ly] <- paste(line1, "\n",  line2)
    }
  }

  if (!horiz)
    max.width <- strwidth(as.character(max(pretty(c(min.y, max.y)))),
                          units="inches")
  else {
    val.split <- unlist(strsplit(val.lab, "\n"))
    # strange strwidth bug, 1st time works, 2nd time too large by 0.5
    # refresh source code, works again until 2nd time
    #max.width <- strwidth(max(val.split), units="inches") 
    max.width <- nchar(max(val.split)) / 16
 }

  margs <- .marg(max.width, y.lab, x.lab, main.lab, x.val=val.lab, prop,
                 rotate.x)
  lm <- margs$lm
  tm <- margs$tm
  rm <- margs$rm
  bm <- margs$bm
 
  if (horiz) {
    bm <- bm + .10  # kludge, for horiz, long labels overrun plot area
    #lm <- lm + .06
    if (las.value == 1) lm <- lm + .12
  }
  if (legend.loc == "right.margin"  &&  (!is.null(by) || is.matrix(x)))
    rm <- rm + .82
 
  orig.params <- par(no.readonly=TRUE)
  on.exit(par(orig.params))

  par(bg=getOption("window.fill"))
  par(mai=c(bm, lm, tm, rm))

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

  # barplot run here only to establish usr coordinates
  #  otherwise usr is just 0,1 for both axes
  # the barplot itself is not retained, but overwritten
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


  # ----------------------------------------------------------------------------
  # bar plot, grid lines

  usr <- par("usr")
  
  rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="transparent")

  grid.x.color <- ifelse(is.null(getOption("grid.x.color")), 
    getOption("grid.color"), getOption("grid.x.color"))
  grid.y.color <- ifelse(is.null(getOption("grid.y.color")), 
    getOption("grid.color"), getOption("grid.y.color"))
 
  grid.x.lwd <- ifelse(is.null(getOption("grid.x.lwd")), 
    getOption("grid.lwd"), getOption("grid.x.lwd"))
  grid.y.lwd <- ifelse(is.null(getOption("grid.y.lwd")), 
    getOption("grid.lwd"), getOption("grid.y.lwd"))

  grid.x.lty <- ifelse(is.null(getOption("grid.x.lty")), 
    getOption("grid.lty"), getOption("grid.x.lty"))
  grid.y.lty <- ifelse(is.null(getOption("grid.y.lty")), 
    getOption("grid.lty"), getOption("grid.y.lty"))
 
  ax.num <- ifelse(horiz, 1, 2)  # location of numerical axis
  vy <- pretty(min.y:max.y)
  abline(h=axTicks(ax.num), col=grid.y.color, lwd=grid.y.lwd, lty=grid.y.lty)

  # box around plot
  rect(usr[1], usr[3], usr[2], usr[4], col="transparent", border=col.box,
    lwd=getOption("panel.lwd"), lty=getOption("panel.lty"))

  ## PLOT
  if (rescale == 0)
    coords <- barplot(x, add=TRUE, col=clr, beside=beside, horiz=horiz,
          axes=FALSE, ann=FALSE, border=col.color, las=las.value, 
          space=gap, axisnames=FALSE, ...)
  else
    coords <- barplot(x, add=TRUE, col=clr, beside=beside, horiz=horiz,
          axes=FALSE, ann=FALSE, border=col.color, las=las.value, 
          space=gap, width=width.bars, xlim=c(0,1), axisnames=FALSE, ...)

  # axes
  if (!horiz) las.value <- 1
  axis.y.color <- ifelse(is.null(getOption("axis.y.color")), 
    getOption("axis.color"), getOption("axis.y.color"))
  axis.y.cex <- ifelse(is.null(getOption("axis.y.cex")), 
    getOption("axis.cex"), getOption("axis.y.cex"))
  adj <- .RSadj(axis.cex=axis.y.cex); axis.y.cex <- adj$axis.cex
  axis(ax.num, col=axis.y.color,
       col.axis=axis.y.color, cex.axis=axis.y.cex,
       las=las.value, ...) 
  
  if (beside) coords <- apply(coords, 2, mean)  # one label per group
  if (!horiz) {
    ax.value <- 1;  xx <- coords;  yy <- par("usr")[3]
  }
  else {
    ax.value <- 2;  xx <- par("usr")[3];  yy <- coords
  }
  if (horiz) xx <- -0.032  # otherwise usr3 is a + value
  axis.x.color <- ifelse(is.null(getOption("axis.x.color")), 
    getOption("axis.color"), getOption("axis.x.color"))
  axis.x.cex <- ifelse(is.null(getOption("axis.x.cex")), 
    getOption("axis.cex"), getOption("axis.x.cex"))
  adj <- .RSadj(axis.cex=axis.x.cex); axis.x.cex <- adj$axis.cex
  axis(ax.value, at=coords, labels=FALSE, tck=-.01,
       col=axis.x.color, cex.axis=axis.x.cex, ...)
  text(x=xx, y=yy, labels=val.lab,
       pos=ax.value, xpd=TRUE, cex=axis.x.cex, col=getOption("axis.text.color"),
       srt=rotate.x, offset=offset, ...)
    
  # title
  title(main=main.lab, cex.main= getOption("main.cex"),
        col.main=getOption("main.color"))

  # xlab positioning
  lblx.lns <- ifelse (grepl("\n", x.lab, fixed=TRUE), 3.1, 2.2)
  new.ln <- FALSE
  if (!is.null(val.lab))
    for (i in 1:length(val.lab)) 
      if (grepl("\n", val.lab[i], fixed=TRUE)) new.ln <- TRUE
  if (new.ln) lblx.lns <- lblx.lns + 1
  if (offset > 0.5) lblx.lns <- lblx.lns + 1 
  if (horiz) lblx.lns <- lblx.lns + .6 
  lab.x.color <- ifelse(is.null(getOption("lab.x.color")), 
    getOption("lab.color"), getOption("lab.x.color"))
  title(xlab=x.lab, line=lblx.lns, cex.lab=lab.x.cex, col.lab=lab.x.color)

  # ylab positioning (based on .axlabs function)
  lbl.lns <- 3.6
  multi <- FALSE
  for (i in 1:length(y.lab))
    if (!is.null(y.lab))
      if (grepl("\n", y.lab[i], fixed=TRUE)) multi <- TRUE  # multi-line
  lm <- par("mar")[2]  # get the current left margin
  lbly.lns <- ifelse (multi, lm - 2.3, lm - 1.4)
  lab.y.color <- ifelse(is.null(getOption("lab.y.color")), 
    getOption("lab.color"), getOption("lab.y.color"))
  title(ylab=y.lab, line=lbly.lns, cex.lab=lab.y.cex, col.lab=lab.y.color)

  # ----------------------------------------------------------------------------
  # legend for two variable plot including variable labels
  if ( (!is.null(by) || is.matrix(x)) && !is.null(legend.loc))  {

    col.txt <- ifelse (sum(col2rgb(col.bg))/3 > 80, "black", rgb(.97,.97,.97))

    # default right.margin option
    if (legend.loc == "right.margin") {

      options(byname = getOption("byname"))
      trans.pts <- .6  # dummy value
      .plt.by.legend(legend.labels, col.color, clr, shp=22, trans.pts,
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
  # one variable, dim == 0 if x<-x.temp 
  if (n.dim == 1  && !quiet) {
    # only process if counts
    if (.is.integer(x)  &&  all(x >= 0)  &&  is.null(y) ) {

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
                    ", colors=\"rainbow\")  # different bar colors", sep="")
        txsug <- paste(txsug, "\n", fc, sep="")
        fc <- paste("PieChart(", x.name, ")  # doughnut chart", sep="")
        txsug <- paste(txsug, "\n", fc, sep="")
      }

      if (is.null(y)) {

        stats <- .ss.factor(x, by=NULL, brief=TRUE, digits.d=NULL,
                            x.name, by.name, x.lbl, y.lbl, label.max)

        if (!is.null(stats)) {
          txttl <- stats$title
          counts <- stats$count
          chi <- stats$chi

          class(txsug) <- "out_piece"
          class(txttl) <- "out_piece"
          class(counts) <- "out_piece"
          class(chi) <- "out_piece"
          output <- list(out_suggest=txsug, out_title=txttl, out_counts=counts,
                         out_chi=chi)
          class(output) <- "out_all"
          print(output)      
        }
      }  # is.null(y)
    }
    else 
      stats <- NULL

    if (!is.null(y) || !.is.integer(x)) {
      stats <- .ss.real(x, y, by, digits.d=dd,
                   x.name, getOption("yname"), by.name, x.lbl, y.lbl, label.max) 
      txtbl <- stats$txtbl 
      class(txtbl) <- "out_piece"
      output <- list(out_txt=txtbl)

      class(output) <- "out_all"
      print(output)         
    }
  }  # if (n.dim == 1  && !quiet)

  # two variables
  else if (!quiet) {
    # need brief=FALSE for row proportions

    txsug <- ""
    if (getOption("suggest")) {
      txsug <- ">>> Suggestions"
      fc <- paste("Plot(", x.name, ", ", by.name, ")  # bubble plot", sep="")
      txsug <- paste(txsug, "\n", fc, sep="")
      fc <- paste("BarChart(", x.name, ", by=", by.name,
                  ", horiz=TRUE)  # horizontal bar chart", sep="")
      txsug <- paste(txsug, "\n", fc, sep="")
      fc <- paste("BarChart(", x.name, ", by=", by.name,
                  ", colors=\"rainbow\")  # different bar colors", sep="")
      txsug <- paste(txsug, "\n", fc, sep="")
    }

    if (is.null(y) && .is.integer(x)) {

      stats <- .ss.factor(x, by, brief=FALSE, digits.d=NULL,
                        x.name, by.name, x.lbl, y.lbl, label.max) 
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
        output <- list(out_title=txttl, out_text=txfrq, out_row=txrow, out_XV=txXV)
      }
      class(output) <- "out_all"
      print(output)
    }  # end is.null(y)

    else {  # y is present
      stats <- .ss.real(x, y, by, digits.d=3,
                        x.name, getOption("yname"), by.name, x.lbl, y.lbl, label.max) 
      txtbl <- stats$txtbl 
      class(txtbl) <- "out_piece"
      output <- list(out_txt=txtbl)

      class(output) <- "out_all"
      print(output)
    }
  
  }

  cat("\n")

  return(stats)

}
