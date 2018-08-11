.bc.main <- 
function(x, y, by, 
         fill, col.color, col.trans, theme,
         horiz, addtop, gap, prop, scale.y,
         xlab, ylab, main,
         value.labels, label.max, beside,
         rotate.x, offset, break.x, sort.x,
         values, values.color, values.cex, values.digits,
         values.pos, values.cut,
         xlab.adj, ylab.adj, bm.adj, lm.adj, tm.adj, rm.adj,
         legend.title, legend.loc, legend.labels, legend.horiz,
         add, x1, x2, y1, y2, out.size, quiet, ...) {
  
  multi <- ifelse (is.data.frame(x), TRUE, FALSE)

  # if x is integer, not labeled correctly, set to factor
  if (!is.data.frame(x))
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
  if (!is.null(by) || is.data.frame(x))
    by.name <- getOption("byname")

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


  # x = tabulate for counts

  if (!entered) {
    if (!is.data.frame(x)) { # a single x value
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
          x.temp <- table(by, x, dnn=c(by.name, x.name))
        else {
          cat("\n"); stop(call.=FALSE, "\n","------\n",
          x.name, " and ", by.name, " must be of the same size\n\n",
          "Size of ", x.name, ": ", length(x), "\n", 
          "Size of ", by.name, ": ", length(by), "\n\n", sep="")
        }
        x <- x.temp
        if (prop) {
          x.count <- x  # save table of counts for possible bar display
          x <-prop.table(x, 2)
        }
      }
    }   # end single x value
    
    else {  # x is a data frame, so combine x's
      if (!is.factor(x[,1])) {
        resp <- unique(x[,1])
        for (i in 2:ncol(x)) resp <- union(resp, unique(x[,i]))
        resp <- sort(resp)  # sort removes any NA
      }
      else {  # is factor
        resp <- levels(x[,1])
        for (i in 2:ncol(x)) resp <- union(resp, levels(x[,i]))
      }
      n.resp <-length(resp)
      frq <- matrix(nrow=n.resp, ncol=ncol(x))  # all elements NA
      frq <- apply(frq, c(1, 2), function(x) 0)
      rownames(frq) <- as.character(resp)
      colnames(frq) <- names(x)

      for (i in 1:ncol(x)) {  # maybe an x not has a value in full set
        tblx.i <- table(x[,i])
        k <- 0
        for (j in 1:n.resp) {
          if (rownames(frq)[j] %in% names(table(x[,i]))) {
            k <- k + 1
            frq[j,i] <- tblx.i[k]
          }
        }
      }  # end col i of x
      
      x <- as.table(frq)
      if (is.numeric(resp))
        wt <- resp
      else
        wt <- 1:n.resp
      wm <- double(length=ncol(x))
      for (i in 1:ncol(x)) wm[i] <- weighted.mean(wt, x[,i])

      if (sort.x != "0") {
        srt.dwn <- ifelse (sort.x == "-", TRUE, FALSE)
        m.o <- order(wm, decreasing=srt.dwn)
        x <- x[,m.o]
        wm <- wm[m.o]
      }
    }
  }  # end !entered so tabulated
    


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
  if (sort.x != "0") {
    srt.dwn <- ifelse (sort.x == "-", TRUE, FALSE)
    if (!is.matrix(x)) {
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
  # fill is input and usually words, clr are actual colors

  n.cat <- ifelse (is.matrix(x), nrow(x), length(x))

  # see if apply a pre-defined color range to **fill**
  clr <- NULL
  clr <- .color.range(fill, n.cat)  # see if range

  # not a color range such as "colors" or "blues", so assign clr here
  if (is.null(clr)) {
    if (length(fill) == 1  &&  is.matrix(x)) 
      clr <- getColors("colors", n=n.cat)  # default with by variable
    else
      clr <- fill  # user provided the colors
  }

  if (!is.null(col.trans)) 
   for (i in 1:length(clr)) clr[i] <- .maketrans(clr[i], (1-col.trans)*256) 

  # see if apply a pre-defined color range to **color**
  col.clr <- NULL
  col.clr <- .color.range(col.color, n.cat)  # see if range
  if (!is.null(col.clr)) col.color <- col.clr


  # -------------
  # preliminaries

  if (values.pos == "out") addtop <- addtop + .06
  # a 2-D table is an instance of a matrix, a 1-D table is not
  max.y <- ifelse (is.matrix(x) && !beside, max(colSums(x)), max(x))
  max.y <- max.y + (addtop * max.y)

  if (any(x < 0, na.rm = TRUE)) {
    min.y <- ifelse (is.matrix(x) && !beside, min(colSums(x)), min(x))
    min.y <- min.y - abs(addtop * min.y)
  }
  else
    min.y <- 0

  if (is.null(legend.labels)) legend.labels <- row.names(x)
  if (beside) legend.horiz <- FALSE
  if (is.matrix(x) && !beside) legend.horiz <- TRUE

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

  if (legend.loc == "right.margin"  &&  (is.matrix(x)))
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
  if (is.matrix(x) && !beside) if (ncol(x) <= 4) rescale <- ncol(x)
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

  ax.num <- ifelse (horiz, 1, 2)  # location of numerical axis
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
    x.coords <- barplot(x, add=TRUE, col=clr, beside=beside, horiz=horiz,
          axes=FALSE, ann=FALSE, border=col.color, las=las.value, 
          space=gap, axisnames=FALSE, ...)
  else
    x.coords <- barplot(x, add=TRUE, col=clr, beside=beside, horiz=horiz,
          axes=FALSE, ann=FALSE, border=col.color, las=las.value, 
          space=gap, width=width.bars, xlim=c(0,1), axisnames=FALSE, ...)


  # display text labels of values on or above the bars
  # --------------------------------------------------
  if (values != "off") {
    if (is.null(values.cut)) {
      values.cut <- 0.015
      if (prop && is.matrix(x)  ||  multi) values.cut <- 0.045
    }

    # set type of the values to display


    if (!prop) {
      if (!multi)
        x.prop <- x/sum(x)
      else
        x.prop <- x/colSums(x)

      if (values == "input")
        x.txt <- as.character(x)
      else if (values == "%")
        x.txt <- paste(.fmt(x.prop * 100, values.digits), "%", sep="")
      else if (values == "prop")
        x.txt <- .fmt(x.prop, values.digits)
      
      if (is.matrix(x))
          x.txt <- matrix(x.txt, nrow=nrow(x))
        
      if (values.pos != "out") {
        if (is.null(by)) {
          for (i in 1:length(x.prop))
            x.txt[i] <- ifelse (x.prop[i] >= values.cut, x.txt[i], "")
        }
        else {  # by variable
          for (i in 1:nrow(x.prop)) for (j in 1:ncol(x.prop)) 
            x.txt[i,j] <- ifelse (x.prop[i,j] >= values.cut, x.txt[i,j], "")
        }
      }
    }
    
    else {  # prop
      if (values == "input")
        x.txt <- as.character(x.count)
      else if (values == "%")
        x.txt <- paste(.fmt(x * 100, values.digits), "%", sep="")
      else if (values == "prop")
        x.txt <- .fmt(x, values.digits)
      
      if (is.matrix(x))
          x.txt <- matrix(x.txt, nrow=nrow(x))

      if (values.pos != "out") {
        if (is.null(by)) {
          for (i in 1:length(x))
            x.txt[i] <- ifelse (x[i] >= values.cut, x.txt[i], "")
        }
        else {  # by variable
          # as.numeric & as.character convert table to vector: re-convert
          for (i in 1:nrow(x)) for (j in 1:ncol(x)) 
            x.txt[i,j] <- ifelse (x[i,j] >= values.cut, x.txt[i,j], "")
        }
      }
    }

    # vertical bars
    if (!horiz) {
      if (!is.matrix(x)) { # 1 variable
        usr.y.inch <- diff(grconvertY(0:1, 'inches', 'user'))
        if (values.pos == "in")
          ycrd <- x/2
        else
          ycrd <- x + 0.17*usr.y.inch
        text(x.coords, ycrd, labels=x.txt, col=values.color, cex=values.cex)
      }  # no by
      else {  # 2 variables
        if (!beside) {
          for (i in 1:ncol(x)) {
            ycrd <- cumsum(x[,i]) - (x[,i] / 2)
            text(x.coords[i], ycrd, labels=x.txt[,i],
                 col=values.color, cex=values.cex) 
          }
        }  # end !beside
        else {  # beside
          usr.y.inch <- diff(grconvertY(0:1, 'inches', 'user'))
          if (values.pos == "in")
            ycrd <- x/2
          else  # value.pos == "out"
            ycrd <- x + 0.10*usr.y.inch
          for (i in 1:ncol(x)) {
            text(x.coords[,i], ycrd[,i], labels=x.txt[,i],
                 col=values.color, cex=values.cex) 
          }
        }  # end beside
      }  # end 2 variables
	  }  # end vertical bars
	
    # horiz bars
    else {
      if (!is.matrix(x)) {  # 1 variable
        usr.x.inch <- diff(grconvertX(0:1, 'inches', 'user'))
        if (values.pos == "in")
          ycrd <- x/2
        else
          ycrd <- x + 0.17*usr.x.inch
        text(ycrd, x.coords, labels=x.txt, col=values.color, cex=values.cex)
      }  # end no by
      else {  # by variable
        if (!beside) {  # stacked chart
        for (i in 1:ncol(x)) {  # each level of by
          ycrd <- cumsum(x[,i]) - (x[,i] / 2)
          text(ycrd, x.coords[i], labels=x.txt[,i],
             col=values.color, cex=values.cex) 
        }
      }
      else {  # beside chart
        usr.x.inch <- diff(grconvertX(0:1, 'inches', 'user'))
        if (values.pos == "in")
          ycrd <- x/2
        else  # value.pos == "out"
          ycrd <- x + 0.17*usr.x.inch
          for (i in 1:ncol(x)) {
            text(ycrd[,i], x.coords[,i], labels=x.txt[,i],
                 col=values.color, cex=values.cex) 
          }
        }  # end beside
      }  # end by
    }  # end horiz bars
	
  }  # end display values
  

  # y-axis is the numerical axis
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
       tck=-.02, padj=adj1,  at=y.coords, labels=lblval.y, ...) 
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
      .plt.by.legend(legend.labels, col.color, clr, shp=22, trans.pts,
                     col.bg, usr, pt.size=1.6, pt.lwd=0)

    }  # right margin

    else
      legend(legend.loc, legend=legend.labels, title=l.lab, fill=clr, 
             horiz=legend.horiz, cex=.7, bty="n", text.col=col.txt)
  }


  # -----------------------------------------------------------------------
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
  
  if (multi) {
  
    txsug <- ""
    
    # display variable labels
    txlbl <- ""
    l.name <- "mylabels"
    if (exists(l.name, where=.GlobalEnv)) {
      mylabs <- get(l.name, pos=.GlobalEnv)
      mylabs <- mylabs[colnames(x),]
      if (!is.null(mylabs)) {
        tx <- character(length = 0)
        for (i in 1:length(colnames(x))) {
          ml <- mylabs[i]
          if (!is.na(ml))
            tx[length(tx)+1] <- paste(colnames(x)[i], ": ", ml, sep="")
        }
        tx[length(tx)+1] <- " "
        tx[length(tx)+1] <- "Variable Labels"
        txlbl <- rev(tx)
        blnk <- " "
      }
    }
    
    # display frequencies and means of each variable
    txttl <- "Frequencies of Responses by Variable"
    tx <- character(length = 0)
    mx.chr <- max(nchar(rownames(x)))
    mx.len <- 8
    if (mx.chr > mx.len) {
      c.nm <- rownames(x)
      rownames(x) <- .abbrev(rownames(x), mx.len)
    }
    x <- t(x)
    
    txtbl <- .prntbl(x, 0, cc=NULL)
    mx <- max(nchar(round(wm,2)))
    txtbl[1] <- paste(txtbl[1], " ", .fmtc("Mean", w=mx+1))
    n.var <- nrow(x)
    for (i in 1:n.var)
      txtbl[i+1] <- paste(txtbl[i+1], " ", .fmt(wm[i],3,w=mx+1))
     txtbl[2:length(txtbl)] <- rev(txtbl[2:length(txtbl)])
    
    if (mx.chr > mx.len) {
      txtbl[n.var+2] <- ""
      txtbl[n.var+3] <- ""
      txtbl[n.var+4] <- "Unabbreviated labels"
      txtbl[n.var+5] <- "--------------------"
      txtbl[n.var+6] <- paste(c.nm, sep="", collapse="\n")
    }
    strt <- ifelse (mx.chr > mx.len, 7, 2)
    if (is.null(resp)) {
      txtbl[n.var+strt] <- ""
      txtbl[n.var+strt+1] <- paste("Computation of the mean based on coding",
                                   "response categories from 1 to", n.var)
    }

    class(txsug) <- "out_piece"
    class(txtbl) <- "out_piece"
    class(txttl) <- "out_piece"
    
    if (nzchar(txsug))
      output <- list(out_suggest=txsug,
                     out_text=txlbl, out_title=txttl, out_text=txtbl)
    else
      output <- list(out_text=txlbl,
                     out_title=txttl, out_text=txtbl)
                     
    class(output) <- "out_all"
    if (!quiet) print(output)
    
  }  # end multi

  # x is a table, could be real values or integers <0 if y is specified
  # no by variable, dim == 0 if x<-x.temp 
  else if (n.dim == 1) {
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
                 ", fill=\"greens\")  # sequential green bars", sep="")
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

  # -------------
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
        output <- list(out_suggest=txsug, out_title=txttl, out_lbl=txlbl, out_text=txfrq, out_XV=txXV)
      else {
        txrow <- stats$txrow
        class(txrow) <- "out_piece"
        output <- list(out_suggest=txsug, out_title=txttl, out_text=txfrq,
                       out_row=txrow, out_XV=txXV)
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
  
  }  # end a by variable

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

