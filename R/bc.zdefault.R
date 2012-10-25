.bc.default <- 
function(x, by=NULL, 
         col.fill, col.stroke, col.bg, col.grid, random.col, colors,
         horiz, over.grid, addtop, gap, brief, prop, xlab, ylab, main,
         cex.axis, col.axis, col.ticks, beside, col.low, col.hi, count.names,
         legend.title, legend.loc, legend.labels, legend.horiz, text.out, ...) {
 
 if (!is.null(by) && prop) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "Analysis of proportions not valid for two variables.\n\n")
  }

  if (horiz && addtop!=1) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "addtop  only works for a vertical bar plot.\n\n")
  }

  # get variable labels if exist plus axes labels
  if (is.null(ylab)) if (!prop) ylab <- "Frequency" else ylab <- "Proportion"
  gl <- .getlabels(xlab, ylab, main)
  x.name <- gl$xn; x.lbl <- gl$xl; x.lab <- gl$xb
  y.name <- gl$yn; y.lbl <- gl$yl; y.lab <- gl$yb
  main.lab <- gl$mb

  # get legend title, l.lab
  if (!is.null(legend.title)) l.lab <- legend.title 
  else if (!is.null(by)) if (exists("y.lbl"))
    if (length(y.lbl) == 0) l.lab <- y.name else l.lab <- y.lbl

  # title
  if (!is.null(main)) main.lbl <- main else main.lbl <- ""

  # entered counts typically integers as entered but stored as type double
  # if names(x) or rownames(x) is null, likely data from sample and c functions
  # count.names is getting counts directly from a data frame with counts entered
  entered.pre <- FALSE
  if (!is.matrix(x) && !is.null(names(x))) entered.pre <- TRUE
  if (is.matrix(x) && !is.null(rownames(x))) entered.pre <- TRUE
  if (!is.integer(x) && is.double(x) && entered.pre) 
    entered <- TRUE else entered <- FALSE
  if (!is.null(count.names)) {
    x <- as.numeric(x)
    names(x) <- count.names
    entered <- TRUE
  }
  
  # save ordered status before converting x to a table
  if (is.ordered(x) && is.null(by)) order.x <- TRUE else order.x <- FALSE
  if (is.ordered(by)) order.y <- TRUE else order.y <- FALSE

  # convert to table, with variable names, if needed
  if (!entered && !is.table(x))
    if (!is.null(by)) x <- table(by,x, dnn=c(y.name, x.name)) 
    else {  
      x <- table(x, dnn=NULL)
      if (prop) {
        x.temp <- x
        x <- x/sum(x)
      }
    }

  if (is.null(by) && beside && !entered) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "beside=TRUE  is not valid for analysis of only one variable.\n\n")
  }


  # ----------------------------------------------------------------------------
  # colors

  # get number of colors
  if (is.null(by) && !order.x && !is.matrix(x)) ncolors <- 1 else ncolors <- nrow(x)
  if (!is.null(by) && order.y) ncolors <- nrow(x)

  if ( (colors == "rainbow"  ||  colors=="terrain"  || colors=="heat") ) {
    nogo <- FALSE
    if (ncolors == 1) nogo <- TRUE
    if (is.ordered(x) && is.null(by)) nogo <- TRUE
    if (is.ordered(by)) nogo <- TRUE
    if (nogo) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Can only do an R color range when there is more than one color. \n\n")
    }
  }

  # color palette
  if ((order.x && is.null(by)) || order.y) {
    if (colors == "blue") { 
      if (is.null(col.low)) col.low <- "slategray2"
      if (is.null(col.hi)) col.hi <- "slategray4"
    }
    else if (colors == "gray") {
      if (is.null(col.low)) col.low <- "gray70"
      if (is.null(col.hi)) col.hi <- "gray25"
      col.grid <- "white"
      col.bg <- "gray90"
    }
    else if (colors == "green") {
      if (is.null(col.low)) col.low <- "darkseagreen1"
      if (is.null(col.hi)) col.hi <- "darkseagreen4"
      col.bg <- rgb(230,220,143, maxColorValue=256)
    }
    else if (colors == "rose") {
      if (is.null(col.low)) col.low <- "mistyrose1"
      if (is.null(col.hi)) col.hi <- "mistyrose4"
      col.grid <- "snow2"
      col.bg <- "snow1"
    }
    else if (colors == "gold") {
      if (is.null(col.low)) col.low <- "goldenrod1"
      if (is.null(col.hi)) col.hi <- "goldenrod4"
    }
    else if (colors == "red") { 
      if (is.null(col.low)) col.low <- "coral1"
      if (is.null(col.hi)) col.hi <- "coral4"
    }
    else if (colors == "orange") { 
      if (is.null(col.low))
        col.low <- rgb(255,173,91, maxColorValue=256)
      if (is.null(col.hi))
        col.hi <- rgb(169,66,2, maxColorValue=256)
      col.bg <- rgb(4,4,4, maxColorValue=256)
    }
    color.palette <- colorRampPalette(c(col.low, col.hi))
    clr <- color.palette(ncolors)
  }
  else if (colors == "gray") {
    color.palette <- colorRampPalette(c("gray30","gray80"))
    clr <- color.palette(nrow(x))
    if (col.grid == "grey86") col.grid <- getOption("col.grid")
    if (col.bg == "ghostwhite") col.bg <- getOption("col.bg")
  }
  else if ((colors=="blue" || colors=="rose" || colors=="green" 
          || colors=="gold" || colors=="red" || colors=="orange")
          && (is.null(by) && !is.matrix(x))) {
    color.palette <- colorRampPalette(getOption("col.fill.bar"))
    clr <- color.palette(nrow(x))
    col.grid <- getOption("col.grid")
    col.bg <- getOption("col.bg")
  }
    else if (colors == "rainbow") clr <- rainbow(ncolors)
    else if (colors == "terrain") clr <- terrain.colors(ncolors)
    else if (colors == "heat") clr <- heat.colors(ncolors)
    else  {  # mono color range does not make sense here 
      clr <- c("slategray", "peachpuff2", "darksalmon", "darkseagreen1", 
        "thistle4", "azure3", "mistyrose")
      cat("\n>>> Note: For two variables, the color theme only applies to\n",
          "        the levels of an ordered factor, except for the \"gray\"\n",
          "        color theme. However, other choices are available for\n",
          "        the colors option:  \"rainbow\", \"terrain\" and \"heat\". \n\n", 
          sep="")
    }

    if (random.col) clr <- clr[sample(length(clr))]

  if (!is.null(col.fill)) {
    for (i in 1:(min(length(col.fill),length(clr)))) clr[i] <- col.fill[i]
    ncolors <- min(length(col.fill),length(clr))
  }
  palette(clr)
  col <- 1:ncolors 


  # ----------------------------------------------------------------------------
  # preliminaries
 
  if (is.matrix(x) && !beside) max.y <- max(colSums(x)) else max.y <- max(x)
  if (prop) addtop <- .01
  max.y <- max.y + addtop

  if (is.null(legend.labels)) legend.labels <- row.names(x)
# if (!is.null(legend.labels)) if (is.null(legend.loc)) legend.loc <- "topleft"
  if (beside) legend.horiz <- FALSE
  if ((!is.null(by) || is.matrix(x)) && !beside) {
    legend.horiz <- TRUE
#   if (is.null(legend.loc)) legend.loc <- "top"
    max.y <- max.y + .18*max.y
  }

  if (is.null(gap)) if (is.matrix(x) && beside) gap <- c(0.1,1) else gap <- 0.2

  # get max label size
  the.names <- integer(length=0)
  if (length(dim(x)) == 0)
    the.names <- names(x)
  else 
   if (is.null(by)) the.names <- rownames(x) else the.names <- colnames(x)
  max.nm <- 0
  for (i in (1:length(the.names))) {
    li <- nchar(the.names[i])
    if (li > max.nm) max.nm <- li
  }

  # extend the left margin to accommodate horizontal labels
  extend <- FALSE
  if (horiz && max.nm>5) {
    add.left <- max.nm/2.0
    if (y.lab != "") add.left <- add.left + 1.5
    extend <- TRUE
  } 


  # ----------------------------------------------------------------------------
  # set up plot area, color background, grid lines

  if (extend) par(mar=c(5, add.left, 4, 2) + 0.1)

  if (legend.loc == "right.margin"  &&  (!is.null(by) || is.matrix(x)))
    par(oma=c(0,0,0,3))

  if(is.null(count.names)) if (horiz) { 
    temp <- x.lab; x.lab <- y.lab; y.lab <- temp 
  }

  if (class(x) == "numeric"  &&  entered) x <- as.table(x)
  rescale <- 0
  if (is.null(by)) if (nrow(x) <=4) rescale <- nrow(x)
  if (!is.null(by) && !beside) if (ncol(x) <=4) rescale <- ncol(x)
  if (class(x) == "matrix" && entered) rescale <- 0  # turned off for now

  # set rescale to control bar width for small number of bars
  if (rescale == 0) {
    if (!horiz)
      suppressWarnings(barplot(x, col="transparent", ylim=c(0,max.y), axisnames=FALSE,
        beside=beside, space=gap, axes=FALSE, ...))
    else
      suppressWarnings(barplot(x, col="transparent", horiz=TRUE, axisnames=FALSE,
        beside=beside, space=gap, axes=FALSE, ...))
  }
  else {
    if (rescale == 4) width.bars <- .17
    if (rescale == 3) width.bars <- .22
    if (rescale == 2) width.bars <- .28
    gap <- 0.246 + 0.687*width.bars
    if (!horiz)
      suppressWarnings(barplot(x, col="transparent", ylim=c(0,max.y), axisnames=FALSE,
        beside=beside, space=gap, width=width.bars, xlim=c(0,1), axes=FALSE, ...))
    else
      suppressWarnings(barplot(x, col="transparent", horiz=TRUE, axisnames=FALSE,
        beside=beside, space=gap, width=width.bars, ylim=c(0,1), axes=FALSE, ...))
  }

  if (extend) {
    mtext(y.lab, side=2, line=add.left-1)
    y.lab <- ""
    las.value <- 1
  }
  else las.value <- 0


  # ----------------------------------------------------------------------------
  # bar plot, grid lines

  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="black")
  if (max.y > 1) vy <- pretty(0:max.y) else vy <- pretty(1:100*max.y)/100
 
  if (!over.grid) {
    if (!horiz)
      abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
    else
      abline(v=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
  }
  if (rescale == 0) {
     # width.bars <- .8   gap <- .6*width.bars
    p1<-suppressWarnings(barplot(x, add=TRUE, col=col, beside=beside, horiz=horiz,
          xlab=x.lab, ylab=y.lab, main=main.lbl, border=col.stroke, las=las.value, 
          space=gap, cex.axis=cex.axis, cex.names=cex.axis, 
          col.axis=col.axis, col.ticks=col.ticks, ...))
  }
  else
    p1<-suppressWarnings(barplot(x, add=TRUE, col=col, beside=beside, horiz=horiz,
          xlab=x.lab, ylab=y.lab, main=main.lbl, border=col.stroke, las=las.value, 
          space=gap, width=width.bars, xlim=c(0,1), 
          cex.axis=cex.axis, cex.names=cex.axis,
          col.axis=col.axis, col.ticks=col.ticks, ...))
  if (over.grid) {
    if (!horiz)
      abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
    else
      abline(v=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
  }


  # ----------------------------------------------------------------------------
  # legend for two variable plot including variable labels
  if ((!is.null(by) || is.matrix(x)) && !is.null(legend.loc))  {

    if (col.bg != "black")
      col.txt <- "black"
    else
      col.txt <- "white"

    # default right.margin option
    if (legend.loc == "right.margin") {

      par(xpd=NA)  # allow drawing outside of plot region

      # split string into separate words
      wordlist <- as.vector(strsplit(l.lab, " "))
      n.words <- length(wordlist[[1]])

      # put elements of word list into words for ease of programming
      words <- character(length=0)
      for (i in 1:n.words) words[i] <- abbreviate(wordlist[[1]][i],12)

      # combine words into lines of max length 13
      lines <- integer(length=0)
      j <- 0
      iword1 <- 1
      while (iword1 <= n.words) {
        j <- j + 1
        lines[j] <- words[iword1]
        trial <- ""
        iword <- 0
        while (nchar(trial) < 13  &&  iword1 <= n.words) {
          trial <- paste(trial, words[iword1+iword]) 
          if (nchar(trial) <= 13) {
            lines[j] <- trial
            iword1 <- iword1 + 1
          }
        }
      }
      n.lines <- length(lines)

      # remove leading blank in each line
      for (i in 1:n.lines) lines[i] <- substr(lines[i], 2, nchar(lines[i]))

      # get max word in terms of user coordinates
      max.wlen <- 0
      for (i in 1:n.lines) {
        wl <- strwidth(lines[i])
        if (wl > max.wlen) {
          max.wlen <- wl
          max.word <- lines[i]
        }
      }

      # attach a line break at the end of all but the last line
      if (n.lines > 1)
        for (i in 1:(n.lines-1)) lines[i] <- paste(lines[i], "\n", sep="")

      # construct the legend title as a single character string
      l.lab2 <- ""
      for (i in 1:n.lines) l.lab2 <- paste(l.lab2, lines[i], sep="")

      # construct vertical buffer for legend for additional legend title lines
      axis.vert <- usr[4] - usr[3]
      vbuffer <- (n.lines-1)*(0.056*axis.vert)  # usr[4] is top axis coordinate

      # legend function blind to multiple line titles, 
      # so pass largest word to get the proper width of the legend
      # also get height of legend with only one title line
      legend.labels <- abbreviate(legend.labels, 6)
      ll <- legend(0,0, legend=legend.labels, title=max.word, cex=.7,
                   fill=col, plot=FALSE)

      # horizontal placement
      size <- (par("cxy")/par("cin"))  # 1 inch in user coordinates 
      epsilon <- (size[1] - ll$rect$w) / 2

      # legend box
      xleft <- usr[2] + epsilon   # usr[2] is the user coordinate of right axis
      xright <- xleft + ll$rect$w
      lgnd.vhalf <- (vbuffer + ll$rect$h) / 2
      axis.cntr <- axis.vert / 2  + usr[3]
      ytop <- axis.cntr + lgnd.vhalf
      ybottom <- axis.cntr - lgnd.vhalf
      rect(xleft, ybottom, xright, ytop, lwd=.5, border="gray30", col=col.bg)

      # legend not multiple title lines aware, so start at last title line
      legend(x=xleft, y=ytop-vbuffer, legend=legend.labels, title=l.lab2,
             fill=col, horiz=FALSE, cex=.7, bty="n", box.lwd=.5,
             box.col="gray30", text.col=col.txt)

    }  # right margin

    else
      legend(legend.loc, legend=legend.labels, title=l.lab, fill=col, 
             horiz=legend.horiz, cex=.7, bty="n", text.col=col.txt)
  }


  # ----------------------------------------------------------------------------
  # text output
  if (prop) x  <- x.temp
  if (text.out) 
    if (is.null(by)) .ss.factor(x, brief=brief) else .ss.factor(x, by, brief=brief)

  cat("\n")

}

