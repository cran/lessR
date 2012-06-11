.bc.default <- 
function(x, by=NULL, 
         col.bars, col.border, col.bg, col.grid, random.col, colors,
         horiz, over.grid, addtop, gap, brief, prop, xlab, ylab, main,
         cex.axis, col.axis, col.ticks, beside, col.low, col.hi, count.names,
         legend.title, legend.loc, legend.labels, legend.horiz, ...) {
 
  if (!is.null(by) && prop) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "Analysis of proportions not valid for two variables.\n\n")
  }

  if (horiz && addtop!=1) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "addtop  only works for a vertical bar plot.\n\n")
  }

  orig.params <- par(no.readonly=TRUE)
  on.exit(par(orig.params))

  # get variable labels if exist plus axes labels
  if (is.null(ylab)) if (!prop) ylab <- "Frequency" else ylab <- "Proportion"
  gl <- .getlabels(xlab, ylab, main)
  x.name <- gl$xn; x.lbl <- gl$xl; x.lab <- gl$xb
  y.name <- gl$yn; y.lbl <- gl$yl; y.lab <- gl$yb;
  main.lab <- gl$mb

  #get variable labels if exist
  #gl <- .getlabels()
  #x.name <- gl$xn; y.name <- gl$yn; x.lbl <- gl$xl; y.lbl <- gl$yl

  #axis and legend labels
  #if (!is.null(xlab)) x.lab <- xlab 
    #else if (length(x.lbl) == 0) x.lab <- x.name else x.lab <- x.lbl
  #if (is.null(ylab)) if (!prop) y.lab <- "Frequency" else y.lab <- "Proportion"
    #else y.lab <- ylab

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
    if (!is.null(by)) x <- table(by,x, dnn=c(l.lab,x.lab)) 
    else {  
      x <- table(x, dnn=NULL)
      if (prop) x <- x/sum(x)
    }

  if (is.null(by) && beside && !entered) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "beside=TRUE  is not valid for analysis of only one variable.\n\n")
  }


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
      "Can only do an R color range when there is more than one color. Choose a\n",
      "built-in lessR range for colors. See help(set) for available colors.\n\n")
    }
  }

  # color palette
  if ((order.x && is.null(by)) || order.y) {
    if (colors == "blue") { 
      if (is.null(col.low)) col.low <- "slategray2"
      if (is.null(col.hi)) col.hi <- "slategray4"
    }
    else if (colors == "gray") {
      if (is.null(col.low)) col.low <- "gray90"
      if (is.null(col.hi)) col.hi <- "gray30"
    }
    else if (colors == "rose") {
      if (is.null(col.low)) col.low <- "mistyrose1"
      if (is.null(col.hi)) col.hi <- "mistyrose4"
    }
    else if (colors == "green") {
      if (is.null(col.low)) col.low <- "darkseagreen1"
      if (is.null(col.hi)) col.hi <- "darkseagreen4"
    }
    else if (colors == "gold") {
      if (is.null(col.low)) col.low <- "goldenrod1"
      if (is.null(col.hi)) col.hi <- "goldenrod4"
    }
    else if (colors == "red") { 
      if (is.null(col.low)) col.low <- "coral1"
      if (is.null(col.hi)) col.hi <- "coral4"
    }
    color.palette <- colorRampPalette(c(col.low, col.hi))
    clr <- color.palette(ncolors)
  }
  else {
    if (colors == "gray") {
      color.palette <- colorRampPalette(c("gray28","gray92"))
      clr <- color.palette(nrow(x))
      col.grid <- "white"
      col.bg <- "gray90"
    }
    else if (colors == "rainbow") clr <- rainbow(ncolors)
    else if (colors == "terrain") clr <- terrain.colors(ncolors)
    else if (colors == "heat") clr <- heat.colors(ncolors)
    else  # mono color range does not make sense here 
      clr <- c("slategray", "peachpuff2", "darksalmon", "darkseagreen1", 
        "thistle4", "azure3", "mistyrose")
    if (random.col) clr <- clr[sample(length(clr))]
  }
  if (!is.null(col.bars)) {
    for (i in 1:(min(length(col.bars),length(clr)))) clr[i] <- col.bars[i]
    ncolors <- min(length(col.bars),length(clr))
  }
  palette(clr)
  col <- 1:ncolors 

 
  if (is.matrix(x) && !beside) max.y <- max(colSums(x)) else max.y <- max(x)
  if (prop) addtop <- .01
  max.y <- max.y + addtop

  if (is.null(legend.labels)) legend.labels <- row.names(x)
  if (!is.null(legend.labels)) if (is.null(legend.loc)) legend.loc <- "topleft"
  if (beside) legend.horiz <- FALSE
  if ((!is.null(by) || is.matrix(x)) && !beside) {
    legend.horiz <- TRUE
    if (is.null(legend.loc)) legend.loc <- "top"
    max.y <- max.y + .18*max.y
  }

  if (is.null(gap)) if (is.matrix(x) && beside) gap <- c(0.1,1) else gap <- 0.2

  # get max label size
  the.names <- integer(length=0)
  if (length(dim(x)) == 0) the.names <- names(x) else the.names <- rownames(x)
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
    par(mar=c(5, add.left, 4, 2) + 0.1)
  } 

  # set up plot area, color background, grid lines
  if(is.null(count.names))  if (horiz) { temp <- x.lab; x.lab <- y.lab; y.lab <- temp }

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

  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="black")
  if (max.y > 1) vy <- pretty(0:max.y) else vy <- pretty(1:100*max.y)/100
 
  # bar plot, grid lines and legend
  if (!over.grid) {
    if (!horiz) abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
    else abline(v=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
  }
  if (rescale == 0) {
     # width.bars <- .8   gap <- .6*width.bars
     suppressWarnings(barplot(x, add=TRUE, col=col, beside=beside, horiz=horiz,
          xlab=x.lab, ylab=y.lab, main=main.lbl, border=col.border, las=las.value, 
          space=gap, cex.axis=cex.axis, cex.names=cex.axis, 
          col.axis=col.axis, col.ticks=col.ticks, ...))
  }
  else
     suppressWarnings(barplot(x, add=TRUE, col=col, beside=beside, horiz=horiz,
          xlab=x.lab, ylab=y.lab, main=main.lbl, border=col.border, las=las.value, 
          space=gap, width=width.bars, xlim=c(0,1), 
          cex.axis=cex.axis, cex.names=cex.axis,
          col.axis=col.axis, col.ticks=col.ticks, ...))
  if (over.grid) {
    if (!horiz) abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
    else abline(v=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
  }
  if ((!is.null(by) || is.matrix(x)) && !is.null(legend.loc)) 
    legend(legend.loc, legend=legend.labels, title=l.lab, fill=col, 
           horiz=legend.horiz, cex=.7, bty="n")


  if (is.null(by)) .ss.factor(x, brief=brief) else .ss.factor(x, by, brief=brief)

  cat("\n")

}

