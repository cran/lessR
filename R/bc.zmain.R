.bc.main <- 
function(x, by=NULL, 
         col.fill, col.stroke, col.bg, col.grid, random.col, colors,
         horiz, over.grid, addtop, gap, prop, xlab, ylab, main, value.labels,
         cex.axis, col.axis, rotate.values, offset, beside,
         col.low, col.hi, count.levels,
         legend.title, legend.loc, legend.labels, legend.horiz, quiet, ...) {
 
  if (horiz && addtop!=1) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "addtop  only works for a vertical bar plot.\n\n")
  }

  if ( (is.table(x) || is.matrix(x)) && is.null(legend.title) ) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "Need to specify a value for:   legend.title\n\n")
  }

  if (!is.null(value.labels)) value.labels <- gsub(" ", "\n", value.labels) 

  # get values for ... parameter values
  stuff <- .getdots(...)
  col.main <- stuff$col.main
  col.lab <- stuff$col.lab
  col.sub <- stuff$col.sub
  cex.main <- stuff$cex.main

  # get variable labels if exist plus axes labels
  txt <- "Proportion"
  if (!is.null(by)) txt <- "Cell Proportion within Each Row"
  if (is.null(ylab)) if (!prop) ylab <- "Frequency" else ylab <- txt
  gl <- .getlabels(xlab, ylab, main, cex.lab=0.98)
  x.name <- gl$xn; x.lab <- gl$xb; x.lbl <- gl$xl
  y.name <- gl$yn; y.lab <- gl$yb; y.lbl <- gl$yl
  main.lab <- gl$mb
  cex.lab <- gl$cex.lab

  if (is.matrix(x)) {  # get the variable names as counts entered directly
    options(xname = x.lab)
    options(yname = legend.title)
  }

  # if a table, convert to a matrix
  if (is.table(x)) {
    xr.nm <- rownames(x)
    xc.nm <- colnames(x)  # as.numeric makes equivalent to matrix input
    x <- matrix(as.numeric(x), nrow=nrow(x), ncol=ncol(x))
    rownames(x) <- xr.nm
    colnames(x) <- xc.nm
  }

  # get legend title, l.lab
  if (!is.null(legend.title))
    l.lab <- legend.title 
  else
    if (!is.null(by)) if (exists("y.lbl"))
      l.lab <- ifelse (length(y.lbl) == 0, y.name, y.lbl)

  # title
  main.lbl <- ifelse (!is.null(main), main, "")

  # entered counts typically integers as entered but stored as type double
  # if names(x) or rownames(x) is null, likely data from sample and c functions
  # count.levels is getting counts directly from a data frame with counts entered
  entered.pre <- FALSE
  if (!is.matrix(x) && !is.null(names(x))) entered.pre <- TRUE
  if (is.matrix(x) && !is.null(rownames(x))) entered.pre <- TRUE
  entered <- ifelse (!is.integer(x) && is.double(x) && entered.pre, TRUE, FALSE)
  if (!is.null(count.levels)) {
    x <- as.numeric(x)
    names(x) <- count.levels
    entered <- TRUE
  }
  
  # save ordered status before converting x to a table
  order.x <- ifelse (is.ordered(x) && is.null(by), TRUE, FALSE)
  order.y <- ifelse (is.ordered(by), TRUE, FALSE)

  # convert x to a table
  if (!entered) {  # x.temp allows frequencies to be restored for text output
    if (!is.null(by)) {
      if (length(x) == length(by))
        x.temp <- table(by,x, dnn=c(y.name, x.name))
      else {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        x.name, " and ", y.name, " must be of the same size\n\n",
        "Size of ", x.name, ": ", length(x), "\n", 
        "Size of ", y.name, ": ", length(by), "\n\n", sep="")
      }
      x <- table(by,x, dnn=c(y.name, x.name)) 
      if (prop) x <- prop.table(x, 1)
    }
    else {  # one variable 
      x.temp <- x  # save counts
      x <- table(x, dnn=NULL)
      if (prop) x <- x/sum(x)
    }
  }

  if (!is.null(count.levels)) {
    x.temp <- x
    x <- as.table(x)
    if (prop) x <- x/sum(x)
  }

  if (is.null(by) && beside && !entered) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "beside=TRUE  is not valid for analysis of only one variable.\n\n")
  }


  # ----------------------------------------------------------------------------
  # colors

  # get number of colors (does not indicate col.fill multiple colors)
  if (is.null(by) && !order.x && !is.matrix(x))
    n.colors <- 1
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
    col.grid <- getOption("col.grid")
    col.bg <- getOption("col.bg")

    lowhi <- .ordcolors(colors, col.low, col.hi) 
    col.low <- lowhi$col.low
    col.hi <- lowhi$col.hi

    color.palette <- colorRampPalette(c(col.low, col.hi))
    clr <- color.palette(n.colors)
  } # end ordered

  else if (colors == "gray") {
    color.palette <- colorRampPalette(c("gray85","gray30"))
    clr <- color.palette(nrow(x))
    if (col.grid == "gray86") col.grid <- getOption("col.grid")
    if (col.bg == "ghostwhite") col.bg <- getOption("col.bg")
  }

  else if ((colors %in% c("blue","rose","green","gold","red","orange",
          "sienna","dodgerblue","purple","white","orange.black","gray.black")
          && (is.null(by) && !is.matrix(x)))) {
      if (n.colors > 1) {
        color.palette <- colorRampPalette(getOption("col.fill.bar"))
        clr <- color.palette(nrow(x))
      }
      col.grid <- getOption("col.grid")
      col.bg <- getOption("col.bg")
    }

  else if (colors == "rainbow") clr <- rainbow(n.colors)
  else if (colors == "terrain") clr <- terrain.colors(n.colors)
  else if (colors == "heat") clr <- heat.colors(n.colors)

  else  {  # ordered color range does not make sense here 
    clr <- .col.discrete()
    # lighten some default background colors
    if (col.bg == "#EEF0F2") col.bg <- rgb(245,245,245, maxColorValue=255)
    if (col.bg == "#E5DB8E") col.bg <- rgb(251,245,220, maxColorValue=255)
  }

  if (random.col) clr <- clr[sample(length(clr))]

  if (!is.null(col.fill)) {
    if (n.colors > 1) {
      for (i in 1:(min(length(col.fill),length(clr)))) clr[i] <- col.fill[i]
      n.colors <- min(length(col.fill),length(clr))
    }
    else {
      col <- col.fill
    }
  }

  if (n.colors > 1) {
    palette(clr)
    col <- 1:n.colors 
  }
  else {
    if (is.null(col.fill)) col <- getOption("col.fill.bar")  
  }


  # ----------------------------------------------------------------------------
  # preliminaries
 
  max.y <- ifelse (is.matrix(x) && !beside, max(colSums(x)), max(x))
  if (prop) addtop <- .01
  max.y <- max.y + addtop

  if (is.null(legend.labels)) legend.labels <- row.names(x)
  if (beside) legend.horiz <- FALSE
  if ((!is.null(by) || is.matrix(x)) && !beside) {
    legend.horiz <- TRUE
    max.y <- max.y + .18*max.y
  }

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
    the.names <- ifelse (is.null(by), rownames(x), colnames(x))
  max.nm <- 0
  for (i in (1:length(the.names))) {
    li <- ifelse(!is.na(the.names[i]), nchar(the.names[i]), 0)
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

  if (is.null(main)) {
    orig.params <- par(no.readonly=TRUE)
    on.exit(par(orig.params))
    par(mar=c(4,4,2,2)+0.1)
    if (extend) par(mar=c(5, add.left, 4, 2) + 0.1)
  }


  if (legend.loc == "right.margin"  &&  (!is.null(by) || is.matrix(x)))
    par(oma=c(0,0,0,3))

  if(is.null(count.levels)) if (horiz) { 
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
      barplot(x, col="transparent", ylim=c(0,max.y), axisnames=FALSE,
        beside=beside, space=gap, axes=FALSE, ...)
    else
      barplot(x, col="transparent", horiz=TRUE, axisnames=FALSE,
        beside=beside, space=gap, axes=FALSE, ...)
  }
  else {  # rescale
    if (rescale == 4) width.bars <- .17
    if (rescale == 3) width.bars <- .22
    if (rescale == 2) width.bars <- .28
    gap <- 0.246 + 0.687*width.bars
    if (!horiz)
      barplot(x, col="transparent", ylim=c(0,max.y), axisnames=FALSE,
        beside=beside, space=gap, width=width.bars, xlim=c(0,1), axes=FALSE, ...)
    else
      barplot(x, col="transparent", horiz=TRUE, axisnames=FALSE,
        beside=beside, space=gap, width=width.bars, ylim=c(0,1), axes=FALSE, ...)
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
    # axisnames suppressed only works if horiz is FALSE,
    #   otherwise let R generate
    coords <- barplot(x, add=TRUE, col=col, beside=beside, horiz=horiz,
          axes=FALSE, ann=FALSE, border=col.stroke, las=las.value, 
          space=gap, cex.names=cex.axis, axisnames=horiz, ...)
  }
  else
    coords <- barplot(x, add=TRUE, col=col, beside=beside, horiz=horiz,
          axes=FALSE, ann=FALSE, border=col.stroke, las=las.value, 
          space=gap, width=width.bars, xlim=c(0,1), 
          cex.names=cex.axis, axisnames=horiz, ...)
  if (over.grid) {
    if (!horiz)
      abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
    else
      abline(v=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
  }

  # axes  (barplot produces its own axis for the categories,
  #        unless axisnames=FALSE)
  ax.freq <- ifelse(horiz, 1, 2)
  if (!horiz) las.value <- 1
  axis(ax.freq, cex.axis=cex.axis, col.axis=col.axis, las=las.value, ...) 

  ax.value <- ifelse(horiz, 2, 1)
  if (!horiz) {
    if (is.null(value.labels)) {
      if (is.null(by)) val.lab <- names(x) else val.lab <- colnames(x)
      if (length(val.lab) == 0) val.lab <- colnames(x)  # read matrix directly
    }
    else
      val.lab <- value.labels
    val.lab <- gsub(" ", "\n", val.lab) 

    axis(ax.value, at=coords, labels=FALSE, tck=-.01, ...)
    #offset <- .5 + .01 * rotate.values
    text(x=coords, y=par("usr")[3], labels=val.lab,
         pos=1, xpd=TRUE, cex=cex.axis, col=col.axis, srt=rotate.values,
         offset=offset, ...)
  }
  #else
    #text(y=coords, x=par("usr")[1], labels=names(x),
         #pos=1, xpd=TRUE, cex=cex.axis, col=col.axis, ...)
    
  # axis labels
  lbl.lns <- 3
  lblx.lns <- ifelse (grepl("\n", x.lab, fixed=TRUE), lbl.lns + 0.5, lbl.lns)
  lblx.lns <- lblx.lns - 0.5
  if (offset > 0.5) lblx.lns <- lblx.lns + 0.5
  #if (!horiz) lblx.lns <- lblx.lns - 1
  #if (offset > 0.5) lblx.lns <- lblx.lns + 1
  lbly.lns <- ifelse (grepl("\n", y.lab, fixed=TRUE), lbl.lns - .4, lbl.lns)
  if (horiz) lbly.lns <- lbly.lns - .5
  title(xlab=x.lab, line=lblx.lns, cex.lab=cex.lab, col.lab=col.lab)
  title(ylab=y.lab, line=lbly.lns, cex.lab=cex.lab)
  title(main=main.lab, col.main=col.main)


  # ----------------------------------------------------------------------------
  # legend for two variable plot including variable labels
  if ( (!is.null(by) || is.matrix(x)) && !is.null(legend.loc))  {

     col.txt <- ifelse (sum(col2rgb(col.bg))/3 > 80, "black", rgb(.97,.97,.97))

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
  if (prop && is.null(count.levels)) {
    if (!is.null(by) || is.matrix(x))
      x  <- x.temp 
    else
      x <- table(x.temp)
  }
  if (prop && !is.null(count.levels))
    x <- as.table(x.temp)

  if (is.null(by)  &&  !is.matrix(x)  && !quiet) {  # one variable, dim == 0 if x<-x.temp 
  #if (length(dim(x)) == 1  && !quiet) {  # one variable, dim == 0 if x<-x.temp 

    stats <- .ss.factor(x, by=NULL, brief=TRUE, digits.d=NULL,
                        x.name, y.name, x.lbl, y.lbl)

    txttl <- stats$title
    counts <- stats$count
    chi <- stats$chi

    class(txttl) <- "out_piece"
    class(counts) <- "out_piece"
    class(chi) <- "out_piece"
    output <- list(out_title=txttl, out_counts=counts, out_chi=chi)
    class(output) <- "out_all"
    print(output)      
  }

  else {  # two variables
    stats <- .ss.factor(x, by, brief=TRUE, digits.d=NULL,
                        x.name, y.name, x.lbl, y.lbl) 

    txttl <- stats$txttl
    txfrq <- stats$txfrq
    txXV <- stats$txXV
    class(txttl) <- "out_piece"
    class(txfrq) <- "out_piece"
    class(txXV) <- "out_piece"
    if (!prop)
      output <- list(out_title=txttl, out_text=txfrq, out_XV=txXV)
    else {
      txrow <- stats$txrow
      class(txrow) <- "out_piece"
      output <- list(out_title=txttl, out_text=txfrq, out_row=txrow, out_XV=txXV
)   }
    class(output) <- "out_all"
    print(output)

  }

  cat("\n")

  return(stats)

}

