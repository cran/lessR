color.barchart.data.frame <-
function(x, ...)  {
  fname <- paste("BarCharts.",deparse(substitute(x)),".pdf",sep="")
  pdf(file=fname)
  for (i in 1:ncol(x))
    if (!is.numeric(x[,i])) {
      tlbl <- paste("Bar Chart for", names(x)[i])
      if (nlevels(factor(x[,i])) < length(x[,i]))
        color.barchart.default(x[,i], xlab=names(x)[i], main=tlbl, font.main=1, ...)
      else cat("\n", names(x)[i], "appears to contain unique Names or IDs", "\n")
    }
  dev.off()
  if (getwd() == "/")
    workdir <- "top level (root) of your file system"
  else
    workdir <- getwd()
  cat("\n\npdf file of bar charts:",  fname, "\n")
  cat("\nWritten at current working directory:", workdir, "\n\n")
}
color.barchart.default <- 
function(x, y=NULL, col.bars=NULL, border="black", 
         col.bg="ghostwhite", col.grid="grey86", gap=NULL, beside=TRUE, 
         over.grid=FALSE, prop=FALSE, xlab=NULL, legend.title=NULL, 
         legend.loc=NULL, legend.labels=NULL, legend.horiz=FALSE, 
         vivid=FALSE, random.col=FALSE, col.low="slategray2", 
         col.hi="slategray4", addtop=1, horiz=FALSE, chisq=FALSE, ...) {
         
  dash <- function(n.dash) { for (i in 1:(n.dash)) cat("-"); cat("\n") }
  if (chisq && prop) { 
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Chi-square analysis here not valid for proportions.\n\n")
  }
  if (!is.null(y) && prop) { 
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Analysis of proportions not valid for two variables.\n\n")
  }
  # variable labels
  if (is.null(xlab)) x.lbl <- deparse(substitute(x)) else x.lbl <- xlab
  if (!is.null(legend.title)) y.lbl <- legend.title 
    else if (!is.null(y)) y.lbl <- deparse(substitute(y)) else y.lbl=NULL
  # entered counts typically integers as entered but stored as type double
  # if names(x) or rownames(x) is null, likely data from sample and c functions
  entered.pre <- FALSE
  if (!is.matrix(x) && !is.null(names(x))) entered.pre <- TRUE
  if (is.matrix(x) && !is.null(rownames(x))) entered.pre <- TRUE
  if (!is.integer(x) && is.double(x) && entered.pre) 
    entered <- TRUE else entered <- FALSE
  # save ordered status before converting x to a table
  if (is.ordered(x) && is.null(y)) order.x <- TRUE else order.x <- FALSE
  if (is.ordered(y)) order.y <- TRUE else order.y <- FALSE
  # convert to table, with labels, if needed
  if (!entered && !is.table(x))
    if (!is.null(y)) x <- table(y,x, dnn=c(y.lbl,x.lbl)) 
    else {  
      x <- table(x, dnn=x.lbl)
      if (prop) x <- x/sum(x)  # prop option
    }
  # get number of colors
  if (is.null(y) && !order.x && !is.matrix(x)) ncolors <- 1 else ncolors <- nrow(x)
  # color palette
  if ((order.x && is.null(y)) || order.y) {
      color.palette <- colorRampPalette(c(col.low, col.hi))
      clr <- color.palette(ncolors)
  }
  else {
    if (!vivid)
      clr <- c("slategray", "peachpuff2", "darksalmon", "darkseagreen1", 
        "thistle4", "azure3", "mistyrose")
    else {
      clr <- c("coral3", "seagreen3", "maroon3", "dodgerblue3", "purple3", 
        "turquoise3", "yellow3")
      if (col.bg == "seashell") col.bg <- "cornsilk1"
    }
    if (random.col) clr <- clr[sample(length(clr))]
  }
  if (!is.null(col.bars)) {
    for (i in 1:(min(length(col.bars),length(clr)))) clr[i] <- col.bars[i]
    ncolors <- min(length(col.bars),length(clr))
  }
  palette(clr)
  col <- 1:ncolors 
  if (is.null(legend.labels)) legend.labels <- row.names(x)
  if (!is.null(legend.labels)) if (is.null(legend.loc)) legend.loc <- "top"
  if (is.matrix(x) && !beside) max.y <- max(colSums(x)) else max.y <- max(x)
  if (prop) addtop <- .01
  max.y <- max.y + addtop
  if (is.null(gap)) if (is.matrix(x) && beside) gap <- c(0.1,1) else gap <- 0.2
  # set up plot area, color background, grid lines
  if (!horiz)
    barplot(x, col="transparent", border=NA, ylim=c(0,max.y), beside=beside, 
     space=gap, axisnames=FALSE, ...)
  else
    barplot(x, col="transparent", border=NA, beside=beside, space=gap,
      axisnames=FALSE, horiz=TRUE, font.main=1, ...)
  usr <- par("usr");  rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="black")
  if (max.y > 1) vy <- pretty(0:max.y) else vy <- pretty(1:100*max.y)/100
  # bar plot, grid lines and legend
  if (!over.grid) {
    if (!horiz) abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
    else abline(v=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
  }
  barplot(x, add=TRUE, col=col, beside=beside, horiz=horiz, space=gap, xlab=x.lbl, ...)
  if (over.grid) {
    if (!horiz) abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
    else abline(v=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
  }
  if ((!is.null(y) || is.matrix(x)) && !is.null(legend.loc)) 
    legend(legend.loc, legend=legend.labels, title=y.lbl, fill=col, horiz=legend.horiz)
  # print table, chi-square analysis
  if (!is.null(y) || is.matrix(x)) {  # two variables
    cat("\n"); dash(30); cat("Joint and Marginal Frequencies\n"); dash(30); 
      print(addmargins(x))
    cat("\n"); dash(30); cat("Cell Proportions and Marginals\n"); dash(30); 
      print(round(addmargins(prop.table(x)),3))
      cat("\n"); dash(30); cat("Proportions within Each Column\n"); dash(30);
      x.col <- prop.table(x, margin=2)
      Sum <- double(ncol(x.col))
      for (i in 1:ncol(x.col)) Sum[i] <- sum(x.col[,i])
      print(round(rbind(x.col,Sum),3))
    cat("\n"); dash(27); cat("Proportions within Each Row\n"); dash(27); 
      x.row <- prop.table(x, margin=1)
      Sum <- double(nrow(x.row))
      for (i in 1:nrow(x.row)) Sum[i] <- sum(x.row[i,])
      print(round(cbind(x.row,Sum),3))
    if (chisq) { cat("\n");  print(summary(as.table(x)))}
  }
  else {  # one variable
    cat("\n"); dash(11); cat("Frequencies\n"); dash(11); print(x)
    cat("\n"); dash(11); cat("Proportions\n"); dash(11); print(round(prop.table(x),3))
    if (chisq) {
      ct <- chisq.test(x)
      cat("\nChi-squared test of the null hypothesis of equal probabilities\n")
      cat("  Chisq = ", ct$statistic, ",  df = ", ct$parameter, ",  p-value = ", 
        ct$p.value, sep="", "\n")
    }
  }
  cat("\n")
}
color.barchart <-
function(x=NULL, ...)  {
  if (is.null(x)) {
    if (!exists("mydata")) 
      stop("Need to specify an existing data frame or data frame mydata must exist.")
    color.barchart(mydata, ...) 
  }
  else UseMethod("color.barchart")
	
}
color.boxplot.data.frame <-
function(x, ...)  {
  fname <- paste("Boxplots.",deparse(substitute(x)),".pdf",sep="")
  pdf(file=fname)
  for (i in 1:ncol(x))
    if (is.numeric(x[,i])) {
      tlbl <- paste("Boxplot for", names(x)[i])
      color.boxplot.default(x[,i], xlab=names(x)[i], main=tlbl, ...)
    }
  dev.off()
  if (getwd() == "/")
    workdir <- "top level (root) of your file system"
  else
    workdir <- getwd()
  cat("\n\npdf file of histograms:",  fname, "\n")
  cat("\nWritten at current working directory:", workdir, "\n\n")
}
color.boxplot.default <-
function(x, col.box="bisque2", col.point=NULL, 
         horizontal=TRUE, xlab=NULL, digits.d=10, ...) {        
  dash <- function(n.dash) { for (i in 1:(n.dash)) cat("-"); cat("\n") }
  if (is.null(xlab)) x.lbl <- deparse(substitute(x)) else x.lbl <- xlab
  if (is.null(col.point)) col.point <- col.box
  # boxplot
  bv <- boxplot(x, col=col.box, bg=col.point, pch=21, horizontal=horizontal, xlab=x.lbl, ...)
  # summarize data
  cat("\n")
  dash(30)
  cat("Data Summary:", x.lbl, "\n")
  dash(30)
  cat("\n")
  cat("Present:", sum(!is.na(x)), "\n")
  cat("Missing:", sum(is.na(x)), "\n")
  cat("Total  :", length(x), "\n")
  cat("\n")
  cat("Minimum      :", format(signif(min(x, na.rm=TRUE),digits.d), scientific=FALSE), "\n")
  cat("Lower Whisker:", format(signif(bv$stats[1],digits.d), scientific=FALSE), "\n")
  cat("Lower Hinge  :", format(signif(bv$stats[2],digits.d), scientific=FALSE), "\n")
  cat("Median       :", format(signif(bv$stats[3],digits.d), scientific=FALSE), "\n")
  cat("Upper Hinge  :", format(signif(bv$stats[4],digits.d), scientific=FALSE), "\n")
  cat("Upper Whisker:", format(signif(bv$stats[5],digits.d), scientific=FALSE), "\n")
  cat("Maximum      :", format(signif(max(x, na.rm=TRUE),digits.d), scientific=FALSE), "\n")
  cat("\n")
  cat("1st Quartile :", format(signif(quantile(x, na.rm=TRUE)[2],digits.d), scientific=FALSE), "\n")
  cat("3rd Quartile :", format(signif(quantile(x, na.rm=TRUE)[4],digits.d), scientific=FALSE), "\n")
  cat("IQR          :", format(signif(IQR(x, na.rm=TRUE),digits.d), scientific=FALSE), "\n")
  cat("\n")
}
color.boxplot <-
function(x=NULL, ...)  {
  if (is.null(x)) {
    if (!exists("mydata")) 
      stop("Need to specify an existing data frame or data frame mydata must exist.")
    color.boxplot(mydata, ...) 
  }
  else UseMethod("color.boxplot")
	
}
color.density <- 
function(x, col.nrm="darkblue", col.gen="blue",  
         col.bg="ghostwhite", col.grid="grey90", col.hist="grey86",
         col.fill.nrm=rgb(80,150,200, alpha=70, max=255), 
         col.fill.gen=rgb(250,210,230, alpha=70, max=255),
         bin.start=NULL, bin.width=NULL, text.out=TRUE,
         type=c("both", "general", "normal"), x.pt=NULL,
         xlab=NULL, main=NULL, ...) {
  # produce actual argument, such as from an abbreviation, and flag if not exist
  type <- match.arg(type)
  if (type != "general"  &&  !is.null(x.pt)) { 
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        "x.pt only valid when type is equal to \"general\".\n\n")
  }
  # set the labels  
  if (is.null(xlab)) x.lbl <- deparse(substitute(x)) else x.lbl <- xlab
  if (is.null(main)) main.lbl <- "" else main.lbl <- main
      
  # get breaks from user supplied bin width and/or supplied start value
  # otherwise, breaks="Sturges by default
  # copied from color.hist
  if (!is.null(bin.width)  || !is.null(bin.start)) {
    if (is.null(bin.start)) bin.start <- pretty(min(x):max(x))[1]
    if (is.null(bin.width)) {
      h <- hist(x, plot=FALSE, breaks="Sturges")
      bin.width <- h$breaks[2]-h$breaks[1]
    }
    seq.end <- max(x)
    breaks <- seq(bin.start,seq.end,bin.width)
    while (max(breaks) < max(x)) {
      seq.end <- seq.end + bin.width
      breaks <- seq(bin.start,seq.end,bin.width)
    }
  }
  else breaks="Sturges"
  # histogram calculations, no plot
  h <- hist(x, plot=FALSE, breaks)
  # general density curve, no plot
  # suppress warnings about possible graphic parameters
  d.gen <- suppressWarnings(density(x, ...))
  mx <- mean(x)
  # min and max x coordinates for graph, make symmetric
  min.dev.x <- min(d.gen$x) - mx
  max.dev.x <- max(d.gen$x) - mx
  if (abs(min.dev.x) > abs(max.dev.x)) {
    min.x <- min(d.gen$x)
    max.x <- mx + abs(min.dev.x)
  }
  if (abs(max.dev.x) > abs(min.dev.x)) {
    min.x <- mx - abs(max.dev.x)
    max.x <- max(d.gen$x)
  }
  # normal density curve, no plot
  xx <- seq(min.x, max.x, length=200)
  d.nrm <- dnorm(xx,mean(x),sd(x))
  # max y coordinate for graph
  max.y <- max(max(d.nrm), max(d.gen$y), max(h$density))
  # set up plot area
  # bw if specified also gets passed to plot, so suppress warning
  suppressWarnings(plot(h, border="transparent", freq=FALSE, xlab=x.lbl, main=main.lbl, 
    xlim=c(min.x,max.x), ylim=c(0,max.y), ...))
  # colored background for plotting area
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="black")
  # grid lines computation and print
  vy <- pretty(h$density)
  abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
  # plot the histogram
  plot(h, add=TRUE, freq=FALSE, col=col.hist, border=col.hist)
  # plot the normal curve
  if (type == "normal" || type == "both") {
    lines(xx, d.nrm, type="l", col=col.nrm)
    polygon(c(min.x,xx,max.x), c(0,d.nrm,0), col=col.fill.nrm)
  }
  # plot the general curve
  if (type == "general" || type == "both") {
    lines(d.gen, col=col.gen)
    polygon(d.gen, col=col.fill.gen)
    if (text.out)
      cat("\nDensity bandwidth for general curve: ", round(d.gen$bw,4), sep="", "\n")
  }
  # plot the optional bar about a chosen point for general curve only
  if (!is.null(x.pt)  &&  type == "general") {
    d <- d.gen 
    y.pt <- d$y[which.min(abs(x.pt-d$x))]
    xbeg <- x.pt - 0.5
    xend <- x.pt + 0.5
    xsub <- d$x[d$x > xbeg & d$x < xend]
    ysub <- d$y[d$x > xbeg & d$x < xend]
    txt.ypt <- toString(round(y.pt, 3))
    txt <- paste("Density =", txt.ypt, "at x =", toString(round(x.pt, 2)))
    title(main=txt)
    polygon(c(xbeg, xsub, xend), c(0, ysub, 0), col="lightsteelblue")
    if (min(x) > 0) left <- -2*min(x) else left <- 2*min(x) 
    lines(c(left,x.pt), c(y.pt,y.pt), col="darkblue", lwd = 0.5)
    lines(c(x.pt,x.pt), c(0,y.pt), col="darkblue", lwd = 0.5)
  }
  cat("\n")
}
color.hist.data.frame <-
function(x, ...)  {
  fname <- paste("Histograms.",deparse(substitute(x)),".pdf",sep="")
  pdf(file=fname)
  for (i in 1:ncol(x))
    if (is.numeric(x[,i])) {
      tlbl <- paste("Histogram for", names(x)[i])
      color.hist.default(x[,i], xlab=names(x)[i], main=tlbl, ...)
    }
  dev.off()
  if (getwd() == "/")
    workdir <- "top level (root) of your file system"
  else
    workdir <- getwd()
  cat("\n\npdf file of histograms:",  fname, "\n")
  cat("\nWritten at current working directory:", workdir, "\n\n")
}
color.hist.default <- 
function(x, col="lightsteelblue", border="black", col.bg="ghostwhite",
         col.grid="grey90", over.grid=FALSE,
         breaks="Sturges", bin.start=NULL, bin.width=NULL,
         show.values=FALSE, prop=FALSE, cumul=c("off", "on", "both"),
         col.reg="snow2", digits.d=5, xlab=NULL, ylab=NULL, main=NULL, ...) {
  dash <- function(n.dash) { for (i in 1:(n.dash)) cat("-"); cat("\n") }
  if (!is.numeric(x)) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "A histogram is only computed from a numeric variable.\n",
      deparse(substitute(x)), " is not a numeric variable.\n",
      "For the analysis of a categorical variable use color.barhchart.\n\n")
  }
        
  if (is.numeric(breaks) && !is.null(bin.start)) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Choose only one option to specify a start value.\n",
      "Either choose the option  breaks  or the option  bin.start.\n\n")
  }
  # produce actual argument, such as from an abbreviation, and flag if not exist
  cumul <- match.arg(cumul)
  # set the labels  
  if (is.null(xlab)) x.lbl <- deparse(substitute(x)) else x.lbl <- xlab
  if (is.null(ylab)) if (!prop) y.lbl <- "Frequency" else y.lbl <- "Proportion"
    else y.lbl <- ylab
  if (is.null(main)) main.lbl <- "" else main.lbl <- main
  # get breaks from user supplied bin width and/or supplied start value
  if (!is.null(bin.width)  || !is.null(bin.start)) {
    if (is.null(bin.start)) 
      bin.start <- pretty(min(x, na.rm = TRUE):max(x, na.rm = TRUE))[1]
    if (is.null(bin.width)) {
      h <- hist(x, plot=FALSE, breaks="Sturges")
      bin.width <- h$breaks[2]-h$breaks[1]
    }
    max.x <- max(x, na.rm = TRUE)
    seq.end <- max.x
    breaks <- seq(bin.start,seq.end,bin.width)
    while (max(breaks) < max.x) {
      seq.end <- seq.end + bin.width
      breaks <- seq(bin.start,seq.end,bin.width)
    }
  }
  # for user supplied bins, from seq function or bin.start, 
  #  make sure entire data range is spanned
  if (is.numeric(breaks)) {
    cc <- cut(x, breaks, ...)   # replace each data value with its bin
    labs <- levels(cc)  # get list of unique bins, ordered
    bins <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", labs) ),
          upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs) ))
    bin.min <- min(bins)
    bin.max <- max(bins)
    n.under <- length(x[which(x<bin.min)])
    n.over <- length(x[which(x>bin.max)])
    if (n.under+n.over > 0) {
      cat("\nRange of the data: ", min(x), " to ", max(x), "\n", sep="")
      if (length(breaks) > 3)
        cat("Specified bin cutpoints: ", bin.min, breaks[2], "...", 
          breaks[length(breaks)-1], bin.max, "\n\n")
    else
      cat("Range of the specified bins: ", bin.min, " to ", bin.max, "\n", sep="")
      if (n.under > 0) 
        cat("Data values too small to fit in the bins: ", x[which(x<bin.min)], "\n")
      if (n.over > 0)
        cat("Data values too large to fit in the bins: ", x[which(x>bin.max)], "\n")
      cat("\n")
      cat("Each data value must be in a bin.\n")
      txt <- "To fix this problem, extend the bin range "
      if (n.under > 0) cat(txt, "below ", bin.min, ".\n", sep="")
      if (n.over > 0) cat(txt, "above ", bin.max, ".\n", sep="")
      stop("Extend the bin range and rerun.\n\n", call. = FALSE)
    }  
  }
  # calculate but do not plot the histogram
  # stop warnings about any unused parameters due to not plotting 
  h <- suppressWarnings(hist(x, plot=FALSE, breaks, ...))
  # summarize data
  cat("\n")
  dash(49)
  cat("Data Summary:", x.lbl, "\n")
  dash(49)
  cat("\n")
  cat("Present:", sum(!is.na(x)), "\n")
  cat("Missing:", sum(is.na(x)), "\n")  # hist automatically deals with missing data
  cat("Total  :", length(x), "\n")
  cat("\n")
  cat("Mean   :", format(signif(mean(x, na.rm=TRUE),digits.d), scientific=FALSE), "\n")
  cat("Std Dev:", format(signif(sd(x, na.rm=TRUE),digits.d), scientific=FALSE), "\n")
  cat("\n")
  cat("Minimum      :", format(signif(min(x, na.rm=TRUE),digits.d), scientific=FALSE), "\n")
  cat("1st Quartile :", format(signif(quantile(x, na.rm=TRUE)[2],digits.d), scientific=FALSE), "\n")
  cat("Median       :", format(signif(median(x, na.rm=TRUE),digits.d), scientific=FALSE), "\n")
  cat("3rd Quartile :", format(signif(quantile(x, na.rm=TRUE)[4],digits.d), scientific=FALSE), "\n")
  cat("Maximum      :", format(signif(max(x, na.rm=TRUE),digits.d), scientific=FALSE), "\n")
  cat("\n")
  cat("IQR          :", format(signif(IQR(x, na.rm=TRUE),digits.d), scientific=FALSE), "\n")
  cat("\nNumber of Bins:", length(h$breaks)-1, "\n")
  # relative frequency histogram option
  if (prop) h$counts <- h$counts/length(x)
  # cumulative histogram option
  if (cumul != "off") {
    old.counts <- h$counts
    h$counts <- cumsum(h$counts)
  }
  # set up plot area
  plot(h, border="transparent", xlab=x.lbl, ylab=y.lbl, main=main.lbl, font.main=1, 
    freq=TRUE, ...)
  # colored background for plotting area
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="black")
  # grid lines computation
  vy <- pretty(h$counts)
  # plot the histogram and grid lines
  if (!over.grid) abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
  plot(h, add=TRUE, col=col, border=border, labels=show.values, freq=TRUE, ...)
  if (cumul == "both") {
    h$counts <- old.counts
     plot(h, add=TRUE, col=col.reg, freq=TRUE)
  }
  if (over.grid) abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
  # frequency table
  # j<17 condition is to stop the 0.99999... problem
  cum.c <- cumsum(h$counts)
  prop <- round(h$counts/length(x),2)
  cum.p <- cumsum(prop)
  max.dg <- 0
  for (i in 1:length(h$breaks)) {
    j <- nchar(as.character(h$breaks[i]))
    if (j>max.dg && j<17) max.dg <- j
  }
  max.dg.mid <- 0
  for (i in 1:length(h$mids)) {
    j <- nchar(as.character(h$mids[i]))
    if (j>max.dg.mid && j<19) max.dg.mid <- j
  }
  x.breaks <- format(h$breaks, width=max.dg, justify="right", scientific=FALSE)
  x.mids <- format(h$mids, width=max.dg.mid+3, justify="right", scientific=FALSE)
  cat("\n")
  buf1 <- format("", width=max.dg+1)
  strt <- 9 + 2*max.dg
  my.width <- strt-(nchar(buf1)+nchar("Bin"))-7
  buf2 <- format(" ", width=my.width)
  dash(66)
  cat(buf1,"Bin",buf2,"  Midpoint","  Count","  Prop","  Cumul.c"," Cumul.p",sep="","\n")
  dash(66)
  for (i in 2:length(h$breaks)-1)
    cat(sprintf("%s > %s %s %6.0f %6.2f %6.0f %6.2f", x.breaks[i], 
      x.breaks[i+1], x.mids[i], h$counts[i], prop[i], cum.c[i], cum.p[i]), "\n") 
  dash(66)
  cat("\n")
}
color.hist <-
function(x=NULL, ...)  {
  if (is.null(x)) {
    if (!exists("mydata")) 
      stop("Need to specify an existing data frame or data frame mydata must exist.")
    color.hist(mydata, ...) 
  }
  else UseMethod("color.hist")
	
}
color.plot <-
function(x, y=NULL, type=NULL, col.line="darkblue", col.area=NULL,  
           col.point="darkblue", col.fill=NULL, col.grid="grey90", 
           col.bg="ghostwhite", col.box="black", xy.ticks=TRUE, 
           xlab=NULL, ylab=NULL, pch=NULL, cex=NULL, center.line=NULL,
           kind=c("default", "regular", "bubble.freq", "sunflower.freq"),
           x.start=NULL, x.end=NULL, size=.25, text.out=TRUE,
           fit.line=c("none", "lowess", "ls"), col.fit.line="grey55", 
           col.bubble="lightsteelblue", col.flower="steelblue",
           time.start=NULL, time.by=NULL, time.reverse=FALSE,
           ellipse=FALSE, col.ellipse="lightslategray", 
           fill.ellipse=TRUE, ...) {
           
max.dd <- function(x) {
  n.dec <-function(x) {
    xc <- as.character(x)
    nchar(xc)
    ipos <- 0
    for (i in 1:nchar(xc)) if (substr(xc,i,i)==".") ipos <- i
    if (ipos > 0) n.dec <- nchar(xc)-ipos else n.dec <- 0
    return(n.dec)
  }
  max.dd <- 0
  for (i in 1:length(x))
    if (!is.na(x[i])) if (n.dec(x[i]) > max.dd ) max.dd <- n.dec(x[i])   
  return(max.dd)
}
dashes <- function(ndash) { for (i in 1:(ndash)) cat("-"); cat("\n") }
# -------------------------
# Preliminaries
# -------------------------
# produce actual argument, such as from an abbreviation, and flag if not exist
fit.line <- match.arg(fit.line)
kind <- match.arg(kind)
if ( (kind == "bubble.freq") || (kind == "sunflower.freq") ) {
  if (is.null(y))  {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "Option 'bubble.freq' or 'sunflower.freq' are only used in scatterplots.\n\n")
  }
  if ( (!is.integer(x)) || !is.integer(y)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "Option 'bubble.freq' or 'sunflower.freq' can only be used with integer data.\n\n")
  }
}
if (!is.null(type)) if (type != "p" && type != "l" && type != "b") { 
  cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Option 'type' can only be \"p\" for points,\n",
      "  \"l\" for line or \"b\" for both.\n\n")
    }
if (!is.null(center.line))
  if (center.line != "mean" && center.line != "median"&& center.line != "off") { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Option 'center.line' can only be \"mean\", \"median\", or \"off\".\n\n")
    }
if (!is.null(y)) {
  txt <- "ignored when two variables are specified.\n"
  if (!is.null(time.start)) cat("Warning: Option 'time.start'", txt)
  if (!is.null(time.by)) cat("Warning: Option 'time.by'", txt)
  if ((time.reverse)) cat("Warning: Option 'time.reverse'", txt)
}
if (!is.null(time.start) && is.null(time.by)) {
   cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Specified  time.start  so also need  time.by.\n\n")
}
if (ellipse) {
  check.car <- suppressWarnings(require(car, quietly=TRUE))
  if (!check.car) {
    cat(">>> Placing an ellipse around the scatterplot requires package car.", "\n")
    cat(">>> The ellipse is not provided here, but all other output unaffected.", "\n")
    cat(">>> To get the car package, run one time only: install.packages('car')", "\n")
  }
}
#  pch=21 is a filled circle
if (is.null(pch)) point.type <- 21 else point.type <- pch
nrows <- length(x)
if (is.null(cex)) pt.size <- 1 else pt.size <- cex
# -------------------------
# y only, so create x index
# -------------------------
  if (is.null(y)) {
  kind <- "regular"  # kind only applicable when x and y both specified
  if (is.null(ylab)) y.lbl <- deparse(substitute(x)) else y.lbl <- ylab
  if (!time.reverse) y <- x
  else for (i in 1:nrows) y[i] <- x[nrows+1-i]
  if (is.null(time.start)) {
    if (is.null(xlab)) x.lbl <- "Index" else x.lbl <- xlab
    x <- 1:length(y)  # ordinal position of each value on x-axis
  }
  else   {  # time.start date specified
    if (is.null(xlab)) x.lbl <- "" else x.lbl <- xlab      
    date.seq <- seq(as.Date(time.start), by=time.by, length.out=nrows)
    x <- date.seq  # dates on x-axis
  }
  if (is.null(center.line)) {  # by default display center.line only if many runs
    m <- mean(y)
    n.change <- 0
    for (i in 1:(length(y)-1)) if ((y[i+1]>m) != (y[i]>m)) n.change <- n.change+1 
    if (n.change/(length(y)-1) < .15) center.line <- "off"
  }
  if (!is.null(time.start) && is.null(col.area)) col.area <- "slategray3"  # fill ts chart
  if (is.null(type)) 
    if (is.null(col.area) || col.area == "transparent") type <- "b" else type <- "l"
  if (type == "b" && is.null(cex))  # set point size      
    if (nrows < 50) pt.size <- 1.0 else pt.size <- 1 - 0.002*nrows
  if (is.null(col.fill)) col.fill <- "plum"
  if (is.null(center.line)) center.line <- "median"  # default
}
# ----------------------------------------------------------
# x and y specified
# ----------------------------------------------------------
else {
  if (is.null(col.fill)) col.fill <- "transparent"
  if (is.null(col.area)) col.area <- "transparent"
  if (!is.factor(x)) {
    if (is.null(type)) {  # if x is sorted with equal intervals, plot a line chart
      if (sum(is.na(x)) > 0) equal.int <- FALSE  # missing data in x present
      else {
        diff.x <- diff(x)
        for (i in 2:(length(x)-1)) 
          if (diff.x[i-1] != diff.x[i]) equal.int <- FALSE else equal.int <- TRUE
        rm(diff.x)
      }
      if (!is.unsorted(x) && equal.int  && sum(is.na(y))==0)  # also no y missing 
        type <- "l" else type <- "p"
    }
  if (kind == "default")  # set default
    if ( length(x)>10 && length(y)>10 && length(unique(x))<10 && length(unique(y))<10 )
      kind <- "bubble.freq"
    else kind <- "regular"
  }
  else {  # x is a factor
    type <- "p"
    kind <- "xcat"
  }
  if ((kind == "bubble.freq") || (kind == "sunflower.freq")) {  
    if (is.null(x.start)) x.start=min(x)  # x.start and x.min for Likert bubble plot
    if (is.null(x.end)) x.end=max(x)
  }
  if (xy.ticks) {  # assign axes labels with variable names as default
    if (is.null(xlab)) x.lbl <- deparse(substitute(x)) else x.lbl <- xlab
    if (is.null(ylab)) y.lbl <- deparse(substitute(y)) else y.lbl <- ylab
  }
  else {
    x.lbl <- ""
    y.lbl <- ""
  }
}
digits.d <- max.dd(y) + 1
# -------------------------
# Plot
# -------------------------
# plot setup
if (kind == "regular") {
  plot(x, y, type="n", axes=FALSE, xlab=x.lbl, ylab=y.lbl, ...)
  if (xy.ticks){
    if (is.null(time.start)) axis(1, ...)
      else axis.Date(1, at=seq(min(date.seq), max(date.seq), length.out=20), ...)
    axis(2, ...) 
  }
}
else if (kind == "xcat") {
  plot.default(y ~ x, xlim=c(.5,nlevels(x)+.5), type="n", axes=FALSE, xlab=x.lbl, ylab=y.lbl)
  axis(2)
  axis(1,labels=levels(x), at=1:nlevels(x))
}
else { # bubble or sunflower plot, requires integer values of x and y
  mytbl <- table(x, y)  # get the counts
  x.lo <- x.start-.5
  x.hi <- x.end+.5
  # melt the table to a dataframe
  k <- 0
  xx <- integer(length=0)
  yy <- integer(length=0)
  count <- integer(length=0)
  for (i in 1:nrow(mytbl)) {
    for (j in 1:ncol(mytbl)) {
      k <- k + 1
      count[k] <- mytbl[i,j]
      xx[k] <- as.integer(rownames(mytbl)[i])
      yy[k] <- as.integer(colnames(mytbl)[j])
    }
  }
  cords <- data.frame(xx, yy, count)
  if (kind == "bubble.freq")   # bubble plot, convert to factor if x is numeric
    plot(as.factor(x),y, type="n", xlab=x.lbl, ylab=y.lbl, xlim=c(x.lo,x.hi), ylim=c(x.lo,x.hi))
}
# colored plotting area
usr <- par("usr")
rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border=col.box)
# grid lines
vx <- pretty(c(usr[1],usr[2]))
vy <- pretty(c(usr[3],usr[4]))
abline(v=seq(vx[1],vx[length(vx)],vx[2]-vx[1]), col=col.grid, lwd=.5)
abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
# fill area under curve
if (type != "p") col.border <- col.line else col.border <- "transparent"
  if (!is.null(col.area)) 
    polygon(c(x[1],x,x[length(x)]), c(min(y),y,min(y)), col=col.area, border=col.border)
# the plot
if (kind == "regular") {  # plot lines and/or points
  if (type == "l" | type == "b") {
    lines(as.numeric(x),y, col=col.line, ...)
  }
  if (type == "p" | type == "b") {
    points(x,y, col=col.point, pch=point.type, bg=col.fill, cex=pt.size, ...)
    if (ellipse && check.car) {
      dataEllipse(x, y, col=col.ellipse, levels=.95, lwd=1.5, fill=fill.ellipse, 
        fill.alpha=.06, center.cex=0, segments=100, plot.points=FALSE)
    }
  }
}
else if (kind == "xcat") {
  for (i in (1:nlevels(x))) {
    abline(h=mean(y[x==levels(x)[i]]), col="gray80")
    points(rep(i,length(y[x==levels(x)[i]])), y[x==levels(x)[i]], col="darkblue")
    points(i, mean(y[x==levels(x)[i]]), pch=23, bg="steelblue")
  }
}
else if (kind == "bubble.freq") {
  symbols(cords$xx, cords$yy, circles=cords$count, bg=col.bubble, inches=size, add=TRUE, ...)
  zeros <- cords[cords$count==0, ] # 0 plots to a single pixel, so remove
  points(zeros$xx, zeros$yy, col=col.bg, bg=col.bg, pch=21, cex=.5)
  if (ellipse && check.car) {
    dataEllipse(x, y, col=col.ellipse, levels=.95, lwd=1.5, fill=fill.ellipse, 
      fill.alpha=.06, center.cex=0, segments=100, plot.points=FALSE)
  }
}
else if (kind == "sunflower.freq") {
  sunflowerplot(cords$xx, cords$yy, number=cords$count, 
    seg.col=col.flower, col=col.flower,
    xlab=x.lbl, ylab=y.lbl, xlim=c(x.lo,x.hi), ylim=c(x.lo,x.hi))
}
if (fit.line != "none") {  # fit line option
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    if (fit.line == "lowess") lines(lowess(x[ok], y[ok]), col=col.fit.line)
    if (fit.line == "ls") {
      if(!is.factor(x)) {
        model <- lm(y[ok] ~ x[ok])
        abline(model$coef, col=col.fit.line)
      }
      else cat("\nLeast squares line not permitted for a factor.\n")
    }
}
# plot center line, analyze runs
if (!is.null(center.line) && center.line != "off") {
  if (center.line == "mean") {
    m.y <- mean(y)
    lbl <- "mean"
    lbl.cat <- "Mean:"
  }
  else if (center.line == "median") {
    m.y <- median(y)
    lbl <- "medn"
    lbl.cat <- "Median:"
  }
  abline(h=m.y, col="gray50", lty="dashed")
  mtext(lbl, side=4, cex=.9, col="gray50", las=2, at=m.y, line=0.1)
  if (text.out) {
    dashes(20); cat(lbl.cat, round(m.y,digits.d), "\n"); dashes(20)
    dashes(12); cat("Run Analysis\n"); for (i in 1:12) cat("-")
    run <- integer(length=200)  # length of ith run in run[i]
    n.runs <- 1  # total number of runs
    run[n.runs] <- 1
    line.out <- "    1"
    cat("\n")
    for (i in 2:length(y)) {
      if (y[i] != m.y) {  # throw out values that equal m.y
        if (sign(y[i]-m.y) != sign(y[i-1]-m.y)) {  # new run
          if (n.runs < 10) buf <- "  " else buf <- " "
          cat("size=", run[n.runs], "  Run", buf, n.runs, ":", line.out, "\n", sep="")
          line.out <- ""
          n.runs <- n.runs + 1
          run[n.runs] <- 0
        }
        run[n.runs] <- run[n.runs] + 1
        if (i < 10) buf <- "  " else buf <- " "
        line.out <- paste(line.out, buf, i)
      }
    }
    cat("size=", run[n.runs], "  Run", buf, n.runs, ":", line.out, "\n", sep="")
    eq.ctr <- which(y==m.y)
    cat("\nTotal number of runs:", n.runs, "\n")
    txt <- "Total number of values that do not equal the "
    cat(txt, lbl.cat, " ", length(y)-length(eq.ctr), "\n", sep="")
    if (length(eq.ctr) != 0) {
      cat("\nValues ignored that equal the", lbl.cat, eq.ctr, "\n")
      cat("Total number of values ignored:", length(eq.ctr), "\n")
    }
    else 
      cat("Total number of values ignored that equal the", lbl.cat, length(eq.ctr), "\n")
  }
}
# -------------------------
# correlation analysis
# -------------------------
if (text.out && !is.null(y)  &&  !is.factor(x)  &&  type == "p") {
  n.pair <- sum(!is.na(x - y))  # number of points after pairwise deletion
  n.del <- sum(is.na(x - y))  # number of pairwise deleted observations
  ct <- cor.test(x,y)
  cat("\n")
  dashes(55)
  cat("Correlation Analysis for Variables", ct$data.name, "\n")
  dashes(55);  cat("\n")
  cat("Number of paired values with neither missing:  n =", n.pair, "\n")
  cat("Number of observations (rows of data) deleted: n =", n.del, "\n\n")
  cat("Sample Estimate: r =", round(ct$estimate,3), "\n\n")
  cat("Hypothesis Test that Population Correlation is Zero", "\n")
  cat("  t-value: ", round(ct$statistic,4), ",  df: ", ct$parameter, sep="") 
  cat(",  p-value: ", round(ct$p.value,4), sep="", "\n\n")
  cat("95% Confidence Interval of Population Correlation", "\n")
  cat("  Lower Bound:", round(ct$conf.int,3)[1])
  cat("     Upper Bound:", round(ct$conf.int,3)[2], "\n")
  dashes(55)
}
  cat("\n")
}
color.show <- 
function(file="colors.pdf", color=NULL) {
  pdf(file=file)
  par(mfrow=c(5,6), mgp=c(0,1,0))
  if (is.null(color))
    clr <- colors()
  else
    clr <- grep(color, colors(), value = TRUE)
  h <- 1
  for (i in 1:length(clr))
    barplot(h, col=clr[i], main=clr[i], sub=toString(col2rgb(clr[i])), 
       cex.main=.95, axes=FALSE, border=NA)
  if (getwd() =="/")
    workdir <- "top level of your file system"
  else
    workdir <- getwd()
  cat("pdf file written in current working directory.\n")
  cat("       ", file, "at:  ", workdir, "\n")
  par(mfrow=c(1,1), mgp=c(3,1,0))
  dev.off()
}
describe.character <-
function(x, lbl=NULL, ...)  {
  if (is.null(lbl)) lbl <- deparse(substitute(x))
  if (nlevels(factor(x)) < length(x)) { 
    describe(factor(x), lbl=lbl, ...)
  }
  else cat("\n Appears to contain unique Names or IDs", "\n")
}
describe.data.frame <-
function(x, ...)  {
  for (i in 1:ncol(x)) describe(x[,i], lbl=names(x)[i], ...)
}
describe.default <-
function(x, ...)  {
  cat("The variable to be analyzed must be a numeric or a factor, or have\n")
  cat("character values that can be converted to a factor, or logical values\n")
  cat("that can be converted to numerical 0 and 1.\n")
}
describe.factor <-
function(x, lbl=NULL, ...)  {
  dashes <- function(ndash) for (i in 1:(ndash)) cat("-")
  cat("\n")
  if (is.null(lbl)) lbl <- deparse(substitute(x))
  dashes(nchar(lbl)); cat("\n")
  cat(lbl, "\n")
  dashes(nchar(lbl)); cat("\n")
  if (nlevels(x) < length(x)) { 
    x.table <- table(x, ...); 
    names(x.table)[0]=" ";  # get rid of variable label
    print(x.table) 
  }
  else cat("\n Appears to contain unique Names or IDs", "\n")
  cat("\n")
}
describe.formula <-
function (formula, data=mydata, ...) {
  dashes <- function(ndash) for (i in 1:(ndash)) cat("-")
  max.dd <- function(x) {
      n.dec <-function(x) {
        xc <- as.character(x)
        nchar(xc)
        ipos <- 0
        for (i in 1:nchar(xc)) if (substr(xc,i,i)==".") ipos <- i
        if (ipos > 0) n.dec <- nchar(xc)-ipos else n.dec <- 0
        return(n.dec)
      }
       
      max.dd <- 0
      for (i in 1:length(x))
        if (!is.na(x[i])) if (n.dec(x[i]) > max.dd ) max.dd <- n.dec(x[i])   
      return(max.dd)
  }
  if ((length(formula) != 3) || (length(attr(terms(formula[-2]),"term.labels")) !=1)) 
  stop("'Formula' missing or incorrect.")
  m <- match.call(expand.dots = FALSE) 
  if (is.matrix(eval(m$data, parent.frame()))) m$data <- as.data.frame(data)
  m[[1L]] <- as.name("model.frame")
  if (length(m) == 3) set.digits <- as.numeric(m[[3L]]$digits) else set.digits <- NULL
  m$... <- NULL
  # mf is the relevant Y~X columns of data from dataframe, all Y missing are deleted
  mf <- eval(m, parent.frame())
  Ynm <- names(mf)[1]
  Xnm <- names(mf)[2]
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  if (!is.numeric(mf[[response]])) 
    stop("Response variable ", Ynm, " must be numeric")
  if (is.null(set.digits)) digits.d <- max.dd(mf[[response]])+1 else digits.d <- set.digits
  if (digits.d > 10  && is.null(set.digits)) {
    cat("\nThese data contain", digits.d, "significant digits.\n")
    cat("Consider specifying a smaller number to display with the  digits  parameter.\n")
    cat("Example for Variables Y and X:  describe2(Y ~ X, digits=3)\n\n")
  }
  g <- factor(mf[[-response]])   
  cg <- as.character(g)
  for (i in 1:length(cg)) if (cg[i] == "") cg[i] <- "Null"
  for (i in 1:length(cg)) if (cg[i] == " ") cg[i] <- "Blank"
  rm(g)
  g <- factor(cg)
  gu <- unique(g)
  max.char <- 0
  for (i in 1:nlevels(gu)) {
    if (nchar(levels(gu)[i]) > max.char) max.char <- nchar(levels(gu)[i])
  }
  DATA <- split(mf[[response]], g)
  attach(DATA, warn.conflicts=FALSE)
  # width of mean field
  w.m <- 0
  for (i in 1:length(gu)) {
    m <- round(mean(DATA[[i]]), digits.d)
    n.char <- nchar(format(sprintf("%.*f", digits.d, m)))
    if (n.char > w.m) w.m <- n.char
  }
  w.m <- w.m + 2    
 
  cat("\n")
  cat("------------------\n")
  cat(DNAME, "\n")
  cat("------------------\n")
  if (max.char <= nchar("Level")) n.blank <- max.char else n.blank <- max.char-1
  if (w.m < 6) w.m <- 6
  for (i in 1:6) cat(" ")
  cat("non-missing"); for (i in 1:1) cat(" ")
  cat("   "); for (i in 1:1) cat(" ")
  cat("Mean"); for (i in 1:10) cat(" ")
  cat("SD"); for (i in 1:10) cat(" ")
  cat("Min"); for (i in 1:8) cat(" ")
  cat("Median"); for (i in 1:9) cat(" ")
  cat("Max");
  cat("\n")
  dashes(78+n.blank); cat("\n")
  for (i in 1:length(gu)) {
    lv <- levels(gu)[i]
    x <- DATA[[i]]
    n.miss <- sum(is.na(x))
    n <- sum(!is.na(x))
    m <- round(mean(x, na.rm=TRUE), digits.d)
    s <- round(sd(x, na.rm=TRUE), digits.d)
    mn <- round(min(x, na.rm=TRUE), digits.d)
    md <- round(median(x, na.rm=TRUE), digits.d)
    mx <- round(max(x, na.rm=TRUE), digits.d)
    p.lv <- format(lv, width=max.char+3)
    p.n <- format(sprintf("%i", n), width=5, justify="right")
    p.n.miss <- "    "
    p.m <- format(sprintf("%.*f", digits.d, m), width=w.m, justify="right")
    p.s <- format(sprintf("%.*f", digits.d, s), width=12, justify="right")
    p.mn <- format(sprintf("%.*f", digits.d, mn), width=12, justify="right")
    p.md <- format(sprintf("%.*f", digits.d, md), width=12, justify="right")
    p.mx <- format(sprintf("%.*f", digits.d, mx), width=12, justify="right")
    cat(p.lv, p.n, p.n.miss, p.m, p.s, p.mn, p.md, p.mx, "\n")    
  }
  dashes(78+n.blank); cat("\n")
  cat("\n")
}
describe.numeric <-
function(x, digits.d=NULL, lbl=NULL, ...) {
  dashes <- function(ndash) { for (i in 1:(ndash)) cat("-"); cat("\n") }
  max.dd <- function(x) {
      n.dec <-function(x) {
        xc <- as.character(x)
        nchar(xc)
        ipos <- 0
        for (i in 1:nchar(xc)) if (substr(xc,i,i)==".") ipos <- i
        if (ipos > 0) n.dec <- nchar(xc)-ipos else n.dec <- 0
        return(n.dec)
      }
       
      max.dd <- 0
      for (i in 1:length(x))
        if (!is.na(x[i])) if (n.dec(x[i]) > max.dd ) max.dd <- n.dec(x[i])   
      return(max.dd)
  }
  cat("\n")
  if (is.null(digits.d)) digits.d <- max.dd(x) + 1
  if (digits.d > 10) {
    cat("\nThese data contain", digits.d, "significant digits.\n")
    cat("Consider specifying a smaller number to display with the  digits  parameter.\n")
    cat("Example for Variables Y and X:  describe2(Y ~ X, digits=3)\n\n")
  }
  # width of mean field
  n.char <- nchar(format(sprintf("%.*f", digits.d, round(mean(x), digits.d))))
  w.m <- n.char + 2    
  if (is.null(lbl)) lbl <- deparse(substitute(x)) 
  dashes(nchar(lbl))
  cat(lbl, "\n")
  dashes(nchar(lbl))
  if (w.m < 6) w.m <- 6
  for (i in 1:4) cat(" ")
  cat("n"); for (i in 1:3) cat(" ")
  cat("Miss"); for (i in 1:3) cat(" ")
  cat("Mean"); for (i in 1:10) cat(" ")
  cat("SD"); for (i in 1:10) cat(" ")
  cat("Min"); for (i in 1:8) cat(" ")
  cat("Median"); for (i in 1:9) cat(" ")
  cat("Max");
  cat("\n")
  dashes(75)
  n.miss <- sum(is.na(x))
  n <- sum(!is.na(x))
  m <- round(mean(x, na.rm=TRUE), digits.d)
  s <- round(sd(x, na.rm=TRUE), digits.d)
  mn <- round(min(x, na.rm=TRUE), digits.d)
  md <- round(median(x, na.rm=TRUE), digits.d)
  mx <- round(max(x, na.rm=TRUE), digits.d)
  p.n <- format(sprintf("%i", n), width=5, justify="right")
  p.n.miss <- format(sprintf("%i", n.miss), width=5, justify="right")
  p.m <- format(sprintf("%.*f", digits.d, m), width=w.m, justify="right")
  p.s <- format(sprintf("%.*f", digits.d, s), width=12, justify="right")
  p.mn <- format(sprintf("%.*f", digits.d, mn), width=12, justify="right")
  p.md <- format(sprintf("%.*f", digits.d, md), width=12, justify="right")
  p.mx <- format(sprintf("%.*f", digits.d, mx), width=12, justify="right")
  cat(p.n, p.n.miss, p.m, p.s, p.mn, p.md, p.mx, "\n")
  dashes(75)
  cat("\n")
}
describe <-
function(x=NULL, ...)  {
  if (is.null(x)) {
    if (!exists("mydata")) 
      stop("Need to specify an existing data frame or data frame mydata must exist.")
    describe(mydata, ...) 
  }
  else UseMethod("describe")
	
}
full <-
function(x=NULL)  {
  dashes <- function(ndash, cc) { for (i in 1:(ndash)) cat(cc); cat("\n") }
  if (is.null(x))
    if (!exists("mydata")) 
      stop("Need to specify an existing data frame or data frame mydata must exist.")
      
  cat("\n")
  dashes(25,"-")
  cat(format(Sys.time(), "%a %b %d, %Y at %H:%M"), "\n")
  dashes(25,"-")
  cat("\n\n\n")
  dashes(29,"+")
  cat("Data Summary of Each Variable\n")
  dashes(29,"+")
  describe(x)
  cat("\n\n\n")
  dashes(37,"+")
  cat("Histogram for Each Numerical Variable\n")
  dashes(37,"+")
  color.hist(x)
  cat("\n\n\n")
  dashes(39,"+")
  cat("Bar Chart for Each Non-numeric Variable\n")
  dashes(39,"+")
  color.barchart(x)
}
help.me <- 
function(topic=NULL) {
help.more <-
function(fname, yline) {
h1 <- "Complete list of help.me topics, enter:  help.me()   or   help.me(\"help.to.pdf\")"
h2 <- paste("For more help on a function, enter ? in front of its name:  ?", fname, sep="")
lines(c(5,90), c(yline,yline), col="lightsteelblue")
text(0,yline-5, label=h1, adj=0)
text(0,yline-10, label=h2, adj=0)
}
# set up plot window
set.up.plot <- 
function() {
par(mar=c(.5,1,.5,.5), bg=rgb(255,253,250,max=255), fg=rgb(20,15,15,max=255))
plot.new()
plot.window(xlim=c(0,100), ylim=c(0,100))
}
col.line <- "lightsteelblue"
if (is.null(topic)) {
if (sys.nframe() == 1)  # not nested in a call from help.to.pdf
  cat("To obtain a printable pdf of all of the contents, enter:  help.me(\"help.to.pdf\")\n")
t0 <- "Topics for help.me"
fcsv <- bquote(paste(bold("help.me(\"data\")"), "  Create csv data file from Excel or other worksheet apps."))
fread <- bquote(paste(bold("help.me(\"read\")"), "  Read an external data file in csv format."))
fwrite <- bquote(paste(bold("help.me(\"write\")"), "  Write data to an external data file in csv format."))
flib <- bquote(paste(bold("help.me(\"library\")"), "  Access libraries of functions called packages."))
fprob <- bquote(paste(bold("help.me(\"prob\")"), "  Probabilities for normal and t-distributions."))
frand <- bquote(paste(bold("help.me(\"random\")"), "  Generate random numbers."))
fsamp <- bquote(paste(bold("help.me(\"sample\")"), "  Generate random samples."))
fhist <- bquote(paste(bold("help.me(\"histogram\")"), "  Histogram of a numeric variable."))
fbar <- bquote(paste(bold("help.me(\"bar.chart\")"), "  Bar chart of a categorical variable."))
fplot <- bquote(paste(bold("help.me(\"plot\")"), "  Run chart, scatterplot, graph of a function."))
fstat <- bquote(paste(bold("help.me(\"stats\")"), "  Summary statistics."))
fone <- bquote(paste(bold("help.me(\"one.sample\")"), "  Analysis of a single sample of data."))
fmean <- bquote(paste(bold("help.me(\"two.samples\")"), "  Compare groups by their mean difference."))
faov <- bquote(paste(bold("help.me(\"many.samples\")"), "  Compare mean differences for many groups."))
fprop <- bquote(paste(bold("help.me(\"props\")"), "  Compare proportions across two or more groups."))
fpwr <- bquote(paste(bold("help.me(\"power\")"), "  Power analysis for the t-test."))
fcor <- bquote(paste(bold("help.me(\"cor\")"), "  Correlation analysis."))
freg <- bquote(paste(bold("help.me(\"reg\")"), "  Regression analysis."))
set.up.plot()
pos1 <- 93; pos2 <- 73; pos3 <- 58; pos4 <- 42
text(50,100, label=t0, font=4)
text(0,pos1, label=fcsv, adj=0)
text(0,pos1-4, label=fread, adj=0)
text(0,pos1-8, label=fwrite, adj=0)
text(0,pos1-12, label=flib, adj=0)
lines(c(5,90), c(77,77), col=col.line)
text(0,pos2, label=fprob, adj=0)
text(0,pos2-4, label=frand, adj=0)
text(0,pos2-8, label=fsamp, adj=0)
lines(c(5,90), c(62,62), col=col.line)
text(0,pos3, label=fhist, adj=0)
text(0,pos3-4, label=fbar, adj=0)
text(0,pos3-8, label=fplot, adj=0)
lines(c(5,90), c(46,46), col=col.line)
text(0,pos4, label=fstat, adj=0)
text(0,pos4-4, label=fone, adj=0)
text(0,pos4-8, label=fmean, adj=0)
text(0,pos4-12, label=faov, adj=0)
text(0,pos4-16, label=fprop, adj=0)
text(0,pos4-20, label=fpwr, adj=0)
text(0,pos4-24, label=fcor, adj=0)
text(0,pos4-28, label=freg, adj=0)
}
else if (topic == "data") {
t0 <- "Data Files"
t1 <-
"R can read data files in the csv, or \"comma separated values\", format, text 
files with commas separating adjacent values in each row. Usually the variable 
names are in the first row and each remaining row contains the data for one
observation, such as one person or one company, etc. Each column of the 
worksheet contains the data for the corresponding variable.
One way to create a csv data file is with MS Excel or other worksheet application. 
All numeric data should be displayed in the General format, so that the only 
non-digit character for each numeric data value is a decimal point. The General 
format removes all dollar signs and commas, for example, leaving only the pure 
number, stripped of any extra characters, which R will not properly read by default 
as a numeric data value.
To create the csv file from a worksheet, under the File option, do a Save As and 
choose the csv format.
Next, read the csv data file into R, [see help.me(\"read\")]. However, using a 
worksheet such as Excel and R are complementary procedures.  R can do 
extensive data transformations, such as sorting and much else, but so can Excel, 
and often more directly, without the need for programming.  Given the simplicity 
of transferring data from Excel to R, it is often useful to move back and forth 
between the two systems on a regular basis."
set.up.plot()
text(50,100, label=t0, font=4)
text(0,59, label=t1, adj=0)
help.more("rad", 20)
}
else if (topic == "read") {
t0 <- "Read Data into R and Prepare for Analysis"
f1 <- bquote(paste(bold("rad"), "  Read a csv data file into an R data frame called mydata, and more."))
t1 <-
"Browse for a csv data file available on the local computer system.
    > rad()
Or, specify the file to be read. The file can be a path name to a data file 
available on the local computer system, or to a file on the web.
    > rad(\"http://web.pdx.edu/~gerbing/data/twogroup.csv\")
The function, rad, which stands for Read, Attach and Display, sequentially 
invokes four different, standard R functions: read.csv, attach, head and tail.
To see how to create a csv data file, enter: help.me(\"create.data.file\")
The name of the entire rectangular matrix of data, called a data frame in R, is 
specifically named \"mydata\" within R when created by the function rad. Make 
sure to distinguish between the name of the data frame, mydata, and the names 
of the individual variables, columns, contained in the data frame."
set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
lines(c(5,90), c(89,89), col=col.line)
text(0,60, label=t1, adj=0)
help.more("rad", 30)
}
else if (topic == "write") {
t0 <- "Write Contents of Data Frame mydata into a csv Text File"
f1 <- bquote(paste(bold("out"), "  Write a csv data file into an R data frame called mydata, and more."))
f2 <- bquote(paste(bold("write.table"), "  General R statement to write the contents of an R object to a file."))
t1 <-
"The name of the entire rectangular matrix of data, called a data frame in R, can 
be named \"mydata\" within R.  This is also the name of the data frame given 
by the complementary function rad that reads the data.
Here is how to write the contents of mydata to a csv data file with the name of 
mydata.csv.
    > out()
Or, explicitly specify the file name.
    > out(\"mybestdata.csv\")
The function write.table is quite general, with many options.  For more information, 
enter ?write.table"
set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
lines(c(5,90), c(85,85), col=col.line)
text(0,62, label=t1, adj=0)
help.more("rad", 38)
}
else if (topic == "library") {
t0 <- "Contributed Packages"
f1 <- bquote(paste(bold("install.packages"), "  Download a contributed package."))
f2 <- bquote(paste(bold("library"), "  Load an installed package from the library into R for access."))
f3 <- bquote(paste(bold(update.packages), "  Update contributed packages to current versions."))
t1 <-
"The example here is for the contributed package lessR. Install one time 
only for a specific computer, with quotes.
    > install.packages(\"lessR\")
Each time the R application is started, including after the install, load the 
package from the library, without using quotes.
    > library(lessR)
To see the description of the package and a list of its functions,
    > library(help=lessR)
To access new versions of all installed packages, 
    > update.packages()
All of R works with functions contained in specific packages. The distinction is 
that some of those packages are included with the default installation of R, and 
are pre-loaded each time the application is run. Examples are the stat package 
and the graphic package. To see a list of all installed packages in the library, 
    > library()"
set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
text(0,86, label=f3, adj=0)
lines(c(5,90), c(80,80), col=col.line)
text(0,46, label=t1, adj=0)
help.more("install.packages", 12)
}
else if (topic == "prob") {
t0 <- "Probabilities for Normal and t-distributions"
f1 <- bquote(paste(bold("pnorm"), "  Probability for a normal distribution related to a specified value, Y."))
f2 <- bquote(paste(bold("pt"), "  Probability for a t-distribution related to a specified t-value."))
f3 <- bquote(paste(bold("qnorm"), "  Quantile for a normal distribution."))
f4 <- bquote(paste(bold("qt"), "  Quantile for a t-distribution."))
t1 <-
"By default, pnorm or pt provides the corresponding probability of obtaining a 
randomly sampled value, Y or t, in the lower tail of the specified distribution: 
the probability of a value smaller than or equal to the specified value.  This is 
usually the desirable result for a negative value of Y or t.  For a positive value, 
obtain the corresponding upper-tail value by adding the option: lower.tail=FALSE.
Upper tail probability for t=1.627, df=24:  > pt(1.627, df=24, lower.tail=FALSE)
Two-tailed p-value for  t=1.627, df=24:     > 2*pt(1.627, df=24, lower.tail=FALSE)
Lower tail prob for Y=94, mu=100, sigma=15: > pnorm(94, mean=100, sd=15)
The quantile functions are the inverse of the probability functions. For a given 
probability or area under the curve, the corresponding quantile is the 
corresponding value of the distribution, Y or t.
t-value that cuts off the top 2.5% of the t-distribution for df=24.
    > qt(0.025, df=24, lower.tail=FALSE)
Value from the standard normal distribution that cuts off the top 2.5% of the 
distribution.  Without specifying mu and sigma, the respective defaults are 0 and 1.
    > qnorm(0.025, lower.tail=FALSE)"
set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
text(0,86, label=f3, adj=0)
text(0,82, label=f4, adj=0)
lines(c(5,90), c(78,78), col=col.line)
text(0,44, label=t1, adj=0)
help.more("pt", 9)
}
else if (topic == "random") {
t0 <- "Normal and Binomial Random Values"
f1 <- bquote(paste(bold("rnorm"), "  Generate randomly sampled values from a normal distribution."))
f2 <- bquote(paste(bold("rbinom"), "  Generate randomly sampled values from a binomial distribution."))
t1 <-
"R can generate simulated sampling from many different distributions, including 
the normal and the binomial.
This example generates 50 randomly sampled values from the standard normal 
distribution, with a default mu of 0 and sigma of 1.
    > rnorm(50)
This generated data can be stored for further analysis.  Here, generate 100 
values from a normal distribution with a mean of 50 and a standard deviation 
of 10, store in the vector Y, and then display the resulting histogram.
    > Y <- rnorm(100, mean=50, sd=10)
    > hist(Y)
The binomial distribution describes the process of a binary outcome over 
many different trials, such as flipping a coin.  In this example, flip a fair 
coin 20 times with a probability of a Head at 0.5.  Then repeat this set of 20 
flips 10 times to get the number of Heads obtained on each set of 20 flips.
    > rbinom(10, 20, .5)"
set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
lines(c(5,90), c(85,85), col=col.line)
text(0,51, label=t1, adj=0)
help.more("rnorm", 17)
}
else if (topic == "sample") {
t0 <- "Generate Random Samples"
f1 <- bquote(paste(bold("sample"), "  Generate random samples."))
t1 <-
"To use the sample function, first specify the population from which to randomly 
sample. The population can be defined from the values of a specified variable, or 
the values can be directly listed. Next use the size option to specify the number 
of elements to sample. By default, sampling is done without replacement, each 
value in the population can only appear once in the resulting sample. To allow 
sampling with replacement, invoke the replace=TRUE option.
The following randomly samples 5 values of the variable Y without replacement.
    > sample(Y, size=5)
If the size of the resulting list of sample values is larger than the available 
number of values from which to sample, then sampling must be done with 
replacement.
    > sample(c(\"Group1\",\"Group2\"), size=10, replace=TRUE)
Here, 10 numbers are randomly sampled from the first 100 integers, without 
replacement.
    > sample(1:100, 10)"
set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
lines(c(5,90), c(88,88), col=col.line)
text(0,57, label=t1, adj=0)
help.more("sample", 25)
}
else if (topic == "histogram") {
t0 <- "Histogram"
f1 <- bquote(paste(bold("hist"), "  Histogram function."))
f2 <- bquote(paste(bold("color.hist"), "  An enhanced version of hist."))
t1 <-
"The generic variable in the examples below is Y. Replace with the actual name of 
the variable in a specific analysis.
An enhanced histogram, including color by default, is produced from
    > color.hist(Y)
In this histogram, specify a title, labels for the x and y axes, and a color.
    > color.hist(Y, main=\"My Title\", xlab=\"Y (mm)\", ylab=\"Counts\", col=\"seagreen3\")
Here, manually specify bins, starting at 60, going to 100, with bin width of 10.
    > color.hist(Y, breaks=seq(60,100,10))"
set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
lines(c(5,90), c(86,86), col=col.line)
text(0,66, label=t1, adj=0)
help.more("hist", 46)
}
else if (topic == "bar.chart") {
t0 <- "bar.chart"
f1 <- bquote(paste(bold("table"), "  Count the values of one or more categorical variables."))
f2 <- bquote(paste(bold("color.barchart"), "  Produce a bar chart."))
f3 <- bquote(paste(bold("pareto.chart"), "  Produce a Pareto chart."))
t1 <-
"The generic variable in the examples below is generally a categorical variable Y, 
called a factor. Replace with the actual name of the variable in a specific analysis. 
Default bar chart including color.
    > color.barchart(Y)
The table function obtains the counts of each value.
    > Ycount <- table(Y)
    > Ycount
The pareto.chart function is part of the external library called gcc. To view an 
explanation of dealing with libraries, enter help.me(\"libraries\"). Default input 
Pareto chart follows, which works from the counts. 
    > library(gcc)
    > pareto.chart(Ycount)
"
set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
text(0,86, label=f3, adj=0)
lines(c(5,90), c(81,81), col=col.line)
text(0,50, label=t1, adj=0)
help.more("color.barchart", 22)
}
else if (topic == "plot") {
t0 <- "Plot"
f1 <- bquote(paste(bold("plot"), "  Plot values of one or two variables."))
f2 <- bquote(paste(bold("color.plot"), "  Enhances some of the capabilities of the plot function."))
f3 <- bquote(paste(bold("color.density"), "  Plot normal and general densities over the histogram."))
t1 <-
"The function, plot, can produce a wide range of plots. The function, color.plot, 
provides easier access to color enhancement. Either function plots run charts, 
scatter plots and the values of a function. The function, color.density, estimates the 
smooth normal curve or general density function from the data, and then displays 
over the histogram.
This example is the default scatterplot, in color, for variables named X and Y.
    > color.plot(X,Y)
Here a run chart is generated, in color, for a variable named Y. If the data do not 
have a pronounced trend, an added centerline is automatically provided.
    > color.plot(Y)
These graphic functions can access a wide range of graphics parameters, such 
as the size of the margins, the annotations, the line width, etc. These additional 
options are explained in the help files for functions par, title, points and lines. 
This scatter plot has dark red points and the #19 point character, pch, which 
is a filled circle.
    > color.plot(X, Y, col.point=\"darkred\", pch=19)
The help for the function, points, shows the different options for pch."
set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
text(0,86, label=f3, adj=0)
lines(c(5,88), c(81,81), col=col.line)
text(0,45, label=t1, adj=0)
help.more("color.plot", 9)
}
else if (topic == "stats") {
t0 <- "Descriptive Statistics"
f1 <- bquote(paste(bold("describe"), "  summarize all variables in specified data table"))
f2 <- bquote(paste(bold("length"), "  sample size, i.e., count"))
f3 <- bquote(paste(bold("mean"), "  mean, including trimmed mean with trim option"))
f4 <- bquote(paste(bold("sd"), "  standard deviation"))
f5 <- bquote(paste(bold("median"), "  median"))
f6 <- bquote(paste(bold("min"), "  minimum", "       ", bold("max"), "  maximum"))
f7 <- bquote(paste(bold("range"), "  range"))
f8 <- bquote(paste(bold("quantile"), "  min, 1st quartile, median, 3rd quartile, max"))
f9 <- bquote(paste(bold("scale"), "  standardize"))
t1 <-
"Each of these functions, except for describe, applies to the analysis of single variable, 
such as for the mean of variable called Y.
    > mean(Y)
Or summarize all numerical and non-numerical variables in the data frame.
    > describe(mydata)
Or, can apply the describe function to a single variable, Y, with an optional grouping
variable, X, to summarize the numerical variable at each level of the other variable.
    > describe(Y ~ X)
R provides many summary statistics. Enter the following to see the entire list, 
    > library(help=\"stats\")."
set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
text(0,86, label=f3, adj=0)
text(0,82, label=f4, adj=0)
text(0,78, label=f5, adj=0)
text(0,74, label=f6, adj=0)
text(0,70, label=f7, adj=0)
text(0,66, label=f8, adj=0)
text(0,62, label=f9, adj=0)
lines(c(5,90), c(57,57), col=col.line)
text(0,33, label=t1, adj=0)
help.more("describe", 9)
}
else if (topic == "one.sample") {
t0 <- "Inference for a Single Variable"
f1 <- bquote(paste(bold("t.test"), "  Inference for a mean."))
f2 <- bquote(paste(bold("binom.test"), "  Inference for a proportion from exact binomial probability."))
f3 <- bquote(paste(bold("prop.test"), "  Inference for a proportion from approximate normal probability."))
t1 <-
"These inference tests analyze the mean of a numeric variable or the proportion 
of a value of a categorical variable. These tests provide a hypothesis test 
and a confidence interval.
This example is for a variable named Y and a null hypothesis of mu=100.
    > t.test(Y, mu=100)
These examples are for testing for a fair coin after getting 53 out of 100 Heads.
    > binom.test(53,100, p=.5)
    > prop.test(53,100, p=.5)
The prop.test function can be specified with or without the Yate's correction for 
continuity factor. The default is to include the correction."
set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
text(0,86, label=f3, adj=0)
lines(c(5,90), c(82,82), col=col.line)
text(0,58, label=t1, adj=0)
help.more("t.test", 33)
}
else if (topic == "two.samples") {
t0 <- "Compare Two Group Means"
f1 <- bquote(paste(bold("t.test"), "  The standard R function to compare two group means with a t-test."))
f2 <- bquote(paste(bold("smd.t.test"), "  An enhanced version of t.test to compare two group means."))
t1 <-
"When responses to a variable are organized into two or more groups, compare
the group means with a t-test.  For example, suppose the response variable is 
Salary and the grouping variable is Gender, with two values, M and F.
Here the numerical response variable is named Y and the grouping variable, 
also called a factor, is named X, which must have exactly two values.
    >  smd.t.test(Y ~ X)
When the tilde, ~, expresses the relationship between two or more variables, 
R refers to this expression as a formula, read as: Y is described by X.
To do a separate analysis of Y for each group, use the [...] notation to define a 
vector that contains just the Y responses for one group.  In this example, one 
of the two values of X is Group1.
    > Y1 <- Y[X==\"Group1\"]
    > hist(Y1)
Create a new variable for each group.
Sometimes the data for a t-test are arranged so that the responses, Y, for 
each group already are in separate columns called vectors. Here calculate 
the t-test directly from two vectors called Y1 and Y2.
    > smd.t.test(Y1, Y2)"
set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
lines(c(5,90), c(86,86), col=col.line)
text(0,50, label=t1, adj=0)
help.more("smd.t.test", 13)
}
else if (topic == "many.samples") {
t0 <- "Compare Means of Two or More Groups"
f1 <- bquote(paste(bold("aov"), "  Analysis of variance to compare two or more group means."))
f2 <- bquote(paste(bold("TukeyHSD"), "  Tukey Honest Significant Differences post-hoc comparison of means."))
t1 <-
"When responses to a variable are organized into exactly two groups, either the 
t-test or analysis of variance, ANOVA, can compare the group means. With more 
than two groups, ANOVA is required. The function aov works only in formula mode.
Here the numerical response variable is named Y and the grouping variable, or 
factor, is X, which may have more than two discrete values.
    > aov(Y ~ X)
This is called one-way ANOVA because there is only a single factor, X.
If the ANOVA with more than two levels is significant, then a post-hoc examination 
of the mean differences with a controlled error rate will help uncover where the 
differences occurred. The function used here is based on Tukey's HSD procedure.  
Both tabular and plotted output are obtained.
    > a <- aov(Y ~ X)
    > aTukey <- TukeyHSD(a, \"X\")
    > aTukey
    > plot(aTukey)
    > abline(v=0, lty=\"dotted\")"
set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
lines(c(5,90), c(85,85), col=col.line)
text(0,55, label=t1, adj=0)
help.more("aov", 25)
}
else if (topic == "props") {
t0 <- "Analysis of Cross-Tabulation or Pivot Tables"
f1 <- bquote(paste(bold("table"), "  Construct the cross-tabulation table from joint frequencies."))
f2 <- bquote(paste(bold("addmargins"), "  Add row and column margins to the cross-tabulation table."))
f3 <- bquote(paste(bold("chisq.test"),  "  ", chi^2, " (chi-square) test from the null hypothesis of no relation."))
t1 <-
"Calculate the cross-tabulation table from the categorical variables, or 
factors, with the table function, applied here to factors X and Y.
    > table(X,Y)
To store the counts for later analysis, assign the output of the table 
function to an object, here called mytable. Use the addmargins function 
to display the frequencies, as well as the marginal frequencies for the 
rows and columns, of the cross-tabulation table.
    > mytable <- table(X,Y)
    > addmargins(mytable)
Obtain the chi-square statistic and associated p-value calculated with the 
assumption of no relation between the variables, that is, equal proportions.
    > chisq.test(mytable)
"
set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
text(0,86, label=f3, adj=0)
lines(c(5,90), c(81,81), col=col.line)
text(0,55, label=t1, adj=0)
help.more("chisq.test", 31)
}
else if (topic == "power") {
t0 <- "Power"
f1 <- bquote(paste(bold("power.t.test"), "  The standard R function for the power analysis of the t-test."))
f2 <- bquote(paste(bold("smd.t.test"), "  Enhanced power function, also provides power curve."))
t1 <-
"The function, powercurve.t.test, uses the standard R function, power.t.test, to 
calculate a range of power values and automatically provide a power curve. 
To accomplish this analysis otherwise requires setting up the range of alternative 
mean or mean difference values, usually by trial and error, invoking power.t.test, 
saving the results, and then invoking the plot function, including the labeling 
of each axis. Then to analyze related results such as power at a different
sample size, the power.t.test function must be run several more times. 
The enhanced function, powercurve.t.test, does all of this automatically for one 
or two sample t-tests, and also plots the power curve in color. This example is 
for the default power curve plotted in color for a sample size of 20 in each group 
and a within-group or pooled standard deviation of 5.
    > powercurve.t.test(n=20, s=5)
Related analysis is also provided."
set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
lines(c(5,90), c(85,85), col=col.line)
text(0,58, label=t1, adj=0)
help.more("powercurve.t.test", 30)
}
else if (topic == "cor") {
t0 <- "Correlation and Related Graphics"
f1 <- bquote(paste(bold("cor"), "  Correlation coefficient(s) between two or more variables."))
f2 <- bquote(paste(bold("cor.test"), "  Correlation coefficient with statistical inference."))
f3 <- bquote(paste(bold("plot"), "  Graphics, generate a scatterplot for two variables."))
f4 <- bquote(paste(bold("color.plot"), "  Graphics, enhances some of the capabilities of the plot function."))
f5 <- bquote(paste(bold("pairs"), "  Generate a matrix of all possible scatter plots of many variables."))
t1 <-
"The function, cor, can compute correlations for a single pair of variables, such as 
cor(X,Y), or for all numeric variables in the data frame, such as cor(mydata), 
for a data frame named mydata. The cor.test function applies only to a single pair 
of variables, and provides output similar to the t.test and related functions.
The graphic functions, color.plot and plot, display a scatterplot for two variables. 
The graphic function, pairs, generates a scatterplot matrix for all numeric 
variables in an entire data frame, or a subset of variables from the data frame.
This example is for the correlation coefficient, inference and scatterplot for two 
numerical variables, X and Y.
    > cor.test(X,Y)
    > color.plot(X,Y)
This example of functions cor and pairs applies to Variables Y, X1, X2 and X3 in 
the data frame called mydata.
    > cor(subset(mydata, select=c(Y,X1:X3)))
    > pairs(subset(mydata, select=c(Y,X1:X3)))"
set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
text(0,86, label=f3, adj=0)
text(0,82, label=f4, adj=0)
text(0,78, label=f5, adj=0)
lines(c(5,90), c(73,73), col=col.line)
text(0,41, label=t1, adj=0)
help.more("color.plot", 9)
}
else if (topic == "reg") {
t0 <- "Linear Models and Regression"
f1 <- bquote(paste(bold("reg"), "  Regression analysis."))
t1 <-
"The function reg preforms a regression analysis and stores the results in an R 
object called model, which is available for further analysis. This example is for 
a multiple regression with a response variable named Y and predictor variables X1 
and X2.
    > reg(Y ~ X1 + X2)
The function uses the standard R specification for the model's defining formula, 
of which the details are accessed by entering:  ?formula
The function reg consolidates the following three standard R function calls into 
a single statement, as applied here to the previous example.
    > model <- lm(Y ~ X1 + X2)
    > summary(model)
    > confint(model)
    > anova(model)
The output of reg also includes output for the following functions: resid, fitted, 
rstudent, cooks.distance and predict. If there is only one predictor variable, a 
scatterplot of the data with included regression line and prediction and confidence 
intervals is also provided by default."
set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
lines(c(5,90), c(89,89), col=col.line)
text(0,57, label=t1, adj=0)
help.more("reg", 24)
}
else if (topic == "help.to.pdf") {
pdf("R_help.pdf")
t1 <- "Contents of the Help Files for R Function help.me()"
t2 <- "from the R Contributed Package:"
t3 <- "lessR"
t4 <- "David W. Gerbing"
t5 <- "School of Business Administration"
t6 <- "Portland State University"
t7 <- "Version 1.8.8"
set.up.plot()
text(50,84, label=t1)
text(50,80, label=t2)
text(50,76, label=t3)
text(50,58, label=t4)
text(50,54, label=t5)
text(50,50, label=t6)
text(50,24, label=t7)
help.me()
help.me("data")
help.me("read")
help.me("write")
help.me("library")
help.me("prob")
help.me("random")
help.me("sample")
help.me("histogram")
help.me("bar.chart")
help.me("plot")
help.me("stats")
help.me("one.sample")
help.me("two.samples")
help.me("many.samples")
help.me("props")
help.me("power")
help.me("cor")
help.me("reg")
dev.off()
if (getwd() =="/")
  workdir <- "top level of your file system"
else
  workdir <- getwd()
cat("PDF file of help.me contents located at current working directory.\n")
cat("   R_help.pdf at: ", workdir, "\n")
}
else {
cat("
Value ", topic," for help.me not recognized.\n
Complete list of help.me topics, enter:  help.me()\n
PDF file of all help.me topics, enter:  help.me(\"help.to.pdf\")
\n")
}
}
out <- 
function(myfile="mydata.csv") {
  if (!exists("mydata")) 
    stop("First need to have a data frame called mydata or specify one.")
  pre <- ">"
  line <- "------------------------------------------------------------\n"
  cat(line, pre, sep="")
  cat(" write.csv(mydata, file=\"",myfile, "\", row.names=FALSE)", sep="", "\n")
  cat(line)
  write.csv(mydata, file=myfile, row.names=FALSE)
  if (getwd() == "/")
    workdir <- "top level (root) of your file system"
  else
    workdir <- getwd()
  cat("csv file of mydata contents written at current working directory.\n")
  cat("       ", myfile, "at:  ", workdir, "\n")
}
powercurve.t.test <- 
function(n=NULL, s=NULL, n1=NULL, n2=NULL, s1=NULL, s2=NULL, 
         mmd=NULL, msmd=NULL, mdp=.8, mu0=NULL, ...) {
      
  dashes <- function(ndash) { for (i in 1:(ndash)) cat("-"); cat("\n") }
  cat("\n")
  # for all null arguments, pick up values from previous smd.t.test
  if (sum(sapply(list(s, n1, n2), is.null)) == 3) {  # all are NULL
    if (exists("n1", 1, inherits=FALSE) && exists("n2", 1, inherits=FALSE)) {
      n1 <- get("n1", 1, inherits=FALSE)
      n2 <- get("n2", 1, inherits=FALSE)
    } 
    else {
      if (is.null(n)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Need to specify sample size, either n or n1 and n2.\n\n")
      }
    }
    if (exists("sw", 1, inherits=FALSE))
      s <- sw
    else
      if (is.null(s1) && is.null(s2)) { 
            cat("\n"); stop(call.=FALSE, "\n","------\n",
            "Need to specify a sample standard deviation, s,\n",
            "or two standard deviations, s1 and s2.\n\n")
      }
    }
  if ( (is.null(n)) && (is.null(n1)) && (is.null(n2)) ) { 
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Need to specify either a common sample size for both groups, n,\n",
        "or specify both group sample sizes, n1 and n2, from which\n",
        "the within-group or pooled standard deviation, sw, is computed.\n\n")
  }
  if ( (is.null(s)) && (is.null(s1)) && (is.null(s2)) ) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Need to specify either a single standard deviation, s, \n",
        "or specify two group standard deviations, s1 and s2, plus a\n",
        "common sample size n or individual sample sizes n1 and n2, from which\n",
        "the within-group standard deviation is computed.\n\n")
  }
  if ( (mdp < 0)  || (mdp > 1) ) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Minimum desired power, mdp, must be between 0 and 1.\n\n")
  }
  if ( !is.null(mmd) && !is.null(msmd) ) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Specify only one of mmd and msmd as one implies the other.\n\n")
  }
  cat("------------------------------------------------------------\n")
  if (is.null(mu0)) {
    mytype <- "two.sample"
    cat("Power Curve Analysis for Independent Groups t-test\n")
  }
  else {
    mytype <- "one.sample"
    cat("Power Curve Analysis for One Sample t-test\n")
    cat("------------------------------------------------------------\n")
    cat("mu0 =", mu0, "\n")
  }
  # power curve for two groups, assuming mean diff of 0
  if (mytype == "two.sample") {
    cat("------------------------------------------------------------\n")
    # n1, n2 and n all should be set
    if (!is.null(n)) { 
      cat("\n", "Sample size: n = ", n, sep="")
      n1 <- n;  n2 <- n 
    }
    else {
      if ( !is.null(n1) && !is.null(n2) ) {
        n = 2 / ( 1/n1 + 1/n2 )  # harmonic mean
        cat("\n", "Sample sizes: n1 = ", n1, " and n2 = ", n2, sep="")
        cat("\n", "Harmonic mean of the two sample sizes: n = ", n, sep="", "\n")
      }
    }
        
    # within-group standard deviation	(need n1 and n2)
    if (is.null(s)) {
      df1 <- n1 - 1
      df2 <- n2 - 1
      ssq <- (df1*s1^2 + df2*s2^2) / (df1 + df2)
      s <- sqrt(ssq)
      cat("\n", "Equal Group Variances Assumed", sep="", "\n")
    }
    cat("\n", "Within-group (pooled) Standard Deviation:  sw = ", s, sep="", "\n")
    mytitle <- "Power Curve for Independent Groups t-test"
    myxlab <- bquote(paste("Alternative Values of ", mu[1] - mu[2]))
    H0 <- 0
  }
  # power values for a single sample, triggered by nonzero mu0
  else {
    mytitle <- "Power Curve for One Sample t-test"
    myxlab <- bquote(paste("Alternative Values of ", mu))
    H0 <- mu0
  }
  cat("------------------------------------------------------------\n")
  # get value of mmd if msmd supplied
  if ( !is.null(mmd) | !is.null(msmd) ) {
    if (!is.null(mmd)) msmd <- mmd / s   # null, because msmd only informs mmd
    if (!is.null(msmd)) mmd <- msmd * s
  }
  # configure range of deltas (need n)
  pp <- power.t.test(n=n, sd=s, power=.9999, type=mytype)
  if (!is.null(mmd)) { 
    xmax <- max(pp$delta,mmd)
    xmin <- min(-pp$delta,-mmd)
  }
  else {
    xmax <- pp$delta
    xmin <- -pp$delta
  }
  mydeltas <- seq(xmin,xmax,length=100)
  xmax <- H0 + xmax
  xmin <- H0 + xmin 
  rm(pp)
  # power curve
  mypower <- power.t.test(n=n, sd=s, delta=mydeltas, type=mytype)
  s.out <- toString(round(s,4))
  color.plot(H0+mydeltas, mypower$power, type="l", xlab=myxlab, ylab="Power",
    ylim=c(0,1.1), ...)
  abline(h=0, lwd=.5, col="gray50")
  mtext(mytitle, side=3, line=2.5, cex=1.1, font=2)
  n.out <- toString(round(n,3))
  mtext(paste("n=", n.out, ", s=", s.out, sep=""), side=3, line=1, font=3)
  # delta for a power of mdp, default is 0.8
  col80 <- "firebrick4"
  if (mdp != 0) {
    pp <- power.t.test(n=n, sd=s, power=mdp, type=mytype)
    del.hi <- H0 + pp$delta
    del.lo <- H0 - pp$delta
    lines(c(del.hi, del.hi), c(0,mdp), lwd=.5, col=col80)
    arrows(del.hi, mdp, xmax, mdp, lwd=.5, col=col80, length=.15, angle=20)
    lines(c(del.lo, del.lo), c(0,mdp), lwd=.5, col=col80)
    arrows(del.lo, mdp, xmin, mdp, lwd=.5, col=col80, length=.15, angle=20)
    text(del.hi, mdp+.033, labels="    Powerful", col=col80, cex=.85, adj=0)
    text(del.lo, mdp+.033, labels="Powerful    ", col=col80, cex=.85, adj=1)
  del.diff.out <- round(del.hi,3) - H0
  if (mytype == "two.sample") mytitle <- "\nMean difference "
  if (mytype == "one.sample") mytitle <- "\nDifference of mu from mu0 "
  cat(mytitle, "to achieve power of ", mdp, ": Diff = ", del.diff.out, sep="", "\n\n")
  rm(pp)
  dashes(70)
  }
  # for when the minimum meaningful difference, mmd, is provided
  if (!is.null(mmd)) {
    if (mytype == "two.sample") mytitle <- "the two means"
    if (mytype == "one.sample") mytitle <- "mu and mu0"
    cat("Minimum meaningful difference of ", mytitle, ": mmd\n", sep="")
    dashes(70)
    if (mytype == "two.sample" && !is.null(msmd))
      cat("Provided standardized value is msmd = ", msmd, "\n")
    colmmd <- rgb(112,128,144,40, max=255)  # slategray base
    colbrd <- rgb(112,128,144,80, max=255)
    coltrv <- rgb(97,129,129, max=255)  # darkslategray
    # power for mmd
    d.hi <- H0 + mmd
    d.lo <- H0 - mmd
    pp <- power.t.test(n=n, sd=s, delta=mmd, type=mytype)
    rect(d.lo, 0, d.hi, pp$power, lwd=.25, col=colmmd, density=-10, border=colbrd)
    lines(c(d.hi, d.hi), c(0, 1.05), lwd=.5, col=coltrv, lty="longdash")
    lines(c(d.lo, d.lo), c(0, 1.05), lwd=.5, col=coltrv, lty="longdash")
    if (d.hi < xmax) {
      arrows(d.hi, 1.05, xmax, 1.05, lwd=.5, col=coltrv, length=.15, angle=20, lty=5)
      arrows(d.lo, 1.05, xmin, 1.05, lwd=.5, col=coltrv, length=.15, angle=20, lty=5)
    }
    text(d.hi, 1.08, labels="    Meaningful", col=coltrv, cex=.85, adj=0)
    text(H0, 1.02, labels="Trivial", col=coltrv, cex=.85)
    text(d.lo, 1.08, labels="Meaningful    ", col=coltrv, cex=.85, adj=1)
    p.out <- round(pp$power,3)
    txt <- ", for mmd of "
    if (mytype == "two.sample") 
      mytitle <- paste("Given n = ", round(n,3), " and sw = ", signif(s), txt, sep="")
    if (mytype == "one.sample")
      mytitle <- paste("Given n = ", n, " and s = ", s, txt, sep="")
    cat(mytitle, mmd, ": Power = ", p.out, sep="", "\n")
    if (mdp != 0) {
      del.hi.out <- round(del.hi,3)
      if (del.hi > mmd+H0) {
        dashes(70)
        mytitle <- "Warning: Meaningful differences, from "
        cat(mytitle, d.hi, " to ", del.hi.out, ", have Power < ", mdp, "\n", sep="")
      }
      else {
        dashes(70)
        mytitle <- "Warning: Trivial differences, from "
        cat(mytitle, del.hi.out, " to ", mmd+H0, ", have Power > ", mdp, ".\n", sep="")
        cat("Note: All meaningful differences have Power > ", mdp, "\n", sep="")
      }
      dashes(70)
      rm(pp)
      # n needed to achieve a power of mdp=0.8 for mmd
      pp <- power.t.test(sd=s, delta=mmd, power=mdp, type=mytype)
      n.out <- ceiling(pp$n)
      txt <- ", needed n to achieve power= "
      if (mytype == "two.sample") mytitle <- paste("Given sw = ", signif(s), txt, sep="")
      if (mytype == "one.sample") mytitle <- paste("Given s = ", s, txt, sep="")
      cat(mytitle, mdp, " for mmd of ", mmd, ": n = ", n.out, sep="", "\n")
      if (mytype == "two.sample") cat("Sample size n applies to *each* group", "\n")
      rm(pp)
    }
    dashes(70)
    cat("\n")
  }
}
prob.norm <- 
function(lo=NULL, hi=NULL, mu=0, sigma=1, col.nrm="black", 
         col.fill.nrm="grey91", col.fill.int="slategray3", ...) { 
  if (sigma <= 0) { 
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Sigma, the population standard deviation, must be larger than zero.\n\n")
  }
  if (is.null(lo)) {
    lo <- mu-sigma*10
    lo.lbl <- "..."
  }
  else lo.lbl <- as.character(lo)
  if (is.null(hi)) {
    hi <- mu+sigma*10
    hi.lbl <- "..."
  }
  else hi.lbl <- as.character(hi)  
  # normal density curve
  min.x <- mu-4*sigma
  max.x <- mu+4*sigma
  x <- seq(min.x, max.x, length=200)
  d.nrm <- dnorm(x,mu,sigma)
  plot(x, d.nrm, type="l", col=col.nrm, xlab="Y", ylab="Normal Density", ...)
  polygon(c(min.x,x,max.x), c(0,d.nrm,0), col=col.fill.nrm)
  # plot an interval
  y.lo <- dnorm(lo, mu, sigma)
  y.hi <- dnorm(hi, mu, sigma)
  xsub <- x[x>lo & x<hi]
  ysub <- d.nrm[x>lo & x<hi]
  polygon(c(lo,xsub,hi), c(0,ysub,0), col=col.fill.int)
  # prob of interval
  prob <- pnorm(hi, mean=mu, sd=sigma) - pnorm(lo, mean=mu, sd=sigma)
  # decorate
  lbl1 <- paste(" Prob =", toString(signif(prob, 4)))
  lbl2 <- paste(" for Y from", lo.lbl, "to", hi.lbl)
  title(main=paste(lbl1,lbl2), ...)
  lbl3 <- bquote(paste(mu, "=", .(mu), "  ", sigma, "=", .(sigma)))
  title(sub=lbl3, ...)
}
# read, attach, display
rad <- 
function(ref=NULL, display=TRUE, show.R=FALSE, no.attach=FALSE, 
         miss.zero=TRUE, miss.matrix=FALSE, 
         format=c("csv", "SPSS"), ...) {
         
format <- match.arg(format)
pre <- ">"
line <- "------------------------------------------------------------\n"
cat("\n")
if (is.null(ref)) ref <- file.choose()
if (format == "csv") mydata <<- read.csv(file=ref, ...)
if (format == "SPSS") {
  check.foreign <- suppressWarnings(require(foreign, quietly=TRUE))
  if (check.foreign) {
    mydata <<- read.spss(file=ref, to.data.frame = TRUE, ...)
  }
  else {
  cat("\n"); stop(call.=FALSE, "\n","------\n",
      ">>> Reading a SPPS .sav data file requires package:  foreign\n",
      ">>> To obtain the foreign package, run one time only: ",
      "install.packages('foreign')\n\n")
  }
}
if (!no.attach) attach(mydata, warn.conflicts=FALSE)
if(show.R) {
if(ref == "file.choose()") {
  cat(line, pre, " mydata <- read.csv(file.choose())", "\n", sep="")
  cat("\nFile: ", ref, "\n")
 }
 else cat(line, pre, " mydata <- read.csv(file=\"",ref,"\")", "\n", sep="")
 cat(pre, " attach(mydata)", "\n", line, sep="")
}
if (display) {
  n.var <- ncol(mydata)
  n.obs <- nrow(mydata)
  if (nargs() > 1) cat("Plus the optional arguments that were entered in rad.", "\n")
  cat("\n")
  cat("Name of data frame that contains the data:   mydata ", "\n")
  cat("Number of Columns in mydata:    ", n.var, "\n")
  cat("Number of Rows of Data in mydata: ", n.obs, "\n")
  cat("\n\n")
  cat(line)
  cat("Variable names, first and last three rows of data\n")
  cat(line, "\n")
  if (show.R) 
    cat(line, pre, " head(mydata, n=3)   # First three rows", "\n", line, sep="", "\n")
  print(head(mydata, n=3))   
  cat("\n\n")
  if (show.R) 
    cat(line, pre, " tail(mydata, n=3)   # Last three rows", "\n", sep="", line, "\n")
  print(tail(mydata, n=3))
  if (format == "csv") {
    cat("\n\n")
    if (show.R) 
      cat(line, pre, " str(mydata, digits.d=15)   # Types of variables", "\n", sep="")
    cat(line)
    cat("Data type of each variable\n")
    cat(line)
    cat("Factor: Variable with non-numeric values, stored as an integer\n")
    cat("num: Numeric variable that may have decimal digits\n")
    cat("int: Numeric variable limited to integer values\n")
    cat(line, "\n")
    cat(str(mydata, digits.d=15))
  }
  cat("\n\n")
  cat(line)
  cat("Missing Data Analysis\n")
  cat(line, "\n")
  n.miss.tot <- 0
  for (i in 1:n.var) n.miss.tot <- n.miss.tot + sum(is.na((mydata)[, i]))
  if (n.miss.tot > 0) {
    cat("Miss", "Variable\n")
    for (i in 1:n.var) {
      n.miss <- sum(is.na((mydata)[, i]))
      if (n.miss > 0 || miss.zero) cat(n.miss, " ", names((mydata)[i]),  "\n")
    }
    cat("\nMiss", "Observation\n")
    for (i in 1:n.obs) {
      n.miss <- sum(is.na((mydata)[i, ]))
      if (n.miss > 0 || miss.zero) cat(n.miss, " ", row.names((mydata)[i, ]),  "\n")
    }
  cat("\n\n")
  }
  cat("Total number of cells in data table: ", n.var*n.obs,"\n\n")
  cat("Total number of cells with the value missing: ", n.miss.tot,"\n")
  if (miss.matrix && n.miss.tot>0) {
    cat("\n\nTable of Missing Values, 1 means missing\n\n")
    print(matrix(as.numeric(is.na(mydata)), nrow=n.obs, ncol=n.var,
      dimnames = list(row.names(mydata), as.character(1:n.var)) ))
  }
  cat("\n\n")
  cat(line, "What is next? Try one of the following.", "\n", sep="", line)
  cat("mydata: List all rows (observations) of data, or just enter the\n")
  cat("        variable name to see the data just for that variable\n")
  cat("full(): A data summary and graph for each variable in the data table, mydata\n")
  cat("help.me(): List of topics for analysis with related R/lessR functions\n")
  cat(line, sep="")
  cat("\n")
}
}
reg <-
function(my.formula, dframe=mydata, sig.digits=4,
         res.rows=NULL, res.sort=c("cooks","rstudent","dffits","off"), 
         pred=TRUE, pred.all=FALSE, pred.sort=c("predint", "off"),
         subsets=TRUE, cooks.cut=1, results=c("full", "brief"), 
         scatter.coef=FALSE, scatter.3d=NULL, graphics.save=FALSE,
         X1.new=NULL, X2.new=NULL, X3.new=NULL, X4.new=NULL, 
         X5.new=NULL, text.width=100, show.R=FALSE, explain=FALSE) {
         
  dash <- function(n.dash) { for (i in 1:(n.dash)) cat("-"); cat("\n") }
  mydframe <- deparse(substitute(dframe))  # get dataframe name for cor before sort
  # produce actual argument, such as from an abbreviation, and flag if not exist
  res.sort <- match.arg(res.sort)
  pred.sort <- match.arg(pred.sort)
  results <- match.arg(results)
  op <- options()  # save current options to reset at end of reg
  options(show.signif.stars=FALSE, scipen=30, width=text.width)
  # output
  cor <- TRUE
  collinear <- TRUE 
  if (results == "brief") {
    if (is.null(res.rows)) res.rows <- 0
    pred <- FALSE
    cor <- FALSE
    collinear <- FALSE
    show.R <- FALSE
   }
      
  pre <- "> "
  line <- "--------------------------------------------------------------------\n"
  if(graphics.save) pdf("regOut.pdf")
  else  graphics.off()  # graphics get clean start
  nm <- all.vars(my.formula)  # names of vars in the model
  n.vars <- length(nm)
  n.pred <- n.vars - 1
  n.obs <- nrow(dframe)
  if(n.pred > 1) {
    collinear <- TRUE
    subsets <- TRUE
  }
  else {
    collinear <- FALSE
    subsets <- FALSE
  }
  if(is.null(scatter.3d)) if (n.pred==2) scatter.3d <- TRUE else scatter.3d <- FALSE
  if ( scatter.3d && (n.pred)!=2 ) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
       "Can have a 3d scatterplot only with exactly two predictor variables.\n\n")
  }
  if ( !is.null(X1.new) && (n.pred)>5 ) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Cannot specify new data if more than 5 predictor variables.\n\n")
  }
  # check new.data option for consistency  
  new.data <- FALSE
  if ( (n.pred) <= 5 ) { 
    for (i in 1:(n.pred)) {
      pp <- eval(parse(text=paste("X", toString(i),".new",sep="")))
      if (!is.null(pp)) new.data <- TRUE
    }
    if (new.data) for (i in 1:(n.pred)) {
      pp <- eval(parse(text=paste("X", toString(i),".new",sep="")))
      if (is.null(pp)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Specified new data values for one predictor variable, so do for all.\n\n")
      }
    }
  }
  if (is.null(res.rows)) if (n.obs < 25) res.rows <- n.obs else res.rows <- 25 
  if (res.rows == "all") res.rows <- n.obs  # turn off resids with res.rows=0 call
  if (n.vars == 2) { # order values of the one predictor variable for scatterplot
    o <- order(dframe[,nm[2]], decreasing=FALSE)
    dframe <- dframe[o,]
  }
  in.data.frame <- TRUE
  for (i in 1:n.vars) {
    if (!(nm[i] %in% names(dframe))) {
        cat("\n\n\n>>>> Note: ", nm[i], "is not in the data frame.\n")
        in.data.frame <- FALSE
      }
  }
  # reg analysis, all analysis done on data in model construct lm.out$model
  #   this model construct contains only model vars, with Y listed first
  lm.out <<- lm(my.formula, data=dframe)
  if (!explain) 
    cat("\nAdd the following option to see explanations of the output:  explain=TRUE\n")
# ----------
# Background
# ----------
  cat( "\n\n\n", "  BACKGROUND", "\n")
  if(show.R) {
    cv <- paste(nm[1]," ~ ", sep="")
    cv <- paste(cv, nm[2], sep="")
    if (n.vars > 2) for (i in 3:n.vars) cv <- paste(cv, " + ", nm[i], "", sep="")
    cat(line, pre, "model <- lm(", cv, ")", "\n", line, sep="")
  }
  cat("\n")
  cat("Data Frame: ", mydframe, "\n\n")
  cat("Response Variable:  ", nm[1], "\n")
  for (i in 2:n.vars) {
    if (n.vars > 2) txt <- paste("Predictor Variable ", toString(i-1), ": ", sep="")
      else txt <- "Predictor Variable: "
    cat(txt, nm[i], "\n")
  }
  cat("\nNumber of observations (rows) of data: ", n.obs, "\n")
# --------------
# Basic Analysis
# --------------
  cat( "\n\n\n", "  BASIC ANALYSIS", "\n")
  sm <- summary(lm.out)
  if (show.R) cat(line, pre, "summary(model)", "\n", line, sep="")
  cat("\n")
  if (explain) {
    dash(68)
    cat("Estimates\n",
        "  Intercept: Fitted value of response variable, ", nm[1], ", when the\n",
        "             predictor variable", sep="")
    if (n.pred == 1) cat(" is ") else cat("s are ")
    cat("set to zero.\n")
    cat("  Slope Coefficient: Average change in the value of response variable,\n",
        "             ", nm[1], ", for a one-unit increase in the value of the\n",
        "             corresponding predictor variable", sep="")
    if (n.pred == 1) cat(".\n") 
    else cat(", with the value of all\n",
             "             remaining predictor variables held constant.\n", sep="")
    cat("\n")
    cat("Hypothesis Tests\n")
    cat("    Standard error, t-value and p-value of each estimate\n")
    cat("    Null hypothesis: Corresponding population coefficient is 0\n")
    dash(68)
   }
  else cat("Estimates, Hypothesis Tests\n")
  cat("\n")
  print(sm$coefficients, digits=sig.digits)
  cat("\n\n\n")
  if (show.R) cat("\n",line,pre,"confint(model)","\n",line,"\n",sep="")
  if (explain) {
    dash(68)
    cat("95% Confidence Intervals\n",
        "    Each interval is constructed about the\n",
        "      corresponding estimated model coefficient.\n",
        "    The margin of error is half the width of the interval.\n")
    dash(68)
  }
  else cat("Confidence Intervals\n")
  cat("\n")
  print(confint(lm.out), level=0.95, digits=sig.digits)
  cat("\n\n\n")
  cat("Model fit\n")
  cat("\n")
  if (explain) {
    dash(68)
    cat("The \'standard deviation of the residuals\' is also called\n",
        "the \'standard error of estimate\'.  Are the residuals typically\n",
        "close to their mean of zero, or are they scattered with\n",
        "relatively large positive and negative values?\n",
        "\n",
        "For any normal distribution, about 95% of the values are within\n",
        "two standard deviations of the mean, for a range of four.\n", sep="")
    dash(68)
    cat("\n")
  }
  cat("Standard deviation of residuals: ", signif(sm$sigma,4),
    "for", sm$df[2], "degrees of freedom", "\n")
  cat("If normal, the approximate 95% range of residuals about\n")
  cat("  each fitted value is 4*", signif(sm$sigma,4), 
    " or ", signif(4*sm$sigma,4), sep="", "\n\n")
  if (explain) {
    dash(68)
    cat("R-squared: Proportion of the overall variability of response variable\n",
        nm[1], " that is accounted for by the model. The unexplained\n",
        "variability is the variability of the residuals.\n",  
        "\n",
        "Adjusted R-squared: Adjusts R-squared downward according to the\n",
        "degrees of freedom. Unlike R-squared, the adjusted version increases\n",
        "when a new predictor variable is added to the model only if the\n",
        "new variable improves the model more than would be expected by\n",
        "chance.\n", sep="")
    dash(68)
    cat("\n")
  }
  cat("R-squared: ", signif(sm$r.squared,3), 
    "    Adjusted R-squared: ", signif(sm$adj.r.squared,3), "\n")
  cat("\n")
  cat("F-statistic for hypothesis test of population R-squared=0: ", 
    signif(sm$fstatistic[1],4), "\n") 
  cat("Degrees of freedom: ", sm$fstatistic[2], "and", sm$fstatistic[3],"\n")
  cat("p-value: ",
    signif(1-pf(sm$fstatistic[1],sm$fstatistic[2],sm$fstatistic[3]),6),"\n")
  cat("\n")
  if (show.R) cat("\n\n",line, pre,"anova(model)","\n",line,"\n",sep="") else cat("\n")
  anv <- anova(lm.out)
  if (explain) {
    dash(68)
    cat("The ANOVA table presents the details of the explained and unexplained\n",
        "variation. \n",
        "\n",
        "The sum of the squared residuals, the value minimized by the OLS\n",
        "estimation procedure, is ", anv$'Sum Sq'[n.vars], ".\n", sep="")
    dash(68)
    cat("\n")
  }
  cat("\n")
  print(anv)
  # check for all numeric vars  in.data.frame <- TRUE
  numeric.all <- TRUE
  for (i in 1:n.vars) {
      if (in.data.frame && !is.numeric(dframe[1,which(names(dframe) == nm[i])])) {
        cat("\n\n\n>>>> Note: ", nm[i], "is not a numeric variable.\n")
        numeric.all <- FALSE
      }
    }
# -------------------------
# Relations among Variables
# -------------------------
  cat( "\n\n\n", "  RELATIONS AMONG VARIABLES", "\n")
  # correlations
  if (cor) {
    cat("\n")
    if (explain) {
      dash(68)
      cat("Correlations among the variables in the model.\n",
          "\n",
          "The correlations of response variable ", nm[1], " with the predictor\n",
          "variables should be high. The correlations of the predictor variables\n",
          "with each other should be small.\n", sep="")
      dash(68)
    }
    else cat("Correlations\n")
    if (numeric.all && in.data.frame) {
      if (show.R) {
        cv <- paste("\"",nm[1],"\"", sep="")
        for (i in 2:n.vars) cv <- paste(cv, ",\"", nm[i], "\"", sep="")
        cat(line, pre, "cor(", mydframe, "[c(", cv, ")])", "\n", line, "\n", sep="")
      }
      else cat("\n") 
      print(cor(dframe[c(nm)]), digits=2)
    }
    else {
      cat("\n>>> No correlations reported because not all variables are ")
      if (!in.data.frame) cat("in the data frame.\n")
      if (!numeric.all) cat("numeric.\n")
    }
  }
  # collinearity    
  if (collinear) {
    if (explain) {
      cat("\n\n")
      dash(68)
      cat("Collinearity analysis.\n",
          "\n",
          "The predictor variables should not be collinear, where one variable \n",
          "is linearly dependent on the others.\n",
          "\n",
          "Tolerances usually should be > approximately 0.20 or so.\n", 
          "Variance Inflation Factors, usually should be < approximately 5.\n", sep="")
      dash(68)
    }
    else cat( "\n\n", "Collinearity", "\n", sep="")
    cat("\n")
   if (numeric.all) {
      check.car <- suppressWarnings(require(car, quietly=TRUE))
      if (check.car) {
        cat("  Tolerances\n\n")
        print(1/(vif(lm.out)), digits=3)
        cat("\n\n  Variance Inflation Factors\n\n")
        print(vif(lm.out), digits=4)
      }
      else {
        cat("\n>>> Obtaining the collinearity analysis requires package car.", "\n")
        cat(">>> This analysis is not provided here, but all other output is unaffected.", "\n")
        cat(">>> To obtain the car package, run one time only: install.packages('car')", "\n")
      }
     }
     else cat("\n>>> No collinearity analysis because not all variables are numeric.\n")
   }
  # all possible subsets of predictor variables    
  if (subsets) {
    cat("\n\n")
    if (explain) {
      dash(68)
      cat("Analysis of all possible subsets of the predictor variable.\n",
         "\n",
          "Assess fit for models that correspond to all possible combinations\n",
          "of predictor variables.\n",
          "\n",
          "Warning: This analysis only describes the data, so does not literally\n",
          "  generalize to the population. Only use as a descriptive heuristic.\n",
          "\n",
          "A 1 means the predictor variable is in the model, a 0 means it is out.\n", sep="")
      dash(68)
    }
    else cat("Predictor Variable Subsets", "\n")
    cat("\n")
    if (numeric.all) {
      check.leaps <- suppressWarnings(require(leaps, quietly=TRUE))
      if (check.leaps) {
        X <- data.frame(lm.out$model[nm[seq(2,n.vars)]])
        Y <- numeric(length=n.obs)  # convert response to an atomic vector for leaps
        for (i in 1:n.obs) Y[i] <- lm.out$model[nm[1]][i,1]
        leaps.out <- leaps(X, Y, method="adjr2")
        models <- data.frame(cbind(leaps.out$which,leaps.out$adjr2), row.names=NULL)
        names(models) <- c(names(X),"R2adj")
        print(models[order(models$R2adj, decreasing=TRUE),], digits=3)
      }
      else {
        cat("\n>>> Analyzing subsets of predictor variables requires package leaps.", "\n")
        cat(">>> This analysis is not provided, but all other output is unaffected.", "\n")
        cat(">>> To get the leaps package, run once only: install.packages('leaps')", "\n")
      }
    }
    else cat("\n>>> No subset analysis reported because not all variables are numeric.\n")
  }
# -----------------
# residual analysis
# -----------------
  if (res.rows > 0) {
    cat( "\n\n\n", "  ANALYSIS OF RESIDUALS AND INFLUENCE", "\n")
    if (show.R) {
      cat(line, sep="")
      cat(pre, "fitted(model)", sep="", "\n")
      cat(pre, "resid(model)", sep="", "\n")
      cat(pre, "rstudent(model)", sep="", "\n")
      cat(pre, "dffits(model)", sep="", "\n")
      cat(pre, "cooks.distance(model)", sep="", "\n")
      cat(line, "\n")
    }
    else cat("\n")
    if (explain) {
      dash(68)
      cat("The identification of observations that have a large residual\n",
          "and/or undue influence on the estimation of the model helps\n",
          "detect potential outliers.  Each of the following statistics is\n",
          "calculated for a single observation (row of data).\n",
         "\n",          
          "residual: Value of the response variable ", nm[1], " minus its\n",
          "    fitted value.\n",
          "\n",
          "rstudent: Studentized residual, standardized value of the residual\n",
          "    from a model estimated without the observation present.\n",
          "\n",
          "dffits: The influence of an observation on its own fitted value.\n",
         "\n",
          "cooks: Cook's Distance, the aggregate influence of the observation\n",
          "    on the estimation of the model coefficients.\n", sep="")
      dash(68)
      cat("\n")
    }
    cat("Data, Fitted, Residual, Studentized Residual, Dffits, Cook's Distance\n")
    if (res.sort == "cooks") cat("   [sorted by Cook's Distance]\n")
    if (res.sort == "rstudent")  
      cat("   [sorted by Studentized Residual, ignoring + or - sign]\n")
   if (res.sort == "dffits")  
      cat("   [sorted by dffits, ignoring + or - sign]\n")
    txt <- "observations (rows) of data]"
    cat("   [res.rows = ", res.rows, " out of ", n.obs, " ", txt, sep="", "\n")
    dash(68)
    out <- cbind(fitted(lm.out),resid(lm.out),rstudent(lm.out),dffits(lm.out),
      cooks.distance(lm.out))
    out <- cbind(lm.out$model[c(nm[seq(2,n.vars)],nm[1])],out)
    out <- data.frame(out)
    names(out)[n.vars+1] <- "fitted"
    names(out)[n.vars+2] <- "residual"
    names(out)[n.vars+3] <- "rstudent"
    names(out)[n.vars+4] <- "dffits"
    names(out)[n.vars+5] <- "cooks"
    if (res.sort != "off") {
      if (res.sort == "cooks") o <- order(out$cooks, decreasing=TRUE)
      if (res.sort == "rstudent") o <- order(abs(out$rstudent),
        decreasing=TRUE)
      if (res.sort == "dffits") o <- order(abs(out$dffits),
        decreasing=TRUE)
      out <- out[o,]
    }
    print(out[1:res.rows,], digits=sig.digits)
    rm(out)
    # plot of residuals, residuals vs fitted
    res <- residuals(lm.out)
    color.density(res, main="Evaluate Normality of Residuals", 
      xlab="Residuals", text.out=FALSE)
    fit <- fitted(lm.out)
    cook <- cooks.distance(lm.out)
    max.cook <- max(cook)
    if (max.cook < cooks.cut) {
      cooks.cut <- floor(max.cook*100)/100
      txt <- paste("The point with the largest Cook's Distance, ", round(max.cook,2), 
        ", is displayed in red", sep="")
    }
    else
      txt <- paste("Points with Cook's Distance >", cooks.cut, "are displayed in red")
    if (!graphics.save) dev.new()
    color.plot(fit, res, type="p", fit.line="lowess", text.out=FALSE,
      main="Residuals vs Fitted Values", xlab="Fitted Values",
        ylab="Residuals", sub=txt)
    abline(h=0, lty="dotted", col="gray70")
    res.c <- res[which(cook>=cooks.cut)]
    fit.c <- fit[which(cook>=cooks.cut)]
    if (length(fit.c) > 0) {
      points(fit.c, res.c, col="red")
      text(fit.c, res.c, names(fit.c), pos=1, cex=.8)
    }
    rm(fit, res, cook, res.c, fit.c)
  }
# --------------------
# prediction intervals
# --------------------
  if (pred) {
      
    cat( "\n\n", "  FORECASTING ERROR", "\n")
    if (show.R) {
      txt <- "predict(model, interval=\"prediction\")"
      cat(line, pre, txt, sep="", "\n")
      txt <- "predict(model, interval=\"confidence\")"
      cat(pre, txt, sep="", "\n")
      dash(68)
    }
    else cat("\n")
    if (explain) {
      dash(68)
      cat("The 'standard deviation of the residuals', assumed to be the same\n",
          "value for each set of values of the predictor variables, estimates the\n",
          "modeling error. However, even for predictions from the current data\n",
          "from which the model is estimated, the forecasts are based on future\n",
          "responses from the collection of new data. That is, the sampling\n",
          "error of the sample regression line must also be considered. This\n",
          "sampling error varies depending on the values of the predictor variables.\n",
          "\n",
          "The 95% confidence interval around each fitted value of the sample\n",
          "regression model is given below, as well as the likely range of\n",
          "forecasting error, the 95% prediction interval, the expected range\n",
          "in which the actual future value of the response variable, ", nm[1], ", \n",
          "will likely be found.  This forecasting error depends on both modeling\n", 
          "error and sampling error.\n", sep="")
      dash(68)
      cat("\n")
    }
    cat("Data, Fitted Values, Confidence and Prediction Intervals\n")
    cat("   [sorted by lower bound of prediction interval]\n")
    if (n.obs > 50 && pred.all == FALSE && !new.data) 
      cat("   [to save space only some intervals printed, do pred.all=TRUE to see all]\n")
    dash(68)
    if (!new.data) {
      c.int <- data.frame(predict(lm.out, interval="confidence"))
      p.int <- suppressWarnings(data.frame(predict(lm.out, interval="prediction")))
      out <- cbind(lm.out$model[c(nm[seq(2,n.vars)],nm[1])],c.int,p.int$lwr,p.int$upr)
    }
    else {
      Xnew.val <- list(X1.new)
      if (n.vars > 2) for (i in 2:(n.pred)) {
        pp <- eval(parse(text=paste("X", toString(i),".new",sep="")))
        Xnew.val <- c(Xnew.val, list(pp))
      }
      Xnew <- expand.grid(Xnew.val)
      for (i in 1:(n.pred)) names(Xnew)[i] <- nm[i+1]
      c.int <- data.frame(predict(lm.out, interval="confidence", newdata=Xnew))
      p.int <- suppressWarnings(data.frame(predict(lm.out, interval="prediction", newdata=Xnew)))
      Ynew <- character(length = nrow(Xnew))
      Ynew <- ""
      out <- cbind(Xnew, Ynew, c.int, p.int$lwr, p.int$upr)
      names(out)[n.vars] <- nm[1]
    }
    out <- data.frame(out)
    if (pred.sort == "predint") {
      o <- order(out[,n.vars+4])  # lower bound of prediction interval
      out <- out[o,]
    }
    names(out)[n.vars+1] <- "fitted"
    names(out)[n.vars+2] <- "ci:lwr"
    names(out)[n.vars+3] <- "ci:upr"
    names(out)[n.vars+4] <- "pi:lwr"
    names(out)[n.vars+5] <- "pi:upr"
    if (n.obs < 50  || pred.all == TRUE || new.data)
      print(out, digits=sig.digits)
    else {
      print(out[1:5,], digits=sig.digits)
      cat("\n... for the middle 5 rows of sorted data ...\n\n")
      n.mid <- round(n.obs/2)
      print(out[(n.mid-2):(n.mid+2),], digits=sig.digits)
      cat("\n... for the last 5 rows of sorted data ...\n\n")
      print(out[(n.obs-4):n.obs,], digits=sig.digits)
    }
    dash(68)
  }
# ------------
# Scatterplots
# ------------
  if (scatter.3d) {  # 3d scatterplot option
    check.3d <- suppressWarnings(require(scatterplot3d, quietly=TRUE))
    if (check.3d) {
      if (!graphics.save) dev.new()
      text <- paste("Scatter about the Best-Fit Regression Plane: \n", nm[1], "= ")
      text <- paste(text, toString(signif(lm.out$coefficients[2],3)), "*", nm[2],
                " + ", toString(signif(lm.out$coefficients[3],3)), "*", nm[3], sep="")
      s3d <- scatterplot3d(lm.out$model[,nm[2]], lm.out$model[,nm[3]], main=text, 
               lm.out$model[,nm[1]], type="h", color="darkslateblue", box=FALSE,
               angle=55, scale.y=0.7, pch=16, xlab=nm[2], ylab=nm[3], zlab=nm[1])
      s3d$plane3d(lm.out, lty.box="solid", col="gray70")
    }
    else {
      cat("\n>>> Creating a 3d scatterplot requires package scatterplot3d.", "\n")
      cat(">>> This analysis is not provided, but all other output is unaffected.", "\n")
      cat(">>> To get this package, run once only: install.packages('scatterplot3d')", "\n")
    }
  }
 
  if (!graphics.save) dev.new() 
  if (n.vars == 2) {  # scatterplot, if one predictor variable
    if ( (pred==FALSE) || is.factor(lm.out$model[,nm[2]]) || !is.null(X1.new) ) 
     do.int <- FALSE
    else do.int <- TRUE
    if (!do.int) {
      ctitle <- "Scatterplot and Regression Line"
      y.min <- min(lm.out$model[,nm[1]])
      y.max <- max(lm.out$model[,nm[1]])
    }
    else {
      ctitle <- "Regression Line, Confidence and Prediction Intervals"
      y.min <- min(p.int$lwr)
      y.max <- max( max(p.int$upr),  max(lm.out$model[,nm[1]]) )
    }
    if (!is.factor(lm.out$model[,nm[2]])) fl <- "ls" else fl <- "none"
    color.plot(lm.out$model[,nm[2]], lm.out$model[,nm[1]], type="p", 
      cex=.8, fit.line=fl, xlab=nm[2], ylab=nm[1],
      ylim=c(y.min,y.max), main=ctitle, text.out=FALSE)
    if (do.int) {
      lines(lm.out$model[,nm[2]], c.int$lwr, col="lightsteelblue", lwd=2)
      lines(lm.out$model[,nm[2]], c.int$upr, col="lightsteelblue", lwd=2)
      lines(lm.out$model[,nm[2]], p.int$lwr, col="darksalmon", lwd=2)
      lines(lm.out$model[,nm[2]], p.int$upr, col="darksalmon", lwd=2)
    }
  }
  else   # scatterplot matrix for multiple regression
    if (numeric.all && in.data.frame) {
      if (scatter.coef) {
        panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
          usr <- par("usr"); on.exit(par(usr))
          par(usr = c(0, 1, 0, 1))
          r <- cor(x, y)
          txt <- format(c(r, 0.123456789), digits=digits)[1]
          txt <- paste(prefix, txt, sep="")
          if(missing(cex.cor)) cex.cor <- .9/strwidth(txt)
          text(0.5, 0.5, txt, cex=2)  # or cex = cex.cor * r
        }
        suppressWarnings(pairs(dframe[c(nm)], 
          lower.panel=panel.smooth, col.smooth="grey50", upper.panel=panel.cor))
      }
      else suppressWarnings(pairs(dframe[c(nm)], panel=panel.smooth, col.smooth="grey50"))
    }
    else {
      cat("\n\n>>> No scatterplot matrix reported because not all variables are ")
      if (!in.data.frame) cat("in the data frame.\n")
      if (!numeric.all) cat("numeric.\n")
    }
  if (graphics.save) {
    cat("\n\n")
    dash(68)
    if (getwd() == "/")
      workdir <- "top level (root) of your file system"
    else
      workdir <- getwd()
    cat("regression graphics written at current working directory\n")
    cat("       regOut.pdf", "at:  ", workdir, "\n")
    dev.off()
  }
  cat("\n")
  options(op)  # restore options going into reg
}
smd.t.test.default <-
function(x, y, Ynm = "Y", Xnm = "X", X1nm = "Group1", X2nm = "Group2", 
           conf.level = 0.95, mmd = NULL, msmd = NULL,
           digits.d = 2, bw1 = "nrd", bw2 = "nrd", ...) {
ODDSMD <-
function(YA, YB) {
  cat("------------------------------------------------------------\n")
  cat("Compare", Ynm, "across", Xnm, "levels", X1nm, "and", X2nm, "\n")
  cat("------------------------------------------------------------\n\n")
  cat("------ Description ------\n\n")
  n1 <<- length(YA)
  n2 <<- length(YB)
  m1 <<- mean(YA)
  m2 <<- mean(YB)
  s1 <<- sd(YA)
  s2 <<- sd(YB)
  v1 <- var(YA)
  v2 <- var(YB)
  clpct <- paste(toString(round((conf.level)*100, 2)), "%", sep="")
  m1.out <- round(m1,digits.d)
  m2.out <- round(m2,digits.d)
  s1.out <- round(s1,digits.d)
  s2.out <- round(s2,digits.d)
  Xnmval <- paste(Xnm, X1nm)
  mytitle <- paste(Ynm, " for ", Xnmval, ":  n = ", sep="")
  cat(mytitle, n1, ",   mean = ", m1.out, ",   sd = ", s1.out, sep="", "\n")
  Xnmval <- paste(Xnm, X2nm)
  mytitle <- paste(Ynm, " for ", Xnmval, ":  n = ", sep="")
  cat(mytitle, n2, ",   mean = ", m2.out, ",   sd = ", s2.out, sep="", "\n\n")
  # sw
  df1 <- n1 - 1
  df2 <- n2 - 1
  swsq <- (df1*v1 + df2*v2) / (df1 + df2)
  sw <<- sqrt(swsq)
  sw.out <- round(sw,digits.d)
  cat("Equal Group Variances Assumed, Within-group Standard Deviation:  ", 
      sw.out, "\n\n")
  # mean diff and standardized mean diff
  mdiff <- m1 - m2
  mdiff.out <- round(mdiff,digits.d)
  cat("Mean Difference of ", Ynm, ":  " , mdiff.out, sep="", "\n\n")
  # smd
  smd <- mdiff/sw
  smd.out <- round(smd,digits.d)
  cat("Standardized Mean Difference of ", Ynm, ", Cohen's d:  ", smd.out, sep="", "\n")
  cat("\n\n------ Assumptions ------\n\n")
  cat("Note:  These hypothesis tests can perform poorly, and the", "\n")
  cat("       t-test is typically robust to violations of assumptions.", "\n")
  cat("       Use as heuristic guides instead of interpreting literally.", "\n\n")
  # Normality
  cat("Null hypothesis, for each group, is a normal distribution of ", sep="")
  cat(Ynm, ".", sep="", "\n")
  if (n1 > 30) {
    mytitle <- 
    cat("Group " , X1nm, ": ", sep="")
    cat("Sample mean is normal because n>30, so no test needed.", sep="", "\n")
  }
  else {
    cat("Group", X1nm, " ")
    if (n1 > 2 && n1 < 5000) {
      nrm1 <- shapiro.test(YA)
      W.1 <- round(nrm1$statistic,min(4,digits.d+1))
      p.val1 <- round(nrm1$p.value,min(4,digits.d+1))
      cat(nrm1$method, ":  W = ", W.1, ",  p-value = ", p.val1, sep="", "\n")
    }
    else
      cat("Sample size out of range for Shapiro-Wilk normality test.", "\n")
  }  
  if (n2 > 30) {
    cat("Group " , X2nm, ": ", sep="")
    cat("Sample mean is normal because n>30, so no test needed.", sep="", "\n")
  }
  else {
    cat("Group", X2nm, " ")
    if (n2 > 2 && n2 < 5000) {
      nrm2 <- shapiro.test(YB)
      W.2 <- round(nrm2$statistic,min(4,digits.d+1))
      p.val2 <- round(nrm2$p.value,min(4,digits.d+1))
      cat(nrm2$method, ":  W = ", W.2, ",  p-value = ", p.val2, sep="", "\n")
    }
    else
      cat("Sample size out of range for Shapiro-Wilk normality test.", "\n")
  }  
  cat("\n")
  # Homogeneity of Variance
  # Var Ratio
  v1.out <- toString(round(v1,digits.d+1))
  v2.out <- toString(round(v2,digits.d+1))
  if (v1 >= v2) {
    vratio <- v1/v2
    vr <- paste(v1.out, "/", v2.out, sep="")
    df.num <- df1
    df.den <- df2
  }
  else {
    vratio <- v2/v1
    vr <- paste(v2.out, "/", v1.out, sep="")
    df.num <- df2
    df.den <- df1
  }
  v.out <- round(vratio,digits.d+1)
  p.var <- pf(vratio, df1=df.num, df2=df.den)
  # adjust for two-sided test, results same as var.test{stats}
  p.var <- 2 * min(p.var, 1-p.var)
  pv.out <- round(p.var,min(4,digits.d+1))
  cat("Null hypothesis is equal variances of ")
  cat(Ynm, ", i.e., homogeneous.", sep="", "\n")
  cat("Variance Ratio test:  F = ", vr, " = ", v.out, ",  df = ", df.num, ";", 
      df.den, ",  p-value = ",  pv.out, sep="", "\n")
  # Levene
  YAm <- abs(YA - median(YA))
  YBm <- abs(YB - median(YB))
  t.bf <- t.test(YAm, YBm, var.equal=TRUE)
  tvalue.bf <- round(t.bf$statistic,min(4,digits.d+1))
  df.bf <- round(t.bf$parameter,min(4,digits.d+1))
  pvalue.bf <- round(t.bf$p.value,min(4,digits.d+1))
  cat("Levene's test, Brown-Forsythe:  t = ", tvalue.bf, ",  df = ", df.bf, sep="")
  cat(",  p-value = ", pvalue.bf, sep="", "\n")
  cat("\n\n------ Inference ------\n\n")
  sterr <- sw * sqrt(1/n1 + 1/n2)
  cat("Standard Error of Mean Difference: SE = ", round(sterr,digits.d), "\n\n")
  # t-test
  tt <- t.test(YA, YB, var.equal=TRUE, conf.level=conf.level)
  tvalue <- round(tt$statistic,min(2,digits.d))
  pvalue <- round(tt$p.value,min(4,digits.d+1))
  lb <- round(tt$conf[1],digits.d)
  ub <- round(tt$conf[2],digits.d)
  E <- round((ub-lb)/2,digits.d)
  df <- tt$parameter
  mytitle <- "Hypothesis Test of 0 Mean Diff:  t = "
  cat(mytitle, tvalue, ",  df = ", df, ",  p-value = ", pvalue, sep="", "\n\n")
  cat("Margin of Error for ", clpct, " Confidence Level:  ", E, sep="", "\n")
  cat(clpct," Confidence Interval for Mean Difference:  ", lb, " to ", ub, 
      sep="", "\n\n")
  # smd confidence interval
  check.MBESS <- suppressWarnings(require(MBESS, quietly=TRUE))
  if (check.MBESS) {
    cid <- ci.smd(smd=smd, n.1=n1, n.2=n2, conf.level=conf.level)
    deltaL <- round(cid$Lower.Conf.Limit.smd,digits.d)
    deltaU <- round(cid$Upper.Conf.Limit.smd,digits.d)
    cat(clpct," Confidence Interval for smd:  ", deltaL, " to ", deltaU, sep="", "\n")
  }
  else {
    cat(">>> The confidence interval for smd requires package MBESS.", "\n")
    cat(">>> Confidence interval for smd not provided here, but all other output unaffected.", "\n")
    cat(">>> To get the MBESS package, run one time only: install.packages('MBESS')", "\n")
    cat(">>> If present, IGNORE resulting 'Error in eval' error message below.", "\n")
    deltaL <- NULL
    deltaU <- NULL
  }
    cat("\n\n------ Practical Importance ------\n\n")
    cat("Minimum Mean Difference of practical importance: mmd\n")
  if ( !is.null(mmd) | !is.null(msmd) ) {
    if (!is.null(mmd)) msmd <- mmd / sw
    if (!is.null(msmd)) mmd <- msmd * sw
    cat("Compare mmd =", round(mmd,digits.d), " to the obtained value of md = ", mdiff.out, "\n")
    cat("Compare mmd to the confidence interval for md: ", lb, " to ", ub, "\n\n")
    cat("Minimum Standardized Mean Difference of practical importance: msmd\n")
    cat("Compare msmd = ", round(msmd,digits.d), " to the obtained value of smd = ", smd.out,"\n")
    if (!is.null(deltaL)) cat("Compare msmd to the confidence interval for smd: ", deltaL, " to ", deltaU, "\n")
  }
  else {
    cat("Minimum Standardized Mean Difference of practical importance: msmd\n")
    cat("Neither value specified, so no analysis\n")
  }
  # densities
  dYA <- density(YA, bw1)
  dYB <- density(YB, bw2)
  cat("\n\n------ Graphics Smoothing Parameter ------\n\n")
  mytitle <- "Density bandwidth for "
  cat(mytitle, Xnm, " ", X1nm, ": ", round(dYA$bw,digits.d), sep="", "\n")
  cat(mytitle, Xnm, " ", X2nm, ": ", round(dYB$bw,digits.d), sep="", "\n\n")
  cat("--------------------------------------------------\n")
  # values needed for graph
  min.x <- min(min(dYA$x),min(dYB$x))  # min x coordinate for graph
  max.x <- max(max(dYA$x),max(dYB$x))  # max x coordinate for graph
  max.y <- max(max(dYA$y),max(dYB$y))  # max y coordinate
  max.y <- max.y+.1*max.y  # allow room in graph region for d info
  # colors
  col.1 <- rgb(.63,.46,.15)
  col.1t <- rgb(.63,.46,.15, alpha=.5)
  col.2 <- rgb(.49,.56,.69)
  col.2t <- rgb(.49,.56,.69, alpha=.5)
  # set up coordinate system
  par(mar=c(3,3,8,.4), mgp=c(2,.6,0), cex.axis=1, cex.lab=1)
  plot.new()
  plot.window(xlim=c(min.x,max.x), ylim=c(0,max.y))
  axis(1); axis(2); box()
  title(xlab=Ynm, ylab="Density")
  xleft <- par("usr")[1]  # left side of graph
  xright <- par("usr")[2]  # right side of graph
  ybot <- par("usr")[3]  # bottom of graph
  ytop <- par("usr")[4]  # height of graph
  # vertical line for mean
  lines(c(m1,m1), c(0,ytop), lty="solid", lwd=2, col=col.1)
  lines(c(m2,m2), c(0,ytop), lty="twodash", lwd=2, col=col.2)
  # curve area
  polygon(c(min(dYA$x),dYA$x,max(dYA$x)), c(0,dYA$y,0), col=col.1t, border=NA, 
    density=10, angle=45)
  polygon(c(min(dYB$x),dYB$x,max(dYB$x)), c(0,dYB$y,0), col=col.2t, border=NA, 
    density=10, angle=-45)
  # bottom border of density curve  
  segments(min(dYA$x), 0, max(dYA$x), 0, col=col.1)
  segments(min(dYB$x), 0, max(dYB$x), 0, col=col.2)
  # density curve
  lines(dYA, col=col.1t, lty="solid", lwd=1.5)
  lines(dYB, col=col.2t, lty="twodash", lwd=1.5)
  # minimum mean difference of practical importance
  if ( !is.null(mmd) | !is.null(msmd) ) {
    col.e <- "gray50"  # color for effect
    mid <- (m1 + m2) / 2
    lr <- mid + .5*mmd  # line right
    ll <- mid - .5*mmd  # line left
    lines(c(lr,lr), c(ybot+.44*max.y,ytop-.44*max.y), lty="solid", lwd=2, col=col.e)
    lines(c(ll,ll), c(ybot+.44*max.y,ytop-.44*max.y), lty="solid", lwd=2, col=col.e)
    text(mid, ybot+.41*max.y, label=toString(round(mmd,2)), col=col.e)
    text(mid, ytop-.41*max.y, label=toString(round(msmd,2)), col=col.e)
    text(mid, ybot+.38*max.y, label="mmd", col=col.e)
    text(mid, ytop-.375*max.y, label="msmd", col=col.e)
  }
  # legends with descriptive stats (m1 > m2)
  textR <- paste(Xnm,X1nm);  nR <- n1;  mR <- m1.out;  sR <- s1.out;  col.R <- col.1;  aR <- 45
  textL <- paste(Xnm,X2nm);  nL <- n2;  mL <- m2.out;  sL <- s2.out;  col.L <- col.2;  aL <- -45
  col.lgnd <- "gray25"
  cex.lgnd <- .9
  radj <- xleft + .02*(max.x-min.x)
  legend("topleft", legend = textL, fill=col.L, density=20, angle=aL, bty="n",
    text.col=col.lgnd, cex=cex.lgnd)
  text(radj, ytop-.08*max.y, label=bquote(paste("n = ", .(nL))),  adj=0, col=col.lgnd, cex=cex.lgnd)
  text(radj, ytop-.12*max.y, label=bquote(paste("m = ", .(mL))),  adj=0, col=col.lgnd, cex=cex.lgnd)
  text(radj, ytop-.16*max.y, label=bquote(paste("s = ", .(sL))),  adj=0, col=col.lgnd, cex=cex.lgnd)
  ladj <- xright - .02*(xright-xleft)
  legend("topright", legend = textR, fill=col.R, density=20, angle=aR, bty="n",
    text.col=col.lgnd, cex=cex.lgnd)
  text(ladj, ytop-.08*max.y, label=bquote(paste("n = ", .(nR))), adj=1, col=col.lgnd, cex=cex.lgnd)
  text(ladj, ytop-.12*max.y, label=bquote(paste("m = ", .(mR))), adj=1, col=col.lgnd, cex=cex.lgnd)
  text(ladj, ytop-.16*max.y, label=bquote(paste("s = ", .(sR))), adj=1, col=col.lgnd, cex=cex.lgnd)
  # scale for s-pooled, d, mdiff at top of graph
  mlow <- min(m1, m2)
  mhi  <- max(m1, m2)
  col.d.unit <- "gray30"
  # connect first seg to top
  segments(mlow, max.y-.01*max.y, mlow, ytop, lwd=1, col=col.d.unit) 
  # provide at least 2 labeled d units on sd scale at top
  max.i <- max(ceiling(abs(smd)), 2)
  for (i in 0:max.i) {  # sd scale at top
    x.i <- mlow+i*sw
    # sd units
    segments(x.i, max.y+.025*max.y, x.i, ytop, col=col.d.unit, lwd=1)
    # d units counted
    text(x.i, max.y+.01*max.y, labels=i)
    # horiz bar connects endpoints
    segments(mlow, ytop, x.i, ytop, col=col.d.unit, lwd=4)
    last.coord.x <- x.i
  }
  # connect last seg to top
  segments(last.coord.x, max.y+.025*max.y, last.coord.x, ytop, lwd=1, col=col.d.unit)
  # print d value towards top
  text((m1+m2)/2, ytop-.07*max.y, label=smd.out)
  # horiz bar connects means
  segments(mlow, ytop-.09*max.y, mhi, ytop-.09*max.y, col=col.d.unit, lwd=2)
  # print d towards top
  text((m1+m2)/2, ytop-.11*max.y, label="smd")
  # print mdiff value towards bottom  
  text((m1+m2)/2, ybot+.11*max.y, label=mdiff.out)
  # horiz bar connects means
  segments(mlow, ybot+.09*max.y, mhi, ybot+.09*max.y, col=col.d.unit, lwd=2)
  # print diff towards bottom
  text((m1+m2)/2, ybot+.07*max.y, label="md")
  # title area, above graph
  mtext(paste("ODDSMD Plot"), side=3, line=6.6, font=2)
  mtext(paste("Compare",Ynm,"for",Xnm,X1nm,"and",X2nm), side=3, line=5.6, font=3)
  mtext(bquote(paste("    Classic t-test of 0 mean diff:   t = ", .(tvalue), 
    ",  df = ", .(df), ",   p-value = ", .(pvalue))), side=3, line=4.4, cex=1.08, adj=0)
  mtext(bquote(paste("    ",.(clpct), " Confidence Interval for Mean Difference:  ",
    .(lb), " to ", .(ub))), side=3, line=3.3, cex=1.08, adj=0)
  if (check.MBESS) {
    mtext(bquote(paste("    ",.(clpct), " Confidence Interval for Stnd Mean Diff:   ", 
      .(deltaL), " to ", .(deltaU))), side=3, line=2.1, cex=1.08, adj=0)
  }
  mtext(bquote(paste("s-within")), side=3, line=.9, at=(mlow+(last.coord.x))/2, col="gray40")
  mtext(bquote(paste(.(round(sw,2)))), side=3, line=.2, at=(mlow+(last.coord.x))/2, col="gray40")
}
  cat("\n")
  if ( (length(x) < 2) || (length(y) < 2) ) 
    stop("Need at least two observations per sample.")
  if ( !is.null(mmd) && !is.null(msmd) )
    stop("Specify only one of mmd and msmd as one implies the other.")
  # Always put the group with the largest mean first
  if (mean(x) > mean(y))
    ODDSMD(x, y)
  else {  # switch
    Xtmp <- X2nm
    X2nm <- X1nm
    X1nm <- Xtmp
    ODDSMD(y, x)
  }
  cat("\n")
}
smd.t.test.formula <-
function (formula, data, ...) {
  if ((length(formula) != 3) || (length(attr(terms(formula[-2]),"term.labels")) !=1)) 
      stop("'Formula' missing or incorrect.")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) 
      m$data <- as.data.frame(data)
  m[[1L]] <- as.name("model.frame")
  m$... <- NULL
  mf <- eval(m, parent.frame())
  Ynm <- names(mf)[1]
  Xnm <- names(mf)[2]
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  if (!is.numeric(mf[[response]])) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "You specified ", Ynm, " as the response variable, the 1st variable listed.\n",
    "The response variable must have only numeric values.\n",
    "The first value of ", Ynm, " is ", mf[[response]][1], ".\n",
    "Perhaps you have the order of the variables reversed.\n\n")
  }
  g <- factor(mf[[-response]])      
  gu <- unique(g)
  if (length(gu) != 2) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "Values of the grouping variable: ", levels(g), "\n",
    "Number of unique values: ", length(gu), "\n",
    "The grouping variable for a t-test must have exactly two unique values.\n\n")
  }
  DATA <- split(mf[[response]], g)
  names(DATA) <- c("Y1", "Y2")
  attach(DATA, warn.conflicts=FALSE)
  smd.t.test(Y1, Y2, Ynm=Ynm, Xnm=Xnm, X1nm=levels(gu)[1], X2nm=levels(gu)[2], ...)
}
smd.t.test <-
function(x, ...)  {
  # note the class of the object
  # then call the relevant method for that class
  # relevant methods are default and formula
  UseMethod("smd.t.test")
}
stats.t.test <-
function(n1 = NULL, n2 = NULL,  m1 = NULL, m2 = NULL, s1 = NULL, s2 = NULL, 
           Ynm = "Y", Xnm = "X", X1nm = "Group1", X2nm = "Group2", conf.level = 0.95,
           digits.d = 2, ...) {
  if (is.null(n1) | is.null(n2)) stop("Specify a sample size for each group.")
  if (is.null(m1) | is.null(m2)) stop("Specify a mean for each group.")
  if (is.null(s1) | is.null(s2)) stop("Specify a standard deviation for each group.")
  cat("\n")
  cat("------------------------------------------------------------\n")
  cat("Compare", Ynm, "across", Xnm, "levels", X1nm, "and", X2nm, "\n")
  cat("------------------------------------------------------------\n\n")
  cat("------ Description ------\n\n")
  v1 <- s1^2
  v2 <- s2^2
  clpct <- paste(toString(round((conf.level)*100, 2)), "%", sep="")
  m1.out <- round(m1,digits.d)
  m2.out <- round(m2,digits.d)
  s1.out <- round(s1,digits.d)
  s2.out <- round(s2,digits.d)
  Xnmval <- paste(Xnm, X1nm)
  cat(Ynm, " for ", Xnmval, ":  n = ", n1, ",   mean = ", m1.out, ",   sd = ", s1.out, sep="", "\n")
  Xnmval <- paste(Xnm, X2nm)
  cat(Ynm, " for ", Xnmval, ":  n = ", n2, ",   mean = ", m2.out, ",   sd = ", s2.out, sep="", "\n\n")
  # sw
  df1 <- n1 - 1
  df2 <- n2 - 1
  swsq <- (df1*v1 + df2*v2) / (df1 + df2)
  sw <- sqrt(swsq)
  sw.out <- round(sw,digits.d)
  cat("Equal Group Variances Assumed, Within-group Standard Deviation:  ", sw.out, "\n\n")
  # mean diff and standardized mean diff
  mdiff <- m1 - m2
  mdiff.out <- round(mdiff,digits.d)
  cat("Mean Difference of ", Ynm, ":  " , mdiff.out, sep="", "\n\n")
  # smd
  d <- mdiff/sw
  d.out <- round(d,digits.d)
  cat("Standardized Mean Difference of ", Ynm, ", Cohen's d:  ", d.out, sep="", "\n")
  cat("\n\n------ Homogeneity of Variance------\n\n")
  cat("Note:  This hypothesis test performs poorly in non-normal samples and", "\n")
  cat("       the t-test is typically robust to violations of assumptions.", "\n")
  cat("       Use as a heuristic guide instead of interpreting literally.", "\n\n")
  # Homogeneity of Variance
  if (v1 >= v2) {
    vratio <- v1/v2
    vr <- paste(toString(round(v1,digits.d+1)), "/", toString(round(v2,digits.d+1)), sep="")
    df.num <- df1
    df.den <- df2
  }
  else {
    vratio <- v2/v1
    vr <- paste(toString(round(v2,digits.d+1)), "/", toString(round(v1,digits.d+1)), sep="")
    df.num <- df2
    df.den <- df1
  }
  v.out <- round(vratio,digits.d+1)
  p.var <- pf(vratio, df1=df.num, df2=df.den)
  p.var <- 2 * min(p.var, 1-p.var)  # adjust for two-sided test, results same as var.test{stats}
  pv.out <- round(p.var,min(4,digits.d+1))
  cat("Null hypothesis is equal variances of ", Ynm, ", i.e., homogeneous.", sep="", "\n")
  cat("Variance Ratio test:  F = ", vr, " = ", v.out, ",  df = ", df.num, ";", df.den, ",  p-value = ", 
      pv.out, sep="", "\n")
  cat("\n\n------ Inference ------\n\n")
  # t-test
  df <- df1 + df2
  sterr <- sw * sqrt(1/n1 + 1/n2)
  tcut <- qt((1-conf.level)/2, df=df, lower.tail=FALSE)
  E <- tcut * sterr
  lb <- mdiff - E
  ub <- mdiff + E
  tvalue <- mdiff/sterr
  pvalue <- round(2 * pt(tvalue, df=df, lower.tail=FALSE), min(4,(digits.d+1)))
  tvalue <- round(tvalue, min(2,digits.d+1))
  cat("Standard Error of Mean Difference: SE = ", round(sterr,digits.d), "\n\n")
  cat("Hypothesis Test of 0 Mean Diff:  t = ", tvalue, ",  df = ", df, ",  p-value = ", pvalue, sep="", "\n\n")
  cat("Margin of Error for ", clpct, " Confidence Level:  ", round(E,digits.d), sep="", "\n")
  title <- " Confidence Interval for Mean Difference:  "
  cat(clpct, title, round(lb,digits.d), " to ", round(ub,digits.d), sep="", "\n\n")
  # smd confidence interval  
  check.MBESS <- suppressWarnings(require(MBESS, quietly=TRUE))
  if (check.MBESS) {
    cid <- ci.smd(smd=d, n.1=n1, n.2=n2, conf.level=conf.level)
    deltaL <- round(cid$Lower.Conf.Limit.smd,digits.d)
    deltaU <- round(cid$Upper.Conf.Limit.smd,digits.d)
    cat(clpct," Confidence Interval for smd:  ", deltaL, " to ", deltaU, sep="", "\n")
  }
  else {
    cat(">>> The confidence interval for smd requires package MBESS.", "\n")
    cat(">>> Confidence interval for smd not provided here, but all other output unaffected.", "\n")
    cat(">>> To get the MBESS package, run one time only: install.packages('MBESS')", "\n")
    cat(">>> IGNORE resulting 'Error in eval' error message below.", "\n")
  }
  cat("\n")
}
