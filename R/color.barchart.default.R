color.barchart.default <- 
function(x, y=NULL, col.bars=NULL, border="black", 
         col.bg="seashell", col.grid="grey90", beside=TRUE, 
         over.grid=FALSE, prop=FALSE, xlab=NULL, legend.title=NULL, 
         legend.loc=NULL, legend.labels=NULL, legend.horiz=FALSE, 
         vivid=FALSE, random.col=FALSE, col.low="slategray2", 
         col.hi="slategray4", addtop=1, horiz=FALSE, chisq=FALSE, ...) {
         
  dash <- function(n.dash) { for (i in 1:(n.dash)) cat("-"); cat("\n") }

         
  if (chisq && prop) stop("\nChi-square analysis here not valid for proportions.\n\n")
  if (!is.null(y) && prop) stop("\nAnalysis of proportions not valid for two variables.\n\n")

  # variable labels
  if (is.null(xlab)) x.lbl <- deparse(substitute(x)) else x.lbl <- xlab
  if (!is.null(legend.title)) y.lbl <- legend.title 
    else if (!is.null(y)) y.lbl <- deparse(substitute(y)) else y.lbl=NULL
  
  # entered counts typically integers as entered but stored as type double
  if (!is.integer(x) && is.double(x)) entered <- TRUE else entered <- FALSE
  
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
      clr <- c("slategray3", "bisque3", "darksalmon", "darkolivegreen3", 
        "thistle", "azure3", "moccasin")
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
  
  # set up plot area, color background, grid lines
  if (!horiz) 
   barplot(x, col="transparent", border=NA, ylim=c(0,max.y), beside=beside, 
    axisnames=FALSE, ...)
  else
   barplot(x, col="transparent", border=NA, beside=beside, axisnames=FALSE, 
    horiz=TRUE, font.main=1, ...)  
  usr <- par("usr");  rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="black")
  if (max.y > 1) vy <- pretty(0:max.y) else vy <- pretty(1:100*max.y)/100
  
  # bar plot, grid lines and legend
  if (!over.grid) abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid)
  barplot(x, add=TRUE, col=col, beside=beside, horiz=horiz, xlab=x.lbl, ...)
  if (over.grid) abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid)
  if ((!is.null(y) || is.matrix(x)) && !is.null(legend.loc)) 
    legend(legend.loc, legend=legend.labels, title=y.lbl, fill=col, horiz=legend.horiz)
  
  # print table, chi-square analysis
  if (!is.null(y) || is.matrix(x)) {  # two variables
    cat("\n"); dash(30); cat("Joint and Marginal Frequencies\n"); dash(30); 
      print(addmargins(x))
    cat("\n"); dash(30); cat("Cell Proportions and Marginals\n"); dash(30); 
      print(round(addmargins(prop.table(x)),3))
      cat("\n"); dash(29); cat("Propotions within Each Column\n"); dash(29);
      x.col <- prop.table(x, margin=2)
      Sum <- double(ncol(x.col))
      for (i in 1:ncol(x.col)) Sum[i] <- sum(x.col[,i])
      print(round(rbind(x.col,Sum),3))
    cat("\n"); dash(26); cat("Propotions within Each Row\n"); dash(26); 
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

