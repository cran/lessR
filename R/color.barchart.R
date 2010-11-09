color.barchart <- 
function(x, y=NULL, col.bars=NULL, border="black", col.bg="seashell", 
         col.grid="grey90", beside=FALSE, over.grid=FALSE, 
         legend.loc=NULL, legend.labels=NULL, vivid=FALSE, 
         random=FALSE, addtop=1, ...) {
           

  clr <- character(length = 7)
  if (vivid == FALSE)
    clr <- c("lightsteelblue", "moccasin", "salmon", "palegreen", "thistle", "turquoise", "yellow2")
  else {
    clr <- c("sienna3", "dodgerblue3", "maroon3", "seagreen3", "turquoise3", "purple3", "yellow3")
    col.bg <- "cornsilk1"
  }
  if (!is.null(col.bars)) for (i in 1:(min(length(col.bars),length(clr)))) clr[i] <- col.bars[i]
  if (random == TRUE) clr <- clr[sample(length(clr))]
  palette(clr)
   
  if (!is.null(y)) x <- table(y,x)
  if (is.factor(x)) x <- table(x)
  
  if (is.matrix(x)) {
    col <- 1:nrow(x)
    if (is.null(legend.labels)) legend.labels <- row.names(x)
  }
  else col <- 1
  
  if (!is.null(legend.labels)) if (is.null(legend.loc)) legend.loc <- "top"
  
  if (is.matrix(x) && beside==FALSE) max.y <- max(colSums(x)) else max.y <- max(x)
  max.y <- max.y + addtop
  
  # set up plot area, color background, grid lines
  barplot(x, col="transparent", border=NA, ylim=c(0,max.y), beside=beside, ...)	
  usr <- par("usr");  rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="black")
  vy <- pretty(0:max.y)
  
  # plot the bar plot, grid lines and legend
  if (over.grid == FALSE) abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid)
  barplot(x, add=TRUE, col=col, beside=beside, ...)
  if (over.grid == TRUE) abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid)
  if (!is.null(legend.loc)) legend(legend.loc, legend=legend.labels, fill=col)

}

