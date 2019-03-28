# plot legend for by variables

.plt.by.legend <-
function(mylevels, color, fill, shp, trans.pts, col.bg, usr,
         pt.size=1.25, pt.lwd=0.5, legend.size=NULL) {

  par(xpd=NA)  # allow drawing outside of plot region

  n.levels <- length(mylevels)
  by.name <- getOption("byname")

  legend.labels <- abbreviate(mylevels,10)

  wt <- 10
  legend.title  <- abbreviate(by.name, wt)

  ll <- legend(0,0, legend=legend.labels, title=legend.title, cex=.7,
               pt.cex=pt.size, pt.lwd=pt.lwd, plot=FALSE)

  size <- (par("cxy")/par("cin"))  # 1 inch in user coordinates 

  epsilon <- (size[1] - ll$rect$w) / 2

  axis.vert <- usr[4] - usr[3]
  xleft <- usr[2] + epsilon  # usr[2] user coordinate of right axis
  lgnd.vhalf <- (ll$rect$h) / 2
  axis.cntr <- axis.vert / 2  + usr[3]
  ytop <- axis.cntr + lgnd.vhalf  # user coordinate of legend top

  if (trans.pts > 0.85) {  # points too light, reduce legend transparency
    legend.fill <- integer(length=n.levels)
    for (i in 1:n.levels) legend.fill[i] <- .maketrans(color[i],.7)
  }
  else 
    legend.fill <- fill

  the.clr <- getOption("lab.color")

  yi <- 0.95

  if (is.null(legend.size)) {
    axis.x.cex <- ifelse(is.null(getOption("axis.x.cex")), 
      getOption("axis.cex"), getOption("axis.x.cex"))
    adj <- .RSadj(axis.cex=axis.x.cex); axis.x.cex <- adj$axis.cex
    legend.size <- axis.x.cex
  }

  # fill=length(legend.labels):1  puts the legend labels in the correct
  #   order, but only for inflexible boxes that cannot be resized with pt.cex
  legend(xleft, ytop, legend=legend.labels, title=legend.title, 
         pch=shp, horiz=FALSE, cex=legend.size, pt.cex=pt.size, pt.lwd=pt.lwd,
         bg=col.bg, col=color, pt.bg=fill,
         text.col=the.clr, y.intersp=yi, bty="n")

  par(xpd=FALSE)  # cancel drawing outside of plot region (need for RStudio)

}
