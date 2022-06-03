# plot legend for by variables
.plt.by.legend <-
function(mylevels, color, fill, shp, trans_pts, col.bg, usr,
         pt.size=1.25, pt.lwd=0.5, legend_size=NULL,
         legend_abbrev=NULL, legend_adj=0) {

  par(xpd=NA)  # allow drawing outside of plot region

  n.levels <- length(mylevels)
  legend_title <- getOption("byname")

  if (is.null(legend_abbrev))
    legend_labels <- mylevels
  else {
    if (!is.null(mylevels))
      legend_labels <- abbreviate(mylevels, legend_abbrev)
  }

  # abbreviate title if too large
  if (!is.null(legend_abbrev))
    legend_title  <- abbreviate(legend_title, legend_abbrev)
  mx.ch <- max(c(max(nchar(legend_labels)), nchar(legend_title)-2))

  ll <- legend(0,0, legend=legend_labels, title=legend_title, cex=.7,
               pt.cex=pt.size, pt.lwd=pt.lwd, plot=FALSE)

  size <- (par("cxy")/par("cin"))  # 1 inch in user coordinates 

  epsilon <- (size[1] - ll$rect$w) / 2
  if (epsilon < 0) epsilon <- .04  # do not have label overlap plot

  axis_vert <- usr[4] - usr[3]
  xleft <- usr[2] + epsilon - .01 # usr[2] user coordinate of right axis
  if (mx.ch > 7) xleft <- xleft - .02  # shift legend left a bit   
  lgnd.vhalf <- (ll$rect$h) / 2
  axis_cntr <- axis_vert / 2  + usr[3]
  ytop <- axis_cntr + lgnd.vhalf  # user coordinate of legend top

  if (trans_pts > 0.85) {  # points too light, reduce legend transparency
    legend_fill <- integer(length=n.levels)
    for (i in 1:n.levels) legend_fill[i] <- .maketrans(color[i],.7)
  }
  else 
    legend_fill <- fill

  the.clr <- getOption("lab_color")

  yi <- 0.95

  if (is.null(legend_size)) {
    axis_x_cex <- ifelse(is.null(getOption("axis_x_cex")), 
      getOption("axis_cex"), getOption("axis_x_cex"))
    adj <- .RSadj(axis_cex=axis_x_cex); axis_x_cex <- adj$axis_cex
    legend_size <- axis_x_cex
  }

  legend_size <- 1.1 * legend_size

  # fill=length(legend_labels):1  puts the legend labels in the correct
  #   order, but only for inflexible boxes that cannot be resized with pt.cex
  legend(xleft+legend_adj, ytop, legend=legend_labels, title=legend_title, 
         pch=shp, horiz=FALSE, cex=legend_size, pt.cex=pt.size, pt.lwd=pt.lwd,
         bg=col.bg, col=color, pt.bg=fill,
         text.col=the.clr, y.intersp=yi, bty="n")

  par(xpd=FALSE)  # cancel drawing outside of plot region (need for RStudio)

}
