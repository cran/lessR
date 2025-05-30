.plt.legend <-
function(colnms, horiz, color, fill, shape, box.bg, usr, lab_cex=0.8,
         pt.size=1.25, legend_title=NULL, line_type="solid") {
# legend will have points or lines, shape="" for lines

  par(xpd=NA) 
  
  # text color
  the.clr <- getOption("lab_color")

  if (missing(lab_cex)) {
    text.cex <- ifelse(is.null(getOption("axis_x_cex")),
        getOption("axis_cex"), getOption("axis_x_cex"))
    text.cex <- 1.1 * text.cex
  }
  else
    text.cex <- lab_cex
  text.cex <- .85 * text.cex
  if (text.cex < getOption("axis_cex")) text.cex <- getOption("axis_cex")
  legend_title_size <- 1.1 * text.cex

  if (horiz) {

    ll <- legend(0,0, legend=colnms, cex=.7, pt.cex=0.9,
                 horiz=TRUE, plot=FALSE)  # get coordinates
    size <- (par("cxy")/par("cin"))  # 1 inch in user coordinates, [2] is y 

    bm.user <- par("mai")[1] * size[2]  # size of bot margin in user coords
    # move down, /5 is for the existing line of axis numbers
    ytop <- usr[3] - (bm.user - (2.6 * bm.user/5))
    # ytop <- usr[3] - (bm.user - bm.user/5 + ll$rect$h) 

    axis_horiz <- usr[2] - usr[1]
    lgnd.hhalf <- (ll$rect$w) / 2
    xleft <- usr[1] + axis_horiz/2 - lgnd.hhalf

    legend(xleft, ytop, legend=colnms, horiz=TRUE, box.lwd=.5, 
           box.col="transparent", cex=text.cex, pt.cex=pt.size,
           title.cex=legend_title_size,
           pt.bg=fill, bg=box.bg,
           col=color, pch=shape, text.col=the.clr, title=legend_title) 
  }

  else {  # vertical

    ll <- legend(0,0, legend=colnms, cex=.7, pt.cex=0.9,
                 horiz, plot=FALSE)  # get coordinates
    size <- (par("cxy")/par("cin"))  # 1 inch in user coordinates 
    #dv <- ifelse (options("device") == "RStudioGD", 1, 3)
    epsilon <- (size[1] - ll$rect$w) / 2
    epsilon <- epsilon * .75

    axis_vert <- usr[4] - usr[3]
    xleft <- usr[2] + epsilon   # usr[2] user coordinate of right axis
    lgnd.vhalf <- ll$rect$h / 2
    axis_cntr <- axis_vert / 2  + usr[3]
    ytop <- axis_cntr + lgnd.vhalf  # user coordinate of legend top

#   xi <- ifelse (options("device") == "RStudioGD", 1.4, 1)
#   yi <- ifelse (options("device") == "RStudioGD", 1.4, 1)

    # display legend
    # lwd needed to draw a line
    # rev() function lists the stacked plots in the order they are plotted
    legend(xleft, ytop, legend=rev(colnms), horiz=FALSE, lwd=1.5, lty=line_type,
           box.lwd=.5, box.col="transparent", cex=text.cex, pt.cex=pt.size,
           title.cex=legend_title_size,
           pt.bg=rev(fill), bg=box.bg,
           col=rev(color), pch=shape, text.col=the.clr,
           x.intersp=1, y.intersp=1, title=legend_title) 
  }

  par(xpd=FALSE)  # cancel drawing outside of plot region (need for RStudio)

}
