.plt.legend <-
function(colnms, horiz, color, fill, shape, col.bg, usr, lab.cex=0.8) {

  par(xpd=NA) 
  
  # text color
  the.clr <- getOption("lab.color")

  text.cex <- ifelse(is.null(getOption("axis.x.cex")),
      getOption("axis.cex"), getOption("axis.x.cex"))
  if (text.cex > 0.99) text.cex <- .83 * text.cex

  ll <- legend(0,0, legend=colnms, cex=.7, pt.cex=0.9,
               horiz=TRUE, plot=FALSE)  # get coordinates

  if (horiz) {

    size <- (par("cxy")/par("cin"))  # 1 inch in user coordinates, [2] is y 

    bm.user <- par("mai")[1] * size[2]  # size of bot margin in user coords
    # move down, /5 is for the existing line of axis numbers
    ytop <- usr[3] - (bm.user - (2.6 * bm.user/5))
    # ytop <- usr[3] - (bm.user - bm.user/5 + ll$rect$h) 

    axis.horiz <- usr[2] - usr[1]
    lgnd.hhalf <- (ll$rect$w) / 2
    xleft <- usr[1] + axis.horiz/2 - lgnd.hhalf

    legend(xleft, ytop, legend=colnms, horiz=TRUE, box.lwd=.5, 
           box.col="gray30", cex=text.cex, pt.cex=1.2, pt.bg=fill, bg=col.bg,
           col=color, pch=shape, text.col=the.clr) # display legend
  }

  else {  # vertical

    ll <- legend(0,0, legend=colnms, cex=.7, pt.cex=0.9,
                 horiz, plot=FALSE)  # get coordinates
    size <- (par("cxy")/par("cin"))  # 1 inch in user coordinates 

    #dv <- ifelse (options("device") == "RStudioGD", 1, 3)
    epsilon <- (size[1] - ll$rect$w) / 2

    axis.vert <- usr[4] - usr[3]
    xleft <- usr[2] + epsilon   # usr[2] user coordinate of right axis
    lgnd.vhalf <- ll$rect$h / 2
    axis.cntr <- axis.vert / 2  + usr[3]
    ytop <- axis.cntr + lgnd.vhalf  # user coordinate of legend top

    xi <- ifelse (options("device") == "RStudioGD", 1.4, 1)
    yi <- ifelse (options("device") == "RStudioGD", 1.4, 1)

    legend(xleft, ytop, legend=colnms, horiz=FALSE, box.lwd=.5, 
           box.col="gray30", cex=text.cex, pt.cex=1.2, pt.bg=fill, bg=col.bg,
           col=color, pch=shape, text.col=the.clr,
           x.intersp=xi, y.intersp=yi)  # display legend
  }

  par(xpd=FALSE)  # cancel drawing outside of plot region (need for RStudio)

}
