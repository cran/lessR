.plt.legend <-
function(colnms, horiz, stroke, fill, shape, usr) {

  par(xpd=NA) 
  
  ll <- legend(0,0, legend=colnms, cex=.7, pt.cex=0.9,
               horiz=TRUE, plot=FALSE)  # get coordinates

  if (horiz) {

    mlt <- 1.25
    # kludge to get RStudio box wider 
    if (options("device") == "RStudioGD" && .Platform$OS != "windows") {
      colnms[length(colnms)] <- paste(colnms[length(colnms)], "     ", sep="")
      mlt <- 2
    #mlt <- ifelse (options("device") == "RStudioGD", 2, 1.25)
    } 
    ytop <- usr[3] - (mlt * ll$rect$h) 

    axis.horiz <- usr[2] - usr[1]
    lgnd.hhalf <- (ll$rect$w) / 2
    xleft <- usr[1] + axis.horiz/2 - lgnd.hhalf

    legend(xleft, ytop, legend=colnms, horiz=TRUE, box.lwd=.5, 
           box.col="gray30", cex=.7, pt.cex=1, pt.bg=fill,
           col=stroke, pch=shape)  # display legend
  }

  else {  # vertical

    # kludge to get RStudio box wider 
    if (options("device") == "RStudioGD" && .Platform$OS != "windows") {
      max.width <- which(nchar(colnms) == max(nchar(colnms)))
      colnms[max.width] <- paste(colnms[max.width], "     ", sep="")
    }

    ll <- legend(0,0, legend=colnms, cex=.7, pt.cex=0.9,
                 horiz, plot=FALSE)  # get coordinates
    size <- (par("cxy")/par("cin"))  # 1 inch in user coordinates 

    dv <- ifelse (options("device") == "RStudioGD", 1, 3)
    epsilon <- (size[1] - ll$rect$w) / dv 

    axis.vert <- usr[4] - usr[3]
    xleft <- usr[2] + epsilon   # usr[2] user coordinate of right axis
    lgnd.vhalf <- ll$rect$h / 2
    axis.cntr <- axis.vert / 2  + usr[3]
    ytop <- axis.cntr + lgnd.vhalf  # user coordinate of legend top

    xi <- ifelse (options("device") == "RStudioGD", 1.4, 1)
    yi <- ifelse (options("device") == "RStudioGD", 1.4, 1)
    legend(xleft, ytop, legend=colnms, horiz=FALSE, box.lwd=.5, 
           box.col="gray30", cex=.7, pt.cex=1, pt.bg=fill,
           col=stroke, pch=shape, x.intersp=xi, y.intersp=yi)  # display legend
  }

}
