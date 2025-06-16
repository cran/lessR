# plot legend for size variables, i.e., bubbles
.plt.sym.legend <-
function(size, color, fill, shp, col.bg, usr, radius, power) {

  min.size <- min(size, na.rm=TRUE)
  max.size <- max(size, na.rm=TRUE)
  raw.vals <- seq(min.size, max.size, length.out=3)  # 3 evenly spaced values
  mylevels <- round(raw.vals)

  par(xpd=NA)  # allow drawing outside of plot region

  ll <- legend(0,0, legend=mylevels, title=getOption("sizename"), cex=.7,
               pt.cex=1.25, pt.lwd=0.5, plot=FALSE)

  size.inch <- (par("cxy")/par("cin"))  # 1 inch in user coords for x and y axes

  epsilon <- (size.inch[1] - ll$rect$w) / 2   # x size
  if (epsilon < 0) epsilon <- .04  # do not have label overlap plot

  nch <- nchar(getOption("sizename"))
  hz.buf <- ifelse (nchar(getOption("sizename")) < 13, 1.4, 4.7)
  hz.buf <- -2.23 + (0.467*nch)
  hz.buf <- 1.544 + (0.00014*(nch^3.8))

  axis_vert <- usr[4] - usr[3]  # range of y-axis in user coords
  xleft <- usr[2] + epsilon*hz.buf # usr[2] user coordinate of right axis
  lgnd.vhalf <- (ll$rect$h) / 2
  axis_cntr <- axis_vert / 2  + usr[3]
  ytop <- axis_cntr + lgnd.vhalf  # user coordinate of legend top

  # scale for regular R or RStudio
  adj <- .RSadj(radius=radius)  # reg R multiply by 1.6
  radius <- adj$radius

  x.coords <- rep(xleft, 3)  # in user coordinates
  y.coords <- (ytop + size.inch[2]*.30) + (ll$text$y * 1.8)  # y size

  sz <- mylevels**power  # radius unscaled for all the points
  symbols(x.coords, y.coords, circles=sz, inches=radius,
          fg=color, bg=fill, add=TRUE)

  # labels text and title
  x.offset <- strwidth("00") * 1.2 
  text(x.coords + x.offset, y.coords, labels=mylevels, adj=0,
       cex=getOption("axis_cex")*.95)
  text(xleft+(x.offset/1.25), max(y.coords) + strheight("X")*2,
       labels=getOption("sizename"), cex=getOption("lab_cex")*.9)

  par(xpd=FALSE)  # cancel drawing outside of plot region (need for RStudio)
}
