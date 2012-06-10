.dp.main <- 
function(x, 
         col.pts, col.fill, trans.pts, col.bg, col.grid, colors,
         cex.axis, col.axis, col.ticks, xlab, main, 
         pt.reg, pt.out, 
         col.out30, col.out15, text.out, new, ...) {

  old.opt <- options()
  on.exit(options(old.opt))

  if (!is.null(trans.pts)) options(trans=trans.pts)
  else if (is.null(getOption("trans"))) options(trans=0.66)  # default

  # color palette based on color theme colors
  cp <- .clr(colors)
  if (is.null(col.pts)) col.pts <- cp[5]
  if (is.null(col.fill)) col.fill <- cp[6]
  if (is.null(col.grid)) col.grid <- cp[3]
  if (is.null(col.bg)) col.bg <- cp[4]

  if (colors == "gray") {
    col.out30 <- "black"
    col.out15 <- "gray30"
  }

  # get variable labels if exist plus axes labels
  gl <- .getlabels(xlab, main=main)
  x.name <- gl$xn; x.lbl <- gl$xl; x.lab <- gl$xb
  main.lab <- gl$mb

  # text output (before remove missing)
  if (text.out && new) .ss.numeric(x, brief=TRUE)

  n <- sum(!is.na(x))
  n.miss <- sum(is.na(x))
  if (n.miss > 0) x <- na.omit(x)
 
  if (new) {
    
    # set up plot area
    suppressWarnings(stripchart(x, col="transparent", xlab=x.lab,
       main=main.lab, axes=FALSE, ...))
    op <- options()  # save current options to reset later
    options(scipen=30) # turn off scientific notation
    suppressWarnings(axis(1,
       cex.axis=cex.axis, col.axis=col.axis, col.ticks=col.ticks, ...))
    options(op)
    
    # colored background for plotting area
    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="black")
    
    # grid lines computation and print
    vx <- pretty(c(usr[1],usr[2]))
    abline(v=seq(vx[1],vx[length(vx)],vx[2]-vx[1]), col=col.grid, lwd=.5)
  }

  # mark outliers
  q1 <- quantile(x, probs=0.25)
  q3 <- quantile(x, probs=0.75)
  lo30 <- q1 - 3.0*IQR(x)
  lo15 <- q1 - 1.5*IQR(x)
  up15 <- q3 + 1.5*IQR(x)
  up30 <- q3 + 3.0*IQR(x)
  stripchart(x[x<lo30], add=TRUE, method="stack", col=col.out30, pch=pt.out, ...)
  stripchart(x[x<lo15], add=TRUE, method="stack", col=col.out15, pch=pt.out, ...)
  stripchart(x[x>up15], add=TRUE, method="stack", col=col.out15, pch=pt.out, ...)
  stripchart(x[x>up30], add=TRUE, method="stack", col=col.out30, pch=pt.out, ...)

  # dp for regular points
  suppressWarnings(stripchart(x[x>lo15 & x<up15], add=TRUE, method="stack",
                   col=col.pts, pch=pt.reg, bg=col.fill, ...))

  cat("\n")

}
