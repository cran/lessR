.dp.main <- 
function(x, by,
         col.pts, col.fill, trans.pts, shape.pts,
         col.bg, col.grid, colors,
         cex.axis, col.axis, col.ticks, xlab, main, cex,
         pt.reg, pt.out, 
         col.out30, col.out15, text.out, new, ...) {


  if (is.null(cex)) pt.size <- 0.8 else pt.size <- cex

  if (is.factor(x)) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "The variable cannot be an R factor (categorical).\n")
  }

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
  by.name <- getOption("byname")

  # text output (before remove missing)
  if (text.out && new) .ss.numeric(x, brief=TRUE)

  n <- sum(!is.na(x))
  n.miss <- sum(is.na(x))
  if (n.miss > 0) x <- na.omit(x)
 
  if (new) {
    # set up plot area

    if (!is.null(by)) par(omi=c(0,0,0,0.6))  # legend in right margin

    suppressWarnings(stripchart(x, col="transparent", xlab=x.lab,
       main=main.lab, axes=FALSE, ...))
    suppressWarnings(axis(1,
       cex.axis=cex.axis, col.axis=col.axis, col.ticks=col.ticks, ...))
    
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
  if (is.null(by)) 

    suppressWarnings(stripchart(x[x>lo15 & x<up15], add=TRUE, method="stack",
                     col=col.pts, pch=pt.reg, bg=col.fill, ...))

    else {  # by grouping variable
      n.levels <- nlevels(by)

      clr <- character(length(n.levels))
      if (length(col.pts) == 1) 
        for (i in 1:n.levels) clr[i] <- col.pts
      else
        clr <- col.pts
      clr.tr <- clr

      shp <- integer(length(n.levels))
      if (length(shape.pts) == 1)
        for (i in 1:n.levels) shp[i] <- shape.pts
      else
         shp <- shape.pts
      shape.dft <- c(21,23,22,24,25,7:14)  # shape defaults
      if (length(col.pts)==1 && length(shape.pts)==1)  # both shape and color default
        for (i in 1:n.levels) shp[i] <- shape.dft[i]  # fill with default shapes

      for (i in 1:n.levels) {
        clr.tr[i] <- .maketrans(clr.tr[i], trans.pts)
        x.lv <- subset(x, by==levels(by)[i])
        stripchart(x.lv, pch=shp[i], col=clr[i], bg=clr.tr[i], 
               cex=pt.size, lwd=0.75, add=TRUE, ...)
      }
      cat("\nTransparency level for plotted points: ", trans.pts, "\n")

      .plt.legend(levels(by), col.pts, clr, clr.tr, shp, trans.pts, col.bg, usr)

    }  # end by group



  cat("\n")

}
