.bx.default <-
function(x, col.box, col.pts, col.bg, col.grid,
        colors, cex.axis, col.axis, col.ticks,
        horiz, dotplot, xlab, main, digits.d, text.out, ...) {      

  # color palette based on color theme colors
  cp <- .clr(colors)
  if (is.null(col.box)) col.box <- cp[9]
  if (is.null(col.grid)) col.grid <- cp[3]
  if (is.null(col.bg)) col.bg <- cp[4]
  
  if (is.null(col.pts)) col.pts <- col.box

  # get variable label if exists
  gl <- .getlabels(xlab, main=main)
  x.name <- gl$xn;  x.lbl <- gl$xl;  x.lab <- gl$xb
  main.lab <- gl$mb
  if (horiz) y.lab <- ""
  else {
    y.lab <- x.lab
    x.lab <- ""
  }

  # set up plot area
  bv <- (boxplot(x, col="transparent", bg="transparent",
     horizontal=horiz, xlab=x.lab, ylab=y.lab, main=main.lab, 
     cex.axis=cex.axis, col.axis=col.axis, col.ticks=col.ticks, ...))
  
  # colored background for plotting area
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="black")
  
  # grid lines computation and print
  if (horiz) {
    vx <- pretty(c(usr[1],usr[2]))
    abline(v=seq(vx[1],vx[length(vx)],vx[2]-vx[1]), col=col.grid, lwd=.5)
  }
  else {
    vy <- pretty(c(usr[3],usr[4]))
    abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
  }
  # box plot
  boxplot(x, add=TRUE, col=col.box, bg=col.pts, pch=21, horizontal=horiz, axes=FALSE)

  # dots
  if (dotplot) 
      .dp.main(x, by=NULL,
         col.pts=NULL, col.fill=NULL, trans.pts=NULL, shape.pts=NULL,
         col.bg=NULL, col.grid=NULL, colors=colors,
         cex.axis=.85, col.axis="gray30",
         col.ticks="gray30", xlab=NULL, main=NULL, cex=NULL,
         pt.reg=21, pt.out=19, 
         col.out30="firebrick2", col.out15="firebrick4", 
         text.out=TRUE, new=FALSE, vertical=!horiz, ...)

  # summarize data
  if (text.out) {
    digits.d <- .max.dd(x)
    options(digits.d=digits.d)

    cat("\n")
    main.lab <- as.character(main.lab)
    if (nchar(main.lab) > 0) main.lab <- paste(",", main.lab)
    txt <- paste("--- ", x.lab, main.lab, " ---", sep="")
    cat(txt, "\n")
    cat("\n")
    cat("Present:", sum(!is.na(x)), "\n")
    cat("Missing:", sum(is.na(x)), "\n")
    cat("Total  :", length(x), "\n")
    cat("\n")
    cat("Minimum      :", .fmt(min(x, na.rm=TRUE)), "\n")
    cat("Lower Whisker:", .fmt(bv$stats[1]), "\n")
    cat("Lower Hinge  :", .fmt(bv$stats[2]), "\n")
    cat("Median       :", .fmt(bv$stats[3]), "\n")
    cat("Upper Hinge  :", .fmt(bv$stats[4]), "\n")
    cat("Upper Whisker:", .fmt(bv$stats[5]), "\n")
    cat("Maximum      :", .fmt(max(x, na.rm=TRUE)), "\n")
    cat("\n")
    cat("1st Quartile :", .fmt(quantile(x, na.rm=TRUE)[2]), "\n")
    cat("3rd Quartile :", .fmt(quantile(x, na.rm=TRUE)[4]), "\n")
    cat("IQR          :", .fmt(IQR(x, na.rm=TRUE)), "\n")

    outliers <- boxplot.stats(x)$out
    if (length(outliers > 0)) {
      cat("\nOutlier")
      if (length(outliers) > 1) cat("s: ") else cat(": ")
      for (i in 1:length(outliers)) cat(.fmt(outliers[i]), " ")
      cat("\n")
    } 
  }

  cat("\n")

}
