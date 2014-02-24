.bx.main <-
function(x, col.fill, col.stroke, col.bg, col.grid,
         cex.axis, col.axis, col.ticks,
         horiz, add.points, xlab, main, digits.d, quiet, ...) {      

  if (is.null(col.stroke)) col.stroke <- col.fill

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
     horizontal=horiz, xlab=x.lab, ylab=y.lab, main=main.lab, axes=FALSE, ...))
  if (horiz)
    axis(1, cex.axis=cex.axis, col.axis=col.axis, col.ticks=col.ticks) 
  else
    axis(2, cex.axis=cex.axis, col.axis=col.axis, col.ticks=col.ticks) 
  
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
  boxplot(x, add=TRUE, col=col.fill, bg=col.stroke, pch=21,
          horizontal=horiz, axes=FALSE, border=col.stroke, ...)
  #boxplot(x, add=TRUE, col=col.fill, bg=col.stroke, pch=21,
          #horizontal=horiz, axes=FALSE, border=col.stroke,
          #whiskcol=getOption("col.fill.bar"), staplecol=getOption("col.fill.bar"),
          #medcol=getOption("col.stroke.bar"))

  # dots
  if (add.points) 
      .dp.main(x, by=NULL,
         col.fill, col.stroke, col.bg, col.grid, shape.pts=NULL,
         cex.axis=.85, col.axis="gray30",
         col.ticks="gray30", xlab=NULL, main=NULL, cex=NULL,
         method="stack", pt.reg=21, pt.out=19, 
         col.out30="firebrick2", col.out15="firebrick4", 
         quiet=TRUE, new=FALSE, vertical=!horiz, ...)

  # summarize data
  if (!quiet) {
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

    # outlier analysis
    .outliers(x)

    cat("\n")
  }

  return(bv)

}

