bx.default <-
function(x, col.box="lightsteelblue", col.pts=NULL, 
        col.bg="ghostwhite", col.grid="grey85",
        horiz=TRUE, dotplot=FALSE, mag.axis=.85,
        xlab=NULL, main=NULL, digits.d=10, ...) {        

  
  if (is.null(xlab)) x.lab <- x.name else x.lab <- xlab
  if (horiz) y.lab=""
  else {
    if (is.null(xlab)) y.lab <- x.name else y.lab <- xlab
    x.lab=""
  }
  # use variable label for main if it exists and main not specified
  if (!is.null(main)) main.lbl <- main
  else {
    main.lbl <- ""
    if (exists("mylabels")) {
      lbl <- mylabels[which(row.names(mylabels)==x.name), "label"]
      if (length(lbl) > 0) main.lbl <- lbl
    }
  }
  
  if (is.null(col.pts)) col.pts <- col.box
 
  # set up plot area
  op <- options()  # save current options to reset later
  options(scipen=30) # turn off scientific notation
  bv <- (boxplot(x, col="transparent", bg="transparent",
     horizontal=horiz, xlab=x.lab, ylab=y.lab, main=main.lbl, 
     cex.axis=mag.axis, ...))
  options(op)
  
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
  if (dotplot) dots(x, new=FALSE, vertical=!horiz, ...)
 
  # summarize data
  cat("\n")
  main.lbl <- as.character(main.lbl)
  if (nchar(main.lbl) > 0) main.lbl <- paste(",", main.lbl)
  txt <- paste("--- ", x.lab, main.lbl, " ---", sep="")
  cat(txt, "\n")
  cat("\n")
  cat("Present:", sum(!is.na(x)), "\n")
  cat("Missing:", sum(is.na(x)), "\n")
  cat("Total  :", length(x), "\n")
  cat("\n")
  cat("Minimum      :", format(signif(min(x, na.rm=TRUE),digits.d), scientific=FALSE), "\n")
  cat("Lower Whisker:", format(signif(bv$stats[1],digits.d), scientific=FALSE), "\n")
  cat("Lower Hinge  :", format(signif(bv$stats[2],digits.d), scientific=FALSE), "\n")
  cat("Median       :", format(signif(bv$stats[3],digits.d), scientific=FALSE), "\n")
  cat("Upper Hinge  :", format(signif(bv$stats[4],digits.d), scientific=FALSE), "\n")
  cat("Upper Whisker:", format(signif(bv$stats[5],digits.d), scientific=FALSE), "\n")
  cat("Maximum      :", format(signif(max(x, na.rm=TRUE),digits.d), scientific=FALSE), "\n")
  cat("\n")
  cat("1st Quartile :", format(signif(quantile(x, na.rm=TRUE)[2],digits.d), scientific=FALSE), "\n")
  cat("3rd Quartile :", format(signif(quantile(x, na.rm=TRUE)[4],digits.d), scientific=FALSE), "\n")
  cat("IQR          :", format(signif(IQR(x, na.rm=TRUE),digits.d), scientific=FALSE), "\n")

  outliers <- boxplot.stats(x)$out
  if (length(outliers > 0)) {
    cat("\nOutlier")
    if (length(outliers) > 1) cat("s: ") else cat(": ")
    for (i in 1:length(outliers)) cat(format(outliers[i], scientific=FALSE), " ")
    cat("\n")
  } 

  cat("\n")
}


