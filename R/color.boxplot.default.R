color.boxplot.default <-
function(x, col.box="bisque2", col.point=NULL, 
         horizontal=TRUE, xlab=NULL, digits.d=10, ...) {        

  dashes <- function(ndash) { for (i in 1:(ndash)) cat("-"); cat("\n") }
  
  if (is.null(xlab)) x.lbl <- deparse(substitute(x)) else x.lbl <- xlab
  
  if (is.null(col.point)) col.point <- col.box
    
  # boxplot
  bv <- boxplot(x, col=col.box, bg=col.point, pch=21, horizontal=horizontal, xlab=x.lbl, ...)
  
  # summarize data
  cat("\n")
  dashes(30)
  cat("Data Summary:", x.lbl, "\n")
  dashes(30)
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
  cat("\n")
  
}
