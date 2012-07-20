.cr.default <-
function(x, y, brief, ...) {

  # get variable labels if exist
  gl <- .getlabels()
  x.name <- gl$xn; x.lbl <- gl$xl;
  y.name <- gl$yn; y.lbl <- gl$yl

  if (!is.factor(x)) {

    ct <- cor.test(x,y, ...)
    if (ct$method == "Pearson's product-moment correlation") {

      if (!brief) {
        cat("\n")
        .dash(55)
        cat("Correlation Analysis for Variables", x.name, "and", y.name, "\n")
        .dash(55)
      }
      cat("\n")
      if (!is.null(x.lbl)) cat(x.name, ", ", as.character(y.lbl), sep="", "\n")
      if (!is.null(y.lbl)) cat(y.name, ", ", as.character(x.lbl), sep="", "\n")
      if (!is.null(x.lbl) || !is.null(y.lbl)) cat("\n")
     

      if (!brief) {
        n.pair <- sum(!is.na(x - y))  # number of points after pairwise deletion
        n.del <- sum(is.na(x - y))  # number of pairwise deleted observations
        cat("Number of paired values with neither missing, n:", n.pair, "\n")
        cat("Number of observations (rows of data) deleted:", n.del, "\n\n")
        covr <- cov(x, y, use="pairwise.complete.obs")
        cat("Sample Covariance: cov =", .fmt(covr,3), "\n\n")
      }

      cat("Sample Correlation of ", x.name, " and ", y.name,
          ": r = ", .fmt(ct$estimate,3), sep="", "\n\n")
      cat("Hypothesis Test that Population Correlation is Zero", "\n")
      cat("  t-value: ", .fmt(ct$statistic,3), ",  df: ", ct$parameter, sep="") 
      cat(",  p-value: ", .fmt(ct$p.value,3), sep="", "\n\n")
      cat("95% Confidence Interval of Population Correlation", "\n")
      cat("  Lower Bound:", .fmt(ct$conf.int,3)[1])
      cat("     Upper Bound:", .fmt(ct$conf.int,3)[2], "\n")

      if (!brief) .dash(55)
    }

    else cat("\n >>> Only Pearson correlations at this time, use R cor.test\n")
  }

    cat("\n")

}
