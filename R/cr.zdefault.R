.cr.default <-
function(x, y, dframe=mydata, brief=FALSE, ...) {

  # get variable labels if exist
  gl <- .getlabels()
  x.name <- gl$xn; x.lbl <- gl$xl;
  y.name <- gl$yn; y.lbl <- gl$yl

  if (!is.factor(x)) {

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
      cat("Sample Covariance: cov =", round(covr,3), "\n\n")
    }

    ct <- cor.test(x,y)
    cat("Sample Correlation of ", x.name, " and ", y.name,
        ": r = ", round(ct$estimate,3), sep="", "\n\n")
    cat("Hypothesis Test that Population Correlation is Zero", "\n")
    cat("  t-value: ", round(ct$statistic,4), ",  df: ", ct$parameter, sep="") 
    cat(",  p-value: ", round(ct$p.value,4), sep="", "\n\n")
    cat("95% Confidence Interval of Population Correlation", "\n")
    cat("  Lower Bound:", round(ct$conf.int,3)[1])
    cat("     Upper Bound:", round(ct$conf.int,3)[2], "\n")

    if (!brief) .dash(55)
  }

    cat("\n")

}
