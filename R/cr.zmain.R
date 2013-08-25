.cr.main <-
function(x, y, brief, ...) {

  # get variable labels if exist
  gl <- .getlabels()
  x.name <- gl$xn; x.lbl <- gl$xl;
  y.name <- gl$yn; y.lbl <- gl$yl

  if (!is.factor(x)) {

    ct <- cor.test(x,y, ...)
    if (ct$method == "Pearson's product-moment correlation") {
      c.type <- "pearson" 
      sym <- "r"
    }
    else if (ct$method == "Spearman's rank correlation rho") {
      c.type <- "spearman" 
      sym <- names(ct$estimate)
    }
    else if (ct$method == "Kendall's rank correlation tau") {
      c.type <- "kendall" 
      sym <- names(ct$estimate)
    }
    if (c.type == "pearson") sym.pop <- "correlation" else sym.pop <- sym

    if (ct$alternative == "two.sided")
      h.txt <- "not equal to"
    else if (ct$alternative == "less")
      h.txt <- "less than"
    else if (ct$alternative == "greater")
      h.txt <- "greater than"

    if (!brief) {
      cat("\n")
      .dash(55)
      cat("Correlation Analysis for Variables", x.name, "and", y.name, "\n")
      .dash(55)
    }

    cat("\n")
    cat(">>> ",ct$method, "\n", sep="")

    cat("\n")
    if (!is.null(x.lbl)) cat(x.name, ", ", as.character(y.lbl), sep="", "\n")
    if (!is.null(y.lbl)) cat(y.name, ", ", as.character(x.lbl), sep="", "\n")
    if (!is.null(x.lbl) || !is.null(y.lbl)) cat("\n")
   

    if (!brief) {
      n.pair <- sum(!is.na(x - y))  # number of points after pairwise deletion
      n.del <- sum(is.na(x - y))  # number of pairwise deleted observations
      cat("Number of paired values with neither missing, n:", n.pair, "\n")
      cat("Number of cases (rows of data) deleted:", n.del, "\n\n")
      if (c.type == "pearson") {
        covr <- cov(x, y, use="pairwise.complete.obs")
        cat("Sample Covariance: s =", .fmt(covr,3), "\n\n")
      }
    }

    if (brief) 
      cat("Sample Correlation of ", x.name, " and ", y.name,
        ": ", sym, " = ", .fmt(ct$estimate,3), sep="", "\n\n")
    else
      cat("Sample Correlation: ", sym, " = ", .fmt(ct$estimate,3), sep="", "\n\n")
    cat("Alternative Hypothesis: True", sym.pop, "is", h.txt, "0", "\n")
    cat("  ", names(ct$statistic), "-value: ", .fmt(ct$statistic,3), sep="")
    if (c.type == "pearson")
        cat(",  df: ", ct$parameter, sep="") 
    cat(",  p-value: ", .fmt(ct$p.value,3), sep="", "\n")
    if (c.type == "pearson") {
      cat("\n")
      cat("95% Confidence Interval of Population Correlation", "\n")
      cat("  Lower Bound:", .fmt(ct$conf.int,3)[1])
      cat("     Upper Bound:", .fmt(ct$conf.int,3)[2], "\n")
    }

    if (!brief) .dash(55)
  }

  cat("\n")

}
