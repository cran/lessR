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
    if (c.type == "pearson") sym.pop <- "Correlation" else sym.pop <- sym

    if (ct$alternative == "two.sided")
      h.txt <- "not equal to"
    else if (ct$alternative == "less")
      h.txt <- "less than"
    else if (ct$alternative == "greater")
      h.txt <- "greater than"

    tvalue <-  round(ct$statistic, 3)
    df <-  ct$parameter
    pvalue <-  round(ct$p.value, 3)
    coef <- round(ct$estimate, 3)
    null <- ct$null.value
    if (c.type == "pearson") {
      lb <- round(ct$conf.int[1], 3)
      ub <- round(ct$conf.int[2], 3)
      clpct <- "95%"
    }

    # background
    tx <- character(length = 0)
    if (!brief) {
      txt <- paste("Correlation Analysis for Variables", x.name, "and", y.name)
      tx[length(tx)+1] <- txt
      tx[length(tx)+1] <- " "
#     tx[length(tx)+1] <- .dash2(nchar(txt))
    }

    tx[length(tx)+1] <- paste("\n>>>", ct$method)

    if (!is.null(x.lbl) || !is.null(y.lbl)) {
      tx[length(tx)+1] <- ""
      if (!is.null(x.lbl))
        tx[length(tx)+1] <- paste(x.name, ": ", as.character(x.lbl), sep="")
      else
        tx[length(tx)+1] <- paste(x.name)
      if (!is.null(y.lbl))
        tx[length(tx)+1] <- paste(y.name, ": ", as.character(y.lbl), sep="")
      else
        tx[length(tx)+1] <- paste(y.name)
    }
   
      n.pair <- sum(!is.na(x - y))  # number of points after pairwise deletion
      tx[length(tx)+1] <- ""
      tx[length(tx)+1] <- paste("Number of paired values with neither",
        "missing, n =", n.pair)

    if (!brief) {
      n.del <- sum(is.na(x - y))  # number of pairwise deleted observations
      tx[length(tx)+1] <- paste("Number of cases (rows of data) deleted:",
        n.del)
    }

    txb <- tx

    # descriptive
    tx <- character(length = 0)
    if (!brief) {
      if (c.type == "pearson") {
        covr <- cov(x, y, use="pairwise.complete.obs")
        tx[length(tx)+1] <- paste("Sample Covariance: s =", .fmt(covr,3))
      tx[length(tx)+1] <- ""
      }
      tx[length(tx)+1] <- paste("Sample Correlation: ", sym, " = ",
        .fmt(ct$estimate,3), sep="")
    }
    else
      tx[length(tx)+1] <- paste("Sample Correlation of ", x.name, " and ",
        y.name, ": ", sym, " = ", .fmt(ct$estimate,3), sep="")

    txd <- tx


    # inferential
    tx <- character(length = 0)

    mytitle <- paste("Hypothesis Test of 0 ", sym.pop, ":  t = ", sep="")
    tx[length(tx)+1] <- paste(mytitle, .fmt(tvalue,3), ",  df = ", df,
        ",  p-value = ", .fmt(pvalue,3), sep="")

    if (c.type == "pearson") {
      tx[length(tx)+1] <- paste(clpct," Confidence Interval for Correlation:  ",
           .fmt(lb,3), " to ", .fmt(ub,3), sep="")
    }
    else {
      df <- NA; lb <- NA; ub <- NA
    }

    txi <- tx

  }

  return(list(txb=txb, txd=txd, txi=txi,
    r=round(ct$estimate,3), tvalue=round(ct$statistic,3),
    df=df, pvalue=round(ct$p.value,3), lb=lb, ub=ub))

}
