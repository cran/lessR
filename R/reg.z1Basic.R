.reg1Basic <-
function(nm, mydframe, 
         n.vars, n.pred, n.obs, n.keep, digits.d, explain, show.R, pre, line) {

# ----------
# Background
# ----------

  if (!explain) 
    cat("\nAdd this option to view explanations of the results:  explain=TRUE\n")
  cat( "\n\n\n", "  BACKGROUND", "\n")

  if(show.R) {
    cv <- paste(nm[1]," ~ ", sep="")
    cv <- paste(cv, nm[2], sep="")
    if (n.vars > 2) for (i in 3:n.vars) cv <- paste(cv, " + ", nm[i], "", sep="")
    cat(line, pre, "model <- lm(", cv, ")", "\n", line, sep="")
  }
  
  cat("\n")
  if (sys.nframe() == 1) {  # only accurate if not called from model
    cat("Data Frame: ", mydframe, "\n\n")
  }

  for (i in 1:n.vars) {
    ind <- i
    .varlist(n.pred, ind, nm[i])
  }
  
  cat("\nNumber of observations (rows) of data: ", n.obs, "\n")
  cat("Number of observations retained for analysis: ", n.keep, "\n")



# --------------
# Basic Analysis
# --------------
  cat( "\n\n\n", "  BASIC ANALYSIS", "\n")
 
# estimates, HTs 
  sm <- summary(lm.out)
  if (show.R) cat(line, pre, "summary(model)", "\n", line, sep="")
  cat("\n")
  
  if (explain) {
    .dash(68)
    cat("Estimates\n",
        "  Intercept: Fitted value of response variable, ", nm[1], ", when the\n",
        "             values of the predictor variable", sep="")
    if (n.pred == 1) cat(" is ") else cat("s are ")
    cat("set to zero.\n")
    cat("  Slope Coefficient: Average change in the value of response variable,\n",
        "             ", nm[1], ", for a one-unit increase in the value of the\n",
        "             corresponding predictor variable", sep="")
    if (n.pred == 1) cat(".\n") 
    else cat(", with the values of all\n",
             "             remaining predictor variables held constant.\n", sep="")
    cat("\n")
    cat("Hypothesis Tests\n")
    cat("    Standard error, t-value and p-value of each estimate\n")
    cat("    Null hypothesis: Corresponding population coefficient is 0\n")
    .dash(68)
   }
  else cat("The Model, Hypothesis Tests\n")

  smc <- sm$coefficients
  buf <- 0 
  for (i in 1:length(n.vars)) {
    lng.lbl <- nchar(rownames(smc)[i])
    if (lng.lbl > buf) buf <- lng.lbl 
   }
  max.num <- integer(length=0)
  for (icol in 1:3) {
    max.num[icol] <- 0 
    for (i in 1:n.vars) {
      ln.nm <- nchar(as.character(trunc(smc[i,icol]))) + digits.d + 1
      if (ln.nm > max.num[icol]) max.num[icol] <- ln.nm
    }
    if (max.num[icol] < 9) max.num[icol] <- 9 
  }
  est.lbl <- .fmtc("Estimate", max.num[1]+1)
  ste.lbl <- .fmtc("Std. Err", max.num[2]+1)
  tvl.lbl <- .fmtc(" t-value", max.num[3]+1)
  cat("\n", rep(" ", buf), est.lbl, ste.lbl, tvl.lbl, "   p-value", sep="", "\n")
  for (i in 1:(n.vars)) {
    rlb <- .fmtc(rownames(smc)[i], buf)
    est <- format(sprintf("%7.*f", digits.d, smc[i,1]), width=max.num[1], justify="right")
    ste <- format(sprintf("%7.*f", digits.d, smc[i,2]), width=max.num[2], justify="right")
    tvl <- format(sprintf("%7.*f", digits.d, smc[i,3]), width=max.num[3], justify="right")
    pvl <- format(sprintf("%6.4f", smc[i,4]), width=9, justify="right")
    cat(rlb, est, ste, tvl, pvl, "\n")
  }
 
# confidence intervals 
  cat("\n\n\n")
  if (show.R) cat("\n",line,pre,"confint(model)","\n",line,"\n",sep="")
  if (explain) {
    .dash(68)
    cat("95% Confidence Intervals\n",
        "    Each interval is constructed about the\n",
        "      corresponding estimated intercept or slope coefficient.\n",
        "    The margin of error is half the width of the interval.\n")
    .dash(68)
  }
  else cat("95% Confidence Intervals\n")

  smc <- confint(lm.out, level=0.95) 
  max.num <- integer(length=0)
  for (icol in 1:2) {
    max.num[icol] <- 0 
    for (i in 1:n.vars) {
      ln.nm <- nchar(as.character(trunc(smc[i,icol]))) + digits.d + 1
      if (ln.nm > max.num[icol]) max.num[icol] <- ln.nm
    }
    if (max.num[icol] < 9) max.num[icol] <- 9 
  }
  lb.lbl <- .fmtc("Lower", max.num[1]+1)
  ub.lbl <- .fmtc("Upper", max.num[2]+1)
  cat("\n", rep(" ", buf), lb.lbl, ub.lbl, sep="", "\n")
  for (i in 1:(n.vars)) {
    rlb <- .fmtc(rownames(smc)[i],buf)
    lb <- format(sprintf("%7.*f", digits.d, smc[i,1]), width=max.num[1], justify="right")
    ub <- format(sprintf("%7.*f", digits.d, smc[i,2]), width=max.num[2], justify="right")
    cat(rlb, lb, ub, "\n")
  }
 
# model fit
  cat("\n\n\n")
  cat("Model fit\n")
  cat("\n")
  if (explain) {
    .dash(68)
    cat("The \'standard deviation of the residuals\' is also called\n",
        "the \'standard error of estimate\'.  Are the residuals typically\n",
        "close to their mean of zero, or are they scattered with\n",
        "relatively large positive and negative values?\n",
        "\n",
        "For any normal distribution, about 95% of the values are within\n",
        "two standard deviations of the mean, for a range of four.\n", sep="")
    .dash(68)
    cat("\n")
  }
  cat("Standard deviation of residuals: ", signif(sm$sigma,4),
    "for", sm$df[2], "degrees of freedom", "\n")
  cat("If normal, the approximate 95% range of residuals about\n")
  cat("  each fitted value is 4*", signif(sm$sigma,4), 
    " or ", signif(4*sm$sigma,4), sep="", "\n\n")
  if (explain) {
    cat("\n")
    .dash(68)
    cat("R-squared: Proportion of the overall variability of response variable\n",
        nm[1], " that is accounted for by the model. The unexplained\n",
        "variability is the variability of the residuals.\n",  
        "\n",
        "Adjusted R-squared: Adjusts R-squared downward according to the\n",
        "degrees of freedom. Unlike R-squared, the adjusted version increases\n",
        "when a new predictor variable is added to the model only if the\n",
        "new variable improves the model more than would be expected by\n",
        "chance.\n", sep="")
    .dash(68)
    cat("\n")
  }
  cat("R-squared: ", signif(sm$r.squared,3), 
    "    Adjusted R-squared: ", signif(sm$adj.r.squared,3), "\n")
  cat("\n")
  cat("F-statistic for null hypothesis that population R-squared=0: ", 
    signif(sm$fstatistic[1],4), "\n") 
  cat("Degrees of freedom: ", sm$fstatistic[2], "and", sm$fstatistic[3],"\n")
  pvl <- 1-pf(sm$fstatistic[1],sm$fstatistic[2],sm$fstatistic[3])
  cat("p-value:",
    format(sprintf("%5.4f", pvl), width=9, justify="right"), sep="", "\n")
 
# ANOVA 
  smc <- anova(lm.out)
  cat("\n\n")
  if (show.R) cat("\n\n",line, pre,"anova(model)","\n",line,"\n",sep="") else cat("\n")
  if (explain) {
    .dash(68)
    cat("The ANOVA table presents the amount of the explained and unexplained\n",
        "variation. \n",
        "\n",
        "The sum of the squared residuals, the value minimized by the OLS\n",
        "estimation procedure by the choice of estimated coefficients, is ",
        smc$'Sum Sq'[n.vars], ".\n", sep="")
    .dash(68)
    cat("\n")
  }
  cat("Analysis of Variance table\n")
  buf <- 0 
  for (i in 1:n.vars) {
    lng.lbl <- nchar(rownames(smc)[i])
    if (lng.lbl > buf) buf <- lng.lbl 
   }
  max.num <- integer(length=0)
  for (icol in 1:4) {
    max.num[icol] <- 0 
    for (i in 1:n.vars) {
      ln.nm <- nchar(as.character(trunc(smc[i,icol]))) + digits.d + 2
      if (ln.nm > max.num[icol]) max.num[icol] <- ln.nm
    }
    if (icol != 1) if (max.num[icol] < 9) max.num[icol] <- 9 
  }
  df.lbl <- .fmtc("     df", max.num[1]+1)
  SS.lbl <- .fmtc(" Sum Sq", max.num[2]+1)
  MS.lbl <- .fmtc("Mean Sq", max.num[3]+1)
  fv.lbl <- .fmtc("F-value", max.num[4]+1)
  cat("\n", rep(" ", buf-5), df.lbl, SS.lbl, MS.lbl, fv.lbl, "   p-value", sep="", "\n")
  for (i in 1:(n.vars)) {
    rlb <- .fmtc(rownames(smc)[i], buf)
    df <- format(sprintf("%i", smc[i,1]), width=max.num[1]-5, justify="right")
    SS <- format(sprintf("%7.*f", digits.d, smc[i,2]), width=max.num[2], justify="right")
    MS <- format(sprintf("%7.*f", digits.d, smc[i,3]), width=max.num[3], justify="right")
    fv <- format(sprintf("%7.*f", digits.d, smc[i,4]), width=max.num[4], justify="right")
    pv <- format(sprintf("%6.4f", smc[i,5]), width=9, justify="right")
    if (i < n.vars) cat(rlb, df, SS, MS, fv, pv, "\n") else cat(rlb, df, SS, MS, "\n") 
  } 

}
