.reg1Basic <-
function(lm.out, nm, dname, 
         n.vars, n.pred, n.obs, n.keep, digits.d, explain, show.R, pre, line) {

# ----------
# Background
# ----------

  cat( "\n\n", "  BACKGROUND", "\n")

  if(show.R) {
    cv <- paste(nm[1]," ~ ", sep="")
    cv <- paste(cv, nm[2], sep="")
    if (n.vars > 2) for (i in 3:n.vars) cv <- paste(cv, " + ", nm[i], "", sep="")
    cat(line, pre, "model <- lm(", cv, ")", "\n", line, sep="")
  }
  
  cat("\n")
  if (sys.nframe() == 1) {  # only accurate if not called from model
    cat("Data Frame: ", dname, "\n\n")
  }

  for (i in 1:n.vars) {
    ind <- i
    .varlist(n.pred, ind, nm[i], "Predictor", n.obs, n.keep)
  }



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

  if (show.R) cat("\n",line,pre,"confint(model)","\n",line,"\n",sep="")
  if (explain) {
    .dash(68)
    cat("95% Confidence Intervals\n",
        "    Each interval is constructed about the\n",
        "      corresponding estimated intercept or slope coefficient.\n",
        "    The margin of error is half the width of the interval.\n")
    .dash(68)
  }
  else cat("Model Coefficients\n")

  # model coefficients
  sm1 <- sm$coefficients
  sm2 <- confint(lm.out, level=0.95) 
  smc <- cbind(sm1, sm2)

  buf <- 0 
  for (i in 1:length(n.vars)) {
    lng.lbl <- nchar(rownames(smc)[i])
    if (lng.lbl > buf) buf <- lng.lbl 
   }

  max.num <- integer(length=0)
  for (icol in 1:6) {
    max.num[icol] <- 0 
    for (i in 1:n.vars) {
      ln.nm <- nchar(as.character(trunc(smc[i,icol]))) + digits.d + 1
      if (ln.nm > max.num[icol]) max.num[icol] <- ln.nm
    }
    if (max.num[icol] < 9) max.num[icol] <- 9 
  }

  est.lbl <- .fmtc("Estimate", max.num[1]+1)
  ste.lbl <- .fmtc("  Std Err", max.num[2]+2)
  t.lbl <-  "  t-value"
  p.lbl <-  "  p-value"
  lb.lbl <- .fmtc("Lower 95%", max.num[5]+3)
  ub.lbl <- .fmtc("Upper 95%", max.num[6]+3)
  cat("\n", rep(" ", buf), est.lbl, ste.lbl, t.lbl, p.lbl, lb.lbl, ub.lbl, sep="", "\n")
  for (i in 1:(nrow(smc))) {
    rlb <- .fmtc(rownames(smc)[i], buf)
    ub <- .fmt(smc[i,6], digits.d, max.num[6])
    est <- .fmt(smc[i,1], digits.d, max.num[1])
    ste <- .fmt(smc[i,2], digits.d, max.num[2]+1)
    tvl <- .fmt(smc[i,3], 3, 8)
    pvl <- .fmt(smc[i,4], 3, 8)
    lb <- .fmt(smc[i,5], digits.d, max.num[5])
    ub <- .fmt(smc[i,6], digits.d, max.num[6])
    cat(rlb, est, ste, tvl, pvl, " ", lb, " ", ub, "\n")
  }

  # model fit
  cat("\n\n\n")
  cat("Model Fit\n")
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
  se <- sm$sigma
  cat("Standard deviation of residuals: ", .fmt(se,digits.d-1),
    "for", sm$df[2], "degrees of freedom", "\n")
  cat("If normal, the approximate 95% range of residuals about each fitted\n")
  tcut <- -qt(0.025, df=sm$df[2])
  cat("  value is 2*t-cutoff*", se, 
    ", with a 95% interval t-cutoff of ", .fmt(tcut,3), "\n", sep="")
  cat("95% range of variation: ", .fmt(2*tcut*se,digits.d-1), sep="", "\n\n")
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
  pvl <- 1-pf(sm$fstatistic[1],sm$fstatistic[2],sm$fstatistic[3])
  cat("R-squared: ", .fmt(sm$r.squared,3), 
    "    Adjusted R-squared: ", .fmt(sm$adj.r.squared,3), "\n")
  cat("\n")
  cat("Null hypothesis that population R-squared=0\n", 
      "  F-statistic: ", .fmt(sm$fstatistic[1],3),
      "     df: ", sm$fstatistic[2], " and ", sm$fstatistic[3],
      "     p-value:", .fmt(pvl, 3, 7), sep="", "\n")
 
  # ANOVA 
  smc <- anova(lm.out)
  cat("\n\n")
  if (show.R) cat("\n\n",line, pre,"anova(model)", "\n",line,"\n",sep="") else cat("\n")
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
  cat("Analysis of Variance\n")
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
