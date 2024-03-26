.reg1fitBasic <-
function(lm.out, TotSS, sy, digits_d=3, show_R=FALSE) {

  nm <- all.vars(lm.out$terms)  # names of vars in the model
  n.vars <- length(nm)
  n.pred <- n.vars - 1L
  n.obs <- nrow(lm.out$model)

  tx <- character(length = 0)

  # model fit
  sm <- summary(lm.out)

  if (is.null(options()$knitr.in.progress)) {
    tx[length(tx)+1] <- "-- Model Fit"
    tx[length(tx)+1] <- ""
  }
  
  if (!is.null(sy)) {  # assigned NULL in reg.zKfold.R)
    tx[length(tx)+1] <- paste("Standard deviation of ", nm[1], ": ",
    .fmt_cm(sy,digits_d), sep="")
    tx[length(tx)+1] <- ""
  }  

  se <- sm$sigma
  tx[length(tx)+1] <- paste("Standard deviation of residuals:  ",
                            .fmt_cm(se,digits_d),
                            " for df=", sm$df[2], sep="")

  tcut <- -qt(0.025, df=sm$df[2])
  range <- 2*tcut*se
  tx[length(tx)+1] <- paste("95% range of residuals:  ",
          .fmt_cm(range,digits_d),
          " = 2 * (", .fmt(tcut,3), " * ", .fmt_cm(se,digits_d), ")", sep="")

  # predicted residual sum of squares
  prs.terms <- residuals(lm.out)/(1 - lm.influence(lm.out)$hat)
  PRESS <- sum(prs.terms^2)
  RsqPRESS <- 1 - (PRESS / TotSS)

  if (n.pred > 0) {
    pvl <- 1-pf(sm$fstatistic[1],sm$fstatistic[2],sm$fstatistic[3])
    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste("R-squared:", .fmt(sm$r.squared,3), 
      "   Adjusted R-squared:", .fmt(sm$adj.r.squared,3),
      "   PRESS R-squared:", .fmt(RsqPRESS,3))
    tx[length(tx)+1] <- paste("\n", 
        "Null hypothesis of all 0 population slope coefficients:\n", 
        "  F-statistic: ", .fmt(sm$fstatistic[1],3),
        "     df: ", sm$fstatistic[2], " and ", sm$fstatistic[3],
        "     p-value:", .fmt(pvl, 3, 7), sep="")
  }

  return(list(out_fit=tx, se=sm$sigma, range=range, Rsq=sm$r.squared,
    Rsqadj=sm$adj.r.squared, PRESS=PRESS, RsqPRESS=RsqPRESS))
 
}
