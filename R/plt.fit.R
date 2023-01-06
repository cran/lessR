.plt.fit <- 
function(x.lv, y.lv, fit.line, fit_power) {

  b00 <- NULL
  b11 <- NULL
  Rsqq <- NULL
  ok <- is.finite(x.lv) & is.finite(y.lv)
  if (any(ok)) {
    x.lv <- x.lv[ok]
    y.lv <- y.lv[ok]
    od <- order(x.lv)
    x.lv <- x.lv[od]
    y.lv <- y.lv[od]

    # fit line
    if (fit.line == "loess")
      l.ln <- loess(y.lv ~ x.lv)
    else if (fit.line == "lm")
      l.ln <- lm(y.lv ~ x.lv)
    else if (fit.line == "null")
      l.ln <- lm(y.lv ~ 1)
    if (fit.line %in% c("loess", "lm", "null"))
      f.ln <- fitted(l.ln)
    if (fit.line %in% c("lm", "null")) {
      b00 <- l.ln$coefficients[1] 
      b11 <- l.ln$coefficients[2] 
      Rsqq <- summary(l.ln)$r.squared
    }

    if (fit.line %in% c("quad", "power")) {  # quad model
      if (fit.line == "quad") {
        l.ln <- lm(sqrt(y.lv) ~ x.lv)
        fit_power <- 2
      }
      else
        l.ln <- lm((y.lv^(1/fit_power)) ~ x.lv)
      b00 <- l.ln$coefficients[1]
      b11 <- l.ln$coefficients[2]
      f.ln <- (b00 + (b11*x.lv))^fit_power
      Rsqq <- summary(l.ln)$r.squared
    }

    if (fit.line == "exp") {  # exponential model
      if (any(y.lv < 0))
        message("\n>>>  Negative values of Y dropped, no log(Y).\n")
#           fi <- which(y.lv < 0)
#           if (length(fi) > 0) {
#             y.lv <- y.lv[-fi]
#             x.lv <- x.lv[-fi]
#           }
      if (all(y.lv <= 0)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "All values of Y are non-positive, cannot take log(Y)\n\n")
      }
      # log(neg y) generates a NaN, which reduces data for lm
      #  but does not hurt the exp function back transform
      if (fit_power == 1) {
        l.ln <- lm(log(y.lv) ~ x.lv)
      }
      else {
        l.ln <- lm(log(y.lv^(1/fit_power)) ~ x.lv)
      }
      b00 <- l.ln$coefficients[1]
      b11 <- l.ln$coefficients[2]
      f.ln <- exp(b00 + (b11*x.lv))
      Rsqq <- summary(l.ln)$r.squared
      ok <- is.finite(f.ln)
      if (length(ok) > 0) {
        f.ln <- f.ln[ok]
        x.lv <- x.lv[ok]
      }
    }

    if (fit.line == "log") {  # logarithmic model
#           if (any(y.lv == 0))
#             message("\n>>> 0 value of log(y) is undefined.\n")
#           fi <- which(y.lv == 0)  # no log, BUT taking exp
#           if (length(fi) > 0) {
#             y.lv <- y.lv[-fi]
#             x.lv <- x.lv[-fi]
#           }
      if (fit_power == 1) {
        y.exp <- exp(y.lv)
        if (any(is.infinite(y.exp))) {
          cat("\n"); stop(call.=FALSE, "\n","------\n",
            "Some values of y too large for exp(y). Rescale.\n\n")
        }
        l.ln <- lm(y.exp ~ x.lv)
      }
      else
        l.ln <- lm(exp(y.lv^(1/fit_power)) ~ x.lv)
      b00 <- l.ln$coefficients[1]
      b11 <- l.ln$coefficients[2]
      f.ln <- log(b00 + (b11*x.lv))
      if (any(is.nan(f.ln))) {
        message("\n>>> Warning: ",
          "Some values of log() back transformation not defined.\n\n")
      }
      Rsqq <- summary(l.ln)$r.squared
    }

    if (fit.line != "loess") {
      a <- anova(l.ln)
      mse <- a[row.names(a) == "Residuals", ][3]
    }
    else {  # anova() only applies to lm
      e.lv <- l.ln$residuals
      sse <- sum(e.lv^2)
      mse <- sse / (length(e.lv) - 2)  # only 1 pred var
    }
    b0 <- ifelse (is.null(b00), NA, b00) 
    b1 <- ifelse (is.null(b11), NA, b11) 
    Rsq <- ifelse (is.null(Rsqq), NA, Rsqq) 
  }  # end any(ok)

  return(list(x.lv=x.lv, y.lv=y.lv, f.ln=f.ln, l.ln=l.ln,
              mse=mse, b0=b0, b1=b1, Rsq=Rsq))

}

