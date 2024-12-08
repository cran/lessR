.plt.forecast <- 
function(x, y, by=NULL,
         time_unit=NULL, time_ahead=0,
         time_fit=FALSE, n_date_tics=NULL,
         es_level=NULL, es_trend=NULL, es_seasons=NULL,
         es_type="additive", es_PIlevel=0.95,
         digits_d=NULL)  {

  x.name <- getOption("xname")
  if (is.null(digits_d)) digits_d <- .max.dd(y[,1]) + 1

  # get x.dates, a superset of x-axis tics 
  n.by <- ifelse (is.null(by), 0, nlevels(by))
  if (n.by == 0)
    x.dates <- x[,1]  # save actual dates for later
  else {  # x-axis tics just for one level of by
    cnt <- sum(by == levels(by)[1])  # just dates for the first level
    x.dates <- x[1:cnt,1]  # assumes by is sorted by level
  }


  # compute forecast
  # ----------------

  # extend x-access tics through the forecasted values
  # when computing x.hat, need the +1 and [-1] to not duplicate last x value
  xorig.len <- nrow(x)
  mx.d <- x[nrow(x),1]  # numeric form
  x.hat <- seq.Date(mx.d, by=time_unit, length.out=time_ahead+1)[-1]
  x.dates <- c(x.dates, x.hat)
  x.hat <- data.frame(x.hat)


  # fit: fit$fitted is mts, a multivariate time series
  x.orig <- x.dates[1:xorig.len]
  y.ts <- .tsMake(x.orig, y)  # convert data to ts form
  if (time_unit == "years") es_seasons <- FALSE  # no seasons in annual data
  fit <- HoltWinters(y.ts, alpha=es_level, beta=es_trend, gamma=es_seasons,
                     seasonal=es_type)
  colnames(fit$fitted)[1] <- "fitted"

  SSE <- fit$SSE
  alpha.f <- fit$alpha
  beta.f <- fit$beta
  gamma.f <- fit$gamma

  coefs.f <- fit$coefficients 
  if (names(coefs.f)[1] == "a") names(coefs.f)[1] <- "b0"
  if (length(coefs.f) > 1)
    if (names(coefs.f)[2] == "b") names(coefs.f)[2] <- "b1"

  # predict with prediction intervals
  yf <- .tsExtract(fit$fitted[,1], x.name)
  x.fit <- yf$x.dates
  y.fit <- yf$y
  y.frcst <- predict(fit, n.ahead=time_ahead,
                 prediction.interval=TRUE, level=es_PIlevel)
  colnames(y.frcst)[1] <- "predicted"
  colnames(y.frcst)[2] <- "upper95"
  colnames(y.frcst)[3] <- "lower95"

  # get predicted values and PI
  y.ahead <- .tsExtract(y.frcst[,"predicted"], x.name)
  y.hat <- y.ahead$y
  x.hat <- y.ahead$x.dates
  y.ahead <- .tsExtract(y.frcst[,"upper95"], x.name)
  y.upr <- y.ahead$y
  y.ahead <- .tsExtract(y.frcst[,"lower95"], x.name)
  y.lwr <- y.ahead$y

  # adjust forecasted values to match the date format of the data values
  if (time_unit %in% c("weeks", "months")) {
    x.hat[,1] <- zoo::as.Date(zoo::as.yearmon(x.hat[,1]), frac=1)
  }
  else if (time_unit == "quarters") {
    x.hat[,1] <- zoo::as.Date(zoo::as.yearqtr(x.hat[,1]))
  }


  # margin adjustments for plot
  # ---------------------------
  mx.x <- max(as.numeric(x.hat[,1]))
  mx.y <- max(max(y), max(y.fit), max(y.upr))
  mn.y <- min(min(y), min(y.fit), min(y.lwr))


  # compute MSE
  # -----------
  n.param <- 1
  if (is.numeric(beta.f)) n.param <- n.param + 1
  if (is.numeric(gamma.f)) n.param <- n.param + 1
  MSE <- SSE / (nrow(x.fit) - n.param)


  # generate text output for exponential smoothing forecast
  # -------------------------------------------------------

  # if !es_trend then no b1, if !es_seasons then no s1, etc.
  out_frcst <- NULL;  out_fitted <- NULL
  n.d <- digits_d - 1
  if (n.d <= 2) n.d <- 3

  tx <- character(length = 0)
  tx[length(tx)+1] <- paste("\nSum of squared fit errors:", .fmt_cm(SSE, n.d))
  tx[length(tx)+1] <- paste("Mean squared fit error:   ", .fmt_cm(MSE, n.d))
  txerr <- tx

  tx <- character(length = 0)
  tx[length(tx)+1] <- "Coefficients for Linear Trend" 
  if (!is.logical(gamma.f))
    tx[length(tx)] <- paste(tx[length(tx)], "and Seasonality")
  tx[length(tx)+1] <- " "
  for (i in 1:length(coefs.f)) {
    if (names(coefs.f)[i] %in% c("s1", "s7"))
      tx[length(tx)+1] <- " " 
    tx[length(tx)] <- paste(tx[length(tx)],
      names(coefs.f)[i], ": ", .fmt_cm(coefs.f[i], n.d), "  ", sep="")
  }
  txcoef <- tx

  tx <- character(length = 0)
  tx[length(tx)+1] <- "Smoothing Parameters"
  tx[length(tx)+1] <- paste(" alpha:", .fmt_cm(alpha.f,n.d))
  if (is.numeric(beta.f))
    tx[length(tx)] <- paste(tx[length(tx)], " beta:", .fmt_cm(beta.f,n.d))
  if (is.numeric(gamma.f))
    tx[length(tx)] <- paste(tx[length(tx)], " gamma:", .fmt_cm(gamma.f,n.d))
  txparam <- tx

  if (time_fit) out_fitted <- fit$fitted

# no output here unless print suggestions before calling main to print

  return(list(y.fit=y.fit, y.hat=y.hat, y.frcst=y.frcst,
              x.fit=x.fit, x.hat=x.hat, 
              y.upr=y.upr, y.lwr=y.lwr, 
              mx.x=mx.x, mn.y=mn.y, mx.y=mx.y,
              out_frcst=out_frcst, out_fitted=out_fitted,
              out_err=txerr, out_coefs=txcoef, out_params=txparam))
}

