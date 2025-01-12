.plt.forecast <-
function(x, y, by=NULL,
         ts_unit=NULL, ts_ahead=0, ts_method=NULL,
         ts_fitted=FALSE, n_date_tics=NULL,
         ts_level=NULL, ts_trend=NULL, ts_seasons=NULL,
         ts_type="additive", ts_PIlevel=0.95,
         digits_d=getOption("digits_d"))  {

  if (is.null(ts_seasons))  # default value
    do.seasons <- TRUE
  else if (ts_seasons)  # any non-zero numerical value is considered TRUE
    do.seasons <- TRUE
  else
    do.seasons <- FALSE

  if (is.null(digits_d)) digits_d <- 3
  n.d <- digits_d - 1
  if (n.d <= 2) n.d <- 3

  x.name <- getOption("xname")
  y.name <- names(y)
  if (is.null(digits_d)) digits_d <- .max.dd(y[,1]) + 1

  # get x.dates, a superset of x-axis tics
  n.by <- ifelse (is.null(by), 0, nlevels(by))
  if (n.by == 0)
    x.fit <- x[,1]  # save actual dates as a vector
  else {  # x-axis tics just for one level of by
    cnt <- sum(by == levels(by)[1])  # just dates for the first level
    x.fit <- x[1:cnt,1]  # assumes by is sorted by level
  }

  # when computing x.hat, need the +1 and [-1] to not duplicate last x value
  hold.unit <- ifelse (ts_unit == "days7", TRUE, FALSE)
  if (hold.unit) ts_unit <- "days"
  x.hat <- seq.Date(x[nrow(x),1], by=ts_unit, length.out=ts_ahead+1)[-1]
  if (hold.unit) ts_unit <- "days7"

  # create time series for stl() and HoltWinters(),
  y.ts <- .tsMake(x.fit, y, ts_unit)
  freq <- frequency(y.ts)
  min.2per <- nrow(y) >= (2 * freq)  # evaluate seasonality eligibility
  if (do.seasons) {
    if (ts_unit == "years")
      message("Seasonal effects are not possible with annual data.")
    else if (!min.2per)
      message("\nSeasonal effects over a year of aggregated data are \n",
              "usually limited to monthly or quarterly data.\n",
              "Usually need two years worth of data to estimate seasonality.\n")
    if (ts_unit=="years" || !min.2per) do.seasons <- FALSE
  }  # end do.seasons


  # Linear Regression with Seasonality
  # ----------------------------------
  if (ts_method == "lm") {

    # Decompose the time series with stl()
    if (do.seasons) {
      decomp <- stl(y.ts, s.window="periodic")
      if (is.null(ts_trend))  # de-seasonalize
        y.trend <- decomp$time.series[,"trend"] +
                   decomp$time.series[,"remainder"]
      else
        y.trend <- decomp$time.series[,"remainder"]
    }  # end do.seasons
    else {  # no seasonal analysis
      if (is.null(ts_trend))
        y.trend <- y[,1]
      else
        y.trend <- rep(mean(y[,1], na.rm=TRUE), length(y[,1]))
    }

    # Fit linear regression on (usually) deseasonalized data
    x.seq <- 1:length(y.trend)
    fit <- lm(y.trend ~ x.seq)
    y.fit.trend <- fit$fitted.values  # reg fitted y.trend values
    varcov <- vcov(fit)  # variance-covariance matrix of coefficients
    coefs <- fit$coefficients
    names(coefs) <- c("b0", "b1")

    # Forecast
    if (do.seasons) {
      # Get seasonal indices of data, start at the next season
      n.cycles <- length(y[,1]) / freq
      season.ind <- rep(1:freq, ceiling(n.cycles))[1:length(y[,1])]
      last_index <- tail(season.ind, 1)
      start.ind <- (last_index %% freq) + 1

      # Get seasonal indices of forecast
      new.ind <- (start.ind + seq_len(ts_ahead) - 1) %% freq
      new.ind[new.ind == 0] <- freq  # replace 0

      # Map seasonal effects to forecasted indices
      y.seas.eff <- as.numeric(decomp$time.series[,"seasonal"])
      for (i in 1:freq) {
        coefs[length(coefs)+1] <- y.seas.eff[i]
        names(coefs)[length(coefs)] <- paste0("s", i, sep="")
      }
      new.seas.eff <- y.seas.eff[new.ind]
    }  # end if do.seasons

    else {  # no seasonal effects analyzed
      new.seas.eff <- double(length=ts_ahead)  # 0 by default
      y.seas.eff <- double(length=length(y[,1]))
    }

    y.fit <- y.fit.trend + y.seas.eff[1:length(y.fit.trend)]  # trend + seasonal
    y.fit <- data.frame(y.fit)
    SSE <- sum((y[,1] - y.fit[,1])^2)
    MSE <- SSE / (nrow(y)-2)

    # y.hat from trend and (usually) seasonality plus prediction intervals
    new.x.seq <- seq(max(x.seq)+1, by=1, length.out=ts_ahead)  # future times
    y.hat <- (coefs[1] + coefs[2]*new.x.seq) + new.seas.eff

    # usual formula for std error with MSE, here from trend and seasonality
    x_mean <- mean(x.seq)
    Sxx <- sum((x.seq - x_mean)^2)
    se_forecast <- sqrt(MSE * (1 + 1/nrow(y) + (new.x.seq-x_mean)^2 / Sxx))

    # get prediction intervals
    t.crit <- qt((1 + ts_PIlevel)/2, df=fit$df.residual)  # critical t-value
    half.width <- t.crit * se_forecast
    y.lwr <- as.vector(y.hat) - half.width
    y.upr <- as.vector(y.hat) + half.width

    # Output data frame of fitted values
    if (ts_fitted) {
      # adjust forecasted values to get correct date format
      xc.fit <- x.fit
      if (ts_unit == "months")   # display monthly fit
        xc.fit <- as.character(zoo::as.yearmon(x.fit))
      else if (ts_unit == "quarters")  # display quarterly fit
        xc.fit <- as.character(zoo::as.yearqtr(x.fit))
      else if (ts_unit == "years")  # display annual fit
        xc.fit <- as.character(format(xc.fit, "%Y"))
      out_fitted <- data.frame(
        date=xc.fit,
        y=y[,1],
        fitted=y.fit[,1],
        error=y[,1]-fit$fitted
      )
      names(out_fitted)[1:2] <- c(x.name, y.name)
    }
    else
      out_fitted <- NULL

    txparam <- NULL
  }  # end ts_method=="lm"


  # HoltWinters
  # -----------
  else if (ts_method == "es") {
    # fit: fit$fitted is mts, a multivariate time series
    if (!do.seasons) ts_seasons <- FALSE
    fit <- HoltWinters(y.ts, alpha=ts_level, beta=ts_trend, gamma=ts_seasons,
                       seasonal=ts_type)
    colnames(fit$fitted)[1] <- "fitted"
    yf <- .tsExtract(fit$fitted[,1], x.name)
    xhw.fit <- yf$x.dates
    y.fit <- yf$y

    alpha.f <- fit$alpha;  beta.f <- fit$beta;  gamma.f <- fit$gamma

    coefs <- fit$coefficients
    if (names(coefs)[1] == "a") names(coefs)[1] <- "b0"
    if (length(coefs) > 1)
      if (names(coefs)[2] == "b") names(coefs)[2] <- "b1"

    # predict with prediction intervals
    y.pred <- predict(fit, n.ahead=ts_ahead,
                      prediction.interval=TRUE, level=ts_PIlevel)  # mts
    width <- y.pred[, "upr"] - y.pred[, "lwr"]
    y.pred <- cbind(y.pred, width=width)

    # get predicted values and PI from each individual time series
    y.ahead <- .tsExtract(y.pred[,1], x.name)
    y.hat <- y.ahead$y
    xhw.hat <- y.ahead$x.dates
    diffx.fit <- diff(x.fit)
    incr <- 0
    x.pred1 <- x.fit[length(x.fit)]
    if (ts_unit=="weeks")
      x.hat <- seq.Date(from=x.pred1+7, by="week", length.out=nrow(xhw.hat))
    y.ahead <- .tsExtract(y.pred[,2], x.name)
    y.upr <- y.ahead$y
    y.ahead <- .tsExtract(y.pred[,3], x.name)
    y.lwr <- y.ahead$y

    # compute MSE
    n.param <- 1
    if (is.numeric(beta.f)) n.param <- n.param + 1
    if (is.numeric(gamma.f)) n.param <- n.param + 1
    MSE <- fit$SSE / (nrow(xhw.fit) - n.param)

    # if !trend then no b1, if !seasons then no s1, etc.
    # smoothing parameter output for exponential smoothing forecast
    tx <- character(length = 0)
    tx[length(tx)+1] <- "Smoothing Parameters"
    tx[length(tx)+1] <- paste(" alpha:", .fmt_cm(alpha.f,n.d))
    if (is.numeric(beta.f))
      tx[length(tx)] <- paste(tx[length(tx)], " beta:", .fmt_cm(beta.f,n.d))
    if (is.numeric(gamma.f))
      tx[length(tx)] <- paste(tx[length(tx)], " gamma:", .fmt_cm(gamma.f,n.d))
    tx[length(tx)+1] <- ""
    txparam <- tx

    # Output data frame of fitted values
    diff.df <- length(x.fit) - nrow(xhw.fit)
    x.fit <- x.fit[(diff.df+1) : length(x.fit)]  # es drops first set of dates
    xc.fit <- x.fit
      if (ts_unit == "months")   # display monthly fit
        xc.fit <- as.character(zoo::as.yearmon(xc.fit))
      else if (ts_unit == "quarters")  # display quarterly fit
        xc.fit <- as.character(zoo::as.yearqtr(xc.fit))
      else if (ts_unit == "years")  # display annual fit
        xc.fit <- as.character(format(xc.fit, "%Y"))
    if (ts_fitted) {
      out_fitted <- data.frame(
        date=xc.fit,
        fitted=fit$fitted[,1],
        level=fit$fitted[,2],
        trend=fit$fitted[,3]
      )
      names(out_fitted)[1] <- x.name
      if (do.seasons) {
        out_fitted <- cbind(out_fitted, fit$fitted[,4])
        names(out_fitted)[5] <- "season"
      }
    }
    else
      out_fitted <- NULL

  }  # end HoltWinters


  # Results
  # -------

  # adjust forecasted values to match the date format of the data values
  xc.hat <- x.hat
  if (ts_unit == "months")  # display monthly dates
    xc.hat <- as.character(zoo::as.yearmon(x.hat))
  else if (ts_unit == "quarters")  # display quarterly dates
    xc.hat <- as.character(zoo::as.yearqtr(x.hat))
  else if (ts_unit == "years")  # display annual fit
    xc.hat <- as.character(format(xc.hat, "%Y"))

  # Output data frame of forecasts
  y.frcst <- data.frame(
    date=xc.hat,
    predicted=as.vector(y.hat),
    lower=y.lwr,
    upper=y.upr,
    width=y.upr-y.lwr
  )
  names(y.frcst) <- c(x.name, "predicted", "upper", "lower", "width")

  x.fit <- data.frame(x.fit)
  x.hat <- data.frame(x.hat); names(x.hat) <- "x.dates"
  y.hat <- data.frame(y.hat)
  y.lwr <- data.frame(y.lwr);  names(y.lwr) <- y.name
  y.upr <- data.frame(y.upr);  names(y.upr) <- y.name

  tx <- character(length = 0)
  if (!getOption("suggest")) tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- paste("Mean squared error of fit to data:",
                            .fmt_cm(MSE, n.d+2))
  txerr <- tx

  tx <- character(length = 0)
  tx[length(tx)+1] <- "Coefficients for Linear Trend"
  if (do.seasons) {
      tx[length(tx)] <- paste(tx[length(tx)], "and Seasonality")
  }
  tx[length(tx)+1] <- " "
  for (i in 1:length(coefs)) {
    if (names(coefs)[i] %in% c("s1", "s7"))
      tx[length(tx)+1] <- " "
    tx[length(tx)] <- paste(tx[length(tx)],
      names(coefs)[i], ": ", .fmt_cm(coefs[i], digits_d+1), "  ", sep="")
  }
  tx[length(tx)+1] <- ""
  txcoef <- tx

  # margin adjustments for plot
  # ---------------------------
  mx.x <- max(as.numeric(x.hat[,1]))
  mx.y <- max(max(y), max(y.fit), max(y.upr), max(y.hat))
  mn.y <- min(min(y), min(y.fit), min(y.lwr), min(y.hat))

  # no output here unless print suggestions before calling main to print
  return(list(y.fit=y.fit, y.hat=y.hat, y.frcst=y.frcst,
              x.fit=x.fit, x.hat=x.hat,
              y.upr=y.upr, y.lwr=y.lwr,
              mx.x=mx.x, mn.y=mn.y, mx.y=mx.y,
              out_fitted=out_fitted,
              out_err=txerr, out_coefs=txcoef, out_params=txparam))
}

