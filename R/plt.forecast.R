.plt.forecast <-
function(x, y, by=NULL, exog.df=NULL, ts_new_x=NULL,
         ts_unit=NULL, ts_ahead=0, ts_method=NULL,
         ts_fitted=FALSE, ts_source="classic", n_date_tics=NULL,
         ts_trend=NULL, ts_seasons=NULL, ts_error=NULL,
         ts_alpha=NULL, ts_beta=NULL, ts_gamma=NULL, ts_PI=0.95,
         digits_d=getOption("digits_d"), quiet=getOption("quiet"))  {


  nd <- .max.dd(y[,1])

  if (is.null(digits_d)) digits_d <- 3
  n.d <- digits_d - 1
  if (n.d <= 2) n.d <- 3

  x.name <- getOption("xname")
  y.name <- names(y)
  if (is.null(digits_d)) digits_d <- .max.dd(y[,1]) + 1

  # get x.dates, a superset of x.fit for HoltWinters()
  n.by <- ifelse (is.null(by), 0, nlevels(by))
  if (n.by == 0)
    x.dates <- x[,1]  # save actual dates as a vector
  else {  # x-axis tics just for one level of by
    cnt <- sum(by == levels(by)[1])  # just dates for the first level
    x.dates <- x[1:cnt,1]  # assumes by is sorted by level
  }

  # when computing x.hat, need the +1 and [-1] to not duplicate last x value
  hold.days7 <- ifelse (ts_unit == "days7", TRUE, FALSE)
  if (hold.days7) ts_unit <- "days"
  x.hat <- seq.Date(x[nrow(x),1], by=ts_unit, length.out=ts_ahead+1)[-1]
  if (hold.days7) ts_unit <- "days7"

  # create time series for stl() for lm and HoltWinters()
  y.ts <- .tsMake(x.dates, y, ts_unit)
  freq <- frequency(y.ts)
  min.2per <- nrow(y) >= (2 * freq)  # evaluate seasonality eligibility
  if (!is.null(ts_seasons)) if (ts_seasons!="N") {
    if (ts_unit == "years")
      message("Seasonal effects are not possible with annual data.")
    else if (!min.2per)
      message("\nSeasonal effects over a year of aggregated data are \n",
              "usually limited to monthly or quarterly data. Usually\n",
              "need two years worth of data to estimate seasonality.\n")
    if (ts_unit=="years" || !min.2per) ts_seasons <- "N"
  }  # end no seasons


  # fable
  # -----
  if (ts_source == "fable")  { 

    .check.packages()  # do needed packages exist?
    .valid.params(ts_method, ts_error, ts_trend, ts_seasons)  # valid params?

    if (!quiet) {
      txt <- "Hyndman and Athanasopoulos's, fpp3 packages]"
      cat("[with functions from", txt, "\n",
          "  -- standard reference: https://otexts.com/fpp3/\n")
    }

    decomp.frm <- NULL  # only passed to Plot() if ts_fitted=TRUE
    out_report <- NULL  # report(fit) implicitly displayed, no need to save 

    # create base data frame, then to tsibble
    d.data <- data.frame(x = x, y = y, check.names = FALSE)
    if (!is.null(exog.df) && ncol(exog.df) > 0)  # add any exog preds
      d.data <- cbind(d.data, exog.df)
    index.var <- names(d.data)[1]
    tsbl <- .to_tsbl(d.data, ts_unit) # index <date>, <week> <mth>, <qtr>, <int>

    # model components
    rhs_terms <- c()
    if (ts_method == "es") {
      if (!is.null(ts_error)) {
        if (ts_error != "N") {
          term <- paste0("error('", ts_error, "'")
          if (!is.null(ts_alpha)) term <- paste0(term, ", alpha = ", ts_alpha)
          term <- paste0(term, ")")
          rhs_terms <- c(rhs_terms, term)
        }
      }
      if (!is.null(ts_trend)) {
        if (ts_trend != "N") {
          term <- paste0("trend('", ts_trend, "'")
          if (!is.null(ts_beta)) term <- paste0(term, ", beta = ", ts_beta)
          term <- paste0(term, ")")
          rhs_terms <- c(rhs_terms, term)
        }
      }
      if (!is.null(ts_seasons)) {
        if (ts_seasons != "N") {
          term <- paste0("season('", ts_seasons, "'")
          if (!is.null(ts_gamma)) term <- paste0(term, ", gamma = ", ts_gamma)
          term <- paste0(term, ")")
          rhs_terms <- c(rhs_terms, term)
        }
      }
    }  # end es
    else if (ts_method == "lm") {
      if (!is.null(ts_trend)) {
        if (ts_trend != "N")
          rhs_terms <- c(rhs_terms, "trend()")
      }
      if (!is.null(ts_seasons)) {
        if (ts_seasons != "N")
          rhs_terms <- c(rhs_terms, "season()")
      }
      if (!is.null(exog.df))
        rhs_terms <- c(rhs_terms, names(exog.df))
    }  # end lm


    # Fit model
    # ----------
    empty_rhs <- is.null(rhs_terms) || length(rhs_terms) == 0L

    if (ts_method == "es") {
      # ----- ETS -----
      if (empty_rhs) {
        fit <- fabletools::model(
          tsbl, ets = do.call(fable::ETS, list(as.name(y.name)))
        )
        fml <- paste(y.name, " [with no specifications]")  # for display only
      }
      else {
        fml <- stats::as.formula(
          paste(y.name, "~", paste(rhs_terms, collapse = " + "))
        )
        fit <- fabletools::model(tsbl, ets = fable::ETS(fml))
      }

    }
    else if (ts_method == "lm") {
      # ----- TSLM -----
      if (empty_rhs) {
        fml <- stats::as.formula(paste(y.name, "~ 1"))
      } else {
        fml <- stats::as.formula(
          paste(y.name, "~", paste(rhs_terms, collapse = " + "))
        )
      }
      fit <- fabletools::model(tsbl, tslm = fable::TSLM(fml))
    }

    # extract the ets model object to check for a NULL model fit
    if (ts_method == "es") {
      model_obj <- fit$ets[[1]]
      model_str <- capture.output(print(model_obj$model))
      if (any(grepl("null_mdl", model_str))) {
        stop(paste(
          'Model fitting failed. Simplify the model such as,\n',
          '      ts_trend="N" and ts_season="N", or use more data.\n\n'
        ), call. = FALSE)
      }
    }

    # display specified and estimated models
    if (ts_method == "es") {
      actual_model_string <- format(fit$ets[[1]])
      components <- gsub("ETS\\(|\\)", "", actual_model_string)
      parts <- strsplit(components, ",")[[1]]
      names(parts) <- c("error", "trend", "season")
      formula_terms <- c()
      formula_terms <- c(formula_terms,
                           paste0('error("', parts["error"], '")'))
      if (parts["trend"] != "N")
        formula_terms <- c(formula_terms,
                           paste0('trend("', parts["trend"], '")'))
      if (parts["season"] != "N")
        formula_terms <- c(formula_terms,
                           paste0('season("', parts["season"], '")'))
      actual_model_expr <- paste0(y.name, " ~ ",
                                  paste(formula_terms, collapse=" + "))
    }
    else if (ts_method == "lm")
      actual_model_expr <- paste(deparse(fit$tslm[[1]]$model$formula),
                                 collapse=" ")

    if (!quiet) {
      cat("\nSpecified model\n---------------\n")
      if (empty_rhs) {
        if (ts_method=="es")
          cat("  ", fml, "\n")
        else if (ts_method=="lm")
          cat("  ", deparse(fml), 
              " [consider adding terms: ts_trend and ts_seasons]\n")
      }
      else 
        cat("  ", deparse(fml), "\n")
#     if (ts_method == "es") {
        cat("The specified model is only suggested,",
            "and may differ from the estimated model.\n\n")
        cat("Estimated model\n---------------\n")
        cat(actual_model_expr, "\n")
#     }
      cat("\n")
    }

    # summary of model type and fit
    if (!quiet) {
      cat("\nModel analysis\n--------------\n")
      fabletools::report(fit)
    }
    if (ts_method == "es") {
      m <- fit$ets[[1]]
      MSE <- m$fit$fit$MSE
    }
    else if (ts_method == "lm") {
      res <- fabletools::augment(fit)$`.resid`
      MSE <- mean(res^2, na.rm = TRUE)
    }
    if (!quiet)
      cat("\nMean squared error of fit to data:", .fmt_pn(MSE,3), "\n\n")

    # get x.fit and y.fit
    fitted.tbl <- fabletools::augment(fit)
    index.var <- tsibble::index_var(fitted.tbl)

    x.fit <- as.character(fitted.tbl[[index.var]])
    if (ts_unit == "days"){
      x.fit <- zoo::as.Date(x.fit)  # if already ISO: "2024-01-01"
    }
    else if (ts_unit == "weeks") {
      x.fit <- as.Date(tsibble::yearweek(x.fit))
    }
    else if (ts_unit == "months") {
      yearmon_obj <- zoo::as.yearmon(x.fit, format = "%Y %b")
      x.fit <- zoo::as.Date(yearmon_obj)
    }
    if (ts_unit == "quarters") {
      yearqtr_obj <- zoo::as.yearqtr(x.fit, format = "%Y Q%q")
      x.fit <- zoo::as.Date(yearqtr_obj)
    }
    else if (ts_unit == "years") { # e.g., 2026 to Date ("2026-01-01")
      x.fit <- zoo::as.Date(paste0(as.character(x.fit), "-01-01"))
    }

    y.fit <- fitted.tbl[[".fitted"]]

  
    # forecast
    # --------
    if (!is.null(exog.df)) exog.df <- .to_tsbl(ts_new_x, ts_unit)

    if (is.null(exog.df)) {
      frcst <- fabletools::forecast(fit, h = ts_ahead)
    }
    else {
      if (!(names(exog.df)[1] == names(d.data)[1]))
        exog.df <- .generate_future_df(d.data, ts_unit, ts_ahead, exog.df)
      frcst <- fabletools::forecast(fit, new_data = exog.df)
    }

    # prediction interval at level ts_PI
    mu    <- vapply(frcst[[y.name]], mean, numeric(1))
    sigma <- sqrt(vapply(frcst[[y.name]], distributional::variance,
                  numeric(1)))
    z <- qnorm((1 + ts_PI) / 2)  # two-sided z-score
    y.lwr <- mu - z * sigma
    y.upr <- mu + z * sigma

    # get x.hat and y.hat
    x.hat <- frcst[[2]]
    if (ts_unit == "years" && is.numeric(x.hat)) {
      x.hat <- as.Date(paste0(x.hat, "-01-01"))
    }
    else
      x.hat <- zoo::as.Date(x.hat)

    y.hat <- frcst$.mean
  }  # end fable


  # classic
  # -------
  else if (ts_source == "classic") {

    # HoltWinters
    # -----------
    if (ts_method == "es") {

      if (!is.null(ts_trend)) {
        if (!ts_trend %in% c("A", "N")) {
          stop("\n------\n",
               'HoltWinters() only supports "A" for additive trend\n',
               'or "N" for no trend.\n\n')
        }
      }
      if (!is.null(ts_seasons)) {
        if (!ts_seasons %in% c("A", "M", "N")) {
          stop("\n------\n",
               'HoltWinters() only supports "A" for additive, "M" for\n',
               'multiplicative, or "N" for no seasonality.\n\n')
        }
      }

      if (!is.null(ts_trend))
        do.trend <- ifelse (ts_trend == "A", TRUE, FALSE)
      else
        do.trend <- FALSE
      if (!is.null(ts_seasons))
        do.seasons <- ifelse (ts_seasons %in% c("A","M"), TRUE, FALSE)
      else
        do.seasons <- FALSE

      bet <- NULL
      if (do.trend) {
        if (!is.null(ts_beta)) bet <- ts_beta
      }
      else  # "N"
        bet <- FALSE

      seasonal <- "additive"  # default
      gam <- NULL
      if (do.seasons) {
        if (ts_seasons == "A") {
          seasonal <- "additive"
          if (!is.null(ts_gamma)) gam <- ts_gamma
        }
        else if (ts_seasons == "M") {
          seasonal <- "multiplicative"
          if (!is.null(ts_gamma)) gam <- ts_gamma
        }  # "N"
        else
          gam <- FALSE 
      }

      # fit the data
      fit <- HoltWinters(y.ts, alpha=NULL, beta=bet, gamma=gam,
                         seasonal=seasonal)
      colnames(fit$fitted)[1] <- "fitted"
      yf <- .tsExtract(fit$fitted[,1], x.name)
      x.fit <- yf$x.dates
      y.fit <- yf$y

      alpha.f <- fit$alpha;  beta.f <- fit$beta;  gamma.f <- fit$gamma

      coefs <- fit$coefficients
      if (names(coefs)[1] == "a") names(coefs)[1] <- "b0"
      if (length(coefs) > 1)
        if (names(coefs)[2] == "b") names(coefs)[2] <- "b1"

      # predict with prediction intervals
      y.pred <- predict(fit, n.ahead=ts_ahead,
                        prediction.interval=TRUE, level=ts_PI)  # mts
      width <- y.pred[, "upr"] - y.pred[, "lwr"]
      y.pred <- cbind(y.pred, width=width)

      # get predicted values and PI from each individual time series
      y.ahead <- .tsExtract(y.pred[,1], x.name)
      y.hat <- y.ahead$y
      xhw.hat <- y.ahead$x.dates
      diffx.dates <- diff(x.dates)
      x.pred1 <- x.dates[length(x.dates)]
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
      MSE <- fit$SSE / (nrow(x.fit) - n.param)

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

      decomp.frm <- NULL
    } # end HW


    # Linear Regression with Seasonality
    # ----------------------------------
    if (ts_method == "lm") {

     if (!is.null(ts_trend)) {
       if (!ts_trend %in% c("A", "N")) {
          stop("\n------\n",
               'Enter either "A" for additive trend or "N" for no trend.\n\n')
        }
      }
      if (!is.null(ts_seasons)) {
        if (!ts_seasons %in% c("A", "N")) {
          stop("\n------\n",
             'Enter either "A" for additive seasons or "N" for no seasons.\n\n')
        }
      }

      x.fit <- x.dates
      decomp.frm <- NULL
   
      if (!is.null(ts_trend))
        do.trend <- ifelse (ts_trend == "A", TRUE, FALSE)
      else
        do.trend <- FALSE
      if (!is.null(ts_seasons))
        do.seasons <- ifelse (ts_seasons == "A", TRUE, FALSE)
      else
        do.seasons <- FALSE

      # decompose the time series with stl()
      if (do.seasons) {
        decomp <- stl(y.ts, s.window="periodic")
        if (do.trend)  # de-seasonalize
          y.trend <- decomp$time.series[,"trend"] +
                     decomp$time.series[,"remainder"]
        else
          y.trend <- decomp$time.series[,"remainder"] + mean(y[,1])
      }  # end do.seasons
      else {  # no seasonal analysis
        if (do.trend)
          y.trend <- y[,1]
        else
          y.trend <- rep(mean(y[,1], na.rm=TRUE), length(y[,1]))
      }

      # fit linear regression on (usually) deseasonalized data
      x.seq <- 1:length(y.trend)
      fit <- lm(y.trend ~ x.seq)
      y.fit.trend <- fit$fitted.values  # reg fitted y.trend values
      varcov <- vcov(fit)  # variance-covariance matrix of coefficients
      coefs <- fit$coefficients
      names(coefs) <- c("b0", "b1")

      # forecast
      if (do.seasons) {
        # get seasonal indices of data, start at the next season
        n.cycles <- length(y[,1]) / freq
        season.ind <- rep(1:freq, ceiling(n.cycles))[1:length(y[,1])]
        last_index <- tail(season.ind, 1)
        start.ind <- (last_index %% freq) + 1

        # get seasonal indices of forecast
        new.ind <- (start.ind + seq_len(ts_ahead) - 1) %% freq
        new.ind[new.ind == 0] <- freq  # replace 0

        # map seasonal effects to forecasted indices
        y.seas.eff <- as.numeric(decomp$time.series[,"seasonal"])
        for (i in 1:freq) {
          coefs[length(coefs)+1] <- y.seas.eff[i]
          names(coefs)[length(coefs)] <- paste0("s", i, sep="")
        }
        new.seas.eff <- y.seas.eff[new.ind]
      }  # end if no seasons

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
      t.crit <- qt((1 + ts_PI)/2, df=fit$df.residual)  # critical t-value
      half.width <- t.crit * se_forecast
      y.lwr <- as.vector(y.hat) - half.width
      y.upr <- as.vector(y.hat) + half.width

      # output data frame of fitted values
      if (ts_fitted) {
        xc.fit <- .toFmtDate(x.dates, ts_unit)  # e.g., 2020-01-01 to 2020 Q1
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

      out_report <- NULL
      txparam <- NULL
    }  # end ts_method=="lm"


    tx <- character(length = 0)
    if (!getOption("suggest")) tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste("Mean squared error of fit to data:",
                              .fmt_cm(MSE, n.d+2))
    txerr <- tx

    tx <- character(length = 0)
    tx[length(tx)+1] <- "Coefficients for Linear Trend"
    if (!is.null(ts_seasons)) if (ts_seasons!="N") {
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

    out_report <- c(txparam, txerr, "", txcoef)
  }  # end classic
  # --------------


  # Output data frame with data + fitted values
  if (ts_fitted) {

    xc <- .toFmtDate(x[,1], ts_unit)  # e.g., 2020-01-01 to 2020 Q1

    # Convert numeric vectors to character and pad for no data values
    if (ts_source == "classic") {
      if (ts_method == "es") {

        diff.df <- length(x.dates) - nrow(x.fit)
        x.fit <- x.dates[(diff.df+1) : length(x.dates)]  # es drops first  dates
        xc.fit <- x.dates
        if (ts_unit == "months")   # display monthly fit
          xc.fit <- as.character(zoo::as.yearmon(xc.fit))
        else if (ts_unit == "quarters")  # display quarterly fit
          xc.fit <- as.character(zoo::as.yearqtr(xc.fit))
        else if (ts_unit == "years")  # display annual fit
          xc.fit <- as.character(format(xc.fit, "%Y"))

        fdf <- as.data.frame(fit$fitted) # do a df instead of a multivariate ts 
        pad <- rep("", nrow(x) - nrow(fdf))
        fitted_chr <- c(pad, as.character(.fmt(fdf[,1],nd)))
        level_chr  <- c(pad, as.character(.fmt(fdf[,2],nd)))
        out_fitted <- data.frame(
          date=xc.fit,
          y=y[,1],
          fitted=fitted_chr,
          level=level_chr
        )
        names(out_fitted)[1:2] <- c(x.name, y.name)

        if (do.trend) {
          trend_chr  <- c(pad, as.character(.fmt(fdf$trend,nd)))
          out_fitted <- cbind(out_fitted, trend=trend_chr)
        }
        if (do.seasons) {
          seasons_chr  <- c(pad, as.character(.fmt(fdf$season,nd)))
          out_fitted <- cbind(out_fitted, season=seasons_chr)
        }
        res <- residuals(fit)          # stats::residuals.HoltWinters
        res_chr  <- c(pad, as.character(.fmt(res,nd)))
        out_fitted <- cbind(out_fitted, error=res_chr)
      }  # end classic fit HW

      else if (ts_method == "lm") {
        out_fitted <- data.frame(
          date=x.fit,  # x.fit already in correct date format
          y=y[,1],
          fitted=y.fit[,1],
          error=y[,1]-fit$fitted
        )
        names(out_fitted)[1:2] <- c(x.name, y.name)
      }  # end lm
    }  # end classic fit

    else if (ts_source == "fable") { # fable fit

      if (ts_method == "es") {
        cmp <- fabletools::components(fit)
        fitted_tbl <- fabletools::augment(fit)
        out <- capture.output(print(cmp, n = 1))
        formula_line <- grep("^#\\s*:", out, value = TRUE)
        if (length(formula_line) > 0)
          frm <- sub("^#\\s*:\\s*", "", formula_line)
        else
          frm <- NA_character_
        decomp.frm <- paste("\nDecomposition formula:\n", frm, "\n\n")
        n.pad <- nrow(cmp) - nrow(fitted_tbl)
        xc <- c(rep(NA, n.pad), xc)
        y.fit.pad <- c(rep(NA, n.pad), y.fit)
        y <- data.frame(y.name = c(rep(NA, n.pad), y[,1]))
      } 

      else if (ts_method == "lm") {
        fitted_tbl <- fabletools::augment(fit)
        cmp <- data.frame(fitted = fitted_tbl$.fitted,
                      resid = fitted_tbl$.resid)
        decomp.frm <- "TSLM only provides fitted and residuals.\n\n"
      }

      out_fitted <- data.frame(date=xc, y=y[,1])
        names(out_fitted) <- c(x.name, y.name)

        if (ts_method == "es") {
          out_fitted <- cbind(out_fitted, fitted=y.fit.pad)
          out_fitted <- cbind(out_fitted, level=cmp$level)
        if (!is.null(ts_trend)) {
          if (ts_trend != "N")
            out_fitted <- cbind(out_fitted, trend=cmp$slope)
        }
        if (!is.null(ts_seasons)) {
          if (ts_seasons != "N")
            out_fitted <- cbind(out_fitted, season=cmp$season)
          out_fitted <- cbind(out_fitted, error=cmp$remainder)
        }

        # Replace NAs with blanks across all columns of out_fitted
        out_fitted[] <- lapply(out_fitted, function(col) {
          if (is.numeric(col)) 
            ifelse(is.na(col), "", as.character(.fmt(col, nd)))
          else
            ifelse(is.na(col), "", col)
        })
        }  # end es fit
      else if (ts_method == "lm")
        out_fitted <- cbind(out_fitted, cmp)
    }  # end fable fit
  } # end fitted

  else  # not fitted
    out_fitted <- NULL



  # Results
  # -------

  # Output data frame of forecasts
  xc.hat <- .toFmtDate(x.hat, ts_unit)  # e.g., 2020-01-01 to 2020 Q1
  y.frcst <- data.frame(
    date=xc.hat,
    predicted=as.vector(y.hat),
    lower=y.lwr,
    upper=y.upr,
    width=y.upr-y.lwr
  )
  names(y.frcst) <- c(x.name, "predicted", "upper", "lower", "width")

  x.fit <- data.frame(x.fit)
  y.fit <- data.frame(y.fit)
  x.hat <- data.frame(x.hat); names(x.hat) <- "x.dates"
  y.hat <- data.frame(y.hat)
  y.lwr <- data.frame(y.lwr);  names(y.lwr) <- y.name
  y.upr <- data.frame(y.upr);  names(y.upr) <- y.name


  # margin adjustments for plot
  # ---------------------------
  mn.x <- min(as.numeric(x.hat[,1]))
  mx.x <- max(as.numeric(x.hat[,1]))
  mx.y <- max(max(y, na.rm=TRUE), max(y.fit, na.rm=TRUE),
              max(y.upr, na.rm=TRUE), max(y.hat, na.rm=TRUE))
  mn.y <- min(min(y, na.rm=TRUE), min(y.fit, na.rm=TRUE),
              min(y.lwr, na.rm=TRUE), min(y.hat, na.rm=TRUE))

  return(list(y.fit=y.fit, y.hat=y.hat, y.frcst=y.frcst,
              x.fit=x.fit, x.hat=x.hat,
              y.upr=y.upr, y.lwr=y.lwr,
              mn.x=mn.x, mx.x=mx.x, mn.y=mn.y, mx.y=mx.y,
              out_fitted=out_fitted,
              out_decomp=decomp.frm, out_report=out_report))
}

