
.format_date_labels <- function(dates, ts_unit) {
  # ts_unit is unknown if x is a time series to begin with
  ts_unit <- match.arg(ts_unit, c("years","quarters","months","weeks", 
                                  "days","days7", "unknown"))
  switch(ts_unit,
    years    = format(dates, "%Y"),
    quarters = paste0(format(dates, "%Y"), " ", quarters(dates)),
    months   = format(dates, "%b %Y"),
    weeks    = format(dates, "%d %b %Y"),
    days     = format(dates, "%d %b %Y"),
    days7    = format(dates, "%d %b %Y")
  )
}


.check.packages <- function() {

  required_pkgs <- c("tsibble", "fable", "fabletools")

  quiet_require <- function(pkg) {
    suppressMessages(
      suppressPackageStartupMessages(
        requireNamespace(pkg, quietly = TRUE)
      )
    )
  }

  missing <- required_pkgs[!vapply(required_pkgs, quiet_require, logical(1))]

  if (length(missing)) {
    stop("To use ts_source = \"fable\" to forecast\n\n",
         "Please install:\n",
         paste0("  install.packages(\"", missing, "\")", collapse = "\n"),
         call. = FALSE)
  }

  # Optional: pre-load quietly (so later :: calls don’t trigger messages)
  invisible(lapply(required_pkgs, quiet_require))

  invisible(TRUE)
}


.valid.params <- function(ts_method, ts_error, ts_trend, ts_seasons) {
  if (ts_method == "es") {
    if (!is.null(ts_error)) {
      if (!ts_error %in% c("A", "M")) {
        stop("\n------\n",
             'ts_error can only be "A" (additive) or "M" (multiplicative).\n\n',
             call. = FALSE)
      }
    }
    if (!is.null(ts_trend)) {
      if (!ts_trend %in% c("N", "A", "M", "Ad", "Md")) {
        stop("\n------\n",
             'Specify  ts_trend  only as "N" (none), "A" (additive),\n',
             '"M" (multiplicative), "Ad" (damped additive),\n',
             '"Md" (damped multiplicative).\n\n', call. = FALSE)
      }
    }
    if (!is.null(ts_seasons)) {
      if (!ts_seasons %in% c("N", "A", "M")) {
        stop("\n------\n",
             'Specify  ts_seasons  only as "N" (none), "A" (additive),\n',
             'or "M" (multiplicative).\n\n', call. = FALSE)
      }
    }
  }  # end es

  else if (ts_method == "lm") {
    if (!is.null(ts_trend)) {
      if (!ts_trend %in% c("N", "A")) {
        stop("\n------\n",
             'Specify  ts_trend  only as "N" (none) or "A" (additive),\n',
             'for linear regression models.\n\n', call. = FALSE)
      }
    }
    if (!is.null(ts_seasons)) {
      if (!ts_seasons %in% c("N", "A")) {
      stop("\n------\n",
           'Specify  ts_seasons  only as "N" (none) or "A" (additive),\n',
           'for linear regression models.\n\n', call. = FALSE)
      }
    }
  }  # end lm
}


.to_tsbl <- function(df, ts_unit) {
  # df: data frame where first column is the index (date/time) variable
  # ts_unit: one of "days", "weeks", "months", "quarters", "years"
  
  # Name of the first column (index variable)
  index.var <- names(df)[1]
  
  # Convert index to correct type
  if (ts_unit == "weeks") {
    df[[index.var]] <- tsibble::yearweek(df[[index.var]])
  } 
  else if (ts_unit == "months") {
    df[[index.var]] <- tsibble::yearmonth(df[[index.var]])
  } 
  else if (ts_unit == "quarters") {
    df[[index.var]] <- tsibble::yearquarter(df[[index.var]])
  } 
  else if (ts_unit == "years") {
    df[[index.var]] <- as.integer(format(df[[index.var]], "%Y"))
  }
  # "days" assumed to already be Date
  
  # Convert to tsibble using Base R as.symbol()
  tsibble::as_tsibble(df, index = as.symbol(index.var))
}


.generate_future_df <- function(train.df, ts_unit, ts_ahead, predictors.df) {
  index.var <- names(train.df)[1]
  last_index <- train.df[[index.var]][nrow(train.df)]
  
  # Generate base future index sequence
  if (ts_unit == "weeks") {
    future_index <- seq(last_index + 7, by=7, length.out = ts_ahead)
  } else if (ts_unit == "months") {
    future_index <- seq(as.Date(last_index)+31, by="1 month",
                        length.out=ts_ahead)
  } else if (ts_unit == "quarters") {
    future_index <- seq(as.Date(last_index)+92, by="3 months",
                        length.out=ts_ahead)
  } else if (ts_unit == "years") {
    future_index <- seq(as.Date(last_index)+365, by="1 year",
                        length.out=ts_ahead)
  } else {
    future_index <- seq(as.Date(last_index)+1, by=1, length.out=ts_ahead)
  }
  
  # Adjust predictors.df row count if needed
  if (nrow(predictors.df) < ts_ahead) {
    stop("\n------\n",
         "The `ts_new_x` data frame has fewer rows than `ts_ahead`.\n",
         "Please provide future values for all forecast periods.\n",
         call. = FALSE)
  }
  if (nrow(predictors.df) > ts_ahead) {
    predictors.df <- predictors.df[1:ts_ahead, , drop=FALSE]
  }
  
  # Combine with predictors
  df.future <- data.frame(index=future_index, predictors.df, check.names=FALSE)
  names(df.future)[1] <- index.var
  
  # Match index type to training data
  if (ts_unit == "weeks") {
    df.future[[index.var]] <- tsibble::yearweek(df.future[[index.var]])
  } else if (ts_unit == "months") {
    df.future[[index.var]] <- tsibble::yearmonth(df.future[[index.var]])
  } else if (ts_unit == "quarters") {
    df.future[[index.var]] <- tsibble::yearquarter(df.future[[index.var]])
  } else if (ts_unit == "years") {
    df.future[[index.var]] <- as.integer(format(df.future[[index.var]], "%Y"))
  }
  
  df.future
}
