STL <- function(x, y=NULL, data=d, filter=NULL,
                ts_format=NULL, ts_unit=NULL, ts_agg=c("sum","mean"),
                show_range=FALSE, robust=FALSE, quiet=FALSE, do_plot=TRUE,
                pdf_file=NULL, width=6.5, height=6) {
  ts_agg <-  match.arg(ts_agg)

  x.name <- deparse(substitute(x))
  y.name <- deparse(substitute(y))

  # subset filter
  if (!missing(filter)) {
    txt <- .filter(deparse(substitute(filter)))
    if (!missing(filter))  # subset filter
      r <- eval(str2expression(txt), envir=data, enclos=parent.frame())
    r <- r & !is.na(r)  # set missing for a row to FALSE
    nr.before <- nrow(data)
    if (any(r))
      data <- data[r,,drop=FALSE]
    if (!getOption("quiet")) {
      if (!missing(filter))  # filter parameter present
        cat("filter: ",  txt, "\n-----\n")
      cat("Rows of data before filtering: ", nr.before, "\n")
      cat("Rows of data after filtering:  ", nrow(data), "\n\n")
    }
  }  # end filter

  if (!is.null(x.name))
    x.in.global <- .in.global(x.name, getOption("quiet"))
  else
    x.in.global <- FALSE

  if (!x.in.global)  {
    data.vars <- as.list(seq_along(data))
    names(data.vars) <- names(data)

    ind <- eval(substitute(x), envir=data.vars)  # col num of x var
    x.date <- data[, ind, drop=FALSE]

    # see if x.date has char string values to convert to type Date
    if (is.character(x.date[1,1])) {
      if (!is.null(ts_format))  # specify the date format
        x.date[,1] <- as.Date(x[,1], format=ts_format)
      else  # see if a date, and if so, infer the date format
        x.date <- date.infer(x.date)
    }

    is_ordered <- all(diff(x.date[,1]) >= 0) 
    if (!is_ordered) {
      cat("\n"); stop(call.=FALSE, "\n------\n",
        "The date variable, ", x.name, ", must be ordered across all data.\n\n",
        "Perhaps you have multiple groups of ", y.name, ".\n",
        "If so, use the parameter  filter  to focus on just one group.\n\n")
    }

    ind <- eval(substitute(y), envir=data.vars)  # col num of x var
    y.data <- data[, ind]

    #   if not specified, get the existing ts_unit
    #   if specified, aggregate the values of y.data over x
    tsdata <- .plt.time(x.date, y.data, by.call=NULL, x.name, n.by=0,
                        ts_unit, ts_agg)
    ts_unit <- tsdata$ts_unit
    x.date <- tsdata$x.call[,1]
    y.data <- tsdata$y.call[,1]
#   do.agg <- tsdata$do.agg
    y.ts <- .tsMake(x.date, y.data, ts_unit)  # convert data to ts form

    is.ts <- FALSE
  }  # end x is not in global

  else if (is.ts(x)) {
    is.ts <- TRUE
    y.ts <- x
    tsPull <- .tsExtract(x)
    y.data <- tsPull$y
    y.name <- tsPull$y.name
  }

  # compute components
  decomp <- stl(y.ts, s.window="periodic", robust=robust)

  # set up pdf_file if needed
  if (!is.null(pdf_file)) {
    if (!grepl(".pdf", pdf_file))
      pdf_file <- paste(pdf_file, ".pdf", sep="")
    pdf(file=pdf_file, width=width, height=height, onefile=FALSE)
  }

  if (do_plot)
    plot(decomp, col.range="lightgoldenrod") 

  if (!is.null(pdf_file)) {
    dev.off()
    if (!quiet) .showfile(pdf_file, "STL decomposition")
  }

  # extract components from the stl() decomposition
  seasonal <- decomp$time.series[, "seasonal"]
  trend <- decomp$time.series[, "trend"]
  error <- decomp$time.series[, "remainder"]

  if (!quiet) {

    # calculate the range of y and of each component
    range.y <- diff(range(y.data, na.rm=TRUE))
    range.seasonal <- diff(range(seasonal, na.rm=TRUE))
    range.trend <- diff(range(trend, na.rm=TRUE))
    range.error <- diff(range(error, na.rm=TRUE))

    # calculate the variance of y and of each component
    var.y <- var(y.data, na.rm = TRUE)
    var.seasonal <- var(seasonal, na.rm =TRUE)
    var.trend <- var(trend, na.rm=TRUE)
    var.error <- var(error, na.rm=TRUE)

    # calculate the proportion of variance explained by each component
    prop.seasonal <- var.seasonal / var.y
    prop.trend <- var.trend / var.y
    prop.error <- var.error / var.y

    # display the results
    cat("\nTotal variance of ", y.name, ": ", var.y, "\n", sep="")
    cat("Proportion of variance for components:\n")
    cat("  seasonality ---", .fmt(prop.seasonal,3), "\n")
    cat("  trend ---------", .fmt(prop.trend, 3), "\n")
    cat("  remainder -----", .fmt(prop.error, 3), "\n")

    if (show_range) {
      cat("\nRange of ", y.name, ": ", range.y, "\n", sep="")
      cat("Range of components:\n")
      cat("  seasonality ---", .fmt(range.seasonal,3), "\n")
      cat("  trend ---------", .fmt(range.trend, 3), "\n")
      cat("  remainder -----", .fmt(range.error, 3), "\n")
    }

    cat("\n")

  }  # end !quiet

  # output data structure
  te <- .tsExtract(trend)  # te is a multivariate time series
  if (!is.ts)
    de <- .toFmtDate(te[[1]][,1], ts_unit)  # e.g., 2020-01-01 to 2020 Q1
  else
    de <- te[[1]][,1]
  te <- as.numeric(te[[2]][,1])  # trend component
  se <- .tsExtract(seasonal)
  se <- as.numeric(se[[2]][,1])  # seasonal component
  ee <- .tsExtract(error)
  ee <- as.numeric(ee[[2]][,1])  # error component
  out <- data.frame(de, y.data, te, se, ee)
  if (is.ts) {
    x.name <- "date"
    y.name <- "data"  # not recovered
  }
  names(out) <- c(x.name, y.name, "trend", "season", "error")
  return(invisible(out))

}
