STL <- function(x, y=NULL, data=d, filter=NULL,
                ts_format=NULL, ts_unit=NULL, ts_agg=c("sum","mean"),
                show_range=FALSE, robust=FALSE, quiet=FALSE, do_plot=TRUE) {

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
    x.date <- data[, ind]

    # see if x.date has char string values to convert to type Date
    x11 <- x.date[1]
    if (is.character(x11)) {
      if (!is.null(ts_format))  # specify the date format
        x.date <- as.Date(x.date, format=ts_format)
      else {  # guess the date format
        n.ch <- nchar(x11)
        if (n.ch %in% 6:10) {
          isQ <- grepl("Q1|Q2|Q3|Q4", x11)
          isM <- grepl("Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec", x11)
          if (isM) {
            x.date <- gsub(" ", "", x.date)  # remove all spaces
            year <- substr(x.date, 1, 4)
            monthNm <- substr(x.date, 5, 7)
            x.date <- as.Date(paste(year, monthNm, "01", sep="-"),
                                   format="%Y-%b-%d")
          }
          else if (isQ) {  # convert dates entered as 2024 Q3 to R Date
            # returns a list, each element contains the Year and Quarter
            parts <- strsplit(gsub("\\s+", "", x.date), "Q")  # split on Q
            # Extract year and quarter, `[` is R extraction operator
            year <- as.numeric(sapply(parts, `[`, 1))  # 1st list element (year)
            quarter <- as.numeric(sapply(parts, `[`, 2))  # 2nd list element
            month <- 1 + (quarter - 1) * 3  # get month Q1=1, Q2=4, Q3=7, Q4=10
            x.date <- as.Date(paste(year, month, "01", sep="-"))  #to Date
          }  # end quarter
          else {  # regular numeric date format
            punct <- " "  # see if there are two punctuation delimiters
            if (length(gregexpr("/", x.date[1], fixed=TRUE)[[1]]) == 2) punct <- "/"
            if (length(gregexpr("-", x.date[1], fixed=TRUE)[[1]]) == 2) punct <- "-"
            if (length(gregexpr(".", x.date[1], fixed=TRUE)[[1]]) == 2) punct <- "."
            if (punct %in% c("/", "-", "."))   # only evaluate probable dates
              x.date <- .charToDate(x.date, punct)
          }  # regular date format
        }  # end n.ch is 6, 7, 8
      }  # end do best guess
    }  # is.char x.date[1]

    ind <- eval(substitute(y), envir=data.vars)  # col num of x var
    y.data <- data[, ind]

    #   if not specified, get the existing ts_unit
    #   if specified, aggregate the values of y.data over x.call
    tsdata <- .plt.time(x.date, y.data, by.call=NULL, x.name, n.by=0,
                        ts_unit, ts_agg)
    ts_unit <- tsdata$ts_unit
    x.date <- tsdata$x.call[,1]
    y.data <- tsdata$y.call[,1]
#   do.agg <- tsdata$do.agg
    y.ts <- .tsMake(x.date, y.data, ts_unit)  # convert data to ts form
  }

  else if (is.ts(x)) {
    y.ts <- x
    tsPull <- .tsExtract(x)
    y.data <- tsPull$y
    y.name <- tsPull$y.name
  }

  # compute components
  decomp <- stl(y.ts, s.window="periodic", robust=robust)
  if (do_plot)
    plot(decomp, col.range="lightgoldenrod")

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
  de <- as.character(te[[1]][,1])  # dates
  te <- as.numeric(te[[2]][,1])  # trend component
  se <- .tsExtract(seasonal)
  se <- as.numeric(se[[2]][,1])  # seasonal component
  ee <- .tsExtract(error)
  ee <- as.numeric(ee[[2]][,1])  # error component
  out <- data.frame(de, te, se, ee)
  names(out) <- c(x.name, "trend", "season", "error")
  return(invisible(out))

}
