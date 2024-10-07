STL <- function(x, y=NULL, data=d, filter=NULL,
                time_format=NULL, robust=FALSE) {

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
    data.x <- data[, ind]

    # if needed, convert data.x to a Date variable
    if (is.character(data.x[1])) {
      if (!is.null(time_format))  # specify the date format
        data.x <- as.Date(data.x, format=time_format)
      else {  # guess the date format
        n.ch <- nchar(data.x[1])
        n.valid <- ifelse (n.ch %in% c(6,8), TRUE, FALSE)
        punct <- " "
        if ((grepl("/", data.x[1], fixed=TRUE) && n.valid)) punct <- "/"
        if ((grepl("-", data.x[1], fixed=TRUE) && n.valid)) punct <- "-"
        if ((grepl(".", data.x[1], fixed=TRUE) && n.valid)) punct <- "."
        if (punct %in% c("/", "-", ".")) {  # only evaluate probable dates
          data.x <- .charToDate(data.x, punct, n.ch)
        }  # end do best guess
      }  # end is.char x.call[,1]data.y <- data[, ind]
    }

    ind <- eval(substitute(y), envir=data.vars)  # col num of x var
    data.y <- data[, ind]

    y.ts <- .tsMake(data.x, data.y)  # convert data to ts form
  }

  else if (is.ts(x)) {
    y.ts <- x 
    tsPull <- .tsExtract(x)
    data.y <- tsPull$y
    y.name <- tsPull$y.name
  }

  # compute components
  decomp <- stl(y.ts, s.window = "periodic", robust=robust)
  plot(decomp, col.range="lightgoldenrod")

  # extract components from the stl() result
  seasonal <- decomp$time.series[, "seasonal"]
  trend <- decomp$time.series[, "trend"]
  error <- decomp$time.series[, "remainder"]

  # calculate the range of y and of each component
  range.y <- diff(range(data.y, na.rm=TRUE))
  range.seasonal <- diff(range(seasonal, na.rm=TRUE))
  range.trend <- diff(range(trend, na.rm=TRUE))
  range.error <- diff(range(error, na.rm=TRUE))

  # calculate the variance of y and of each component
  var.y <- var(data.y, na.rm = TRUE)
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

  cat("\nRange of ", y.name, ": ", range.y, "\n", sep="")
  cat("Range of components:\n")
  cat("  seasonality ---", .fmt(range.seasonal,3), "\n")
  cat("  trend ---------", .fmt(range.trend, 3), "\n")
  cat("  remainder -----", .fmt(range.error, 3), "\n")

}
