.plt.time <- function(x.call, y.call, by.call, x.name, n.by,
                      ts_unit, ts_agg) {


  ts_truncate <- function(x.call, y.call, tu_exist, ts_unit) {

    # helper function: end date of the target period containing 'dd'
    end_of_period <- function(dd, unit) {
      y <- as.integer(format(dd, "%Y"))
      m <- as.integer(format(dd, "%m"))

      if (unit == "weeks") {
        # week ending Sunday (ISO weekday: 1=Mon ... 7=Sun)
        wd <- as.integer(strftime(dd, "%u"))
        dd + (7L - wd)
      }
      else if (unit == "months") {
        first_next <- as.Date(sprintf("%04d-%02d-01",
                                      y + (m == 12L),
                                      ifelse(m == 12L, 1L, m + 1L)))
        first_next - 1L
      }
      else if (unit == "quarters") {
        q <- ((m - 1L) %/% 3L) + 1L
        end_m <- q * 3L
        first_next <- as.Date(sprintf("%04d-%02d-01",
                                      y + (end_m == 12L),
                                      ifelse(end_m == 12L, 1L, end_m + 1L)))
        first_next - 1L
      }
      else if (unit == "years") {
        as.Date(sprintf("%04d-12-31", y))
      }
      else { # days
        dd
      }
    }  # end end_of_period()

    stopifnot(is.data.frame(x.call), ncol(x.call) == 1L)
    d <- x.call[[1L]]
    stopifnot(inherits(d, "Date"))

    units <- c("days","weeks","months","quarters","years")
    if (!tu_exist %in% units)
      stop("tu_exist must be one of: ", paste(units, collapse=", "))
    if (!ts_unit  %in% units)
      stop("ts_unit must be one of: ",  paste(units, collapse=", "))

    # if not aggregating upward, nothing to truncate
    iu <- match(tu_exist, units)
    it <- match(ts_unit,  units)
    if (it <= iu)
      return(list(x = x.call, y = y.call, keep = rep(TRUE, length(d))))

    # group key for target unit, to identify which rows are in the last group
    yr <- as.integer(format(d, "%Y"))
    mo <- as.integer(format(d, "%m"))

    g_target <- switch(
      ts_unit,
      "years"    = paste0(yr),
      "quarters" = paste0(yr, "-Q", ((mo - 1L) %/% 3L) + 1L),
      "months"   = paste0(yr, "-", sprintf("%02d", mo)),
      "weeks"    = paste0(format(d, "%G"), "-W", format(d, "%V")),
      "days"     = format(d, "%Y-%m-%d")
    )

    # last observed date determines whether the final target period is complete
    max_d    <- max(d, na.rm = TRUE)
    end_last <- end_of_period(max_d, ts_unit)

    keep <- rep(TRUE, length(d))

    if (max_d < end_last) {
      last_g  <- g_target[which.max(d)]     # group containing max date
      keep[g_target == last_g] <- FALSE
    }

    list(
      x = x.call[keep, , drop = FALSE],
      y = y.call[keep, , drop = FALSE],
      keep = keep
    )
  }  # end ts_truncate()


# begin Analysis ----------------------------------------------------------

  # from STL() that passes vectors, not dfs
  if (!is.data.frame(x.call)) x.call <- data.frame(x.call)
  if (!is.data.frame(y.call)) y.call <- data.frame(y.call)

  # get existing ts_unit
  # --------------------
  if (is.null(by.call))
    intervals <- diff(x.call[,1])
  else {  # eval intervals only for 1st level of by, assume sorted
    n.1 <- table(by.call)[1]
    intervals <- diff(x.call[1:n.1, 1])
  }

  # Determine the frequency based on date interval, all but last value
  # intervals should be computed on unique sorted dates
  d <- sort(unique(x.call[[1L]]))
  intervals <- as.integer(diff(d))

  # remove any nonpositive diffs that should not exist after unique/sort
  intervals <- intervals[intervals > 0L]

  if (length(intervals) == 0L) {
    tu_exist <- "days"
  } else {

  # typical gap (median is robust to occasional missing dates)
  gap <- as.integer(stats::median(intervals))

  if (gap == 1L) {
    tu_exist <- "days"
  } else if (gap == 7L) {
    tu_exist <- "weeks"
  } else if (gap >= 28L && gap <= 31L && all(as.integer(format(d, "%d")) == 1L)) {
    tu_exist <- "months"
  } else if (gap >= 89L && gap <= 92L &&
             all(as.integer(format(d, "%d")) == 1L) &&
             all(as.integer(format(d, "%m")) %in% c(1L,4L,7L,10L))) {
    tu_exist <- "quarters"
  } else if (gap %in% c(365L, 366L) &&
             all(as.integer(format(d, "%d")) == 1L) &&
             all(as.integer(format(d, "%m")) == 1L)) {
    tu_exist <- "years"
  } else {
    tu_exist <- "unknown"
  }
}

  # does data support aggregation level?
  t.units <- c("unknown","days","weeks","months","quarters","years")
  t.units <- factor(t.units, levels=t.units, ordered=TRUE)
  do.agg <- FALSE
  if (!is.null(ts_unit)) {
    hold.unit <- ifelse (ts_unit == "days7", TRUE, FALSE)
    if (hold.unit) ts_unit <- "days"
    if (which(t.units==ts_unit) < which(t.units == tu_exist)) {
        stop("Resolution of data is  ", tu_exist, "\n",
             "Requested data for ", ts_unit, " is not available\n\n",
             call. = FALSE)
    }
    else
      do.agg <- TRUE
  }
  else
    hold.unit <- FALSE


  # option: aggregate date variable by specified ts_unit
  # ----------------------------------------------------
  if (!is.null(ts_unit) &&  do.agg) {

    # define the aggregation function: aggfun()
    if (ts_agg == "sum")
      aggfun <- function(x) colSums(x)  # leave na.rm=FALSE to have NA agg
    else if (ts_agg == "mean")
      aggfun <- function(x) colMeans(x)

    if (is.null(by.call)) {  # aggregate all data
      res <- ts_truncate(x.call, y.call, tu_exist=tu_exist, ts_unit=ts_unit)
      x.call <- res$x
      y.call <- res$y

      # create xts time series, get ts_unit endpoints, aggregate y
      d.xts <- xts::xts(y.call[, seq_len(ncol(y.call))],
                        order.by=x.call[,1])
      names(d.xts) <- names(y.call)
      ep <- xts::endpoints(d.xts, on=ts_unit)
      da.xts <- xts::period.apply(d.xts[,], INDEX=ep, FUN=aggfun)
      # move dates from end of month to beginning of month
      if (ts_unit == "quarters") {
        idx <- zoo::as.yearqtr(zoo::index(da.xts))
        zoo::index(da.xts) <- zoo::as.Date(idx)
      } else if (ts_unit == "months") {
        idx <- zoo::as.yearmon(zoo::index(da.xts))
        zoo::index(da.xts) <- zoo::as.Date(idx)
      } else if (ts_unit == "years") {  # use January 1st of each year
        zoo::index(da.xts) <- as.Date(format(zoo::index(da.xts), "%Y-01-01"))
      }
      # extract aggregated x.call and y.call from xts time series
      x.call <- data.frame(zoo::index(da.xts))
      names(x.call)[1] <- x.name
      y.call <- data.frame(zoo::coredata(da.xts))
    }

    else {  # aggregate separately for each level of by, join
      x.cl <- data.frame(date = as.Date(character(0)))
      y.cl <- data.frame(y = numeric(0))
      by.cl <- data.frame(byc = NA)
      by.cl <- by.cl[-1, ]
      for (k in seq_len(n.by)) {
        xl <- x.call[by.call==levels(by.call)[k], , drop=FALSE]
        yl <- y.call[by.call==levels(by.call)[k], , drop=FALSE]

        # truncate trailing partial target periods within each group
        res <- ts_truncate(xl, yl, tu_exist = tu_exist, ts_unit = ts_unit)
        xl  <- res$x
        yl  <- res$y
        if (nrow(xl) == 0L) next

        d.xts <- xts::xts(yl[, seq_len(ncol(yl))], order.by=xl[,1])
        names(d.xts) <- names(y.call)
        ep <- xts::endpoints(d.xts, on=ts_unit)
        da.xts <- xts::period.apply(d.xts[,], INDEX=ep, FUN=aggfun)
        if (ts_unit == "years")
          zoo::index(da.xts) <- as.Date(format(zoo::index(da.xts), "%Y-01-01"))
        else if (ts_unit %in% c("months", "quarters"))
          zoo::index(da.xts) <- as.Date(format(zoo::index(da.xts), "%Y-%m-01"))
        x.c <- data.frame(date = zoo::index(da.xts))
        y.c <- data.frame(y = zoo::coredata(da.xts))
        by.c <- data.frame(byc = rep(levels(by.call)[k], nrow(x.c)))
        # append
        x.cl <- rbind(x.cl, x.c)
        y.cl <- rbind(y.cl, y.c)
        by.cl <- rbind(by.cl, by.c)
      }  # end by loop
      names(x.cl)[1] <- x.name
      x.call <- x.cl
      y.call <- y.cl
      by.call <- by.cl[, 1, drop=TRUE]
      by.call <- factor(by.call)
    }
  }  # end aggregate ts_unit

  else  # ts_unit not specified, take as is, no aggregation
      ts_unit <- tu_exist

  if (hold.unit) ts_unit <- "days7"

  return(list(x.call=x.call, y.call=y.call, by.call=by.call,
          ts_unit=ts_unit, do.agg=do.agg))
}
