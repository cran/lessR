.plt.time <- function(x.call, y.call, by.call, x.name, n.by,
                      ts_unit, ts_agg) {

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
  if (all(head(intervals == 1, -1)))
      tu_exist <- "days"
  else if (all(head(intervals == 7, -1)))
      tu_exist <- "weeks"
  else if (all(head(intervals >= 28 & intervals <= 31, -1)))
      tu_exist <- "months"
  else if (all(head(intervals >= 80 & intervals <= 100, -1)))
      tu_exist <- "quarters"
  else if (all(head(intervals >= 365, -1)))
      tu_exist <- "years"
  else
      tu_exist <- "unknown"

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

    if (!getOption("quiet")) {
      txt <- "Ryan, Ulrich, Bennett, and Joy's xts package]"
      cat("[with functions from", txt, "\n")
    }

    # define the aggregation function: aggfun()
    if (ts_agg == "sum")
      aggfun <- function(x) colSums(x)  # leave na.rm=FALSE to have NA agg
    else if (ts_agg == "mean")
      aggfun <- function(x) colMeans(x)

    if (is.null(by.call)) {  # aggregate all data
      # create xts time series, get ts_unit endpoints, aggregate y
      d.xts <- xts::xts(y.call[, seq_len(ncol(y.call))],
                        order.by=x.call[,1])
      names(d.xts) <- names(y.call)
      ep <- xts::endpoints(d.xts, on=ts_unit)
      da.xts <- xts::period.apply(d.xts[,], INDEX=ep, FUN=aggfun)
      # move dates from end of month to beginning of month
      if (ts_unit == "years")
        zoo::index(da.xts) <- as.Date(format(zoo::index(da.xts), "%Y-01-01"))
      else if (ts_unit %in% c("months", "quarters"))
        zoo::index(da.xts) <- as.Date(format(zoo::index(da.xts), "%Y-%m-01"))
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
