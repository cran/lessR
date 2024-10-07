.plt.time <- function(x.call, y.call, by.call, x.name, n.by,
                      time_unit, time_agg) {


  # time_unit not set, get existing time_unit
  # -----------------------------------------
  if (is.null(by.call))
    intervals <- diff(x.call[,1])
  else {  # eval intervals only for 1st level of by, assume sorted
    n.1 <- table(by.call)[1]
    intervals <- diff(x.call[1:n.1, 1])
  }

  # Determine the frequency based on interval, all but last value
  if (all(head(intervals == 1, -1)))
      tu_exist <- "days"
  else if (all(head(intervals >= 28 & intervals <= 31, -1)))
      tu_exist <- "months"
  else if (all(head(intervals == 7, -1)))
      tu_exist <- "weeks"
  else if (all(head(intervals >= 80 & intervals <= 100, -1)))
      tu_exist <- "quarters"
  else if (all(head(intervals >= 365, -1)))
      tu_exist <- "years"
  else
      tu_exist <- "unknown"

  # does data support aggregation level?
  t.units <- c("unknown","days","weeks","months","quarters","years")
  t.units <- factor(t.units,
          levels=c("unknown","days","weeks","months","quarters","years"),
          ordered=TRUE)
  do.agg<- FALSE
  if (!is.null(time_unit)) {
    if (which(t.units==time_unit) < which(t.units == tu_exist)) {
        stop("Resolution of data is  ", tu_exist, "\n",
             "Requested data for ", time_unit, " is not available\n\n",
             call. = FALSE)
    }
    do.agg <- ifelse (which(t.units==time_unit) > which(t.units == tu_exist),
                      TRUE, FALSE)
  }


  # aggregate date variable by specified parameter: time_unit
  # ---------------------------------------------------------
  if (!is.null(time_unit) &&  do.agg) {

    if (!getOption("quiet")) {
      txt <- "Ryan, Ulrich, Bennett, and Joy's xts package]"
      cat("[with functions from", txt, "\n\n")
    }

    # define the aggregation function: aggfun()
    if (time_agg == "sum")
      aggfun <- function(x) colSums(x, na.rm=TRUE)
    else if (time_agg == "mean")
      aggfun <- function(x) colMeans(x, na.rm=TRUE)

    if (is.null(by.call)) {
      # create xts time series, get time_unit endpoints, aggregate y
      d_xts <- xts::xts(y.call[, seq_len(ncol(y.call))],
                        order.by=x.call[,1])
      names(d_xts) <- names(y.call)
      ep <- xts::endpoints(d_xts, on=time_unit)
      da_xts <- xts::period.apply(d_xts[,], INDEX=ep, FUN=aggfun)
      # extract aggregated x.call and y.call from xts time series
      x.call <- data.frame(zoo::index(da_xts))
      names(x.call)[1] <- x.name
      y.call <- data.frame(zoo::coredata(da_xts))
    }
    else {  # aggregate separately for each level of by, join
      x.cl <- data.frame(date = as.Date(character(0)))
      y.cl <- data.frame(y = numeric(0))
      by.cl <- data.frame(byc = NA)
      by.cl <- by.cl[-1, ]
      for (k in seq_len(n.by)) {
        xl <- x.call[by.call==levels(by.call)[k], , drop=FALSE]
        yl <- y.call[by.call==levels(by.call)[k], , drop=FALSE]
        d_xts <- xts::xts(yl[, seq_len(ncol(yl))], order.by=xl[,1])
        names(d_xts) <- names(y.call)
        ep <- xts::endpoints(d_xts, on=time_unit)
        da_xts <- xts::period.apply(d_xts[,], INDEX=ep, FUN=aggfun)
        x.c <- data.frame(date = zoo::index(da_xts))
        y.c <- data.frame(y = zoo::coredata(da_xts))
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
  }  # end aggregate time_unit

  else  # time_unit not specified, take as is, no aggregation
    time_unit <- tu_exist

  return(list(x.call=x.call, y.call=y.call, by.call=by.call,
          time_unit=time_unit, do.agg=do.agg))
}
