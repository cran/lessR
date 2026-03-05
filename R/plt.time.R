.plt.time <- function(x.call, y.call, by.call, x.name, n.by,
                      ts_unit, ts_agg="sum") {

  ts_truncate <- function(x.call, y.call, ts_unit) {

    end_of_period <- function(dd, unit) {
      y <- as.integer(format(dd, "%Y"))
      m <- as.integer(format(dd, "%m"))

      if (unit == "weeks") {
        wd <- as.integer(strftime(dd, "%u"))  # 1=Mon ... 7=Sun
        dd + (7L - wd)

      } else if (unit == "months") {
        first_next <- as.Date(sprintf("%04d-%02d-01",
                                      y + (m == 12L),
                                      ifelse(m == 12L, 1L, m + 1L)))
        first_next - 1L

      } else if (unit == "quarters") {
        q <- ((m - 1L) %/% 3L) + 1L
        end_m <- q * 3L
        first_next <- as.Date(sprintf("%04d-%02d-01",
                                      y + (end_m == 12L),
                                      ifelse(end_m == 12L, 1L, end_m + 1L)))
        first_next - 1L

      } else if (unit == "years") {
        as.Date(sprintf("%04d-12-31", y))

      } else {  # days
        dd
      }
    }

    stopifnot(is.data.frame(x.call), ncol(x.call) == 1L)
    d <- x.call[[1L]]
    stopifnot(inherits(d, "Date"))

    units <- c("days", "weeks", "months", "quarters", "years")
    if (!ts_unit %in% units) {
      stop("ts_unit must be one of: ", paste(units, collapse = ", "))
    }

    # days: nothing to truncate
    if (ts_unit == "days") {
      return(list(x = x.call, y = y.call, keep = rep(TRUE, length(d))))
    }

    # group key for target unit
    yr <- as.integer(format(d, "%Y"))
    mo <- as.integer(format(d, "%m"))
    g_target <- switch(
      ts_unit,
      "years"    = paste0(yr),
      "quarters" = paste0(yr, "-Q", ((mo - 1L) %/% 3L) + 1L),
      "months"   = paste0(yr, "-", sprintf("%02d", mo)),
      "weeks"    = paste0(format(d, "%G"), "-W", format(d, "%V"))
    )

    # last observed date determines whether final target period is complete
    max_d    <- max(d, na.rm = TRUE)
    end_last <- end_of_period(max_d, ts_unit)

    keep <- rep(TRUE, length(d))
    if (max_d < end_last) {
      last_g <- g_target[which.max(d)]
      keep[g_target == last_g] <- FALSE
    }

    list(
      x = x.call[keep, , drop = FALSE],
      y = y.call[keep, , drop = FALSE],
      keep = keep
    )
  }


  # begin Analysis --------------------------------------------------------

  # from STL() that passes vectors, not dfs
  if (!is.data.frame(x.call)) x.call <- data.frame(x.call)
  if (!is.data.frame(y.call)) y.call <- data.frame(y.call)

  # get existing ts_unit (evaluate 1st by level if by is present)
  if (is.null(by.call)) {
    d <- sort(unique(x.call[[1L]]))
  } else {
    lv1 <- if (is.factor(by.call)) levels(by.call)[1L]
            else sort(unique(by.call))[1L]
    d <- sort(unique(x.call[by.call == lv1, 1L]))
  }

  # ---- positive day gaps
  intervals <- as.integer(diff(d))
  intervals <- intervals[intervals > 0L]

  missing_periods <- FALSE
  tu_exist <- "unknown"

  # 1 handle trivial / very short series
  if (length(d) <= 1L || length(intervals) == 0L) {
    tu_exist <- "days"

  } else { # 2 Fine units from typical gap (median robust)
    gap <- as.integer(stats::median(intervals))

    if (gap == 1L) {
      tu_exist <- "days"

    } else if (gap == 7L) { # is a weekly series or irregular?
      p14 <- mean(intervals == 14L)

      # If lots of 14s (or more like biweekly), treat as unknown (irregular)
      if (p14 >= 0.40 || abs(gap - 14L) < abs(gap - 7L)) {
        tu_exist <- "unknown"

      } else {
        tu_exist <- "weeks"
      }

    } else { # 3 Coarse units from calendar structure (allow missing)
      dd <- as.Date(d)
      yr <- as.integer(format(dd, "%Y"))
      mo <- as.integer(format(dd, "%m"))
      da <- as.integer(format(dd, "%d"))

      # precompute indices
      yi <- yr
      qi <- yr * 4L + ((mo - 1L) %/% 3L + 1L)  # year-quarter index
      mi <- yr * 12L + mo                      # year-month index

      # YEARS: same month/day each year; at most one per year; allow missing
      if (tu_exist == "unknown") {
        same_md <- (length(unique(mo)) == 1L) &&
                   (length(unique(da)) == 1L)

        if (same_md && !any(duplicated(yi))) {
          yi_u <- sort(unique(yi))
          tu_exist <- "years"
        }
      }

      # Quarters: one obs per quarter; anchored quarter months; allow missing
      if (tu_exist == "unknown") {
        med_gap <- stats::median(intervals)
        plausible_quarterly <- med_gap >= 60L && med_gap <= 122L

        if (plausible_quarterly && !any(duplicated(qi))) {
          qi_u <- sort(unique(qi))
          tu_exist <- "quarters"
        }
      }

      # Months: one obs per month; allow missing months.
      # Auto-detect requires evidence of monthly intent (anchored day).
      if (tu_exist == "unknown") {
        if (!any(duplicated(mi))) {

          anchored_month <-
            (length(unique(da)) == 1L) ||  # same day-of-month each month
            all(da == 1L) ||               # always month start
            all(da >= 28L) ||              # near month-end (28/29/30/31)
            all(da <= 3L)                  # near month-start window

          if (anchored_month) {
            mi_u <- sort(unique(mi))
            tu_exist <- "months"
          } else {
            # leave tu_exist as "unknown" in auto mode
          }
        }
      }
    }
  }


  # does data support aggregation level?
  t.units <- c("unknown", "days", "weeks", "months", "quarters", "years")
  t.units <- factor(t.units, levels = t.units, ordered = TRUE)

  do.agg <- FALSE
  hold.unit <- FALSE

  if (!is.null(ts_unit)) {
    hold.unit <- (ts_unit == "days7")
    if (hold.unit) ts_unit <- "days"

    # Only enforce "cannot ask for finer than data" when tu_exist is known
    if (tu_exist != "unknown" &&
        which(t.units == ts_unit) < which(t.units == tu_exist)) {
      stop("Resolution of data is  ", tu_exist, "\n",
           "Requested data for ", ts_unit, " is not available\n\n",
           call. = FALSE)
    } else {
      do.agg <- TRUE
    }
  }

  # option: aggregate date variable by specified ts_unit
  if (!is.null(ts_unit) && do.agg) {

    # define the aggregation function: aggfun()
    if (ts_agg == "sum") {
      aggfun <- function(x) colSums(x)  # leave na.rm=FALSE to have NA agg
    } else if (ts_agg == "mean") {
      aggfun <- function(x) colMeans(x)
    }

    if (is.null(by.call)) {  # aggregate all data
      res <- ts_truncate(x.call, y.call, ts_unit = ts_unit)
      x.call <- res$x
      y.call <- res$y

      d.xts <- xts::xts(y.call[, seq_len(ncol(y.call))],
                        order.by = x.call[, 1])
      names(d.xts) <- names(y.call)

      ep <- xts::endpoints(d.xts, on = ts_unit)
      da.xts <- xts::period.apply(d.xts[, ], INDEX = ep, FUN = aggfun)

      # move dates from end of period to beginning of period
        if (ts_unit == "years") {
          zoo::index(da.xts) <- as.Date(format(zoo::index(da.xts), "%Y-01-01"))
        } else if (ts_unit == "quarters") {
          idx <- zoo::as.yearqtr(zoo::index(da.xts))
          zoo::index(da.xts) <- zoo::as.Date(idx)
        } else if (ts_unit == "months") {
          zoo::index(da.xts) <- as.Date(format(zoo::index(da.xts), "%Y-%m-01"))
        }

      x.call <- data.frame(zoo::index(da.xts))
      names(x.call)[1] <- x.name
      y.call <- data.frame(zoo::coredata(da.xts))

    } else {  # aggregate separately for each level of by, join
      x.cl <- data.frame(date = as.Date(character(0)))  # always 1 column
      y.cl <- y.call[0L, , drop = FALSE]
      by.cl <- data.frame(byc = factor(character(0), levels = levels(by.call)))
      by.cl <- by.cl[-1, ]

      for (k in seq_len(n.by)) {
        xl <- x.call[by.call == levels(by.call)[k], , drop = FALSE]
        yl <- y.call[by.call == levels(by.call)[k], , drop = FALSE]

        # truncate trailing partial target periods within each group
        es <- ts_truncate(xl, yl, ts_unit = ts_unit)
        xl <- es$x
        yl <- es$y
        if (nrow(xl) == 0L) next

        d.xts <- xts::xts(yl[, seq_len(ncol(yl))], order.by = xl[, 1])
        names(d.xts) <- names(y.call)

        ep <- xts::endpoints(d.xts, on = ts_unit)
        da.xts <- xts::period.apply(d.xts[, ], INDEX = ep, FUN = aggfun)

        if (ts_unit == "years") {
          zoo::index(da.xts) <- as.Date(format(zoo::index(da.xts), "%Y-01-01"))
        } else if (ts_unit == "quarters") {
          idx <- zoo::as.yearqtr(zoo::index(da.xts))
          zoo::index(da.xts) <- zoo::as.Date(idx)
        } else if (ts_unit == "months") {
          zoo::index(da.xts) <- as.Date(format(zoo::index(da.xts), "%Y-%m-01"))
        }

        x.c <- data.frame(date = zoo::index(da.xts))
        y.c <- as.data.frame(zoo::coredata(da.xts))
        by.c <- data.frame(byc = rep(levels(by.call)[k], nrow(x.c)))

        x.cl <- rbind(x.cl, x.c)
        y.cl <- rbind(y.cl, y.c)
        by.cl <- rbind(by.cl, by.c)
      }

      names(x.cl)[1] <- x.name
      x.call <- x.cl
      y.call <- y.cl
      by.call <- factor(by.cl[, 1, drop = TRUE])  # redundant, but safety
    }

  } else # ts_unit not specified: use inferred unit, no aggregation
      ts_unit <- tu_exist

  # check for gaps in the final (possibly aggregated) series
  if (ts_unit != "unknown") {
    if (is.null(by.call)) {
      d_final <- sort(unique(x.call[[1L]]))
    } else {
      lv1 <- levels(by.call)[1L]
      d_final <- sort(unique(x.call[by.call == lv1, 1L]))
    }

    if (length(d_final) > 1L) {
      yr_f <- as.integer(format(d_final, "%Y"))
      mo_f <- as.integer(format(d_final, "%m"))

      missing_periods <- switch(ts_unit,
        "days"    = ,
        "days7"   = any(diff(as.integer(d_final)) > 1L),
        "weeks"   = any(diff(as.integer(d_final)) > 7L),
        "months"  = any(diff(yr_f * 12L + mo_f) > 1L),
        "quarters"= any(diff(yr_f * 4L + ((mo_f - 1L) %/% 3L) + 1L) > 1L),
        "years"   = any(diff(yr_f) > 1L),
        FALSE
      )
    }
  }

  if (missing_periods)
    message("There are gaps in the dates, so that there are not regular ",
            "intervals between all the dates.")

  if (hold.unit) ts_unit <- "days7"

  return(list(
    x.call = x.call,
    y.call = y.call,
    by.call = by.call,
    ts_unit = ts_unit,
    do.agg = do.agg
  ))
}
