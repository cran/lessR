.do.plotly <- function(
  x, x.name, y, y.name, ylab, by, by.name=NULL,
  type, stat,
  fill, border, opacity,
  hole, power, radius, ncols = NULL,
  labels, labels_position, labels_color, labels_size, labels_decimals,
  main_cex, main, main.miss,        # main_cex unused; reserved
  digits_d,
  is.agg, quiet                     # quiet unused; reserved
) {

# set the labels ----------------------------------------------------------

  # use variable label for main if it exists and main not specified
  gl <- .getlabels(main=main, lab_cex=getOption("lab_cex"))
  x.lbl <- gl$xl %||% x.name   # display label; falls back to variable name

  if (!is.null(main))
    main.lbl <- main
  else {
    if (main.miss)  # main was not explicitly set to NULL
      main.lbl <- x.lbl
    else
      main.lbl <- NULL
  }


# construct summary table x.tbl from x, optional by and y -----------------

  # resolve stat function for aggregation
  STAT <- tolower(stat %||% "sum")
  stat_fun <- switch(
    STAT,
    mean   = function(z) mean(z,            na.rm = TRUE),
    sum    = function(z) sum(z,             na.rm = TRUE),
    median = function(z) stats::median(z,   na.rm = TRUE),
    min    = function(z) min(z,             na.rm = TRUE),
    max    = function(z) max(z,             na.rm = TRUE),
    sd     = function(z) stats::sd(z,       na.rm = TRUE),
    stop("Unsupported stat: '", stat, "'. Use mean, sum, median, min, max, or sd.")
  )

  if (!is.agg) {
    if (is.null(y)) {  # no y var, so table contains counts
      if (is.null(by))
        x.tbl <- xtabs(~ x, drop.unused.levels = FALSE)
      else
        x.tbl <- xtabs(~ by + x, drop.unused.levels = FALSE)
    }
    else {  # aggregate numeric y by categories using stat_fun
      if (is.null(by)) {
        agg <- tapply(as.numeric(y), x, stat_fun)
        x.tbl <- agg[!is.na(names(agg))]
      } else {
        agg <- tapply(as.numeric(y), list(by = by, x = x), stat_fun)
        if (identical(STAT, "sum")) agg[is.na(agg)] <- 0
        x.tbl <- agg
      }
    }
  }
  else {  # construct x.tbl if data variables are already aggregated
    if (is.null(by)) {  # 1-D aggregated: y per x, y must be provided
      if (length(y) == length(x)) {
        # y given per label (parallel vectors)
        labs <- as.character(x)
        keep <- !is.na(labs) & nzchar(labs)
        x.tbl <- setNames(as.numeric(y[keep]), labs[keep])
      }
      else if (is.factor(x) && length(levels(x)) == length(y)) {
        # y given per factor level of x
        labs <- as.character(levels(x))
        keep <- !is.na(labs) & nzchar(labs)
        x.tbl <- setNames(as.numeric(y[keep]), labs[keep])
      }
      else {
        stop("Aggregated 1-D: length(y) must equal length(x) or nlevels(x).")
      }
    }
    else {  # 2-D aggregated: y per (by, x)
      by_fac <- if (is.factor(by)) droplevels(by)
                else factor(by, levels = unique(by))
      x_fac  <- if (is.factor(x))  droplevels(x)
                else factor(x, levels = unique(x))

      if (length(y) != length(by_fac) || length(y) != length(x_fac))
        stop("Aggregated 2-D: y, by, and x must be the same length.")

      x.tbl <- xtabs(as.numeric(y) ~ by_fac + x_fac, drop.unused.levels = TRUE)
      dimnames(x.tbl) <- list(by = levels(by_fac), x = levels(x_fac))
    }
  }  # end already aggregated

  is.count <- is.null(y)


# y-value formatting for Plotly charts ------------------------------------

  fmt_y <- function(z) .fmt(z, d=digits_d)

  # attach formatted y-values to numeric x.tbl
  if (is.null(by)) {  # 1D: named vector / table
    y.fmt <- fmt_y(as.numeric(x.tbl))
    attr(x.tbl, "y.fmt") <- setNames(y.fmt, names(x.tbl))
  }
  else {  # 2D: matrix/table (rows = by, cols = x)
    y.fmt <- matrix(
      fmt_y(as.numeric(x.tbl)),
      nrow = nrow(x.tbl), ncol = ncol(x.tbl),
      dimnames = dimnames(x.tbl)
    )
    attr(x.tbl, "y.fmt") <- y.fmt  # print(attr(x.tbl, "y.fmt"))
  }


# construct title ---------------------------------------------------------

  if (is.count) y.name <- "Count"
  y.pre <- if (!is.null(stat)) ylab else y.name
  ttl_auto <- paste(y.pre, "for each", x.name)
  if (!is.null(by)) ttl_auto <- paste(ttl_auto, "by", by.name)
  ttl <- main.lbl %||% ttl_auto

# build the plotly chart --------------------------------------------------

  if (type == "pie") {
    plt <- piechart.plotly(
      x = x.tbl, x.name = x.name, y.name = y.name, by.name = by.name, ttl = ttl,
      fill = fill, border = border, opacity = opacity,
      hole = hole,
      ncols = ncols,
      labels = labels,
      labels_position = labels_position,
      labels_color = labels_color,
      labels_size = labels_size,
      labels_decimals = labels_decimals,
      digits_d = digits_d
    )
  } else if (type == "bubble") {
    plt <- bubble.plotly(
      x = x.tbl, x.name = x.name, y.name = y.name, by.name = by.name,
      x.lab = x.name, y.lab = if (is.null(by)) "" else by.name,
      ttl = ttl,
      fill = fill, clr = border, opacity = opacity,
      power = power, radius = radius,
      digits_d = digits_d,
      labels = labels, labels_position = labels_position,
      labels_size = labels_size, labels_color = labels_color
    )
  }

  # strip any title set by helpers
  if (!is.null(plt$x) && !is.null(plt$x$layout))
    plt$x$layout$title <- NULL

  # finalize consistently for pie/bubble
  plt <- .finalize_plotly_widget(
    plt,
    kind         = type,
    x.name       = x.name,
    by.name = if (!is.null(by)) by.name else NULL,
    add_title    = FALSE,
    nudge_viewer = (.allow.interactive() &&
                    !isTRUE(getOption("knitr.in.progress")))
  )

  return(invisible(plt))

}


