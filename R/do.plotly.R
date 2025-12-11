.do.plotly <- function(
  x, x.name, y, y.name, ylab, by, by.name=NULL,
  type, stat,
  fill, border, opacity,
  hole, power, radius, ncols = NULL,
  labels, labels_position, labels_color, labels_size, labels_decimals,
  main_cex, main, main.miss,
  digits_d,
  is.agg, quiet
) {


  group_label_pos <- c("below", "above")    # <-- add default
  group_label_size <- 14                    # <-- optional for completeness
  group_label_gap  <-0.05                   # <-- optional for completeness
# group_label_pos <- match.arg(group_label_pos)  # <-- initialize early


# set the labels ----------------------------------------------------------

  # use variable label for main if it exists and main not specified
  gl <- .getlabels(main=main, lab_cex=getOption("lab_cex"))
  x.name <- gl$xn; x.lbl <- gl$xl
  if (!is.null(main))
    main.lbl <- main
  else {
    if (main.miss)  # main was not explicitly set to NULL
      main.lbl <- ifelse (is.null(x.lbl), x.name, x.lbl)
    else
      main.lbl <- NULL
  }

  # radar plot requires at least three categories to construct a polygon
  if (.is.integer(x) || is.character(x))
    n.x <- length(unique(x))
  else if (is.factor(x))
    n.x <- length(unique(levels(x)))
  if (type == "radar" && n.x < 3) {
    stop(call.=FALSE, "\n------\n",
      "Radar plots require at least 3 categories to construct a polygon.\n",
      x.name, " has only ", n.x, " categories.\n")
  }


# construct summary table x.tbl from x, optional by and y -----------------

  if (!is.agg) {
    if (is.null(y)) {  # no y var, so table contains counts
      if (is.null(by))
        x.tbl <- xtabs(~ x, drop.unused.levels = FALSE)
      else
        x.tbl <- xtabs(~ by + x, drop.unused.levels = FALSE)
    }
    else {  # aggregate numeric y by categories
      if (is.null(by))
        x.tbl <- xtabs(y ~ x, drop.unused.levels = FALSE)
      else
        x.tbl <- xtabs(y ~ by + x, drop.unused.levels = FALSE)
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

  is.count <- ifelse (.is.integer(x.tbl) && all(x.tbl >= 0), TRUE, FALSE) 


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
  y.pre <- ifelse (!is.null(stat), ylab, y.name)
  ttl <- paste(y.pre, "for each", x.name)
  if (!is.null(by)) ttl <- paste(ttl, "by", by.name) 


# plot the plotly chart ---------------------------------------------------

  if (type == "pie") {
    plt <- piechart.plotly(
      x = x.tbl, x.name = x.name, y.name = y.name, by.name = by.name, ttl = ttl,
      fill = fill, border = border, opacity = opacity,
      hole     = hole,
      ncols    = ncols,
      labels   = labels,
      labels_position = labels_position,
      labels_color    = labels_color,
      labels_size     = labels_size,
      labels_decimals = labels_decimals,
      digits_d = digits_d,
    )
  }

  else if (type == "bubble") {
    plt <- bubble.plotly(
      x = x.tbl, x.name = x.name, y.name = y.name, by.name = by.name, 
      x.lab = x.name, y.lab = if (is.null(by)) "" else by.name,
      ttl = ttl,
      fill = fill, clr = border, opacity = opacity,
      power = power, radius = radius,  # pixels
      digits_d = digits_d,
      labels = labels, labels_size = labels_size, labels_color = labels_color
    )
  }


# One-time destination/refresh notice for Plotly charts -------------------
  if (.allow.interactive())
    .viewer_notice_once(plot_name = type, window_target = "Viewer")


# print the plotly chart --------------------------------------------------
  if (.allow.interactive()) {
    if (type == "bubble") {
      txt <- "Can't display both discrete & non-discrete data on same axis"
      withCallingHandlers(
        print(plt),
        warning = function(w) {
          msg <- conditionMessage(w)
          if (grepl(txt, msg, fixed = TRUE))
            invokeRestart("muffleWarning")
        }
      )
    }
    else {
      print(plt)
    }
  }

  return(invisible(plt))

}


