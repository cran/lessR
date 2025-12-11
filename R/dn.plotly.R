dn.plotly <- function(
  x, by = NULL,              # numeric x; optional grouping var
  x.name, by.name = NULL,    # names for finalize/hover/legend
  fill,                      # scalar or length-G color(s)
  x.lab, y.lab = "Density",  # axis labels
  ax, gridT1 = NULL, gridT2 = NULL,  # lessR axis helpers
  # KDE controls (bandwidth externally supplied)
  bw, adjust = 1,
  kernel = "gaussian",
  n = 512, from = NULL, to = NULL,
  digits_d = 3
) {


  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

  # --- groups ---------------------------------------------------------------
  if (is.null(by)) {
    groups <- "Series 1"; G <- 1L
  } else {
    if (is.factor(by)) groups <- levels(by) else groups <- unique(by)
    G <- length(groups)
  }

  # --- shared support --------------------------------------------------------
  xr <- range(x, finite = TRUE)
  if (is.null(from) || is.na(from)) from <- xr[1L]
  if (is.null(to)   || is.na(to))   to   <- xr[2L]
  if (to <= from) to <- from + .Machine$double.eps
  if (n < 2) n <- 2

  xgrid <- seq(from, to, length.out = n)
  dx    <- xgrid[2L] - xgrid[1L]

  # --- sanity: at least 2 finite obs per group ------------------------------
  if (G == 1L) {
    n_ok <- sum(is.finite(x))
    if (n_ok < 2L)
      stop("Density requires at least 2 finite observations; found ", n_ok, ".", call. = FALSE)
  } else {
    for (g in seq_len(G)) {
      xg <- x[by == groups[g]]
      n_ok <- sum(is.finite(xg))
      if (n_ok < 2L)
        stop("Group '", groups[g], "' has ", n_ok,
             " finite observation", ifelse(n_ok == 1, "", "s"),
             "; density requires at least 2.", call. = FALSE)
    }
  }

  # --- densities per group ---------------------------------------------------
  dens_list <- vector("list", G)
  ymax <- 0
  for (g in seq_len(G)) {
    xg <- if (G == 1L) x else x[by == groups[g]]
    xg <- xg[is.finite(xg)]

    d  <- stats::density(
      xg, bw = bw, adjust = adjust, kernel = kernel,
      n = n, from = from, to = to, na.rm = TRUE
    )

    # align to shared grid
    if (length(d$x) != length(xgrid) || any(abs(d$x - xgrid) > 1e-12)) {
      d$y <- approx(d$x, d$y, xout = xgrid, rule = 2)$y
      d$x <- xgrid
    }

    # cumulative % via trapezoid (for hover)
    cum <- c(0, cumsum((d$y[-1] + d$y[-length(d$y)]) * 0.5 * dx))
    if (cum[length(cum)] > 0) cum <- cum / cum[length(cum)]

    dens_list[[g]] <- list(x = d$x, y = d$y, cum = cum)
    ymax <- max(ymax, d$y, na.rm = TRUE)
  }

  # --- ticks / labels --------------------------------------------------------
  gridT1 <- ax$axT1 %||% gridT1 %||% pretty(c(from, to), n = 5)
  gridL1 <- ax$axL1 %||% format(gridT1, big.mark = ",", trim = TRUE)
  gridT2 <- ax$axT2 %||% gridT2 %||% pretty(c(0, ymax), n = 6)
  gridL2 <- ax$axL2 %||% format(gridT2, trim = TRUE)

  # --- colors / alpha --------------------------------------------------------
  alpha_fill <- .auto_opacity(G, "fill")
  alpha_line <- .auto_opacity(G, "lines")
  fill_rgba  <- .maketrans_plotly(rep_len(fill, G), alpha = alpha_fill)
  line_rgba  <- .maketrans_plotly(rep_len(fill, G), alpha = alpha_line)

  # --- hover builder ---------------------------------------------------------
  .hover <- function(xx, yy, cpct) {
    paste0(
      x.name, ": ", format(signif(xx, 6)),
      "<br>density=", format(round(yy, digits_d), trim = TRUE),
      "<br>Cumulative %: ", formatC(100 * cpct, format = "f", digits = 1), "%"
    )
  }

  # --- widget & traces -------------------------------------------------------
  plt <- plotly::plot_ly(
    type       = "scatter",
    mode       = "lines",
    hoverinfo  = "text",
    showlegend = G > 1
    # elementId removed — belongs on the widget, not on the trace
  )

  for (g in seq_len(G)) {
    d  <- dens_list[[g]]
    nm <- if (G > 1) groups[g] else NULL
    plt <- plotly::add_trace(
      plt,
      x = d$x, y = d$y, name = nm,
      line      = list(color = line_rgba[g], width = 1.4),
      fill      = "tozeroy",
      fillcolor = fill_rgba[g],
      hovertext = if (!is.null(nm) && length(by.name))
        paste0(by.name, ": ", groups[g], "<br>", .hover(d$x, d$y, d$cum))
      else
        .hover(d$x, d$y, d$cum)
    )
  }

  # --- axes & grids ----------------------------------------------------------
  border_shapes <- plot_border()
  x.shapes      <- x_grid(gridT1)

  ax_x <- axis_num(x.lab, ax$axT1, ax$axL1)
  ax_y <- axis_num(y.lab, ax$axT2, ax$axL2)

  ax_x$tickmode <- "array"; ax_x$tickvals <- gridT1; ax_x$ticktext <- gridL1
  ax_y$tickmode <- "array"; ax_y$tickvals <- gridT2; ax_y$ticktext <- gridL2
  ax_y$showgrid  <- TRUE
  ax_y$gridcolor <- .to_hex(getOption("grid_col", "gray85"))
  ax_y$gridwidth <- 1
  ax_y$griddash  <- "dot"

  plt <- plotly::layout(
    plt,
    xaxis = ax_x,
    yaxis = ax_y,
    shapes = c(x.shapes, border_shapes),
    template = NULL
  )

  # --- background & legend ---------------------------------------------------
  plt$x$layout$plot_bgcolor  <- .to_hex(getOption("panel_fill",  "white"))
  plt$x$layout$paper_bgcolor <- .to_hex(getOption("window_fill", "white"))

  if (G > 1) {
    leg_border <- .to_hex(getOption("legend_border", getOption("panel_border", "#808080")))
    leg_bg     <- .to_hex(getOption("window_fill", "white"))
    plt <- plotly::layout(plt, legend = list(
      title = list(text = if (length(by.name)) by.name else ""),
      bgcolor = leg_bg,
      bordercolor = leg_border,
      borderwidth = 1
    ))
  }

  # --- finalize --------------------------------------------------------------
  plt <- .finalize_plotly_widget(
    plt,
    kind = "density",
    x.name = x.name,
    by.name = if (G > 1) by.name else NULL,
    add_title = FALSE,
    nudge_viewer = TRUE
  )

  invisible(plt)
}
