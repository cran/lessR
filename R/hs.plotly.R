hs.plotly <- function(
  x, by = NULL,
  x.name, by.name = NULL,
  breaks,
  freq,
  fill, border,
  x.lab, y.lab,
  ax, gridT1, gridT2,
  digits_d = 2,
  position = c("overlay", "stack")
) {


  position <- match.arg(position)

  # --- bin geometry ---
  left   <- breaks[-length(breaks)]
  right  <- breaks[-1L]
  mids   <- (left + right) / 2
  widths <- right - left
  right_closed <- isTRUE(attr(breaks, "right"))
  n_bins <- length(mids)

  # --- groups ---
  if (is.null(by)) {
    groups <- "Series 1"
    G <- 1L
  } else {
    if (is.factor(by)) groups <- levels(by) else groups <- unique(by)
    G <- length(groups)
  }

  # --- counts per bin ---
  bins_fac <- cut(x, breaks, right = right_closed, include.lowest = TRUE)

  if (G == 1L) {
    y_vec <- as.integer(table(bins_fac))
    y_mat <- cbind(y_vec)
  } else {
    tab <- table(bins_fac, factor(by, levels = groups))
    y_mat <- unclass(tab)
  }

  # --- density (per group) ---
  if (!isTRUE(freq)) {
    for (g in seq_len(G)) {
      n_g <- sum(y_mat[, g])
      if (n_g > 0) y_mat[, g] <- y_mat[, g] / (n_g * widths)
    }
  }

  rng_txt <- paste0(
    "[", format(signif(left, 6)), ", ", format(signif(right, 6)),
    if (right_closed) ")" else "]"
  )

  .hover_group <- function(yv) {
    if (isTRUE(freq)) {
      total_y <- sum(yv)
      share   <- if (total_y > 0) yv / total_y else rep(NA_real_, n_bins)
      cum_y   <- cumsum(yv)
      cum_pct <- if (total_y > 0) cum_y / total_y else rep(NA_real_, n_bins)
      y_fmt      <- format(round(yv, digits_d), trim = TRUE)
      cum_valfmt <- format(cum_y, trim = TRUE)
      pct_fmt    <- paste0(formatC(100 * share, format = "f", digits = 1), "%")
      cum_pctfmt <- paste0(formatC(100 * cum_pct, format = "f", digits = 1), "%")
      paste0(
        "Bin: ", rng_txt,
        "<br>", y.lab, ": ", y_fmt,
        "<br>% of total: ", pct_fmt,
        "<br>Cumulative: ", cum_valfmt,
        "<br>Cumulative %: ", cum_pctfmt
      )
    } else {
      amt     <- yv * widths
      total_y <- sum(amt)
      share   <- if (total_y > 0) amt / total_y else rep(NA_real_, n_bins)
      cum_y   <- cumsum(amt)
      cum_pct <- if (total_y > 0) cum_y / total_y else rep(NA_real_, n_bins)
      y_fmt      <- format(round(yv, digits_d), trim = TRUE)
      cum_valfmt <- format(round(cum_y, digits_d), trim = TRUE)
      pct_fmt    <- paste0(formatC(100 * share, format = "f", digits = 1), "%")
      cum_pctfmt <- paste0(formatC(100 * cum_pct, format = "f", digits = 1), "%")
      paste0(
        "Bin: ", rng_txt,
        "<br>", y.lab, ": ", y_fmt,
        "<br>% of total: ", pct_fmt,
        "<br>Cumulative (area): ", cum_valfmt,
        "<br>Cumulative %: ", cum_pctfmt
      )
    }
  }

  # colors
  alpha_fill <- .auto_opacity(
    k = if (G > 1) G else 1L,
    mode = if (G > 1 && position == "stack") "stack" else "overlay"
  )
  fill_rgba_grp  <- .maketrans_plotly(rep_len(fill,   G),     alpha = alpha_fill)
  border_hex_grp <- .to_hex(         rep_len(border, G))
  fill_rgba_bin  <- .maketrans_plotly(rep_len(fill,   n_bins), alpha = alpha_fill)
  border_hex_bin <- .to_hex(         rep_len(border, n_bins))

  # --- base widget (NO elementId here) ---
  plt <- plotly::plot_ly(
    type       = "bar",
    hoverinfo  = "text",
    showlegend = G > 1
  )

  align_id <- "lessR.hs.align"

  for (g in seq_len(G)) {
    yv    <- y_mat[, g]
    hover <- .hover_group(yv)
    nm    <- if (G > 1) groups[g] else NULL

    plt <- plotly::add_trace(
      plt,
      x = mids,
      y = yv,
      name   = nm,
      width  = widths * 0.98,
      offset = 0,
      alignmentgroup = align_id,
      marker = if (G == 1) {
        list(
          color = fill_rgba_bin,
          line  = list(color = border_hex_bin, width = 1)
        )
      } else {
        list(
          color = fill_rgba_grp[g],
          line  = list(color = border_hex_grp[g], width = 1)
        )
      },
      hovertext = if (!is.null(nm) && length(by.name))
        paste0(by.name, ": ", groups[g], "<br>", hover) else hover
    )
  }

  # axes, grid, layout
  border_shapes <- plot_border()
  x.shapes      <- x_grid(gridT1)

  ax_x <- axis_num(x.lab, ax$axT1, ax$axL1)
  ax_y <- axis_num(y.lab, ax$axT2, ax$axL2)

  ax_y$showgrid  <- TRUE
  ax_y$gridcolor <- .to_hex(getOption("grid_col", "gray85"))
  ax_y$gridwidth <- 1
  ax_y$griddash  <- "dot"

  plt <- plotly::layout(
    plt,
    xaxis    = ax_x,
    yaxis    = ax_y,
    shapes   = c(x.shapes, border_shapes),
    template = NULL,
    barmode  = if (G > 1 && position == "stack") "stack" else "overlay",
    bargap   = 0
  )

  plt$x$layout$plot_bgcolor  <- .to_hex(getOption("panel_fill",  "white"))
  plt$x$layout$paper_bgcolor <- .to_hex(getOption("window_fill", "white"))

  if (G > 1) {
    leg_border <- .to_hex(getOption("legend_border",
                                    getOption("panel_border", "#808080")))
    leg_bg     <- .to_hex(getOption("window_fill", "white"))
    plt <- plotly::layout(
      plt,
      legend = list(
        title = list(text = if (length(by.name)) by.name else ""),
        bgcolor = leg_bg,
        bordercolor = leg_border,
        borderwidth = 1
      )
    )
  }

  plt <- plotly::layout(plt, title = NULL)
  plt$x$layout$title <- NULL

  plt <- .finalize_plotly_widget(
    plt,
    kind        = "histogram",
    x.name      = x.name,
    by.name     = if (G > 1) by.name else NULL,
    add_title   = FALSE,
    nudge_viewer = TRUE
  )

  invisible(plt)
}
