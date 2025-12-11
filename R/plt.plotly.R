plt.plotly <- function(
  g,
  by.name, n.by,
  fill, border, shape, pt.size,
  x.lab, y.lab, ax, gridT1, gridT2,
  digits_d = 2,
  fit_color = border, fit_lwd = NULL, ln.type = NULL,
  connect = FALSE,                 # for time series
  power = 0.5, radius.in = 0.12,   # area = size^power (for bubble mode)
  size.name = "Size",
  ellipse_region = NULL, ellipse_fill = NULL,
  ellipse_color = NULL, ellipse_lwd = NULL,
  se.poly = NULL, se_fill = NULL,
  pt_opacity = 0.95               # <-- NEW: user-controlled fill alpha [0,1]
) {

  # null-coalesce helper
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

  # sanitize opacity
  pt_opacity <- as.numeric(pt_opacity[1])
  if (!is.finite(pt_opacity)) pt_opacity <- 0.95
  pt_opacity <- max(0, min(1, pt_opacity))

  # ---- bubbles (global scale) ----
  dpi <- getOption("plotly_dpi", 96)
  pt_rows  <- rep(TRUE, nrow(g))
  size_vec <- as.numeric(g$size)
  use_bubbles <- length(unique(size_vec)) > 1L

  if (use_bubbles) {
    sz_all <- pmax(0, size_vec[pt_rows])^power
    mxru   <- max(sz_all, na.rm = TRUE)
    radius.px <- radius.in * dpi
    bubble.diam <- function(v) {
      v_sz <- pmax(0, as.numeric(v))^power
      if (is.finite(mxru) && mxru > 0)
        2 * (v_sz / mxru) * radius.px else rep(0, length(v_sz))
    }
  } else {
    bubble.diam <- NULL
  }

  # ---- grouping ----
  ind_chr   <- trimws(as.character(g$ind))
  valid     <- !is.na(ind_chr)
  idx_list  <- split(seq_len(nrow(g))[valid], ind_chr[valid], drop = TRUE)
  grp_names <- names(idx_list)
  has_groups <- length(grp_names) > 1L

  # ---- init ----
  plt <- plotly::plot_ly()

  # convenience: fully opaque borders; fill alpha set by pt_opacity
  border_hex <- .to_hex(border)

  # ---- no groups ----
  if (!has_groups) {
    xv <- as.numeric(g$x); yv <- as.numeric(g$y)
    hover_tmpl <- .hover.fmt(xv, yv, x.lab, y.lab, FALSE, digits_d)

    # size/marker setup
    if (use_bubbles) {
      diam_px <- bubble.diam(size_vec)
      hover_tmpl <- sub(
        "<extra></extra>$",
        paste0("<br>", size.name, ": %{customdata:.", digits_d, "f}<extra></extra>"),
        hover_tmpl
      )
      customdata <- size_vec
    } else {
      px <- as.numeric(pt.size) * 7.25
      if (!is.finite(px) || px <= 0) px <- 5
      diam_px   <- rep(px, length(xv))
      customdata <- NULL
    }

    symbol <- switch(as.character(shape),
      "21"="circle","22"="square","23"="diamond",
      "24"="triangle-up","25"="triangle-down","circle"
    )

    # IMPORTANT: set alpha via rgba colors, and leave marker.opacity at 1
    marker_color <- .maketrans_plotly(fill, alpha = pt_opacity)

    plt <- plotly::add_trace(
      plt,
      type  = "scatter",
      mode  = if (connect) "lines+markers" else "markers",
      x = xv, y = yv,
      marker = list(
        symbol   = symbol,
        size     = diam_px,
        sizemode = "diameter",
        color    = marker_color,
        opacity  = 1,  # do not multiply alpha; already in rgba
        line     = list(color = border_hex, width = 1)
      ),
      line  = if (connect) list(color = border_hex, width = 1.5) else NULL,
      hovertemplate = hover_tmpl,
      customdata    = customdata,
      showlegend    = FALSE
    )

  } else {
    # ---- groups ----
    symbol <- switch(as.character(shape),
      "21"="circle","22"="square","23"="diamond",
      "24"="triangle-up","25"="triangle-down","circle"
    )

    for (i in seq_along(grp_names)) {
      nm  <- grp_names[i]
      idx <- idx_list[[nm]]
      if (!length(idx)) next

      xv <- as.numeric(g$x[idx]); yv <- as.numeric(g$y[idx])
      hover.i <- sub(
        "<extra></extra>$",
        paste0("<br>", by.name, ": %{text}<extra></extra>"),
        .hover.fmt(xv, yv, x.lab, y.lab, FALSE, digits_d)
      )

      if (use_bubbles) {
        diam_px  <- bubble.diam(size_vec[idx])
        hover.i  <- sub(
          "<extra></extra>$",
          paste0("<br>", size.name, ": %{customdata:.", digits_d, "f}<extra></extra>"),
          hover.i
        )
        custom.i <- size_vec[idx]
      } else {
        px <- as.numeric(pt.size) * 6.5
        if (!is.finite(px) || px <= 0) px <- 5
        diam_px  <- rep(px, length(idx))
        custom.i <- NULL
      }

      # per-group fill with desired alpha
      fill_i <- .maketrans_plotly(fill[i], alpha = pt_opacity)

      plt <- plotly::add_trace(
        plt,
        type = "scatter",
        mode = if (connect) "lines+markers" else "markers",
        name = nm, legendgroup = nm, text = nm,
        x = xv, y = yv,
        marker = list(
          symbol   = symbol,
          size     = diam_px,
          sizemode = "diameter",
          color    = fill_i,
          opacity  = 1,
          line     = list(color = border[i], width = 1)
        ),
        line  = if (connect) list(color = border[i], width = 1.5) else NULL,
        hovertemplate = hover.i,
        customdata    = custom.i,
        showlegend    = TRUE
      )
    }
  }

  # ---- ellipses (support per-group) ----
  if (!is.null(ellipse_region)) {
    to_poly_list <- function(obj) {
      if (is.list(obj) && !is.data.frame(obj)) return(obj)
      list(obj)
    }
    ell_list <- to_poly_list(ellipse_region)
    n_ell <- length(ell_list)
    target_len <- if (has_groups) length(grp_names) else 1L
    if (n_ell != target_len) ell_list <- rep(ell_list, length.out = target_len)

    edge_vec  <- if (length(ellipse_color)) .to_hex(rep_len(ellipse_color, target_len))
                 else .to_hex(rep_len(border,       target_len))
    lw_vec    <- as.numeric(if (length(ellipse_lwd)) rep_len(ellipse_lwd, target_len) else rep(1, target_len))
    fill_vec  <- if (length(ellipse_fill)) rep_len(ellipse_fill, target_len) else rep("transparent", target_len)

    for (k in seq_len(target_len)) {
      el <- as.data.frame(ell_list[[k]])
      if (ncol(el) >= 2 && nrow(el) >= 3) {
        plt <- plotly::add_trace(
          plt,
          type = "scatter",
          mode = "lines",
          x = el[[1]], y = el[[2]],
          line = list(color = edge_vec[k], width = lw_vec[k]),
          fill = "toself",
          fillcolor = fill_vec[k],
          showlegend = FALSE,
          hoverinfo = "skip",
          hovertemplate = NULL,
          hoveron = "points",
          inherit = FALSE
        )
      }
    }
  }

  # ---- SE bands ----
  if (!is.null(se.poly) && length(se.poly)) {
    for (b in seq_along(se.poly)) {
      sb <- as.data.frame(se.poly[[b]])
      plt <- plotly::add_trace(
        plt,
        type = "scatter",
        mode = "none",
        x = sb[[1]], y = sb[[2]],
        fill = "toself",
        fillcolor = se_fill,
        hoverinfo = "skip",
        showlegend = FALSE,
        inherit = FALSE
      )
    }
  }

  # ---- fits ----
  fit.rows <- is.finite(g$x) & is.finite(g$f)
  if (any(fit.rows)) {
    ind.fit.chr <- trimws(as.character(g$ind[fit.rows]))
    idx.fit     <- split(which(fit.rows), ind.fit.chr, drop = TRUE)
    has.group.fits <- length(idx.fit) > 1L

    if (!has.group.fits) {
      xl <- as.numeric(g$x[fit.rows]); yl <- as.numeric(g$f[fit.rows])
      ok <- is.finite(xl) & is.finite(yl); xl <- xl[ok]; yl <- yl[ok]
      if (length(xl) >= 2L) {
        fmtx <- .get.tick.fmt(xl, digits_d); fmty <- .get.tick.fmt(yl, digits_d)
        hover <- paste0(
          x.lab, ": %{x", if (nzchar(fmtx)) paste0(":", fmtx) else "", "}",
          "<br>Fit: %{y",  if (nzchar(fmty)) paste0(":", fmty) else "", "}",
          "<extra></extra>"
        )
        plt <- plotly::add_trace(
          plt, type = "scatter", mode = "lines",
          name = "Fit",
          x = xl, y = yl,
          line = list(
            color = .to_hex(fit_color[1]),
            width = as.numeric(if (length(fit_lwd)) fit_lwd[1] else 2)
          ),
          hovertemplate = hover,
          showlegend    = TRUE,
          legendgroup   = "fit"
        )
      }
    } else {
      gnames <- names(idx.fit)
      for (i in seq_along(gnames)) {
        nm   <- gnames[i]
        rows <- idx.fit[[nm]]
        xl <- as.numeric(g$x[rows]); yl <- as.numeric(g$f[rows])
        ok <- is.finite(xl) & is.finite(yl); xl <- xl[ok]; yl <- yl[ok]
        if (length(xl) < 2L) next

        fmtx <- .get.tick.fmt(xl, digits_d); fmty <- .get.tick.fmt(yl, digits_d)
        hover <- paste0(
          x.lab, ": %{x", if (nzchar(fmtx)) paste0(":", fmtx) else "", "}",
          "<br>Fit (", nm, "): %{y", if (nzchar(fmty)) paste0(":", fmty) else "", "}",
          "<extra></extra>"
        )

        col.i <- .to_hex(border[i])
        lw.i  <- if (length(fit_lwd)) fit_lwd[min(i, length(fit_lwd))] else 2

        plt <- plotly::add_trace(
          plt, type = "scatter", mode = "lines",
          name = paste0("Fit: ", nm),
          legendgroup = nm,
          x = xl, y = yl,
          line = list(color = col.i, width = lw.i),
          hovertemplate = hover,
          showlegend = TRUE
        )
      }
    }
  }

  # ---- axes, grids, legend, bg ----
  x_axis <- axis_num(x.lab, ax$axT1, ax$axL1)
  y_axis <- axis_num(y.lab, ax$axT2, ax$axL2)
  border_shapes <- plot_border()

  plt <- plotly::layout(
    plt,
    xaxis = x_axis, yaxis = y_axis,
    shapes = c(x_grid(gridT1), y_grid(gridT2), border_shapes),
    legend = list(
      title = list(
        text = if (has_groups) by.name else NULL,
        font = list(size = 14.5, family = "Arial", color = "black")
      ),
      font = list(size = 13, family = "Arial", color = "black"),
      orientation = "v",
      x = 1.05, y = 0.5,
      xanchor = "left", yanchor = "middle",
      bgcolor = "rgba(255,255,255,0.9)",
      bordercolor = "#CCCCCC",
      borderwidth = 1,
      itemsizing = "constant"
    ),
    template = NULL,
    uniformtext = list(minsize = 10, mode = "hide")
  )

  plt$x$layout$plot_bgcolor  <- .to_hex(getOption("panel_fill",  "white"))
  plt$x$layout$paper_bgcolor <- .to_hex(getOption("window_fill", "white"))

  # ---- margin bottom fix ----
  if (is.null(plt$x$layout$margin)) plt$x$layout$margin <- list()
  plt$x$layout$margin$b <- max(plt$x$layout$margin$b %||% 40, 60)

  plt <- .finalize_plotly_widget(
    plt,
    kind = "sp",
    x.name = x.lab,
    by.name = by.name,
    add_title = FALSE,
    nudge_viewer = TRUE
  )

  invisible(plt)
}
