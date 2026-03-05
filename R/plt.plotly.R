plt.plotly <- function(
  g,
  by.name, n.by,                               # n.by unused; reserved
  fill, border, shape, pt.size,
  x.lab, y.lab, ax, gridT1, gridT2,
  digits_d=2, date.var,                        # date.var unused; reserved
  fit_color=border, fit_lwd=NULL, ln.type=NULL, # ln.type unused; reserved
  connect=FALSE, ts_unit=NULL,                 # for time series
  power=0.5, radius.in=0.12,   # area=size^power (for bubble mode)
  size.name="Size",
  ellipse_region=NULL, ellipse_fill=NULL,
  ellipse_color=NULL, ellipse_lwd=NULL,
  se.poly=NULL, se_fill=NULL,
  pt_opacity=0.95
) {

  # sanitize opacity
  pt_opacity <- as.numeric(pt_opacity[1])
  if (!is.finite(pt_opacity)) pt_opacity <- 0.95
  pt_opacity <- max(0, min(1, pt_opacity))

  # ---- bubbles (global scale) ----
  # should also apply to discrete bubble plots, e.g., XY(Dept, Gender)
  dpi <- getOption("plotly_dpi", 96)
  pt_rows  <- rep(TRUE, nrow(g))  # placeholder; reserved for future row filtering
  size_vec <- as.numeric(g$size)
  use_bubbles <- length(unique(size_vec[!is.na(size_vec)])) > 1L

  if (use_bubbles) {
    sz_all <- pmax(0, size_vec)^power
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


# groups set up -----------------------------------------------------------

  ind_chr   <- trimws(as.character(g$ind))
  valid     <- !is.na(g$ind)
  idx_list  <- split(seq_len(nrow(g))[valid], ind_chr[valid], drop=TRUE)
  grp_names <- names(idx_list)
  has_groups <- length(grp_names) > 1L


# initialization ----------------------------------------------------------
  plt <- plotly::plot_ly()
  border_hex <- .to_hex(border)  # fully opaque borders

  symbol <- switch(as.character(shape),
    "21"="circle","22"="square","23"="diamond",
    "24"="triangle-up","25"="triangle-down","circle"
  )


# no groups ---------------------------------------------------------------

  if (!has_groups) {

    if (!is.null(ts_unit)) {
      xv <- as.Date(g$x, origin="1970-01-01")
      xv <- .format_date_labels(xv, ts_unit)
      xlab_h <- "Date"
    } else {
      xv <- as.numeric(g$x)
      xlab_h <- x.lab
    }
    yv <- as.numeric(g$y)

    y.lab.h <- trimws(y.lab)
    hover_tmpl <- .hover.fmt(xv, yv, xlab_h, y.lab.h, FALSE, digits_d, ts_unit)

    xv <- as.numeric(g$x)        # keep numeric for plotly axis

    # size/marker setup
    if (use_bubbles) {
      diam_px <- bubble.diam(size_vec)
      hover_tmpl <- sub(
        "<extra></extra>$",
        paste0("<br>", size.name, ": %{customdata:.",
               digits_d, "f}<extra></extra>"),
        hover_tmpl
      )
      customdata <- size_vec
    }
    else {
      px <- as.numeric(pt.size) * 7.25
      if (!is.finite(px)) px <- 5
      diam_px   <- rep(px, length(xv))
      customdata <- NULL
    }

    # set alpha via rgba colors, and leave marker.opacity at 1
    marker_color <- .maketrans_plotly(fill[1], alpha = pt_opacity)

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
        line     = list(color = border_hex[1], width = 1)
      ),
      line  = if (connect) list(color = border_hex[1], width = 1.5) else NULL,
      hovertemplate = hover_tmpl,
      customdata    = customdata,
      showlegend    = FALSE
    )

  }


# groups ------------------------------------------------------------------

  else {  # ---- groups ----

    for (i in seq_along(grp_names)) {
      nm  <- grp_names[i]
      idx <- idx_list[[nm]]
      if (!length(idx)) next

# --- x for hover (formatted as Date if time series) ---
      if (!is.null(ts_unit)) {
        xv_h <- as.Date(g$x[idx], origin = "1970-01-01")
        xv_h <- .format_date_labels(xv_h, ts_unit)
        xlab_h <- "Date"   # generic label for time series hover
      } else {
        xv_h   <- as.numeric(g$x[idx])
        xlab_h <- x.lab
      }

      yv <- as.numeric(g$y[idx])

      y.lab.h <- trimws(y.lab)
      hover.i <- sub(
        "<extra></extra>$",
        paste0("<br>", by.name, ": %{text}<extra></extra>"),
        .hover.fmt(xv_h, yv, xlab_h, y.lab.h, FALSE, digits_d, ts_unit)
      )

# --- x for plotly axis positioning (keep numeric day index) ---
      xv <- as.numeric(g$x[idx])

      if (use_bubbles) {
        diam_px  <- bubble.diam(size_vec[idx])
        hover.i  <- sub(
          "<extra></extra>$",
          paste0("<br>", size.name,
                 ": %{customdata:.", digits_d, "f}<extra></extra>"), hover.i
        )
        custom.i <- size_vec[idx]

      }
      else {
        # Point sizing for standard scatter / time-series.
        # - If user sets pt.size = 0, we suppress markers entirely (lines only).
        # - Otherwise, scale to plotly pixels.
        px <- as.numeric(pt.size) * 6.5
        if (!is.finite(px)) px <- 5
        if (px < 0) px <- 5
        diam_px  <- rep(px, length(idx))
        custom.i <- NULL
      }

      # per-group fill with desired alpha
      fill_i <- .maketrans_plotly(fill[i], alpha = pt_opacity)

      # ---- Main data trace ----
      # For connected series, keep markers on the plot, remove from the legend
      # by adding a separate legend-only line trace (below).
      mode_data <- if (connect) {
        if (use_bubbles) "lines+markers"
        else if (diam_px[1] <= 0) "lines" else "lines+markers"
      } else {
        "markers"
      }

      plt <- plotly::add_trace(
        plt,
        type = "scatter",
        mode = mode_data,
        name = nm, legendgroup = nm, text = nm,
        x = xv, y = yv,
        marker = if (grepl("markers", mode_data, fixed = TRUE)) list(
          symbol   = symbol,
          size     = diam_px,
          sizemode = "diameter",
          color    = fill_i,
          opacity  = 1,
          line     = list(color = .to_hex(border[i]), width = 1)
        ) else NULL,
        line  = if (connect) list(color = .to_hex(border[i]), width = 1.5) else NULL,
        hovertemplate = hover.i,
        customdata    = custom.i,
        showlegend    = if (connect) FALSE else TRUE
      )

      # ---- Legend-only line sample (no dot) ----
      if (connect) {
        ok2 <- which(is.finite(xv) & is.finite(yv))
        if (length(ok2) >= 2L) {
          idx2 <- ok2[1:2]
          plt <- plotly::add_trace(
            plt,
            type = "scatter",
            mode = "lines",
            name = nm, legendgroup = nm,
            x = xv[idx2], y = yv[idx2],
            line = list(color = .to_hex(border[i]), width = 1.5),
            hoverinfo = "skip",
            showlegend = TRUE
          )
        }
      }
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

    edge_vec <- if (length(ellipse_color))
                    .to_hex(rep_len(ellipse_color, target_len))
                 else .to_hex(rep_len(border, target_len))
    lw_vec   <- as.numeric(if (length(ellipse_lwd))
                       rep_len(ellipse_lwd, target_len) else rep(1, target_len))
    fill_vec <- if (length(ellipse_fill))
                       .to_hex(rep_len(ellipse_fill, target_len))
                 else rep("transparent", target_len)

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
        fillcolor = .to_hex(se_fill %||% "transparent"),
        hoverinfo = "skip",
        showlegend = FALSE,
        inherit = FALSE
      )
    }
  }

  # ---- fits ----
  fit.rows <- is.finite(g$x) & is.finite(g$f)
  if (any(fit.rows)) {
    fit_valid      <- fit.rows & !is.na(g$ind)
    ind.fit.chr <- trimws(as.character(g$ind[fit_valid]))
    idx.fit     <- split(which(fit_valid), ind.fit.chr, drop = TRUE)
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
    }
    else {
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
          "<br>Fit (", nm, "): %{y", if (nzchar(fmty))
                                     paste0(":", fmty) else "", "}",
          "<extra></extra>"
        )

        col.i <- .to_hex(if (length(fit_color) >= i) fit_color[i] else border[i])
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

  if (is.null(plt$x$layout$margin)) plt$x$layout$margin <- list()

  if (!nzchar(x.lab)) {
    plt$x$layout$margin$b <- 0L
  } else {
    b_default <- plt$x$layout$margin$b %||% 40
    plt$x$layout$margin$b <- max(b_default, 60)
  }

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
