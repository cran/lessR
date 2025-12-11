piechart.plotly <- function(
  x, x.name, y.name, by.name = NULL, ttl,
  fill, border, opacity,
  hole,
  ncols = NULL,
  labels, labels_position, labels_color,
  labels_size,
  labels_decimals,
  digits_d = 2,
  group_labels = TRUE,
  group_label_pos = c("below","above"),
  group_label_size = 14,
  group_label_gap = 0.05
) {

  group_label_pos <- match.arg(group_label_pos)

  # --- helpers ------------------------------------------------------------
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

  .pick_by_names_or_recycle <- function(v, needed_names, needed_len) {
    v <- as.character(v)
    nm <- names(v)
    if (!is.null(nm) && length(nm)) {
      out <- v[match(needed_names, nm)]
      if (anyNA(out)) {
        base <- if (length(v)) v else "black"
        out[is.na(out)] <- base[((which(is.na(out)) - 1L) %% length(base)) + 1L]
      }
      return(out)
    }
    if (!length(v)) return(rep("black", needed_len))
    rep_len(v, needed_len)
  }

  .pick_by_names <- function(v, needed_names, fallback = "gray80") {
    v <- as.character(v); nm <- names(v)
    if (!is.null(nm) && length(nm)) {
      out <- v[match(needed_names, nm)]
      out[is.na(out)] <- fallback
      return(out)
    }
    if (!length(v)) return(rep(fallback, length(needed_names)))
    rep_len(v, length(needed_names))
  }

  # Simulated panel bg for pie grid (helps in some Viewers)
  .add_panel_bg <- function(plt) {
    bg_plot  <- toupper(.to_hex(getOption("panel_fill",  "white")))
    bg_paper <- toupper(.to_hex(getOption("window_fill", "white")))
    plt <- plotly::layout(plt, paper_bgcolor = bg_paper)
    shapes <- plt$x$layout$shapes %||% list()
    shapes <- Filter(function(s) {
      !(identical(s$type,"rect") && identical(s$xref,"paper") &&
        identical(s$yref,"paper") && identical(s$x0,0) && identical(s$y0,0) &&
        identical(s$x1,1) && identical(s$y1,1))
    }, shapes)
    bg_shape <- list(
      type="rect", xref="paper", yref="paper",
      x0=0, y0=0, x1=1, y1=1,
      fillcolor=bg_plot, line=list(width=0), layer="below"
    )
    plt$x$layout$shapes <- c(shapes, list(bg_shape))
    plt
  }

  val_spec <- paste0(":.", max(0, as.integer(digits_d)), "f")

  # --- SINGLE PIE: 1D / named vector -------------------------------------
  if (is.null(dim(x)) || length(dim(x)) == 0L || length(dim(x)) == 1L) {

    slices <- names(x)
    values <- suppressWarnings(as.numeric(x))
    stopifnot(length(slices) == length(values))

    # fill/border
    fill_vec   <- .pick_by_names_or_recycle(fill, slices, length(slices))
    border_vec <- if (length(border) <= 1L) rep_len(as.character(border), length(slices))
                  else .pick_by_names_or_recycle(border, slices, length(slices))

    # label text
    label_mode <- if (!missing(labels) && length(labels)) tolower(as.character(labels[1])) else "%"
    fmt_num <- function(z) formatC(z, format = "f", digits = digits_d)

    tot <- sum(values, na.rm = TRUE)
    num_str <- switch(label_mode,
      "%"     = paste0(fmt_num(100 * values / max(tot, 1e-12)), "%"),
      "prop"  = fmt_num(values / max(tot, 1e-12)),
      "input" = fmt_num(values),
      "off"   = rep("", length(values)),
      paste0(fmt_num(100 * values / max(tot, 1e-12)), "%")
    )
    text_vec <- if (identical(label_mode, "off")) slices else paste0(slices, "<br>", num_str)

    # inside/outside
    pos_in  <- if (!missing(labels_position) && length(labels_position))
                 tolower(as.character(labels_position[1])) == "in" else TRUE
    txt_pos <- if (pos_in) "inside" else "outside"

    base_size <- 12
    txt_size  <- round(base_size * 1.38 * (if (!missing(labels_size) && length(labels_size)) labels_size else 1), 0)

    # --- opacity for fill -------------------------------------------------
    alpha_fill <- as.numeric(opacity)
    fills_rgba <- .maketrans_plotly(fill_vec, alpha = alpha_fill)

    # auto text color if requested
    want_adjust <- is.null(labels_color) || identical(labels_color[1], "adjust")
    txt_colors  <- if (want_adjust) .auto_text_color(fills_rgba, bg = getOption("panel_fill", "white"))
                   else rep(labels_color[1], length(fills_rgba))

    dom_y   <- c(0.03, 0.93)
    margins <- list(t = 40, r = 20, b = 30, l = 20)

    grand_total <- sum(values, na.rm = TRUE)
    overall_pct <- values / if (grand_total > 0) grand_total else 1

    plt <- plotly::plot_ly(
      type   = "pie",
      labels = slices,
      values = values,
      sort   = FALSE,
      hole   = hole,
      domain = list(x = c(0, 1), y = dom_y),
      text         = text_vec,
      textinfo     = "text",
      textposition = txt_pos,
      insidetextorientation = "radial",
      textfont       = list(color = txt_colors, size = txt_size),
      insidetextfont = list(color = txt_colors, size = txt_size),
      outsidetextfont= list(color = txt_colors, size = txt_size),
      automargin     = !pos_in,
      marker = list(
        colors = fills_rgba,
        line   = list(color = "rgba(0,0,0,0)", width = 1)
      ),
      customdata = overall_pct,
      hovertemplate = paste0(
        x.name, ": %{label}",
        "<br>", y.name, ": %{value", val_spec, "}",
        "<br>% of total: %{customdata:.2%}",
        "<extra></extra>"
      ),
      showlegend = FALSE
    )

    # Defensive post-build tweaks
    built <- tryCatch(plotly::plotly_build(plt), error = function(e) NULL)
    if (!is.null(built) && length(built$x$data) >= 1L) {
      n_slices <- length(built$x$data[[1]]$labels %||% built$x$data[[1]]$values)
      bvec <- if (length(border_vec) == 1L) rep(border_vec[1], n_slices) else rep_len(border_vec, n_slices)
      bvec <- .to_hex(bvec)
      built$x$data[[1]]$marker$line <- list(color = I(bvec), width = rep(1.6, n_slices))
      built$x$data[[1]]$marker$line$colorsrc <- NULL
      built$x$data[[1]]$marker$line$widthsrc <- NULL

      if (identical(txt_pos, "inside") && isTRUE(hole > 0.62)) {
        built$x$data[[1]]$hole <- 0.62
      }
      plt <- built
    }

    plt$x$layout$template    <- NULL
    plt$x$layout$uniformtext <- NULL
    plt <- plotly::layout(
      plt,
      uniformtext = list(minsize = 8, mode = "show"),
      margin = margins,
      title  = list(text = ttl, y = 0.94, yanchor = "top")
    )

    plt <- .finalize_plotly_widget(
      plt, kind = "pie", x.name = x.name, by.name = by.name,
      add_title = FALSE, nudge_viewer = TRUE
    )

    if (!identical(toupper(.to_hex(getOption("panel_fill"))),  "#FFFFFF") ||
        !identical(toupper(.to_hex(getOption("window_fill"))), "#FFFFFF")) {
      plt <- .add_panel_bg(plt)
    }

    return(invisible(plt))
  }

  # --- GROUPED INPUT (2D): PIE GRID ---------------------------------------
  stopifnot(length(dim(x)) == 2L)
  x <- as.matrix(x)
  groups <- rownames(x); if (is.null(groups)) groups <- as.character(seq_len(nrow(x)))
  slices <- colnames(x); if (is.null(slices)) slices <- paste0(x.name, "_", seq_len(ncol(x)))
  grand_total <- sum(x, na.rm = TRUE)

  build_pie_grid <- function() {
    k  <- nrow(x)
    # default columns: sqrt layout (k=2 -> 2 cols; k=4 -> 2; k>=5 -> 3, etc.)
    nc <- if (is.null(ncols) || ncols < 1) ceiling(sqrt(k)) else as.integer(ncols)
    nr <- ceiling(k / nc)

    plt <- plotly::plot_ly()
    if (is.null(plt$x$layout$annotations)) plt$x$layout$annotations <- list()
    by_title <- if (length(by.name) && nzchar(by.name)) by.name else "Group"

    for (i in seq_len(k)) {
      j   <- i - 1L
      col <- j %% nc
      row <- j %/% nc
      x0 <- col / nc
      x1 <- (col + 1) / nc
      # y from top to bottom, but Plotly y=0 is bottom => flip
      y0 <- 1 - (row + 1) / nr
      y1 <- 1 - row / nr

      shrink <- 0.92
      y_mid  <- (y0 + y1) / 2
      y_half <- (y1 - y0) * shrink / 2
      dom <- list(x = c(x0, x1), y = c(y_mid - y_half, y_mid + y_half))

      grp  <- groups[i]
      vals <- suppressWarnings(as.numeric(x[i, ]))
      vals[!is.finite(vals)] <- NA_real_
      val_sum     <- sum(vals, na.rm = TRUE)
      overall_pct <- vals / if (grand_total > 0) grand_total else 1

      fmt_num <- function(z) formatC(z, format = "f", digits = digits_d)
      label_mode  <- tolower((labels %||% "%")[1])
      num_str <- switch(label_mode,
        "%"     = paste0(fmt_num(100 * vals / max(val_sum, 1e-12)), "%"),
        "prop"  = fmt_num(vals / max(val_sum, 1e-12)),
        "input" = fmt_num(vals),
        "off"   = rep("", length(vals)),
        paste0(fmt_num(100 * vals / max(val_sum, 1e-12)), "%")
      )

      slice_labels <- slices
      text_vec <- if (label_mode == "off") slice_labels else paste0(slice_labels, "<br>", num_str)

      pos_in   <- tolower((labels_position %||% "in")[1]) == "in"
      txt_pos  <- if (pos_in) "inside" else "outside"
      base_sz  <- 12
      txt_size <- round(base_sz * (labels_size %||% 1), 0)

      # --- opacity for fill in grouped pies ------------------------------
      alpha_fill <- as.numeric(opacity)

      fills_this   <- .pick_by_names(fill,   slices, fallback = "#CCCCCC")
      fills_this   <- .maketrans_plotly(fills_this, alpha = alpha_fill)
      borders_this <- .pick_by_names(border, slices, fallback = "gray80")

      txt_colors <- if (!length(labels_color) || identical(labels_color[1], "adjust")) {
        .auto_text_color(fills_this, bg = getOption("panel_fill", "white"))
      } else {
        rep(labels_color[1], length(fills_this))
      }

      plt <- plotly::add_trace(
        plt,
        type       = "pie",
        labels     = slice_labels,
        values     = vals,
        name       = paste0(by_title, ": ", grp),
        legendgroup= "pies",
        sort       = FALSE,
        direction  = "clockwise",
        hole       = hole,
        domain     = dom,
        text       = text_vec,
        textinfo   = "text",
        textposition = txt_pos,
        insidetextorientation = "radial",
        textfont         = list(color = txt_colors, size = txt_size),
        insidetextfont   = list(color = txt_colors, size = txt_size),
        outsidetextfont  = list(color = txt_colors, size = txt_size),
        automargin       = !pos_in,
        marker = list(
          colors = fills_this,
          line   = list(color = borders_this, width = 1)
        ),
        customdata = overall_pct,
        hovertemplate = paste0(
          by_title, ": ", grp,
          "<br>", x.name, ": %{label}",
          "<br>", y.name, ": %{value", val_spec, "}",
          "<br>% of ", grp, ": %{percent}",
          "<br>% of total: %{customdata:.2%}",
          "<extra></extra>"
        ),
        showlegend = FALSE
      )

      if (isTRUE(group_labels) && hole > 0) {
        cx <- mean(dom$x); cy <- mean(dom$y)
        plt$x$layout$annotations <- c(
          plt$x$layout$annotations,
          list(list(
            x = cx, y = cy,
            xref = "paper", yref = "paper",
            text = grp,
            showarrow = FALSE,
            xanchor = "center", yanchor = "middle",
            font = list(size = group_label_size %||% 14, color = "#666666")
          ))
        )
      }
    }

    plotly::layout(plt, uniformtext = list(minsize = 10, mode = "show"))
  }

  plt <- build_pie_grid()
  plt <- plotly::layout(
    plt,
    title  = list(text = ttl, y = 0.94, yanchor = "top"),
    margin = list(t = 55, r = 20, b = 20, l = 20)
  )
  plt <- .finalize_plotly_widget(
    plt, kind = "pie", x.name = x.name, by.name = by.name,
    add_title = FALSE, nudge_viewer = TRUE
  )

  if (!identical(toupper(.to_hex(getOption("panel_fill"))),  "#FFFFFF") ||
      !identical(toupper(.to_hex(getOption("window_fill"))), "#FFFFFF")) {
    plt <- .add_panel_bg(plt)
  }

  plt <- .finalize_plotly_widget(
    plt,
    kind = "pie",
    x.name = x.name,
    by.name = by.name,
    add_title = FALSE,
    nudge_viewer = FALSE
  )

  force_viewer_reload(plt)  # proper alignment in RStudio viewer

  invisible(plt)
}
