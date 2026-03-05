bubble.plotly <- function(
  x, x.name, y.name, by.name, x.lab, y.lab, ttl = NULL,
  fill, clr, opacity,
  power, radius,
  digits_d = 0,
  labels, labels_position = "in", labels_color, labels_size,
  labels_decimals = NULL,
  label_min_px = 26,
  label_autocontrast = TRUE
) {

  # ---------- helpers ----------
  .pick_col <- function(v, i, key) {
    if (!length(v)) return("black")
    nms <- names(v)
    if (!is.null(nms) && length(nms) && key %in% nms) return(v[[key]])
    if (length(v) >= i) return(v[[i]])
    v[[ ((i - 1L) %% length(v)) + 1L ]]
  }

  dpi <- getOption("plotly_dpi", 96)
  bubble_diam_builder <- function(all_values, power, radius, dpi) {
    v <- pmax(0, as.numeric(all_values))
    max_ru <- max(v^power, na.rm = TRUE)
    max_px_diam <- 2 * radius * dpi
    function(vals) {
      vals <- pmax(0, as.numeric(vals))
      if (!is.finite(max_ru) || max_ru <= 0) return(rep(0, length(vals)))
      max_px_diam * (vals^power) / max_ru
    }
  }

  alpha_fill <- if (is.null(opacity)) 0.85 else as.numeric(opacity)
  if (!is.finite(alpha_fill)) alpha_fill <- 0.85
  alpha_fill <- max(0, min(1, alpha_fill))

  # ---------- data prep ----------
  one_d <- (length(dim(x)) == 0L) || (length(dim(x)) == 1L)
  if (one_d) {
    xv <- names(x)
    yv <- suppressWarnings(as.numeric(x))
    if (is.null(xv) || !length(xv)) xv <- as.character(seq_along(yv))
    df <- data.frame(..x = factor(xv, levels = xv), ..val = yv)

    n_x <- nlevels(df$..x)
    if (is.null(fill)) {
      theme <- getOption("theme")
      fill <- .color_range(.get_fill(theme), n_x)
    } else {
      fill <- .color_range(fill, n_x)
    }
  } else {
    if (is.table(x)) df <- as.data.frame(x)
    else if (is.matrix(x)) df <- as.data.frame(as.table(x))
    else stop("For 2-D bubble plot, x must be a table or matrix.")
    names(df)[1:3] <- c("..by", "..x", "..val")

    if (!is.null(rownames(x))) df$..by <- factor(df$..by, levels = rownames(x))
    else df$..by <- factor(df$..by)

    if (!is.null(colnames(x))) df$..x  <- factor(df$..x,  levels = colnames(x))
    else df$..x <- factor(df$..x)

    n_g <- nlevels(df$..by)
    if (is.null(fill)) {
      theme <- getOption("theme")
      fill <- .color_range(.get_fill(theme), n_g)
    } else {
      fill <- .color_range(fill, n_g)
    }
  }

  # ---------- shares ----------
  total <- sum(df$..val, na.rm = TRUE)
  share_tot <- if (total > 0) df$..val / total else rep(0, nrow(df))
  if (one_d) {
    share_x <- share_tot
  } else {
    col_tot <- tapply(df$..val, df$..x, sum, na.rm = TRUE)
    share_x <- df$..val / as.numeric(col_tot[as.character(df$..x)])
    share_x[!is.finite(share_x)] <- 0
  }

  custom_rows <- Map(function(xc, yc, px, pt) {
    list(xcat = as.character(xc), bycat = as.character(yc),
         pct_x = as.numeric(px), pct_tot = as.numeric(pt))
  },
  df$..x,
  if (one_d) rep("", nrow(df)) else df$..by,
  share_x, share_tot)

  # ---------- sizes & colors ----------
  diam_fun   <- bubble_diam_builder(df$..val, power, radius, dpi)
  diam_px    <- diam_fun(df$..val)
  border_hex <- .to_hex(clr)
  fmt_val    <- .fmt(df$..val, d = digits_d)

  if (one_d) {
    col_map <- setNames(as.character(fill), levels(df$..x))
    cat_hex <- .to_hex(unname(col_map))
    names(cat_hex) <- names(col_map)
    cat_text_col <- vapply(cat_hex, .contrast_text_for_hex, "", USE.NAMES = TRUE)
    labels_color_vec <- unname(cat_text_col[as.character(df$..x)])
    fill_rgba <- .maketrans_plotly(unname(col_map[as.character(df$..x)]), alpha = alpha_fill)
  } else {
    col_map <- setNames(as.character(fill), levels(df$..by))
    grp_hex <- .to_hex(unname(col_map))
    names(grp_hex) <- names(col_map)
    grp_text_col <- vapply(grp_hex, .contrast_text_for_hex, "", USE.NAMES = TRUE)
    labels_color_vec <- unname(grp_text_col[as.character(df$..by)])
    fill_rgba <- .maketrans_plotly(unname(col_map[as.character(df$..by)]), alpha = alpha_fill)
  }

  if (length(labels_color)) {
    labels_color_vec <- rep_len(.to_hex(labels_color), length(labels_color_vec))
  } else if (!isTRUE(label_autocontrast)) {
    labels_color_vec <- rep(NA_character_, length(labels_color_vec))
  }

  # resolve decimal places for label text
  lbl_d <- if (!is.null(labels_decimals)) as.integer(labels_decimals) else as.integer(digits_d)

  if (!identical(labels, "off")) {
    if (identical(labels, "input")) {
      label_txt <- fmt_val
    } else if (identical(labels, "%")) {
      label_txt <- sprintf(paste0("%.", lbl_d, "f%%"), 100 * share_tot)
    } else {
      label_txt <- sprintf(paste0("%.", lbl_d, "f"), share_tot)
    }
    label_show <- ifelse(is.finite(diam_px) & (diam_px >= label_min_px), label_txt, "")
  } else {
    label_show <- rep("", length(fmt_val))
  }

  trace_mode <- if (identical(labels, "off")) "markers" else "markers+text"

  txt_pos <- if (identical(tolower(as.character(labels_position[1])), "in"))
               "middle center" else "top center"
  # IMPORTANT: do NOT create an empty trace up front (can trigger axis-type warnings)
  plt <- plotly::plot_ly(
    hoverlabel = list(align = "left")
  )

  if (one_d) {

    hover <- paste0(
      x.name, ": %{customdata.xcat}",
      "<br>", y.name, ": %{hovertext}",
      "<br>% of ", x.name, ": %{customdata.pct_x:.1%}",
      "<br>% of total: %{customdata.pct_tot:.1%}<extra></extra>"
    )

    plt <- plotly::add_trace(
      plt,
      type = "scatter",
      mode = trace_mode,
      x = df$..x,
      y = rep(0, nrow(df)),                 # numeric (avoid category axis warnings)
      hovertext     = fmt_val,
      hovertemplate = hover,
      customdata    = custom_rows,
      marker = list(
        symbol   = "circle",
        size     = diam_px,
        sizemode = "diameter",
        color    = fill_rgba,
        line     = list(color = border_hex, width = 1),
        sizemin  = 12
      ),
      text         = label_show,
      textposition = txt_pos,
      textfont     = list(size = labels_size, color = labels_color_vec),
      cliponaxis   = FALSE,
      showlegend   = FALSE
    )

    plt <- plotly::layout(
      plt,
      xaxis = list(
        type = "category",
        title = list(text = x.lab),
        showgrid = FALSE,
        zeroline = FALSE,
        showline = TRUE,
        ticks = "outside"
      ),
      yaxis = list(
        visible = FALSE,
        fixedrange = TRUE
      ),
      template = NULL
    )

  } else {

    hover <- paste0(
      x.name, ": %{customdata.xcat}",
      "<br>", by.name, ": %{customdata.bycat}",
      "<br>", y.name, ": %{hovertext}",
      "<br>% of ", x.name, ": %{customdata.pct_x:.1%}",
      "<br>% of total: %{customdata.pct_tot:.1%}<extra></extra>"
    )

    by_levels <- levels(df$..by)

    for (i in seq_along(by_levels)) {
      g <- by_levels[i]
      sel <- df$..by == g
      if (!any(sel)) next

      plt <- plotly::add_trace(
        plt,
        type = "scatter",
        mode = trace_mode,
        x = df$..x[sel],
        y = df$..by[sel],
        hovertext     = fmt_val[sel],
        hovertemplate = hover,
        customdata    = custom_rows[sel],
        marker = list(
          symbol   = "circle",
          size     = diam_px[sel],
          sizemode = "diameter",
          color    = fill_rgba[sel][1],
          line     = list(color = .to_hex(.pick_col(clr, i, g)), width = 1)
        ),
        text         = label_show[sel],
        textposition = txt_pos,
        textfont     = list(size = labels_size, color = labels_color_vec[sel]),
        cliponaxis   = FALSE,
        showlegend   = FALSE
      )
    }

    plt <- plotly::layout(
      plt,
      xaxis = list(
        type = "category",
        title = list(text = x.lab),
        showgrid = FALSE,
        zeroline = FALSE
      ),
      yaxis = list(
        type = "category",
        title = list(text = y.lab),
        showgrid = FALSE,
        zeroline = FALSE,
        categoryorder = "array",
        categoryarray = rev(levels(df$..by))
      ),
      template = NULL,
      shapes = list(list(
        type = "rect",
        xref = "paper", yref = "paper",
        x0 = 0, x1 = 1, y0 = 0, y1 = 1,
        line = list(color = .to_hex(getOption("panel_border", "#808080")), width = 1),
        layer = "below"
      ))
    )
  }

  # backgrounds
  plt$x$layout$plot_bgcolor  <- .to_hex(getOption("panel_fill",  "white"))
  plt$x$layout$paper_bgcolor <- .to_hex(getOption("window_fill", "white"))

  # title (if provided)
  if (!is.null(ttl) && length(ttl) && nzchar(ttl[1])) {
    plt <- plotly::layout(
      plt,
      title  = list(text = ttl[1], x = 0.5, xanchor = "center", y = 0.98, yanchor = "top"),
      margin = list(t = 80, r = 25, b = 30, l = 25)
    )
  }

  invisible(plt)
}
