.hier.plotly <- function(
  x.call, by.call = NULL, facet.call = NULL, y.call = NULL,
  x.name = "X", by.name = "Group", facet.name = "Facet", y.name = "Y",
  type = "sunburst",
  stat = "sum",
  fill = NULL,
  border = NULL,
  digits_d = 2,
  facet_gap_x = 0.04, facet_gap_y = 0.11,
  facet_size = 1.00, facet_title_y_base = -0.018,
  facet_title_row_shift = -0.003,
  main = NULL,
  show_labels = TRUE,
  labels = c("input", "%", "prop", "off"),
  labels_position = c("in", "out"),
  labels_color = "white",
  labels_size  = 0.75,
  ...
) {

  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

  labels          <- match.arg(labels)
  labels_position <- match.arg(labels_position)

  # final flag that controls text on/off for nodes
  show_text <- isTRUE(show_labels) && !identical(labels, "off")

  # simple font scaling (same base as other lessR plotlys)
  base_font   <- 12
  label_fsize <- base_font * labels_size

  # ---- route pie+by -> sunburst conceptually ---------------------------
  type_hier <- if (identical(type, "pie") && !is.null(by.call)) "sunburst" else type
  if (!type_hier %in% c("pie", "sunburst", "treemap", "icicle"))
    stop("Unsupported hierarchical type: ", type_hier)

  # ---- build path table ------------------------------------------------
  mk_char <- function(v) as.character(if (is.factor(v)) v else v)

  if (is.null(by.call)) {
    path_df   <- data.frame(x = mk_char(x.call),
                            stringsAsFactors = FALSE, check.names = FALSE)
    lvl_names <- c(x.name)
  } else if (is.data.frame(by.call)) {
    tmp <- as.data.frame(lapply(by.call, mk_char),
                         stringsAsFactors = FALSE, check.names = FALSE)
    path_df   <- data.frame(x = mk_char(x.call), tmp,
                            stringsAsFactors = FALSE, check.names = FALSE)
    lvl_names <- c(x.name, names(tmp))
  } else {
    path_df   <- data.frame(x = mk_char(x.call), by = mk_char(by.call),
                            stringsAsFactors = FALSE, check.names = FALSE)
    lvl_names <- c(x.name, by.name %||% "by")
  }

  path_cols <- names(path_df)
  n_levels  <- length(path_cols)

  # ---- base numeric vector from y (or 1s for counts) -------------------
  base_vals <- if (is.null(y.call)) rep(1, nrow(path_df)) else {
    z <- suppressWarnings(as.numeric(y.call))
    z[!is.finite(z)] <- NA_real_
    z
  }

  # ---- stat function & label -------------------------------------------
  STAT <- tolower(stat %||% if (is.null(y.call)) "sum" else "sum")

  stat_fun <- switch(
    STAT,
    mean   = function(z) mean(z, na.rm = TRUE),
    sum    = function(z) sum(z,  na.rm = TRUE),
    median = function(z) stats::median(z, na.rm = TRUE),
    min    = function(z) min(z, na.rm = TRUE),
    max    = function(z) max(z, na.rm = TRUE),
    sd     = function(z) stats::sd(z,  na.rm = TRUE),
    function(z) sum(z,   na.rm = TRUE)
  )

  non_additive <- !identical(stat_fun, sum)

  # For sizing we ALWAYS need an additive aggregator
  size_fun <- function(z) sum(z, na.rm = TRUE)

  stat_label <- if (is.null(y.call)) "Count" else {
    Stat <- switch(
      STAT,
      mean   = "Mean",
      sum    = "Sum",
      median = "Median",
      min    = "Min",
      max    = "Max",
      sd     = "SD",
      toupper(STAT)
    )
    paste(Stat, y.name %||% "Value")
  }

  # ---- color setup based on top-level categories -----------------------
  # top-level categories are in the first path column
  top_raw <- mk_char(path_df[[1L]])
  top_levels <- if (!is.null(levels(path_df[[1L]]))) levels(path_df[[1L]]) else unique(top_raw)

  # fill palette for top-level categories
  if (is.null(fill)) {
    base_cols <- .plotly_base_colors()
    fill_vec  <- base_cols[seq_len(length(top_levels))]
  } else {
    # allow user ranges like "blues" or named vectors; use existing lessR helper
    fill_vec  <- .color_range(fill, length(top_levels))
  }

  # border color (one color for all nodes; consistent with prior behavior)
  border_hex <- .to_hex(if (is.null(border)) "white" else border[1L])

  # ---- node builder (per facet subset) ---------------------------------
  build_nodes <- function(idx, facet_label = NULL) {
    if (length(idx) == 0L) {
      return(data.frame(
        ids     = character(),
        labels  = character(),
        parents = character(),
        values  = numeric(),
        custom  = list(),
        root    = character(),
        stringsAsFactors = FALSE
      ))
    }

    df  <- path_df[idx, , drop = FALSE]
    val <- base_vals[idx]

    out_ids     <- character(0)
    out_labels  <- character(0)
    out_parents <- character(0)
    out_values  <- numeric(0)   # sizing values (always additive)
    out_custom  <- list()       # holds pretty meta + stat value
    out_root    <- character(0) # top-level category for color mapping

    for (lvl in seq_len(n_levels)) {
      cols <- path_cols[1:lvl]

      # sizing aggregation (always sum for branchvalues="total")
      agg_size <- stats::aggregate(val, by = df[, cols, drop = FALSE], FUN = size_fun)
      names(agg_size)[ncol(agg_size)] <- "size"

      # stat aggregation (statistic shown in hover)
      if (non_additive) {
        agg_stat <- stats::aggregate(val, by = df[, cols, drop = FALSE], FUN = stat_fun)
        names(agg_stat)[ncol(agg_stat)] <- "stat"

        # align rows by key (defensive)
        k_size <- do.call(paste, c(agg_size[, cols, drop = FALSE], sep = "\r"))
        k_stat <- do.call(paste, c(agg_stat[, cols, drop = FALSE], sep = "\r"))
        m <- match(k_size, k_stat)
        agg_stat <- agg_stat[m, , drop = FALSE]
      } else {
        # additive stat: stat equals size
        agg_stat <- agg_size
        names(agg_stat)[ncol(agg_stat)] <- "stat"
        agg_stat$stat <- agg_size$size
      }

      cur_labels <- mk_char(agg_size[[cols[lvl]]])
      # root category = value of first path column at this aggregated node
      root_vals  <- mk_char(agg_size[[path_cols[1L]]])

      if (lvl == 1L) {
        parents <- rep("", nrow(agg_size))
        ids     <- paste0("I_", cur_labels)
      } else {
        parent_df <- agg_size[, cols[-length(cols)], drop = FALSE]
        parents   <- apply(parent_df, 1, function(r)
          paste0("I_", paste(mk_char(r), collapse = "_")))
        ids       <- paste0(parents, "_", cur_labels)
      }

      # human-readable meta lines
      meta_lines <- mapply(function(row_i) {
        parts <- character(0)
        for (k in seq_len(lvl)) {
          parts <- c(parts, paste0(lvl_names[k], ": ",
                                   mk_char(agg_size[row_i, cols[k]])))
        }
        if (!is.null(facet_label))
          parts <- c(parts, paste0(facet.name, ": ", facet_label))
        paste(parts, collapse = "<br>")
      }, seq_len(nrow(agg_size)), SIMPLIFY = TRUE, USE.NAMES = FALSE)

      custom <- Map(
        function(meta_txt, stat_v)
          list(meta = meta_txt, stat = stat_v, level = lvl),
        meta_lines, agg_stat$stat
      )

      out_ids     <- c(out_ids, ids)
      out_labels  <- c(out_labels, cur_labels)
      out_parents <- c(out_parents, parents)
      out_values  <- c(out_values, agg_size$size)
      out_custom  <- c(out_custom, custom)
      out_root    <- c(out_root, root_vals)
    }

    nd <- data.frame(
      ids     = out_ids,
      labels  = out_labels,
      parents = out_parents,
      values  = out_values,
      root    = factor(out_root, levels = top_levels),
      stringsAsFactors = FALSE
    )
    nd$customdata <- I(out_custom)
    nd
  }

  # ---- trace adder ------------------------------------------------------
  make_trace <- function(p, nd, domain_xy) {
    digits  <- max(0L, as.integer(digits_d))
    txtinfo <- if (isTRUE(show_labels)) "label" else "none"

    hover <- paste0(
      "%{customdata.meta}<br>",
      stat_label, ": %{customdata.stat:,.", digits, "f}",
      if (non_additive) "" else
        "<br>% of parent: %{percentParent:.2%}<br>% of total: %{percentRoot:.2%}",
      "<extra></extra>"
    )

    # map top-level category -> color, then inherit down the tree
    col_map   <- setNames(as.character(fill_vec), top_levels)
    node_cols <- col_map[as.character(nd$root)]
    node_cols[is.na(node_cols)] <- col_map[1L]  # fallback if any NA

    marker <- list(
      colors = node_cols,
      line   = list(color = border_hex, width = 1)
    )

    plotly::add_trace(
      p,
      type         = type_hier,
      ids          = nd$ids,
      labels       = nd$labels,
      parents      = nd$parents,
      values       = nd$values,
      customdata   = nd$customdata,
      domain       = list(x = domain_xy$x, y = domain_xy$y),
      branchvalues = "total",
      textinfo     = txtinfo,
      hovertemplate = hover,
      marker       = marker
    )
  }

  # ---- start plot & set base colorway (still used by Plotly defaults) ---
  p <- plotly::plot_ly()
  p <- plotly::layout(p, colorway = fill_vec)

  # ---- non-faceted ------------------------------------------------------
  if (is.null(facet.call)) {
    nd <- build_nodes(seq_len(nrow(path_df)), facet_label = NULL)
    p  <- make_trace(p, nd, list(x = c(0, 1), y = c(0, 1)))

    ttl_text <- main %||% .plotly_build_title(
      x.name, by.name, facet.name = NULL,
      y.name = if (is.null(y.call)) NULL else y.name,
      stat   = if (is.null(y.call)) NULL else STAT
    )

    return(plotly::layout(
      p,
      margin = list(t = 50, b = 8, l = 20, r = 20),
      title  = list(text = ttl_text, font = list(size = 16))
    ))
  }

  # ---- faceted ----------------------------------------------------------
  fac_levels <- if (!is.null(levels(facet.call))) levels(facet.call)
                else unique(mk_char(facet.call))
  n_fac <- length(fac_levels)

  gx <- max(0, as.numeric(facet_gap_x)) *
        (1 / max(1e-9, as.numeric(facet_size)))
  gy <- max(0, as.numeric(facet_gap_y)) *
        (1 / max(1e-9, as.numeric(facet_size)))

  grid    <- .plotly_make_domains_grid(
    n_fac    = n_fac,
    n_col_max = 3L,
    gap_x    = gx,
    gap_y    = gy
  )
  domains <- grid$domains

  # --- Type-specific vertical adjustments for facets ---------------------
  # Only in the faceted case. We alter the panel domains to free room
  # for titles and reduce overlap with the main title.
  if (type_hier == "treemap") {
    domains <- lapply(domains, function(d) {
      y0   <- d$y[1]; y1 <- d$y[2]
      span <- y1 - y0
      shift <- 0.04          # move panel downward a bit
      new_y0 <- max(0, y0 - shift)
      new_y1 <- new_y0 + span
      if (new_y1 > 1) {
        new_y1 <- 1
        new_y0 <- new_y1 - span
      }
      list(x = d$x, y = c(new_y0, new_y1))
    })
  } else if (type_hier == "icicle") {
    domains <- lapply(domains, function(d) {
      y0   <- d$y[1]; y1 <- d$y[2]
      span <- y1 - y0
      shrink <- 0.20         # shrink vertical span a bit
      new_span <- span * (1 - shrink)
      shift <- 0.06          # move down to create top space
      new_y1 <- max(0, y1 - shift)
      new_y0 <- max(0, new_y1 - new_span)
      if (new_y1 > 1) {
        new_y1 <- 1
        new_y0 <- new_y1 - new_span
      }
      list(x = d$x, y = c(new_y0, new_y1))
    })
  } else if (type_hier == "sunburst") {
    # For faceted sunbursts, drop the panels slightly downward
    # to create more space between facet titles and the rings,
    # using some of the empty space at the bottom.
    shift <- 0.06  # about 6% of full figure height

    domains <- lapply(domains, function(d) {
      y0   <- d$y[1]; y1 <- d$y[2]
      span <- y1 - y0

      new_y1 <- min(1, y1 - shift)
      new_y0 <- max(0, new_y1 - span)

      if ((new_y1 - new_y0) < span) {
        new_y0 <- max(0, new_y1 - span)
      }

      list(x = d$x, y = c(new_y0, new_y1))
    })
  }

  # ---- add traces for each facet ---------------------------------------
  for (i in seq_along(fac_levels)) {
    fac_lab <- fac_levels[i]
    idx     <- which(mk_char(facet.call) == fac_lab)
    nd      <- build_nodes(idx, facet_label = fac_lab)
    if (nrow(nd) > 0L)
      p <- make_trace(p, nd, domains[[i]])
  }

  ttl_text <- main %||% .plotly_build_title(
    x.name, by.name, facet.name,
    y.name = if (is.null(y.call)) NULL else y.name,
    stat   = if (is.null(y.call)) NULL else STAT
  )

  # ---- facet title annotations -----------------------------------------
  ann <- .plotly_facet_annotations(
    domains,
    facet_levels = fac_levels,
    facet_name   = facet.name,
    y_base       = facet_title_y_base,
    y_row_shift  = facet_title_row_shift
  )

  # park facet titles just above the top of each domain.
  # ---- facet title annotations -----------------------------------------
  ann <- .plotly_facet_annotations(
    domains,
    facet_levels = fac_levels,
    facet_name   = facet.name,
    y_base       = facet_title_y_base,
    y_row_shift  = facet_title_row_shift
  )

  # park facet titles just above the top of each domain.
  if (type_hier == "sunburst") {
    for (i in seq_along(ann)) {
      top_y <- domains[[i]]$y[2]
      ann[[i]]$y <- min(top_y + 0.03, 0.98)
    }
  } else if (type_hier == "treemap") {
    for (i in seq_along(ann)) {
      top_y <- domains[[i]]$y[2]
      ann[[i]]$y <- min(top_y + 0.03, 0.98)
    }
  } else if (type_hier == "icicle") {
    for (i in seq_along(ann)) {
      top_y <- domains[[i]]$y[2]

      # Force icicle facet titles to use full-figure coordinates,
      # then give them a generous gap above the panel.
      ann[[i]]$yref    <- "paper"
      ann[[i]]$yanchor <- "bottom"
      ann[[i]]$y       <- min(top_y + 0.10, 0.99)
    }
  }

  plotly::layout(
    p,
    annotations = ann,
    margin = list(t = 50, b = 8, l = 20, r = 20),
    title  = list(text = ttl_text, font = list(size = 16))
  )
}
