.hier.plotly <- function(
  x.call, by.call = NULL, facet.call = NULL, y.call = NULL,
  x.name = "X", by.name = "Group", facet.name = "Facet", y.name = "Y",
  type = "sunburst",
  stat = "sum",
  fill = NULL,
  border = NULL,
  digits_d = NULL,
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


# add trace function ------------------------------------------------------

  make_trace <- function(p, nd, domain_xy) {
    if (!is.null(stat) && isTRUE(digits_d == 0)) digits_d <- digits_d + 2  # mean, etc.
    txtinfo <- if (!show_text) "none" else switch(labels,
      "input" = "label+value",
      "%"     = "label+percent root",
      "prop"  = "label+percent root",
      "label"
    )

    digits <- if (is.null(digits_d)) 0L else max(0L, as.integer(digits_d))

    stat_line <- paste0(
      stat_label, ": %{customdata.stat:,.", digits, "f}"
    )

    pct_parent_line <- if (non_additive) ""
                       else "% of parent: %{percentParent:.2%}"

    hover_lines <- c(
      "%{customdata.meta}",
      stat_line,
      paste0("n: %{customdata.n}"),
      pct_parent_line,
      "% of total: %{percentRoot:.2%}"
    )

    hover <- paste(hover_lines[nzchar(hover_lines)], collapse = "<br>")
    hover <- paste0(hover, "<extra></extra>")

    # map top-level category -> color, then inherit down the tree
    col_map   <- setNames(as.character(fill_vec), top_levels)
    node_cols <- unname(col_map[as.character(nd$root)])
    node_cols[is.na(node_cols)] <- unname(col_map[1L])  # fallback if any NA

    marker <- list(
      colors = node_cols,
      line   = list(color = border_hex, width = 1)
    )

    plotly::add_trace(
      p,
      type         = type,
      ids          = nd$ids,
      labels       = nd$labels,
      parents      = nd$parents,
      values       = nd$values,
      customdata   = nd$customdata,
      domain       = list(x = domain_xy$x, y = domain_xy$y),
      branchvalues = "remainder",
      textinfo     = txtinfo,
      textfont     = list(size = label_fsize, color = labels_color),
      hovertemplate = hover,
      marker       = marker
    )
  }  # end make_trace()


  # ---- node builder function --------------------------------------------

  build_nodes <- function(idx, facet_label = NULL) {
    if (length(idx) == 0L) {
      return(data.frame(
        ids     = character(),
        labels  = character(),
        parents = character(),
        values  = numeric(),
        customdata = I(list()),
        root    = character()
      ))
    }

    df  <- path_df[idx, , drop=FALSE]
    val <- base_vals[idx]

    out_ids     <- character(0)
    out_labels  <- character(0)
    out_parents <- character(0)
    out_values  <- numeric(0)   # sizing values (always additive)
    out_custom  <- list()       # holds pretty meta + stat value
    out_root    <- character(0) # top-level category for color mapping

    for (lvl in seq_len(n_levels)) {
      cols <- path_cols[1:lvl]
      # ---- compute size + stat for this level (slice-by-slice) ----

      if (non_additive && identical(STAT, "mean")) {

        # compute sum(y) and n together to guarantee row alignment
        agg_tmp <- stats::aggregate(
          cbind(
            sumy = ifelse(is.na(val), 0, val),
            n    = ifelse(is.na(val), 0, 1)
          ),
          by = df[, cols, drop=FALSE],
          FUN = sum
        )

        # sizing must be additive for branchvalues="total"; use counts for mean
        agg_size <- agg_tmp[, cols, drop=FALSE]
        agg_size$size <- agg_tmp$n

        # hover stat: standard (weighted) mean = sum(y) / n
        agg_stat <- agg_tmp[, cols, drop=FALSE]
        agg_stat$stat <- agg_tmp$sumy / agg_tmp$n

      }
       else {

        # sizing aggregation, always sum for branchvalues="total"
        agg_size <- stats::aggregate(val, by = df[, cols, drop=FALSE],
                                     FUN = size_fun)
        names(agg_size)[ncol(agg_size)] <- "size"

        # default: additive stat, stat equals size, no extra aggregation
        if (!non_additive) {  # additive stat: stat equals size
          agg_stat <- agg_size
          names(agg_stat)[ncol(agg_stat)] <- "stat"
        }
        else {  # non-additive stat: compute stat separately
          agg_stat <- stats::aggregate(val, by = df[, cols, drop=FALSE],
                                       FUN = stat_fun)
          names(agg_stat)[ncol(agg_stat)] <- "stat"
        }
      }

      cur_labels <- mk_char(agg_size[[cols[lvl]]])
      # root category = value of first path column at this aggregated node
      root_vals  <- mk_char(agg_size[[path_cols[1L]]])

      if (lvl == 1L) {
        parents <- rep("", nrow(agg_size))
        ids     <- paste0("I_", cur_labels)
      }
      else {
        parent_df <- agg_size[, cols[-length(cols)], drop=FALSE]
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
      }, seq_len(nrow(agg_size)), SIMPLIFY = TRUE, USE.NAMES=FALSE)

      # node counts (n), aligned to agg_size row order
      if (is.null(y.call)) {
        node_n <- agg_size$size
      }
      else {
        agg_n <- stats::aggregate(ifelse(is.na(val), 0, 1),
                                 by = df[, cols, drop=FALSE], FUN = sum)
        names(agg_n)[ncol(agg_n)] <- "n"
        key_size <- do.call(paste, c(agg_size[, cols, drop=FALSE], sep="\r"))
        key_n    <- do.call(paste, c(agg_n[, cols, drop=FALSE], sep="\r"))
        node_n   <- agg_n$n[match(key_size, key_n)]
      }

      custom <- Map(
        function(meta_txt, stat_v, n_v)
          list(meta = meta_txt, stat = stat_v, n = n_v, level=lvl),
        meta_lines, agg_stat$stat, node_n
      )

      out_ids     <- c(out_ids, ids)
      out_labels  <- c(out_labels, cur_labels)
      out_parents <- c(out_parents, parents)
      # branchvalues="remainder": only leaves carry values; parent value = 0
      # so Plotly sums children automatically — no parent/child mismatch possible
      out_values  <- c(out_values,
                       if (lvl == n_levels) agg_size$size else rep(0, nrow(agg_size)))
      out_custom  <- c(out_custom, custom)
      out_root    <- c(out_root, root_vals)
    }  # end: for (lvl in seq_len(n_levels))

    nd <- data.frame(
      ids     = out_ids,
      labels  = out_labels,
      parents = out_parents,
      values  = out_values,
      root    = factor(out_root, levels = top_levels)
    )
    nd$customdata <- I(out_custom)
    nd  #return
  }  # end build_nodes()


# begin analysis ----------------------------------------------------------

  labels          <- match.arg(labels)
  labels_position <- match.arg(labels_position)

  # final flag that controls text on/off for nodes
  show_text <- isTRUE(show_labels) && !identical(labels, "off")

  # simple font scaling (same base as other lessR plotlys)
  base_font   <- 12
  label_fsize <- base_font * labels_size


# build path table ------------------------------------------------------------ 

  mk_char <- function(v) as.character(v)

  if (is.null(by.call)) {
    path_df   <- data.frame(x = mk_char(x.call), check.names=FALSE)
    lvl_names <- c(x.name)
  }
  else if (is.data.frame(by.call)) {
    tmp <- as.data.frame(lapply(by.call, mk_char), check.names=FALSE)
    path_df   <- data.frame(x = mk_char(x.call), tmp, check.names=FALSE)
    lvl_names <- c(x.name, names(tmp))
  }
  else {
    path_df   <- data.frame(x = mk_char(x.call), by = mk_char(by.call),
                            check.names=FALSE)
    lvl_names <- c(x.name, by.name %||% "by")
  }

  path_cols <- names(path_df)
  n_levels  <- length(path_cols)

  # ---- base numeric vector from y (or 1s for counts) -------------------
  base_vals <- if (is.null(y.call)) rep(1, nrow(path_df)) else {
    z <- suppressWarnings(as.numeric(y.call))
    z[!is.finite(z)] <- 0  # treat non-finite as 0 for sizing; ring must close
    z
  }

  # ---- set the stat function & label ------------------------------------
  STAT <- tolower(stat %||% "sum")
  stat_fun <- switch(
    STAT,
    mean   = function(z) mean(z, na.rm=TRUE),
    sum    = function(z) sum(z,  na.rm=TRUE),
    median = function(z) stats::median(z, na.rm=TRUE),
    min    = function(z) min(z, na.rm=TRUE),
    max    = function(z) max(z, na.rm=TRUE),
    sd     = function(z) stats::sd(z,  na.rm=TRUE),
    function(z) sum(z, na.rm=TRUE)
  )

  non_additive <- !identical(STAT, "sum")

  # For sizing always apply an additive aggregator; replace NA with 0 so
  # all leaf values sum exactly to the root and the outer ring closes.
  size_fun <- function(z) sum(ifelse(is.na(z), 0, z))

  stat_label <- if (is.null(y.call)) "Count" else y.name


# assign colors, setup based on top-level categories ----------------------

  # top-level categories are in the first path column
  top_raw <- mk_char(path_df[[1L]])
  top_levels <- if (is.factor(x.call)) levels(x.call)
                else unique(top_raw)

  if (is.null(fill)) {  # fill palette for top-level categories
    base_cols <- .plotly_base_colors()
    fill_vec  <- base_cols[seq_len(length(top_levels))]
  }
  else {  # allow ranges like "blues" or named vectors
    fill_vec  <- .color_range(fill, length(top_levels))
  }

  # border color, one color for all nodes; consistent with prior behavior
  border_hex <- .to_hex(if (is.null(border)) "white" else border[1L])


# start plot & set base colorway (still used by Plotly defaults) ----------

  p <- plotly::plot_ly()
  p <- plotly::layout(p, colorway = fill_vec)


# non-faceted -------------------------------------------------------------

  if (is.null(facet.call)) {
    nd <- build_nodes(seq_len(nrow(path_df)), facet_label = NULL)
    p  <- make_trace(p, nd, list(x = c(0, 1), y = c(0, 1)))

    ttl_text <- main %||% .plotly_build_title(
      x.name, by.name, facet.name = NULL,
      y.name = if (is.null(y.call)) NULL else y.name,
      stat   = if (is.null(y.call)) NULL else STAT
    )

    p <- plotly::layout(
      p,
      margin = list(t = 50, b = 8, l = 20, r = 20),
      title  = list(text = ttl_text, font = list(size = 16))
    )

    # Ensure unique htmlwidget id (REQUIRED for knitting)
    if (is.null(p$elementId)) {
      p$elementId <- paste0(
        "lessr-hier-",
        format(Sys.time(), "%Y%m%d%H%M%OS3"),
        "-",
        sample.int(1e5, 1)
      )
    }

    return(p)
  }


# faceted -----------------------------------------------------------------

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


# Type-specific vertical adjustments for facets ---------------------------

  # for titles and reduce overlap with the main title.
  if (type == "treemap") {
    domains <- lapply(domains, function(d) {
      y0   <- d$y[1]; y1 <- d$y[2]
      span <- y1 - y0
      shift <- 0.04
      new_y0 <- max(0, y0 - shift)
      new_y1 <- new_y0 + span
      if (new_y1 > 1) {
        new_y1 <- 1
        new_y0 <- new_y1 - span
      }
      list(x = d$x, y = c(new_y0, new_y1))
    })
  }
  else if (type == "icicle") {
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
  }
  else if (type == "sunburst") {
    # For faceted sunbursts, drop the panels slightly downward
    # to create more space between facet titles and the rings
    shift <- 0.06  # about 6% of full figure height

    domains <- lapply(domains, function(d) {
      y0   <- d$y[1]; y1 <- d$y[2]
      span <- y1 - y0

      new_y1 <- min(1, y1 - shift)
      new_y0 <- max(0, new_y1 - span)

      list(x = d$x, y = c(new_y0, new_y1))
    })
  }


# add traces for each facet -----------------------------------------------

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


# facet title annotations -------------------------------------------------

  ann <- .plotly_facet_annotations(
    domains,
    facet_levels = fac_levels,
    facet_name   = facet.name,
    y_base       = facet_title_y_base,
    y_row_shift  = facet_title_row_shift
  )

  if (type %in% c("sunburst", "treemap")) {
    for (i in seq_along(ann)) {
      top_y <- domains[[i]]$y[2]
      ann[[i]]$y <- min(top_y + 0.03, 0.98)
    }
  } else if (type == "icicle") {
    for (i in seq_along(ann)) {
      top_y <- domains[[i]]$y[2]
      # Icicle facet titles use full-figure coordinates, with gap the panel
      ann[[i]]$yref    <- "paper"
      ann[[i]]$yanchor <- "bottom"
      ann[[i]]$y       <- min(top_y + 0.10, 0.99)
    }
  }


  # Plot --------------------------------------------------------------------

  p <- plotly::layout(
    p,
    annotations = ann,
    margin = list(t=50, b=8, l=20, r=20),
    title  = list(text = ttl_text, font = list(size=16))
  )

  p$elementId <- paste0(
    "lessr-hier-",
    format(Sys.time(), "%Y%m%d%H%M%OS3"),
    "-",
    sample.int(1e5, 1)
  )

  p   # last expression returned
}
