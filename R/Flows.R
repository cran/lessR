Flows <- function(
  # data
  value,                 # weights
  stage1,                # left column
  stage2,                # middle column
  stage3 = NULL,         # optional right column; if NULL -> 2-stage
  data = d,
  # appearance
  title = NULL,
  fill = NULL,
  link_alpha = 0.55,
  nodes_gray = FALSE,    # plotly default: sources colored, others gray
  neutral_gray = "gray80",
  border_gray  = "gray60",
  lift_y = 0,            # positive lifts diagram upward; negative lowers
  labels_size = 1.00,    # relative text size
  digits_d = 0           # decimals in hover
) {

  # ----- capture argument names from the call -----
  call <- match.call(expand.dots = FALSE)
  get_name <- function(arg_expr)
    if (is.symbol(arg_expr)) as.character(arg_expr)
    else if (is.character(arg_expr)) arg_expr[1]
    else as.character(arg_expr)

  stage1_name <- get_name(call$stage1)
  stage2_name <- get_name(call$stage2)
  stage3_name <- if (!is.null(call$stage3)) get_name(call$stage3) else NULL
  value_name  <- get_name(call$value)

  # ----- resolve data and columns -----
  if (!is.null(data)) {
    stopifnot(is.data.frame(data))
    need <- c(stage1_name, stage2_name, value_name)
    if (!all(need %in% names(data)))
      stop("Columns not found in `data`: ",
           paste(setdiff(need, names(data)), collapse = ", "), call. = FALSE)
    if (!is.null(stage3_name) && !(stage3_name %in% names(data)))
      stop("Column not found in `data`: ", stage3_name, call. = FALSE)
    stage1 <- data[[stage1_name]]
    stage2 <- data[[stage2_name]]
    stage3 <- if (!is.null(stage3_name)) data[[stage3_name]] else NULL
    value  <- data[[value_name]]
  } else {
    pe <- parent.frame()
    stage1 <- get(stage1_name, envir = pe)
    stage2 <- get(stage2_name, envir = pe)
    stage3 <- if (!is.null(stage3_name)) get(stage3_name, envir = pe) else NULL
    value  <- get(value_name,  envir = pe)
  }

  # ----- ensure desired order via factor levels (preserve first-seen order)
  if (!is.factor(stage1)) stage1 <- factor(stage1, levels = unique(stage1))
  if (!is.factor(stage2)) stage2 <- factor(stage2, levels = unique(stage2))
  if (!is.null(stage3) && !is.factor(stage3))
    stage3 <- factor(stage3, levels = unique(stage3))

  # ordered labels from factor levels
  sources <- levels(stage1)
  mids    <- levels(stage2)
  dests   <- if (is.null(stage3)) character(0) else levels(stage3)

  # ----- nodes (keep order; do NOT sort/unique again) -----
  nodes <- c(sources, mids, dests)
  idx   <- setNames(seq_along(nodes) - 1L, nodes)  # 0-based index for plotly

  # ----- theme colors -----
  neutral_gray <- .to_hex(neutral_gray)
  border_gray  <- .to_hex(border_gray)
  ARROW <- "\u2192"

  # ----- links (aggregate values) -----
  L1 <- aggregate(value, list(stage1, stage2), sum)
  names(L1) <- c("a","b","val")
  links1 <- data.frame(
    source  = idx[as.character(L1$a)],
    target  = idx[as.character(L1$b)],
    value   = L1$val,
    src_key = as.character(L1$a),
    stringsAsFactors = FALSE
  )
  if (length(dests)) {
    L2 <- aggregate(value, list(stage1, stage2, stage3), sum)
    names(L2) <- c("a","b","c","val")
    links2 <- data.frame(
      source  = idx[as.character(L2$b)],
      target  = idx[as.character(L2$c)],
      value   = L2$val,
      src_key = as.character(L2$a),
      stringsAsFactors = FALSE
    )
    links <- rbind(links1, links2)
  } else {
    links <- links1
  }

  # ----- layout positions for nodes (x,y coordinates) -----
  nA <- length(sources); nB <- length(mids); nC <- length(dests)
  xA <- rep(0.02, nA)
  xB <- rep(if (nC) 0.50 else 0.98, nB)
  xC <- if (nC) rep(0.98, nC) else numeric(0)
  node_x <- c(xA, xB, xC)
  y_even <- function(n) if (n <= 1) 0.5 else seq(0.1, 0.9, length.out = n)
  node_y <- c(y_even(nA), y_even(nB), if (nC) y_even(nC) else numeric(0))

# colors: sources get palette, others neutral, unless nodes_gray=TRUE -----
  n.x   <- nA
  theme <- getOption("theme")
  fill_vec <- if (is.null(fill))
    .color_range(.get_fill(theme), n.x)
  else
    .color_range(fill, n.x)
  pal_sources <- .to_hex(fill_vec)
  names(pal_sources) <- sources

  link_color <- .maketrans_plotly(pal_sources[links$src_key], link_alpha)
  node_color <- if (isTRUE(nodes_gray)) {
    rep(neutral_gray, length(nodes))
  } else {
    c(unname(pal_sources), rep(neutral_gray, nB + nC))
  }
  node_line <- list(color = border_gray, width = 1)

  # ----- title text -----
  ttl <- if (!is.null(title) && nzchar(title)) {
    sprintf("<b>%s</b>", title)
  } else if (length(dests)) {
    sprintf("<b>%s %s %s %s %s</b>",
            stage1_name, ARROW, stage2_name, ARROW, stage3_name)
  } else {
    sprintf("<b>%s %s %s</b>", stage1_name, ARROW, stage2_name)
  }

# ----- adaptive vertical domain (always honor lift_y; shrink height as needed) -----
  height_base  <- 0.94   # normal chart height
  height_min   <- 0.80   # minimum height under extreme lift/compression
  compress_per <- 0.50   # shrink factor per unit |lift_y|
  base_y1      <- 0.02   # baseline lower margin

  # shrink height smoothly as |lift_y| grows (helps across Viewer/Zoom/Export)
  h_req <- height_base - compress_per * abs(lift_y)
  h     <- max(height_min, min(height_base, h_req))

  # requested domain based on lift
  y1 <- base_y1 + lift_y
  y2 <- y1 + h

  # ensure the plot always fits in [0,1]; if needed, shrink further to fit
  if (y2 > 1) {
    y2 <- 1
    y1 <- max(0, y2 - h)
    if (y1 < base_y1 + lift_y) {
      y1 <- base_y1 + lift_y
      y2 <- 1
      h  <- y2 - y1
      if (h < 0.20) { y2 <- 1; h <- 0.20; y1 <- y2 - h }
    }
  }
  if (y1 < 0) {
    y1 <- 0
    y2 <- min(1, y1 + h)
    if (y1 > base_y1 + lift_y) {
      y1 <- 0
      y2 <- base_y1 + lift_y + h
      if (y2 > 1) { y2 <- 1; y1 <- y2 - h }
      if ((y2 - y1) < 0.20) { y1 <- 0; y2 <- 0.20; h <- 0.20 }
    }
  }

  y_dom   <- c(y1, y2)
  title_y <- min(0.98, y_dom[2] + 0.03)  # title slightly above chart top

  # ----- build plotly -----
  fmt_val <- sprintf("%%{value:,.%df}", digits_d)

  plt <- plotly::plot_ly(
    type = "sankey",
    arrangement = "fixed",
    domain = list(x = c(0, 1), y = y_dom),
    node = list(
      label = nodes,
      color = node_color,
      x = node_x, y = node_y,
      pad = 12, thickness = 16,
      line = node_line,
      hovertemplate = "%{label}<extra></extra>",
      font = list(size = 25 * labels_size, family = "Arial", color = "black")
    ),
    link = list(
      source = links$source,
      target = links$target,
      value  = links$value,
      color  = link_color,
      hovertemplate = paste0(
        "%{source.label} ", ARROW, " %{target.label}",
        "<br>", value_name, ": ", fmt_val, "<extra></extra>"
      )
    )
  )

  plt <- plotly::layout(
    plt,
    title = list(
      text = ttl,
      x = 0.5, y = title_y, xanchor = "center", yanchor = "top",
      font = list(size = 18 * labels_size, family = "Arial", color = "black")
    ),
    margin = list(t = 90, r = 30, b = 30, l = 30),
    font = list(size = 15 * labels_size, family = "Arial", color = "black"),
    template = NULL
  )

  # finalize / cache
  plt <- plotly::plotly_build(plt)
  plt <- .finalize_plotly_widget(
    plt,
    kind = if (length(dests)) "sankey3" else "sankey2",
    x.name = stage1_name,
    by.name = if (length(dests)) paste(stage2_name, ARROW, stage3_name) else stage2_name,
    add_title = is.null(title) || !nzchar(title),
    nudge_viewer = FALSE
  )

  # re-apply title in case helper overwrote style/position
  plt <- plotly::layout(
    plt,
    title = list(
      text = ttl,
      x = 0.5, y = title_y, xanchor = "center", yanchor = "top",
      font = list(size = 18 * labels_size, family = "Arial", color = "black")
    )
  )

  if (interactive()) print(plt)
  invisible(plt)
}
