.radar.plotly <- function(
  x.call, by.call=NULL, facet.call=NULL, y.call=NULL,
  x.name, by.name, facet.name, y.name,
  stat,
  fill, border, opacity,
  digits_d = 2,
  facet_size, facet_gap_x, facet_gap_y,
  facet_title_y_base, facet_title_row_shift
) {

  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

  # ---- 1) Build data.frame ----
  n <- length(x.call)
  if (!length(n)) stop("radar(): x.call has length 0.")

  x_fac      <- factor(x.call)
  by_fac_in  <- if (is.null(by.call)) factor(rep("(All)", n), levels="(All)")
                else factor(by.call)
  fac_fac_in <- if (is.null(facet.call))factor(rep("(All)", n), levels="(All)")
                else factor(facet.call)

  # keys for detecting pre-aggregated data
  key_df  <- data.frame(
    facet = fac_fac_in,
    by    = by_fac_in,
    x     = x_fac
  )
  has_dups <- any(duplicated(key_df))

  # ---- 2) Value vector + stat ----
  if (is.null(y.call)) {
    # classic count case
    val      <- rep(1, n)
    STAT     <- "sum"
    fun      <- function(z) sum(z, na.rm = TRUE)
    y_label  <- "Count"
  }
  else {
    # y provided
    if (is.null(stat) || !nzchar(stat)) {
      ## No stat provided: allow *pre-aggregated* input
      if (has_dups) {
        stop(
          "radar(): when y.call is provided and there are multiple rows ",
          "per facet/by/x combination, 'stat' must be one of: ",
          "mean, sum, median, min, max, sd."
        )
      }

      ## Pre-aggregated case: carry values through with identity
      STAT <- "identity"
      fun  <- function(z) z[1]

      val <- suppressWarnings(as.numeric(y.call))
      val[!is.finite(val)] <- NA_real_

      y_label <- y.name

    }
    else {
      ## Stat explicitly provided: aggregate as before
      STAT <- tolower(stat)
      fun <- switch(
        STAT,
        mean   = function(z) mean(z,   na.rm = TRUE),
        sum    = function(z) sum(z,    na.rm = TRUE),
        median = function(z) stats::median(z, na.rm = TRUE),
        min    = function(z) min(z,    na.rm = TRUE),
        max    = function(z) max(z,    na.rm = TRUE),
        sd     = function(z) stats::sd(z,     na.rm = TRUE),
        stop("Unsupported stat: ", stat)
      )

      val <- suppressWarnings(as.numeric(y.call))
      val[!is.finite(val)] <- NA_real_

      StatName <- switch(
        STAT,
        mean = "Mean", sum = "Sum", median = "Median",
        min  = "Min",  max = "Max", sd = "SD", toupper(stat)
      )
      y_label <- paste(StatName, y.name)
    }
  }

  df <- data.frame(
    facet = fac_fac_in,
    by    = by_fac_in,
    x     = x_fac,
    val   = val
  )

  # ---- 3) Aggregate to 3-way array ----
  agg <- stats::aggregate(val ~ facet + by + x, data=df, FUN=fun, drop=FALSE)
  names(agg)[names(agg) == "val"] <- "value"

  r_tab <- xtabs(value ~ facet + by + x, data = agg, drop.unused.levels=FALSE)

  facet_fac <- factor(dimnames(r_tab)[[1]])
  by_fac    <- factor(dimnames(r_tab)[[2]])
  x_fac_out <- factor(dimnames(r_tab)[[3]])

  if (!length(x_fac_out)) stop("radar(): x has no levels to plot.")
  if (!length(by_fac))   stop("radar(): by has no levels to plot.")
  if (!length(facet_fac))stop("radar(): facet has no levels to plot.")

  rmax  <- max(r_tab, na.rm = TRUE)
  n_fac <- length(facet_fac)
  n_by  <- length(by_fac)

  # ---- 4) Facet gaps from facet_size ----
  base_gap_x <- 0.04
  base_gap_y <- 0.11
  if (n_fac > 1L) {
    if (is.null(facet_gap_x) && is.null(facet_gap_y)) {
      facet_gap_x <- base_gap_x / (facet_size %||% 1)
      facet_gap_y <- base_gap_y / (facet_size %||% 1)
    } else {
      facet_gap_x <- facet_gap_x %||% base_gap_x
      facet_gap_y <- facet_gap_y %||% base_gap_y
    }
  } else {
    facet_gap_x <- facet_gap_x %||% base_gap_x
    facet_gap_y <- facet_gap_y %||% base_gap_y
  }

  # ---- 5) Facet domains ----
  grid <- .plotly_make_domains_grid(
    n_fac     = n_fac,
    n_col_max = 3L,
    gap_x     = facet_gap_x,
    gap_y     = facet_gap_y,
    facet_size= facet_size
  )
  domains <- grid$domains

  # ---- 6) Angular setup ----
  theta_deg    <- .plotly_theta_from_levels(levels(x_fac_out))
  theta_closed <- c(theta_deg, theta_deg[1])
  text_labels  <- c(levels(x_fac_out), levels(x_fac_out)[1])

  # ---- 7) Colors + opacity ----
  theme <- getOption("theme")

  if (is.null(fill)) {
    fill_vec <- .color_range(.get_fill(theme), n_by)
  } else {
    fill_vec <- .color_range(fill, n_by)
  }

  border_vec <- border %||% "transparent"
  border_vec <- rep_len(border_vec, n_by)

  cols_hex   <- .to_hex(fill_vec)
  fills_rgba <- .maketrans_plotly(cols_hex, alpha = opacity)
  border_hex <- .to_hex(border_vec)

  # ---- 8) Build traces ----
  p <- plotly::plot_ly()
  val_spec <- paste0(":.", max(0L, as.integer(digits_d)), "f")

  for (i in seq_along(facet_fac)) {
    fac <- facet_fac[i]
    subplot_name <- if (i == 1) "polar" else paste0("polar", i)
    show_leg <- (i == 1 && n_by > 1)

    for (j in seq_along(by_fac)) {
      r_j <- as.numeric(r_tab[fac, by_fac[j], ])
      r_closed <- c(r_j, r_j[1])

      p <- plotly::add_trace(
        p,
        type  = "scatterpolar",
        theta = theta_closed,
        thetaunit = "degrees",
        r     = r_closed,
        mode  = "lines+markers",
        line  = list(width = 2, color = border_hex[j]),
        marker = list(size = 4, color = cols_hex[j]),
        fill  = "toself",
        fillcolor = fills_rgba[j],
        name  = as.character(by_fac[j]),
        text  = text_labels,
        hovertemplate = paste0(
          x.name, ": %{text}<br>",
          by.name, ": ", by_fac[j], "<br>",
          facet.name, ": ", fac, "<br>",
          y_label, ": %{r", val_spec, "}<extra></extra>"
        ),
        subplot = subplot_name,
        showlegend = show_leg
      )
    }
  }

  # ---- 9) Polar layout ----
  p <- .plotly_apply_polar_layout(
    p,
    domains   = domains,
    theta_deg = theta_deg,
    x_labels  = levels(x_fac_out),
    rmax      = rmax
  )

  # ---- 10) Titles + facet annotations ----
  ttl_text <- .plotly_build_title(
    x.name     = x.name,
    by.name    = by.name,
    facet.name = facet.name,
    y.name     = if (is.null(y.call)) NULL else y.name,
    stat       = if (is.null(y.call)) NULL else stat
  )

  ann <- .plotly_facet_annotations(
    domains,
    facet_levels = levels(facet_fac),
    facet_name   = facet.name,
    y_base       = facet_title_y_base + 0.090,
    y_row_shift  = facet_title_row_shift,
    font_size    = 14
  )

  # ---- 11) Final layout (legend shifted left a bit) ----
  p <- plotly::layout(
    p,
    title = list(
      text = ttl_text,
      x = 0.5, xanchor = "center",
      y = 0.985, yanchor = "top"
    ),
    margin = list(t = 90, b = 0, l = 25, r = 25),
    annotations = ann,
    legend = list(
      x = 1.0,
      xanchor = "right",
      y = 1.0,
      yanchor = "top",
      bordercolor = .to_hex(getOption("panel_color", "gray70")),
      borderwidth = 1
    )
  )

  p
}
