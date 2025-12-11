.bc.plotly <-
function(x, x.name, y.name, by.name, x.lab, y.lab,
         fill, clr, opacity = NULL,
         beside, horiz, ax, gridT, digits_d,
         labels=c("%", "input", "prop", "off"),
         labels_size = 12,
         labels_color = NULL,
         labels_autocontrast = TRUE) {

  # did the caller actually supply labels= ?
  missing_labels <- missing(labels)

  labels <- match.arg(labels)
  alpha_fill <- if (is.null(opacity)) 0.92 else as.numeric(opacity)

  .row_customdata <- function(a, b = NULL) {
    a <- as.numeric(a)
    if (is.null(b)) unname(lapply(a, function(z) c(z)))
    else {
      b <- as.numeric(b)
      unname(Map(function(z1, z2) c(z1, z2), a, b))
    }
  }

  .pick <- function(v, nm, i) {
    v   <- as.character(v)
    nms <- names(v)
    if (!is.null(nms) && nm %in% nms) return(v[[nm]])
    if (length(v) >= i) return(v[[i]])
    if (!length(v)) return("black")
    v[[ ((i - 1L) %% length(v)) + 1L ]]
  }

  .fmt_val <- function(v, d) .fmt(v, d = d)

  # we'll capture the category order from the incoming data
  cat_vals <- NULL

  # ------------------ 1-D ------------------
  if (length(dim(x)) == 1L) {
    df <- setNames(data.frame(names(x), as.numeric(x), check.names = FALSE),
                   c(x.name, y.name))

    # the order of names(x) here already reflects any sort done upstream
    cat_vals <- as.character(df[[x.name]])

    total_n <- sum(df[[y.name]], na.rm = TRUE)
    share   <- if (total_n > 0) df[[y.name]] / total_n else rep(NA_real_, nrow(df))

    # only force "input" when the user did NOT specify labels=
    if (missing_labels) {
      labels <- "input"
    }

    tick.fmt <- .get.tick.fmt(df[[y.name]], digits_d)

    hover <- if (!horiz)
      paste0(x.name, ": %{x}<br>",
             y.name, ": %{y:", tick.fmt, "}",
             "<br>% of total: %{customdata:.1%}<extra></extra>")
    else
      paste0(x.name, ": %{y}<br>",
             y.name, ": %{x:", tick.fmt, "}",
             "<br>% of total: %{customdata:.1%}<extra></extra>")

    fill_vec   <- .to_hex(rep_len(fill, nrow(df)))
    border_hex <- .to_hex(rep_len(clr,  nrow(df)))

    if (labels == "off") {
      txt <- rep("", nrow(df))
    } else if (labels == "input") {
      txt <- .fmt_val(df[[y.name]], digits_d)
    } else if (labels == "%") {
      txt <- sprintf("%.0f%%", 100 * share)
    } else {  # "prop"
      txt <- sprintf("%.2f", share)
    }

    if (length(labels_color)) {
      txt_col <- rep_len(.to_hex(labels_color), nrow(df))
    } else if (isTRUE(labels_autocontrast)) {
      txt_col <- vapply(fill_vec, .contrast_text_for_hex, "")
    } else {
      txt_col <- NULL
    }

    plt <- plotly::plot_ly(
      type        = "bar",
      x           = if (!horiz) df[[x.name]] else df[[y.name]],
      y           = if (!horiz) df[[y.name]] else df[[x.name]],
      orientation = if (!horiz) NULL else "h",
      marker = list(
        color   = fill_vec,
        opacity = alpha_fill,
        line    = list(color = border_hex, width = 1)
      ),
      customdata       = .row_customdata(share),
      hovertemplate    = hover,
      text             = txt,
      textposition     = "inside",
      insidetextanchor = "middle",
      textfont         = list(size = labels_size, color = txt_col),
      cliponaxis       = FALSE
    )

  } else {
    # ------------------ 2-D ------------------
    if (is.table(x)) df <- as.data.frame(x)
    else if (is.matrix(x)) df <- as.data.frame(as.table(x))
    else stop("For 2-D input, x must be a table or matrix.")

    names(df) <- c(by.name, x.name, y.name)

    # preserve row/column order of the incoming table/matrix,
    # which upstream code may already have sorted by totals
    df[[by.name]] <- factor(df[[by.name]], levels = rownames(x))
    df[[x.name]]  <- factor(df[[x.name]],  levels = colnames(x))

    cat_vals <- as.character(df[[x.name]])

    total_n <- sum(df[[y.name]], na.rm = TRUE)
    df$share_total <- df[[y.name]] / total_n

    x_totals <- tapply(df[[y.name]], df[[x.name]], sum, na.rm = TRUE)
    df$share_within_x <- df[[y.name]] /
      as.numeric(x_totals[as.character(df[[x.name]])])

    grp    <- df[[by.name]]
    groups <- levels(grp)

    tick.fmt <- .get.tick.fmt(df[[y.name]], digits_d)

    hover <- if (!horiz)
      paste0(
        x.name, ": %{x}<br>",
        by.name, ": %{fullData.name}<br>",
        y.name, ": %{y:", tick.fmt, "}",
        "<br>% of ", x.name, ": %{customdata[1]:.1%}",
        "<br>% of total: %{customdata[0]:.1%}<extra></extra>"
      )
    else
      paste0(
        x.name, ": %{y}<br>",
        by.name, ": %{fullData.name}<br>",
        y.name, ": %{x:", tick.fmt, "}",
        "<br>% of ", x.name, ": %{customdata[1]:.1%}",
        "<br>% of total: %{customdata[0]:.1%}<extra></extra>"
      )

    plt <- plotly::plot_ly()

    for (i in seq_along(groups)) {
      gname <- groups[i]
      rows  <- which(grp == gname)
      if (!length(rows)) next

      fill_i   <- .to_hex(.pick(fill, gname, i))
      border_i <- .to_hex(.pick(clr,  gname, i))

      cd_list <- .row_customdata(df$share_total[rows], df$share_within_x[rows])

      xv <- if (!horiz) df[[x.name]][rows] else df[[y.name]][rows]
      yv <- if (!horiz) df[[y.name]][rows] else df[[x.name]][rows]

      if (labels == "off") {
        txt <- rep("", length(rows))
      } else if (labels == "input") {
        txt <- .fmt_val(yv, digits_d)
      } else if (labels == "%") {
        txt <- sprintf("%.0f%%", 100 * df$share_total[rows])
      } else {  # "prop"
        txt <- sprintf("%.2f", df$share_total[rows])
      }

      if (length(labels_color)) {
        txt_col <- rep_len(.to_hex(labels_color), length(rows))
      } else if (isTRUE(labels_autocontrast)) {
        txt_col <- rep_len(.contrast_text_for_hex(fill_i), length(rows))
      } else {
        txt_col <- NULL
      }

      plt <- plotly::add_trace(
        plt,
        type        = "bar",
        x           = xv,
        y           = yv,
        orientation = if (!horiz) NULL else "h",
        name        = gname,
        legendgroup = gname,
        marker = list(
          color   = fill_i,
          opacity = alpha_fill,
          line    = list(color = border_i, width = 1)
        ),
        customdata       = cd_list,
        hovertemplate    = hover,
        text             = txt,
        textposition     = "inside",
        insidetextanchor = "middle",
        textfont         = list(size = labels_size, color = txt_col),
        cliponaxis       = FALSE
      )
    }

    plt <- plotly::layout(
      plt,
      barmode = if (beside) "group" else "stack"
    )
  }

  # --- axes & background ------------------------------------------------
  border_shapes <- plot_border()

  # unique category order taken from incoming (potentially sorted) table
  cat_array <- unique(as.character(cat_vals))

  if (horiz) {
    # categories along y-axis
    y_axis_cat <- c(
      axis_cat(x.lab),
      list(
        categoryorder = "array",
        categoryarray = cat_array
      )
    )

    plt <- plotly::layout(
      plt,
      xaxis  = axis_num(y.lab, ax$axT2, ax$axL2),
      yaxis  = y_axis_cat,
      shapes = c(x_grid(gridT), border_shapes),
      template = NULL
    )
  } else {
    # categories along x-axis
    x_axis_cat <- c(
      axis_cat(x.lab),
      list(
        categoryorder = "array",
        categoryarray = cat_array
      )
    )

    plt <- plotly::layout(
      plt,
      xaxis  = x_axis_cat,
      yaxis  = axis_num(y.lab, ax$axT1, ax$axL1),
      shapes = c(y_grid(gridT), border_shapes),
      template = NULL
    )
  }

  plt$x$layout$plot_bgcolor  <- .to_hex(getOption("panel_fill",  "white"))
  plt$x$layout$paper_bgcolor <- .to_hex(getOption("window_fill", "white"))

  if (!is.null(by.name) && nzchar(by.name)) {
    plt <- plotly::layout(
      plt,
      legend = list(
        title = list(text = by.name),
        bgcolor     = "rgba(255,255,255,0.9)",
        bordercolor = "#CCCCCC",
        borderwidth = 1
      )
    )
  }

  plt <- .finalize_plotly_widget(
    plt,
    kind       = "barchart",
    x.name     = x.name,
    by.name    = by.name,
    add_title  = FALSE,
    nudge_viewer = TRUE
  )

  invisible(plt)
}
