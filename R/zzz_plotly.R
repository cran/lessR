# generate unique ID
.next_eid <- function(prefix = "lessR") {
  counter <- getOption("lessR.widget_counter", 0L) + 1L
  options(lessR.widget_counter = counter)

  # millisecond timestamp as character (no integer overflow)
  stamp <- format(Sys.time(), "%Y%m%d%H%M%OS3")

  sprintf("%s-%s-%d", prefix, stamp, counter)
}

# Plots and Viewer window notices ----------------------------------------

if (!exists(".lessR_deprec_env", inherits = FALSE)) {
  .lessR_deprec_env <- new.env(parent = emptyenv())
}

# One-time notice with ANSI color (auto-disables if not supported).
# Persists per R session via options(lessR.seen_notices = list(...)).
.viewer_notice_once <- function(plot_name, window_target = "Viewer") {
  # allow global silence
  if (isTRUE(getOption("lessR.silence_notices", FALSE))) return(invisible(NULL))

  # build a stable key (e.g., "notice_viewer_radar")
  key <- paste0(
    "notice_",
    tolower(gsub("\\s+", "_", window_target)),
    "_",
    tolower(gsub("\\s+", "_", plot_name))
  )

  seen <- getOption("lessR.seen_notices", default = list())
  if (isTRUE(seen[[key]])) return(invisible(NULL))

  # ANSI colors (disable automatically if console doesn't support)
  bold      <- "\033[1m"   # start bold
  unbold    <- "\033[22m"  # end bold
  darkred <- "\033[38;5;88m"   # Viewer
  darkblue <- "\033[38;5;19m"  # Plots
  purple  <- "\033[38;5;93m"   # Both
  reset   <- "\033[0m"

  msg <- switch(
    window_target,
    "Viewer" = paste0(
      " ", darkred, "Viewer", reset, " Window --> Plotly interactive visualization\n",
      "   You may need to properly align the plot by clicking the ", bold, "Refresh button", unbold, " at the\n",
      "   top-right of the Viewer window, especially for legends or labels to display.\n"
    ),
    "Plots" = paste0(
      " ", darkred, "Plots", reset, " Window --> Static visualization\n",
      "   This chart appears in the Plots pane. Use Export to save or copy.\n"
    ),
    "Both" = paste0(
      " ", purple, "Viewer + Plots", reset, " --> Interactive (Viewer) and static (Plots)\n",
      "   Tip: If alignment looks off in Viewer, click the Refresh button at the top-right.\n"
    ),
    # default fallback
    paste0(" Output --> Visualization generated in ", window_target, ".\n")
  )

  message(msg)

  # mark as seen for the remainder of the session
  seen[[key]] <- TRUE
  options(lessR.seen_notices = seen)

  invisible(NULL)
}


# color functions ---------------------------------------------------------

# --- Perceived luminance + black/white auto-contrast -----------------------
.hex_to_rgb3 <- function(h) {
  h <- gsub("#", "", toupper(h))
  if (nchar(h) %in% c(3,4)) {
    h <- paste0(substr(h,1,1), substr(h,1,1),
                substr(h,2,2), substr(h,2,2),
                substr(h,3,3), substr(h,3,3))
  }
  c(strtoi(substr(h,1,2), 16L),
    strtoi(substr(h,3,4), 16L),
    strtoi(substr(h,5,6), 16L))
}

.srgb_to_lin3 <- function(c) {
  c <- c / 255
  ifelse(c <= 0.03928, c / 12.92, ((c + 0.055) / 1.055)^2.4)
}

.contrast_text_for_hex <- function(hex) {
  # expects #RRGGBB (use .to_hex first if needed)
  rgb <- .hex_to_rgb3(hex)
  lin <- .srgb_to_lin3(rgb)
  L   <- 0.2126 * lin[1] + 0.7152 * lin[2] + 0.0722 * lin[3]
  # Compare contrast with white vs black; choose the higher
  if ((1.05) / (L + 0.05) > (L + 0.05) / 0.05) "#FFFFFF" else "#000000"
}
# --------

.resolve_alpha <- function(k, opacity) {
  if (!is.null(opacity)) return(as.numeric(opacity))
  .auto_opacity(k, "fill")
}


.to_hex <- function(col) {
  if (length(col) == 0) return(character(0))

  conv1 <- function(c1) {
    if (is.na(c1)) return(NA_character_)
    c1 <- trimws(as.character(c1))

    # --- rgb()/rgba(): convert to #RRGGBB (drop alpha) ---------------------
    m <- regexec("^rgba?\\(([^)]+)\\)$", c1, perl = TRUE)
    g <- regmatches(c1, m)[[1]]
    if (length(g)) {
      parts <- as.numeric(strsplit(g[2], "\\s*,\\s*")[[1]])
      r <- as.integer(round(parts[1]))
      g1 <- as.integer(round(parts[2])); b <- as.integer(round(parts[3]))
      r <- pmax(0, pmin(255, r))
      g1 <- pmax(0, pmin(255, g1)); b <- pmax(0, pmin(255, b))
      return(sprintf("#%02X%02X%02X", r, g1, b))
    }

    # --- #RRGGBB  ----------------------------------------------------------
    if (grepl("^#[0-9A-Fa-f]{6}$", c1)) return(c1)

    # --- #RGB -> expand -----------------------------------------------------
    if (grepl("^#[0-9A-Fa-f]{3}$", c1)) {
      r <- substr(c1, 2, 2); g1 <- substr(c1, 3, 3); b <- substr(c1, 4, 4)
      return(paste0("#", r, r, g1, g1, b, b))
    }

    # --- #RRGGBBAA: drop alpha and return #RRGGBB --------------------------
    if (grepl("^#[0-9A-Fa-f]{8}$", c1)) {
      return(paste0("#", substr(c1, 2, 7)))
    }

    # --- named colors (or other forms) -> col2rgb, return #RRGGBB ----------
    # (no alpha retention here; keep function contract = hex output)
    rgb <- grDevices::col2rgb(c1)
    sprintf("#%02X%02X%02X", rgb[1,1], rgb[2,1], rgb[3,1])
  }

  col <- ifelse(tolower(trimws(col)) == "off", "transparent", col)
  vapply(col, conv1, "", USE.NAMES = FALSE)
}


# Accepts: #RRGGBB, #RRGGBBAA, #RGB, named colors, or already rgba()
# Returns: rgba(r,g,b,a) with a in [0,1], preserving alpha if present
# shim: convert any color to Plotly rgba(), with alpha boosted for visual parity
# alpha_adjust: add e.g. +0.25 if Plotly looks too transparent
.as_plotly_rgba <- function(col, alpha_adjust = 0) {
  sapply(col, function(c1) {
    s <- trimws(c1)
    if (!nzchar(s)) return("rgba(0,0,0,0)")
    if (grepl("^#[0-9A-Fa-f]{8}$", s)) {
      r <- strtoi(substr(s, 2, 3), 16L)
      g <- strtoi(substr(s, 4, 5), 16L)
      b <- strtoi(substr(s, 6, 7), 16L)
      a <- strtoi(substr(s, 8, 9), 16L) / 255
    } else if (grepl("^#[0-9A-Fa-f]{6}$", s)) {
      r <- strtoi(substr(s, 2, 3), 16L)
      g <- strtoi(substr(s, 4, 5), 16L)
      b <- strtoi(substr(s, 6, 7), 16L)
      a <- 1
    } else {
      rgbm <- grDevices::col2rgb(s)
      r <- rgbm[1,1]; g <- rgbm[2,1]; b <- rgbm[3,1]; a <- 1
    }
    a <- max(0, min(1, a + alpha_adjust))
    sprintf("rgba(%d,%d,%d,%.3f)", r, g, b, a)
  }, USE.NAMES = FALSE)
}


# Lighten colors toward white by 'amount' in [0,1]. Vectorized; returns hex.
.lighten_plotly <- function(col, amount = 0.35) {
  if (!length(col)) return(character(0))
  amount  <- max(0, min(1, as.numeric(amount[1])))
  col_chr <- as.character(col)

  # If any entries are rgba(...), strip alpha -> rgb(...) so col2rgb can parse
  is_rgba <- grepl("^\\s*rgba\\(", col_chr, ignore.case = TRUE)
  if (any(is_rgba)) {
    col_chr[is_rgba] <- sub(
      "^\\s*rgba\\s*\\(([^,]+),([^,]+),([^,]+),.*\\)\\s*$",
      "rgb(\\1,\\2,\\3)", col_chr[is_rgba], ignore.case = TRUE
    )
  }

  # Vectorized col2rgb; fallback element-wise if needed
  M <- tryCatch(
    grDevices::col2rgb(col_chr, alpha = FALSE),
    error = function(e) {
      mm <- sapply(col_chr, function(cc)
        tryCatch(grDevices::col2rgb(cc, alpha = FALSE)[, 1],
                 error = function(e) c(NA_real_, NA_real_, NA_real_)))
      if (is.null(dim(mm))) dim(mm) <- c(3L, length(col_chr))
      mm
    }
  )
  if (is.null(dim(M))) dim(M) <- c(3L, length(col_chr))  # handle single color case

  # Replace NA channels with a neutral mid-gray to avoid rgb() failure
  M[is.na(M)] <- 204

  # Lighten toward white while PRESERVING DIMENSIONS (avoid pmin/pmax)
  M2 <- M * (1 - amount) + 255 * amount
  M2 <- round(M2)
  M2[M2 < 0]   <- 0
  M2[M2 > 255] <- 255
  dim(M2) <- dim(M)  # ensure still 3 x n

  grDevices::rgb(M2[1, ] / 255, M2[2, ] / 255, M2[3, ] / 255)
}


# Adaptive opacity tuned for overlays/fills so traces remain distinguishable.
# Linear adaptive opacity: opacity = 0.65 - 0.058 * k
# k = number of overlapping groups/traces
.auto_opacity <- function(k, mode = c("overlay","fill","lines","stack")) {
  mode <- match.arg(mode)
  k <- max(1L, as.integer(k))

  # base line (your fit)
  base <- 0.65 - 0.058 * k
  if (k == 1L) base <- 1.00  # single series should be fully opaque

  # mode-specific clamps
  rng <- switch(mode,
    overlay = c(0.28, 0.80),  # barpolar overlays
    fill    = c(0.20, 0.65),  # radar polygon fills
    lines   = c(0.40, 0.90),  # line-only traces
    stack   = c(1.00, 1.00)   # stacks don't need transparency
  )

  opacity <- max(rng[1], min(rng[2], base))
  as.numeric(opacity)
}


# Uniform or per-point transparency for FILL colors
# col   : character vector of colors (names or hex)
# alpha : numeric in [0,1]; scalar or same length as 'col'
# recycle: if TRUE, recycle 'alpha' to length(col)
.maketrans_plotly <- function(col, alpha, recycle = TRUE) {
  if (missing(alpha)) stop(".maketrans_plotly(): 'alpha' is required")
  if (!length(col))   return(character(0))

  col_chr <- as.character(col)
  if (recycle) alpha <- rep_len(alpha, length(col_chr))
  a <- pmax(0, pmin(1, as.numeric(alpha)))

  out <- character(length(col_chr))
  lc  <- trimws(tolower(col_chr))

  # transparent / missing shortcuts
  special <- is.na(col_chr) | lc %in% c("", "na", "transparent", "off")
  out[special] <- "rgba(0,0,0,0)"

  # already rgba(): replace alpha
  is_rgba <- grepl("^rgba\\s*\\(", lc)
  if (any(is_rgba & !special)) {
    idx <- which(is_rgba & !special)
    out[idx] <- sub(
      pattern = "rgba\\s*\\(([^,]+),([^,]+),([^,]+),[^\\)]+\\)\\s*$",
      replacement = function(m) {
        # keep r,g,b; swap a
        paste0("rgba(", sub("rgba\\s*\\(|\\)\\s*$", "", regmatches(col_chr[idx], regexpr("rgba\\s*\\([^\\)]*\\)", col_chr[idx]))), ")")
      },
      x = col_chr[idx]
    )
    # Now apply the new alpha cleanly
    # Parse r,g,b fast:
    rgbm <- grDevices::col2rgb(sub("^rgba\\s*\\(", "rgb(", col_chr[idx]))
    out[idx] <- sprintf("rgba(%d,%d,%d,%.3f)", rgbm[1,], rgbm[2,], rgbm[3,], a[idx])
  }

  # normal colors (hex/named)
  norm <- !special & !is_rgba
  if (any(norm)) {
    idx <- which(norm)
    rgbm <- grDevices::col2rgb(col_chr[idx])
    out[idx] <- sprintf("rgba(%d,%d,%d,%.3f)", rgbm[1, ], rgbm[2, ], rgbm[3, ], a[idx])
  }
  out
}


# Opaque border color for Plotly markers/lines (forces a = 1)
.to_plotly_color <- function(col) {
  if (!length(col)) return(character(0))
  col_chr <- as.character(col)
  special <- is.na(col_chr) |
             trimws(tolower(col_chr)) %in% c("", "na", "transparent", "off")
  out <- character(length(col_chr))
  out[special] <- "rgba(0,0,0,0)"  # invisible border if asked to be transparent

  if (any(!special)) {
    idx  <- which(!special)
    rgbm <- grDevices::col2rgb(col_chr[idx])
    out[idx] <- sprintf("rgba(%d,%d,%d,1.000)", rgbm[1, ], rgbm[2, ], rgbm[3, ])
  }
  out
}


# Choose/recycle by names (used everywhere)
.pick_by_names_or_recycle <- function(v, needed_names, needed_len, fallback=NULL) {
  v <- as.character(v); nm <- names(v)
  if (!is.null(nm) && length(nm)) {
    out <- v[match(needed_names, nm)]
    if (anyNA(out)) {
      base <- if (length(v)) v else (fallback %||% grDevices::hcl.colors(needed_len, "Dark 3"))
      out[is.na(out)] <- base[((which(is.na(out)) - 1L) %% length(base)) + 1L]
    }
    return(out)
  }
  if (!length(v)) {
    return(if (!is.null(fallback)) rep_len(fallback, needed_len)
           else grDevices::hcl.colors(needed_len, "Dark 3"))
  }
  rep_len(v, needed_len)
}


# ---- hover format -------------------------------------------------------

.get.tick.fmt <- function(v, digits_d = 2) {
  if (is.null(digits_d)) digits_d <- 2
  if (.is.integer(v)) {
    ""                        # integers: no format
  } else {
    paste0(".", digits_d, "f")  # floats: d3-format string
  }
}

.hover.fmt <- function(x, y, x.lab, y.lab, horiz=FALSE,
                       digits_d=2, ts_unit=NULL) {

  fmtx <- .get.tick.fmt(x, digits_d)
  fmty <- .get.tick.fmt(y, digits_d)

  addfmt <- function(var, fmt) {
    if (nzchar(fmt)) paste0("%{", var, ":", fmt, "}") else paste0("%{", var, "}")
  }

  if (!horiz) {
    # vertical scatter: first line shows X name + value
    paste0(
      x.lab, ": ", if(is.null(ts_unit)) addfmt("x", fmtx) else x,
      "<br>", y.lab, ": ", addfmt("y", fmty),
      "<extra></extra>"
    )
  }
  else {
    # horizontal scatter: first line shows Y name + value
    paste0(
      y.lab, ": ", addfmt("y", fmty),
      "<br>", x.lab, ": ", addfmt("x", fmtx),
      "<extra></extra>"
    )
  }
}


.grid_style <- function() {
  list(
    color = .to_hex(getOption("grid_color", "gray85")),  # convert here
    width = getOption("grid_lwd", 0.5),
    dash  = getOption("grid_lty", NULL)
  )
}

x_grid <- function(x_vals, xref="x", yref="paper", y0=0, y1=1,
                   color=NULL, width=NULL, dash=NULL, layer="below") {
  if (is.null(x_vals) || !length(x_vals)) return(list())
  st  <- .grid_style()
  col <- if (is.null(color)) st$color else color
  wid <- if (is.null(width)) st$width else width
  dsh <- if (is.null(dash))  st$dash  else dash

  # ensure hex for Plotly
  col <- .to_hex(col)

  lapply(x_vals, function(x0) {
    list(
      type  = "line",
      xref  = xref, yref = yref,
      x0    = x0, x1 = x0,
      y0    = y0, y1 = y1,
      line  = c(list(color = col, width = wid),
                if (!is.null(dsh)) list(dash = dsh) else list()),
      layer = layer
    )
  })
}


y_grid <- function(y_vals, xref="paper", yref="y", x0=0, x1=1,
                   color=NULL, width=NULL, dash=NULL, layer="below") {
  if (is.null(y_vals) || !length(y_vals)) return(list())
  st  <- .grid_style()
  col <- if (is.null(color)) st$color else color
  wid <- if (is.null(width)) st$width else width
  dsh <- if (is.null(dash))  st$dash  else dash

  col <- .to_hex(col)

  lapply(y_vals, function(y0) {
    list(
      type  = "line",
      xref  = xref, yref = yref,
      y0    = y0, y1 = y0,
      x0    = x0, x1 = x1,
      line  = c(list(color = col, width = wid),
                if (!is.null(dsh)) list(dash = dsh) else list()),
      layer = layer
    )
  })
}


axis_base <- function() {
        
# ax <- list(
  list(
    zeroline  = FALSE, showline = TRUE,
    linecolor = .to_hex(getOption("axis_color", "black")),
    linewidth = getOption("axis_lwd", 1),
    ticks     = "outside", ticklen = 4,
    automargin = TRUE,
    tickfont = list(
      color  = .to_hex(getOption("axis_color", "black")),
      size   = 15 * getOption("axis_cex", 0.8)
    ),
    title = list(
      font = list(
        color = .to_hex(getOption("lab_color", "black")),
        size  = 15 * getOption("lab_cex", 1)
      )
    ),
#   showgrid  = getOption("axis_showgrid", TRUE),
    gridcolor = .to_hex(getOption("grid_color", "gray90")),
    gridwidth = getOption("grid_lwd", 1)
  )
}


axis_num <- function(title_txt, tickvals, ticktext) {
  utils::modifyList(axis_base(), list(
    title      = list(text = title_txt),
    tickmode   = "array",
    tickvals   = tickvals,
    ticktext   = ticktext,
    showgrid = FALSE
  ))
}


# categorical axis: grid off
axis_cat <- function(title_txt) {
  utils::modifyList(
    axis_base(),
    list(title = list(text=title_txt), showgrid=FALSE)
  )
}


# Draw top/right frame lines inside the plotting area
plot_border <- function(top = TRUE, right = TRUE,
                        color =getOption("panel_color"),
                        width = getOption("panel_lwd")) {

  if (is.character(color) && grepl("^gray[0-9]+$", color)) 
    color <- "gray75"  # make a gray a little lighter for plotly 

  out <- list()
  if (isTRUE(top)) {
    out[[length(out)+1]] <- list(
      type  = "line",
      xref  = "paper", yref = "paper",
      x0 = 0, x1 = 1, y0 = 1, y1 = 1,
      line = list(color = .to_hex(color), width = width)
    )
  }
  if (isTRUE(right)) {
    out[[length(out)+1]] <- list(
      type  = "line",
      xref  = "paper", yref = "paper",
      x0 = 1, x1 = 1, y0 = 0, y1 = 1,
      line = list(color = .to_hex(color), width = width)
    )
  }
  out
}


legend_base <- function() {
  fam <- getOption("legend_family", NULL)
  leg <- list(
    font = list(
      color = .to_hex(getOption("legend_color", getOption("lab_color", "black"))),
      size  = 15 * getOption("legend_cex", getOption("lab_cex", 1))
    )
  )
  if (!is.null(fam)) leg$font$family <- fam
  leg
}


.nudge_viewer <- function(w) {
  # Coerce plotly -> htmlwidget if needed
  if (!inherits(w, "htmlwidget")) {
    if (inherits(w, "plotly")) {
      w <- plotly::plotly_build(w)
    } else {
      return(w)  # not a widget; don’t try to onRender
    }
  }

  htmlwidgets::onRender(
    w,
    "
    function(el, x){
      requestAnimationFrame(function(){
        if(window.Plotly) Plotly.Plots.resize(el); });
      setTimeout(function(){ if(window.Plotly) Plotly.Plots.resize(el); }, 150);
      setTimeout(function(){ if(window.Plotly) Plotly.Plots.resize(el); }, 400);
    }
    "
  )
}


# --- Optional: legend typography across all plotly charts -------------------
.apply_common_legend <- function(p, title_text = NULL) {
  p <- plotly::layout(
    p,
    legend = list(
      title = list(text = title_text %||% "", font = list(size = 13)),
      font  = list(size = 12),
      itemsizing = "constant"
    )
  )
  p
}

.legend_style <- function(by.name = NULL) {
  list(
    title = list(
      text = by.name,
      font = list(size = 14, family = "Arial", color = "black")
    ),
    font = list(size = 13, family = "Arial", color = "black"),
    orientation = "v",
    x = 1.02, y = 0.5,
    xanchor = "left", yanchor = "middle",
    bgcolor = "rgba(255,255,255,0.6)",
    bordercolor = "#CCCCCC",
    borderwidth = 1,
    tracegroupgap = 6
  )
}


# Legend style: same look everywhere
.legend_box <- function(by.name=NULL) {
  if (!is.null(by.name)) {
    list(
      title = list(text = by.name, font = list(size = 12, family = "Arial", color = "black")),
      orientation = "v",
      x = 1.05, y = 0.5, xanchor = "left", yanchor = "middle",
      bgcolor = "rgba(255,255,255,0.9)",
      bordercolor = "#CCCCCC",
      borderwidth = 1
    )
  } else {
    list(orientation = "v", bgcolor = "rgba(255,255,255,0.0)")
  }
}




# Should an interactive plotly chart auto-display?
# User can override with options(lessR.use_plotly = FALSE)
.allow.interactive <- function() {
  opt <- getOption("lessR_interactive", TRUE)
  isTRUE(opt) && base::interactive()
}



# nudge viewer to address RStudio bug of mis-alignment
force_viewer_reload <- function(plt, delays = c(16, 90, 250, 600, 1200)) {
  # Only "kick" Plotly when the element actually has a size.
  # Also re-kick when the Viewer or its parents change size/visibility.
  js <- sprintf(
    "(function(){
      return function(el){
        var kicks = [%s];
        var lastW = -1, lastH = -1;

        function measurable(){
          if (!el) return false;
          // skip until attached to layout tree
          if (!(el.offsetParent || el.getClientRects().length)) return false;
          var r = el.getBoundingClientRect();
          return (r.width > 0 && r.height > 0);
        }

        function doKick(){
          try {
            if (!measurable()) return false;
            var r = el.getBoundingClientRect();
            if (window.Plotly && el && el.data) {
              try { Plotly.relayout(el, {autosize: true}); } catch(e){}
              try { Plotly.Plots.resize(el); } catch(e){}
            } else {
              try { window.dispatchEvent(new Event('resize')); } catch(e){}
            }
            lastW = r.width; lastH = r.height;
            return true;
          } catch(e) { return false; }
        }

        // A guarded kick that retries until measurable
        function guardedKick(deadlineMs){
          var t0 = Date.now();
          (function loop(){
            if (doKick()) return;
            if (deadlineMs && (Date.now() - t0) > deadlineMs) return;
            setTimeout(loop, 32);
          })();
        }

        // Early kicks (animation frames) for first paint
        try { requestAnimationFrame(function(){ guardedKick(250); }); } catch(e){}
        try { requestAnimationFrame(function(){ requestAnimationFrame(function(){ guardedKick(250); }); }); } catch(e){}

        // Staggered delayed kicks
        for (var i=0; i<kicks.length; i++) {
          setTimeout(function(){ guardedKick(0); }, kicks[i]);
        }

        // Re-kick when the widget or its container resizes
        if (typeof ResizeObserver !== 'undefined') {
          try {
            var ro = new ResizeObserver(function(){
              var r = el.getBoundingClientRect();
              if (r.width !== lastW || r.height !== lastH) {
                doKick();
              }
            });
            ro.observe(el);
            if (el.parentElement) ro.observe(el.parentElement);
          } catch(e){}
        }

        // Also re-kick on DOM mutations that often accompany Viewer layout shifts
        try {
          var mo = new MutationObserver(function(){ guardedKick(0); });
          mo.observe(document.documentElement, {attributes:true, childList:true, subtree:true});
        } catch(e){}

        // When the window/tab becomes visible/focused
        window.addEventListener('focus', function(){ guardedKick(250); });
        document.addEventListener('visibilitychange', function(){
          if (!document.hidden) guardedKick(250);
        });

        // One last kick after full load
        if (document.readyState !== 'complete') {
          window.addEventListenee('load', function(){ guardedKick(250); }, {once:true});
        }
      }
    })();",
    paste(as.integer(delays), collapse = ",")
  )
  htmlwidgets::onRender(plt, js)
}


# -------------------------------------------------------------------
# Finalize a plotly widget (or payload) for lessR
#   - Adds optional title
#   - Pins plotly.js dependency if available
#   - Makes widget responsive in RStudio Viewer
#   - Optionally nudges the Viewer for correct sizing
#   - NOW: robust to vector x.name/by.name (collapses them neatly)
# -------------------------------------------------------------------
.finalize_plotly_widget <- function(
  plt,
  kind,
  x.name = NULL,
  by.name = NULL,
  add_title = TRUE,
  nudge_viewer = TRUE,
  unique_element = TRUE   # if TRUE, force a unique elementId to avoid stale mounts
) {

  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

  # collapse name vectors (safe for scalars, NULL)
  .collapse_names <- function(nm, sep = " \u2192 ") {  # default arrow
    if (is.null(nm) || !length(nm)) return(NULL)
    nm_chr <- trimws(as.character(nm))
    nm_chr <- nm_chr[nchar(nm_chr) > 0]
    if (!length(nm_chr)) return("")
    if (length(nm_chr) == 1L) return(nm_chr)
    paste(nm_chr, collapse = sep)
  }

  # choose separators for titles/meta
  x.name_coll  <- .collapse_names(x.name,  sep = " + ")
  by.name_coll <- .collapse_names(by.name, sep = " \u2192 ")

  # ---- title builder --------------------------------------------------------
  .mk_title <- function(xn, byn) {
    xn  <- trimws(as.character(xn %||% ""))
    byn <- trimws(as.character(byn %||% ""))
    if (nzchar(xn) && nzchar(byn)) paste0(xn, " by ", byn)
    else if (nzchar(xn)) xn
    else if (nzchar(byn)) byn
    else ""
  }

  # ---- attach meta used by savePlotly() etc. --------------------------------
  tag_meta <- function(obj) {
    attr(obj, "lessR_kind")   <- as.character(kind %||% "plotly")
    attr(obj, "lessR_xname")  <- as.character(x.name_coll %||% "")
    attr(obj, "lessR_byname") <- as.character(by.name_coll %||% "")
    attr(obj, "lessR_title")  <- .mk_title(x.name_coll, by.name_coll)
    obj
  }

  # helper: millisecond timestamp as character (no integer overflow)
  .ms_chr <- function() format(Sys.time(), "%Y%m%d%H%M%OS3")

  # helper: per-session sequence counter via options()
  .next_seq <- function() {
    s <- getOption("lessR.seq", 0L)
    s <- s + 1L
    options(lessR.seq = s)
    s
  }

  # ---- normalize plotly -> htmlwidget if needed -----------------------------
  if (inherits(plt, "plotly") && !inherits(plt, "htmlwidget")) {
    plt <- plotly::plotly_build(plt)
  }

  # ---- Case A: raw payload list with $data/$layout --------------------------
  if (is.list(plt) && is.null(plt$x) && !is.null(plt$data)
                   && !is.null(plt$layout)) {
    if (isTRUE(add_title)) {
      ttl <- .mk_title(x.name_coll, by.name_coll)
      if (nzchar(ttl)) {
        if (is.null(plt$layout)) plt$layout <- list()
        plt$layout$title <- list(text = ttl)
      }
    }
    plt <- tag_meta(plt)
    options(lessR.last_plotly = plt)
    return(invisible(plt))
  }

  # ---- Case B: htmlwidget (standard) ----------------------------------------
  if (inherits(plt, "htmlwidget")) {

    # (1) Optional title injection
    if (isTRUE(add_title)) {
      ttl <- .mk_title(x.name_coll, by.name_coll)
      if (nzchar(ttl)) {
        plt$x$layout <- plt$x$layout %||% list()
        plt$x$layout$title <- list(text = ttl)
      }
    }

    # (2) Meta for downstream helpers
    plt <- tag_meta(plt)

    # (3) Optionally force a unique elementId to avoid stale mount issues in Viewer
    if (isTRUE(unique_element)) {
      stamp <- .ms_chr()      # "YYYYmmddHHMMSSmmm" as character
      pid   <- Sys.getpid()
      seqn  <- .next_seq()
      plt$elementId <- sprintf("lessr-%s-%d-%d", stamp, pid, seqn)
    }

    # (4) (optional) pin plotly.js — left disabled/commented by default
    # if (exists(".pin_plotly_js", mode = "function")) {
    #   plt <- .pin_plotly_js(plt)
    # } else if (requireNamespace("htmltools", quietly = TRUE)) {
    #   js_path <- system.file("htmlwidgets/lib/plotlyjs/plotly-2.34.0.min.js",
    #                          package = "lessR")
    #   if (nzchar(js_path)) {
    #     dep <- htmltools::htmlDependency(
    #       name    = "plotly-main",
    #       version = "2.34.0",
    #       src     = c(file = dirname(js_path)),
    #       script  = basename(js_path)
    #     )
    #     plt <- htmltools::attachDependencies(plt, dep, append = FALSE)
    #   }
    # }

    # (5) Register last widget for savePlotly()
    options(lessR.last_plotly = plt)

    # (6) Viewer robustness: autosize + responsive + margins + fill container
    plt$x$layout <- plt$x$layout %||% list()
    plt$x$layout$autosize <- TRUE
    plt$x$layout$margin <- utils::modifyList(
      list(t = 40, r = 20, b = 40, l = 60, autoexpand = TRUE),
      plt$x$layout$margin %||% list(),
      keep.null = TRUE
    )
    plt$x$config <- utils::modifyList(plt$x$config %||% list(),
                                      list(responsive = TRUE),
                                      keep.null = TRUE)

    # ensure the widget fills the Viewer pane (avoid fixed pixels)
    plt$sizingPolicy <- htmlwidgets::sizingPolicy(
      viewer.fill   = TRUE,
      browser.fill  = TRUE,
      padding       = 0,
      defaultWidth  = NULL,
      defaultHeight = NULL
    )

    # (7) Gentle but smarter Viewer nudge
    if (isTRUE(nudge_viewer)) {
      plt <- force_viewer_reload(plt, delays = c(16, 90, 250, 600, 1200))
    }

    return(invisible(plt))
  }
}


.force_full_plotly_js <- function(widget, version = "2.34.0") {
  # Path to vendored full Plotly.js inside *this* package
  js_dir <- system.file("htmlwidgets", "lib", "plotlyjs", package = "lessR")
  file_ok <- nzchar(js_dir) &&
            file.exists(file.path(js_dir, sprintf("plotly-%s.min.js", version)))
  if (!file_ok) {
    warning(
      "Full Plotly.js not found at inst/htmlwidgets/lib/plotlyjs/. ",
      "Sunburst may not render in RStudio Viewer. ",
      "Expected file: plotly-", version, ".min.js"
    )
    return(widget)
  }

  dep <- htmltools::htmlDependency(
    name    = "plotlyjs-full",
    version = version,
    src     = c(file = js_dir),
    script  = sprintf("plotly-%s.min.js", version)
  )

  # PREPEND our full Plotly dep so it overrides any partial one
  deps_existing <- htmltools::htmlDependencies(widget)
  deps_new <- c(list(dep), deps_existing)

  htmltools::attachDependencies(widget, deps_new, append = FALSE)
}



# ==== Common Plotly helpers for lessR facet charts =========================

# ----- small utility -----------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# ----- qualitative palette (lessR sequence) ------------------------------
.plotly_base_colors <- function() {
  c(
    "#4398D0","#B28B2A","#5FA140","#D57388",
    "#9A84D6","#00A898","#C97E5B","#909711",
    "#00A3BA","#D26FAF","#00A76F","#BD76CB"
  )
}

# make rgba fills from hex vector (stable indices across facets)
.plotly_make_fills <- function(cols, alpha = 0.30) {
  to_rgba <- function(hex) {
    rgb <- grDevices::col2rgb(hex)
    sprintf("rgba(%d,%d,%d,%.3f)", rgb[1], rgb[2], rgb[3], alpha)
  }
  vapply(cols, to_rgba, character(1))
}

# ----- grid of facet domains (equal sizes) -------------------------------
# returns list(domains=..., n_row=..., n_col=...)
.plotly_make_domains_grid <- function(n_fac, n_col_max = 3L,
                                      gap_x = 0.04, gap_y = 0.11,
                                      facet_size = 1.0) {
  n_col <- min(n_col_max, n_fac)
  n_row <- ceiling(n_fac / n_col)

  width  <- (1 - (n_col + 1) * gap_x) / n_col
  height <- (1 - (n_row + 1) * gap_y) / n_row

  domains <- vector("list", n_fac)
  k <- 1L
  for (r in seq_len(n_row)) {
    for (c in seq_len(n_col)) {
      if (k > n_fac) break

      # base cell
      x0 <- gap_x + (c - 1) * (width + gap_x)
      x1 <- x0 + width
      y1 <- 1 - (r - 1) * (height + gap_y)
      y0 <- y1 - height

      # --- center-preserving scaling (keeps charts from drifting up) ---
      cx <- (x0 + x1) / 2
      cy <- (y0 + y1) / 2
      half_w <- (x1 - x0) * facet_size / 2
      half_h <- (y1 - y0) * facet_size / 2

      x0s <- max(0, cx - half_w); x1s <- min(1, cx + half_w)
      y0s <- max(0, cy - half_h); y1s <- min(1, cy + half_h)

      domains[[k]] <- list(x = c(x0s, x1s), y = c(y0s, y1s), row = r, col = c)
      k <- k + 1L
    }
  }
  list(domains = domains, n_row = n_row, n_col = n_col)
}

# ----- angular ticks from factor levels ----------------------------------
.plotly_theta_from_levels <- function(x_levels) {
  K <- length(x_levels)
  if (!is.finite(K) || K < 1L) stop("x_levels must have length >= 1")
  seq(0, 360 - 360 / K, length.out = K)
}

# ----- apply per-facet polar layout blocks -------------------------------
.plotly_apply_polar_layout <- function(p, domains, theta_deg, x_labels, rmax) {
  for (i in seq_along(domains)) {
    nm <- if (i == 1L) "polar" else paste0("polar", i)
    blk <- list(
      domain     = list(x = domains[[i]]$x, y = domains[[i]]$y),
      radialaxis = list(visible = TRUE, showline = TRUE, range = c(0, rmax * 1.08)),
      angularaxis = list(
        type = "linear", direction = "clockwise",
        rotation = 10, tickmode = "array",
        tickvals = theta_deg, ticktext = as.list(x_labels)
      )
    )
    # dynamic-named layout arg: polar, polar2, polar3, ...
    p <- do.call(plotly::layout, c(list(p), setNames(list(blk), nm)))
  }
  p
}


# ----- facet title annotations -------------------------------------------
.plotly_facet_annotations <- function(domains, facet_levels, facet_name,
                                      y_base = 0.054,
                                      y_row_shift = -0.002,
                                      font_size = 14) {

  ann <- vector("list", length(domains))
  for (i in seq_along(domains)) {
    d <- domains[[i]]
    y_title <- d$y[2] + y_base + y_row_shift * (d$row - 1L)
    ann[[i]] <- list(
      text = paste0(facet_name, ": ", facet_levels[i]),
      x = mean(d$x), y = y_title,
      xref = "paper", yref = "paper",
      xanchor = "center", yanchor = "top",
      showarrow = FALSE, font = list(size = font_size)
    )
  }
  ann
}


# ----- title builder (consistent across chart types) ---------------------
.plotly_build_title <- function(x.name,
                               by.name   = NULL,
                               facet.name= NULL,
                               y.name    = NULL,
                               stat      = NULL) {
  cap_stat <- function(s) switch(tolower(s),
                                 mean="Mean", sum="Sum", median="Median",
                                 min="Min",  max="Max",  sd="SD",
                                 toupper(s))

  # helper: does y.name already start with a stat word?
  y_has_stat <- function(y) {
    if (is.null(y)) return(FALSE)
    grepl("^(Mean|Sum|Median|Min|Max|SD)\\b", y)
  }

  # --- COUNT titles (no y or no stat) ---
  if (is.null(y.name) || is.null(stat)) {
    ttl <- paste("Count of", x.name)
  }
  else {
    S <- cap_stat(stat)
    if (y_has_stat(y.name)) {
      # y.name already includes the stat (e.g., "Mean of Salary")
      ttl <- paste(y.name, "by", x.name)
    }
     else
      ttl <- paste(S, "of", y.name, "by", x.name)
  }

  if (!is.null(by.name) && nzchar(by.name)) ttl <- paste(ttl, "by", by.name)
  if (!is.null(facet.name) && nzchar(facet.name))
     ttl <- paste(ttl, "across", facet.name)
  ttl
}




# display or summarize an already-built table, x.tbl
# output: formatted to the R console
`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a

.build_xtab <- function(x, y = NULL, by = NULL, facet = NULL,
                        stat = NULL, is.agg = FALSE, digits_d = 2) {
  # construct summary table x.tbl from x, optional by / facet and optional y

  ## helper: coerce a generic grouping object (vector, matrix, data.frame, list)
  ## into a data.frame with at least one column, or NULL
  to_group_df <- function(obj, prefix = "g") {
    if (is.null(obj)) return(NULL)

    if (is.data.frame(obj))
      df <- obj
    else if (is.matrix(obj))
      df <- as.data.frame(obj)
    else if (is.list(obj) && !is.atomic(obj)) # list of equal-length vectors
      df <- as.data.frame(obj)
    else   # single vector
      df <- data.frame(obj)

    if (is.null(colnames(df)))
      colnames(df) <- paste0(prefix, seq_len(ncol(df)))

    df
  }  # end to_group_df()

  ## ------------------------------------------------------------
  ## 1) RAW DATA: build table from x / y / groupings
  ## ------------------------------------------------------------
  if (!is.agg) {

    by_df    <- to_group_df(by,    prefix = "by")
    facet_df <- to_group_df(facet, prefix = "facet")

    # combine all grouping columns (by + facet); may be NULL
    if (is.null(by_df) && is.null(facet_df))
      group_df <- NULL
    else if (is.null(by_df))
      group_df <- facet_df
    else if (is.null(facet_df))
      group_df <- by_df
    else
      group_df <- cbind(by_df, facet_df)

    if (is.null(y)) {  # counts
      if (is.null(group_df)) {
        # simple one-way table of x
        x.tbl <- xtabs(~ x, drop.unused.levels = FALSE)
      }
      else {
        # multi-way table over all grouping columns + x
        df <- data.frame(group_df, x = x)
        form <- reformulate(c(names(group_df), "x"))
        x.tbl <- xtabs(form, data = df, drop.unused.levels = FALSE)
      }

    }
    else {  # numeric summary
      if (is.null(group_df))  # numeric y summarized by x only
        x.tbl <- xtabs(y ~ x, drop.unused.levels = FALSE)
      else {
        df <- data.frame(group_df, x = x, y = y)
        # y ~ g1 + g2 + ... + x
        form <- reformulate(c(names(group_df), "x"), response = "y")
        x.tbl <- xtabs(form, data = df, drop.unused.levels = FALSE)
      }
    }
  }  # end !is.agg

  else {
    ## ------------------------------------------------------------
    ## 2) ALREADY AGGREGATED DATA: treat x as table / vector
    ## ------------------------------------------------------------
    if (is.null(y)) {
      # x is (or should be) already in table-like form
      if (is.table(x))
        x.tbl <- x
      else
        x.tbl <- as.table(x)
    }
    else {
      # aggregated y supplied explicitly; restore to table form
      # here we assume caller has already shaped x/y/by/facet as needed
      by_df    <- to_group_df(by,    prefix = "by")
      facet_df <- to_group_df(facet, prefix = "facet")

      if (is.null(by_df) && is.null(facet_df)) {
        x.tbl <- xtabs(y ~ x, drop.unused.levels = FALSE)
      }
      else {
        if (is.null(by_df) && !is.null(facet_df)) {
          group_df <- facet_df
        }
        else if (!is.null(by_df) && is.null(facet_df)) {
          group_df <- by_df
        }
        else
          group_df <- cbind(by_df, facet_df)
        df <- data.frame(group_df, x = x, y = y)
        form <- reformulate(c(names(group_df), "x"), response = "y")
        x.tbl <- xtabs(form, data = df, drop.unused.levels = FALSE)
      }
    }
  }

  x.tbl
}


# display input table at the console --------------------------------------

.print_table <- function(x.tbl, x.name, x.lbl=NULL,
                         by.name = NULL, y.name = NULL,
                         stat = NULL, digits_d = 2) {

  # helper: is this table "count-like"?
  is.count <- (is.integer(x.tbl) && all(x.tbl >= 0))

  nd <- length(dim(x.tbl))

  if (is.count) {  # x.tbl is count-like values
    # --- 1D / 2D paths still get the .ss.factor treatment ----------------
    if (is.null(dim(x.tbl))) {
      # 1D: named vector of counts -> coerce to table with dimname "x"
      dn <- list(names(x.tbl))
      x.tbl <- as.table(array(unname(x.tbl),
                              dim       = length(x.tbl),
                              dimnames  = dn))
      names(dimnames(x.tbl)) <- "x"
      nd <- 1L
    }
    else {
      # Already has dims: if first dim has no name, call it "x"
      dn_names <- names(dimnames(x.tbl))
      if (is.null(dn_names) || !nzchar(dn_names[1L])) {
        dn_names <- if (is.null(dn_names)) character(nd) else dn_names
        dn_names[1L] <- "x"
        names(dimnames(x.tbl)) <- dn_names
      }
    }

    if (nd <= 2L) {
      # Use existing .ss.factor summaries for 1D or 2D counts
      stats <- .ss.factor(
        x.tbl,
        by    = NULL,
        brief = TRUE,
        digits_d = digits_d,
        x.name, by.name, x.lbl=NULL, y.lbl = NULL
      )

      ## >>> changed logic here: key off nd, not by.name <<<
      if (nd == 1L) {
        # 1D counts
        title <- stats$title
        freq  <- stats$counts
        test  <- stats$chi
      } else {  # nd == 2L
        # 2D counts (including facet-only case)
        title <- stats$txttl
        freq  <- stats$txfrq
        test  <- stats$txXV
      }
      ## <<< end change <<<

      class(title) <- "out"
      class(freq)  <- "out"
      class(test)  <- "out"

      output <- list(
        out_title  = title,
        out_counts = freq,
        out_chi    = test
      )
      class(output) <- "out_all"
      print(output)
    }
    else {
      # 3+ dimensional count table: just print the multiway table
      cat("\nMulti-way count table:\n\n")
      print(x.tbl)
    }

  } else {
    # --------------------------------------------------------------------
    # NOT counts: display numeric summary table
    # --------------------------------------------------------------------
    has_by <- !is.null(by.name)

    # For 1D/2D numeric tables, preserve old printing behavior
    if (nd <= 2L) {
      if (has_by) {
        # Ensure dimnames have (by.name, x.name)
        dimnames(x.tbl) <- setNames(dimnames(x.tbl), c(by.name, x.name))
      }

      x.t <- x.tbl
      attributes(x.t) <-
        attributes(x.tbl)[c("dim", "dimnames", "names", "row.names", "class")]
      if (!is.null(names(dimnames(x.t)))) names(dimnames(x.t)) <- NULL

      print(x.t)  # a table
    }
    else {
      # 3+ dimensional numeric table: print as-is
      cat("\nMulti-way numeric summary table:\n\n")
      print(x.tbl)
    }
  }

  cat("\n")  # space after all the text output
}
