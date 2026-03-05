savePlotly <- function(obj = NULL, file = NULL, open = TRUE,
                       selfcontained = TRUE, libdir = NULL) {
  if (is.null(obj)) obj <- getOption("lessR.last_plotly", NULL)
  if (is.null(obj))
    stop("No recent Plotly chart found. Run a chart first or pass an object.")

  if (!inherits(obj, "htmlwidget")) {
    if (inherits(obj, "plotly")) obj <- plotly::plotly_build(obj)
    else stop("The object is not a Plotly/htmlwidget chart.")
  }

  kind  <- attr(obj, "lessR_kind")
  if (is.null(kind)  || !nzchar(kind))  kind  <- "plotly"
  xname <- attr(obj, "lessR_xname");  if (is.null(xname)) xname <- ""
  bynm  <- attr(obj, "lessR_byname"); if (is.null(bynm))  bynm  <- ""

  # Fallback: parse title like "X by Y"
  if (!nzchar(xname) || is.na(xname) || !nzchar(bynm) || is.na(bynm)) {
    ttl <- attr(obj, "lessR_title")
    if (is.null(ttl) && !is.null(obj$x$layout$title)) {
      raw <- obj$x$layout$title
      ttl <- if (is.list(raw) && !is.null(raw$text))
               raw$text else as.character(raw)
    }
    ttl_plain <- trimws(gsub("<[^>]+>", "", `%||%`(ttl, ""), perl = TRUE))
    parts <- strsplit(ttl_plain, "\\s+by\\s+", perl = TRUE)[[1]]
    if (length(parts) == 2L) {
      if (!nzchar(xname)) xname <- parts[1]
      if (!nzchar(bynm))  bynm  <- parts[2]
    } else if (!nzchar(xname)) {
      xname <- ttl_plain
    }
  }

  # Robust cleanup: strip any trailing "chart", then sanitize
  normalize_name <- function(s) {
    s <- trimws(s)
    s <- sub("\\s+chart\\s*$", "", s, ignore.case = TRUE)  # "Years chart" -> "Years"
    s <- sub("chart\\s*$",      "", s, ignore.case = TRUE)  # "Yearschart"  -> "Years"
    s <- gsub("[^A-Za-z0-9]+", "", s)                       # collapse to safe tokens
    s <- sub("chart$", "", s, ignore.case = TRUE)           # final safety pass
    if (!nzchar(s)) "" else s
  }

  kind_s  <- gsub("[^A-Za-z0-9]+", "", kind)
  xname_s <- normalize_name(xname)
  bynm_s  <- normalize_name(bynm)

  tag <- if (nzchar(bynm_s)) paste0(xname_s, "_", bynm_s) else xname_s

  # Build parts while avoiding duplicate "plotly"
  parts <- c("plotly", if (nzchar(kind_s) && tolower(kind_s) != "plotly") kind_s, tag)
  parts <- parts[nzchar(parts)]

  base <- paste(parts, collapse = "_")
  if (!nzchar(base)) base <- "plotly_chart"  # final fallback

  if (is.null(file) || !nzchar(file)) {
    file <- file.path(getwd(), paste0(base, ".html"))
  }

  htmlwidgets::saveWidget(obj, file, selfcontained = selfcontained,
                          libdir = libdir, background = "white")

  abs <- tryCatch(normalizePath(file, winslash = "/", mustWork = TRUE),
                  error = function(e) file)
  cat("Plotly chart saved to:\n  ", abs, "\n", sep = "")
  if (isTRUE(open)) utils::browseURL(abs)
  invisible(abs)
}
