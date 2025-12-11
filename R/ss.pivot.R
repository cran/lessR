ss.pivot <- function(
  x, by = NULL,
  data,                         # REQUIRED data frame
  x.name = NULL,                # label for x (e.g., "Salary")
  by.name = NULL,               # label for by,  ignored if by is missing
  compute    = c(mean, median, sd, IQR, min, max),
  out_names  = c("Mean","Median","SD","IQR","Min","Max"),
  show_n     = TRUE,
  digits_d   = NULL,
  quiet      = getOption("quiet"),
  header     = TRUE,            # print header line
  print_result = TRUE,          # print table/header here (otherwise silent)
  debug      = FALSE
) {


  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)

  pe      <- parent.frame()
  x_expr  <- substitute(x)
  by_expr <- substitute(by)

  # helper: is a symbol that also names a column in df?
  is_name_in_df <- function(e, df) is.symbol(e) && (deparse(e) %in% names(df))

  # --- detect x label ---
  x_disp <- if (!is.null(x.name) && nzchar(x.name)) x.name else deparse(x_expr)

  # --- initial (symbolic) by-missing test (no evaluation yet) ---
  by_missing <- missing(by) ||
                is.null(by_expr) ||
                (is.symbol(by_expr) && identical(by_expr, quote(NULL)))

  # PROBE the runtime value if not obviously missing
  by_val <- NULL
  if (!by_missing) {
    by_val <- try(eval(by_expr, envir = data, enclos = pe), silent = TRUE)
    if (inherits(by_val, "try-error"))
      by_val <- try(eval(by_expr, envir = pe), silent = TRUE)
    # If it evaluates to NULL or length 0, treat as missing-by
    if (!inherits(by_val, "try-error") && (is.null(by_val) ||
        length(by_val) == 0)) {
      by_missing <- TRUE
      by_val <- NULL
    }
  }

  # by label (resolved only if grouped)
  by_disp <- if (!by_missing) {
    if (!is.null(by.name) && nzchar(by.name)) by.name else deparse(by_expr)
  } else ""

  # --- Build DF and symbols for pivot() ---
  if (is_name_in_df(x_expr, data)) {
    DF    <- data
    x_sym <- x_expr
  } else {
    x_vals <- try(eval(x_expr, envir = data, enclos = pe), silent = TRUE)
    if (inherits(x_vals, "try-error")) x_vals <- eval(x_expr, envir = pe)
    if (!nzchar(x_disp)) x_disp <- "Value"
    DF <- data.frame(x_vals, check.names = TRUE)
    names(DF) <- x_disp
    x_sym <- as.name(x_disp)
  }

  # ensure numeric aggregated variable
  x_nm <- deparse(x_sym)
  if (!is.numeric(DF[[x_nm]]))
    DF[[x_nm]] <- suppressWarnings(as.numeric(DF[[x_nm]]))
  if (!is.numeric(DF[[x_nm]]))
    stop("Aggregated variable `", x_nm,
         "` must be numeric after coercion.", call. = FALSE)

  # if grouping, ensure BY is a symbol/column in DF
  if (!by_missing) {
    if (is_name_in_df(by_expr, DF)) {
      by_sym <- by_expr
    } else {
      # use probed value (already evaluated); if not available, evaluate now
      if (is.null(by_val) || inherits(by_val, "try-error")) {
        by_val <- try(eval(by_expr, envir = data, enclos = pe), silent = TRUE)
        if (inherits(by_val, "try-error")) by_val <- eval(by_expr, envir = pe)
      }
      if (length(by_val) != nrow(DF))
        stop("`by` length (", length(by_val),
             ") must match number of rows (", nrow(DF), ").", call. = FALSE)
      if (!nzchar(by_disp)) by_disp <- "Group"
      DF[[by_disp]] <- by_val
      by_sym <- as.name(by_disp)
    }
  }

  compute_call <- substitute(compute)

  if (isTRUE(debug)) {
    cat("\n--- PIVOT SANITY CHECK ---\n")
    cat("Columns:         ", paste(names(DF), collapse = ", "), "\n")
    cat("Variable symbol: ", x_nm, "  (x.name:", x_disp, ")\n")
    if (by_missing) cat("By:              <none>\n")
    else cat("By symbol:       ", deparse(by_sym), "  (by.name:", by_disp, ")\n")
    cat("Compute:         ", paste(deparse(compute_call), collapse = " "), "\n")
    cat("--------------------------\n\n")
  }

  # --- Build pivot() call safely ---
  pv_call <- NULL
  if (by_missing) {
    pv_call <- substitute(
      lessR::pivot(
        data      = DF_,
        compute   = COMP_,
        variable  = VAR_,
        out_names = OUT_,
        show_n    = SHOW_,
        digits_d  = DIG_,
        quiet     = QUIET_
      ),
      list(
        DF_    = DF,
        COMP_  = compute_call,
        VAR_   = x_sym,
        OUT_   = out_names,
        SHOW_  = show_n,
        DIG_   = digits_d,
        QUIET_ = quiet
      )
    )
  } else {
    pv_call <- substitute(
      lessR::pivot(
        data      = DF_,
        compute   = COMP_,
        variable  = VAR_,
        by        = BY_,
        out_names = OUT_,
        show_n    = SHOW_,
        digits_d  = DIG_,
        quiet     = QUIET_
      ),
      list(
        DF_    = DF,
        COMP_  = compute_call,
        VAR_   = x_sym,
        BY_    = by_sym,
        OUT_   = out_names,
        SHOW_  = show_n,
        DIG_   = digits_d,
        QUIET_ = quiet
      )
    )
  }

  if (is.null(pv_call)) stop("Internal error: pv_call not set.", call. = FALSE)

  res <- eval(pv_call)
  # --- Header & labeling / printing control ---
  if (print_result && isTRUE(header)) {
    if (by_missing) {
      cat(x_disp, "\n\n")
    } else {
      cat(x_disp, "  by  ", by_disp, "\n\n")
    }
  }

  # For grouped case: ensure displayed first column label = by.name (if provided)
  if (!by_missing && !is.null(by.name) && nzchar(by.name) &&
      inherits(res, "data.frame")) {
    names(res)[1] <- by.name
  }

  if (print_result) {
    if (is.character(res)) {
      cat(paste(res, collapse = "\n"), "\n")
    } else if (inherits(res, "data.frame")) {
      print(res, row.names = FALSE)
    } else {
      print(res)
    }
  }

  invisible(res)
}
