# aliases.R — legacy entry points (soft-deprecated) ---------------------------
# - Keep only historically existing public aliases.
# - Forward to Chart(), X(), or XY() exactly as today.
# - Emit a once-per-session deprecation message that suggests the new call.
# - No hard deprecations; everything still works.


# one-time deprecation message helper -------------------------------------
.lessR_deprec_env <- new.env(parent = emptyenv())

## cache for "seen" flags (once-per-session)
if (!exists(".lessR_deprec_env", inherits = FALSE)) {
  .lessR_deprec_env <- new.env(parent = emptyenv())
}

.deprecate_once <- function(old_name, new_suggest) {
  # Allow users to silence these messages globally
  if (isTRUE(getOption("lessR.silence_deprec", FALSE))) return(invisible(NULL))

  key <- paste0("seen_", old_name)  # identify the string that triggered message

  if (!isTRUE(.lessR_deprec_env[[key]])) {
    msg <- sprintf(
      paste0(
        "lessR visualizations are now unified over just three core functions:\n",
        "  - Chart() for pivot tables, such as bar charts. More info: ?Chart\n",
        "  - X() for a single variable x, such as histograms. More info: ?X\n",
        "  - XY() for scatterplots of two variables, x and y. More info: ?XY\n",
        "\n",
        "%s is deprecated, though still working for now.\n",
        "Please use %s going forward."
      ),
      old_name, new_suggest
    )
    message(msg)
    .lessR_deprec_env[[key]] <- TRUE
  }

  invisible(NULL)
}


# Chart() aliases (legacy)-------------------------------------------------

BarChart <- function(...) {
  .deprecate_once("BarChart()", 'Chart(..., type = "bar")')
  Chart(..., type = "bar")
}

PieChart <- function(...) {
  .deprecate_once("PieChart()", 'Chart(..., type = "pie")')
  Chart(..., type = "pie")
}

pc <- function(...) {
  .deprecate_once("pc", 'Chart(..., type = "pie")')
  Chart(..., type = "pie")
}


# X() aliases (legacy)-----------------------------------------------------

Histogram <- function(...) {
  .deprecate_once("Histogram()", 'X(..., type = "histogram")')
  X(fun_call = match.call(), type = "histogram", ...)
}

hs <- function(...) {
  .deprecate_once("hs()", 'X(..., type = "histogram")')
  X(fun_call = match.call(), type = "histogram", ...)
}

Density <- function(...) {
  .deprecate_once("Density()", 'X(..., type = "density")')
  X(fun_call = match.call(), type = "density", ...)
}

BoxPlot <- function(...) {
  .deprecate_once("BoxPlot()", 'X(..., type = "vbs", vbs_plot = "b")')
  X(fun_call = match.call(), type = "vbs", vbs_plot = "b", ...)
}

bx <- function(...) {
  .deprecate_once("bx()", 'X(..., type = "vbs", vbs_plot = "b")')
  X(fun_call = match.call(), type = "vbs", vbs_plot = "b", ...)
}

ViolinPlot <- function(...) {
  .deprecate_once("ViolinPlot()", 'X(..., type = "vbs", vbs_plot = "v")')
  X(fun_call = match.call(), type = "vbs", vbs_plot = "v", ...)
}

vp <- function(...) {
  .deprecate_once("vp()", 'X(..., type = "vbs", vbs_plot = "v")')
  X(fun_call = match.call(), type = "vbs", vbs_plot = "v", ...)
}


# XY() aliases (legacy)----------------------------------------------------

Plot <- function(...) {
  .deprecate_once("Plot()", "X(...) or XY(...)")
  
  # Capture the call expression
  call_expr <- match.call()
  call_args <- as.list(call_expr)[-1]  # Remove function name
  arg_names <- names(call_args)
  
  # Check if the first argument is unnamed
  first_unnamed <- is.null(arg_names) || arg_names[1] == ""
  
  # Check for y argument
  has_second     <- length(call_args) >= 2
  second_unnamed <- has_second && (is.null(arg_names) || arg_names[2] == "")
  has_y <- (first_unnamed && second_unnamed) ||
           (!is.null(arg_names) && "y" %in% arg_names)
  
  # facet1 / facet2 rewriting
  if (!is.null(arg_names)) {
    has_facet1 <- "facet1" %in% arg_names
    has_facet2 <- "facet2" %in% arg_names
    
    if (has_facet1 && has_facet2) {
      facet1_value <- call_args[["facet1"]]
      facet2_value <- call_args[["facet2"]]
      call_args[["facet"]] <- call(quote(c), facet1_value, facet2_value)
      call_args[["facet1"]] <- NULL
      call_args[["facet2"]] <- NULL
    } else if (has_facet1) {
      names(call_args)[which(arg_names == "facet1")] <- "facet"
    }
  }
  
  # --- 2-argument case → always XY() ---
  if (has_y) {
    new_call <- as.call(c(quote(XY), call_args))
    return(eval(new_call, envir = parent.frame()))
  }
  
  # --- One-argument case: decide between Chart() and X() ---
  
  # Extract x expression
  x_expr <- if (first_unnamed) {
    call_args[[1]]
  } else if ("x" %in% arg_names) {
    call_args[["x"]]
  } else {
    call_args[[1]]
  }
  
  # Try evaluating x
  x_value <- tryCatch(eval(x_expr, envir = parent.frame()),
                           error = function(e) NULL)
  
  # If not found, look in data frame d
  if (is.null(x_value) && exists("d", envir = parent.frame())) {
    d <- get("d", envir = parent.frame())
    if (is.data.frame(d) && is.name(x_expr) &&
        as.character(x_expr) %in% names(d)) {
          x_value <- d[[as.character(x_expr)]]
    }
  }
  
  # Is x categorical?
  is_categorical <- FALSE
  if (!is.null(x_value))
    is_categorical <- is.factor(x_value) || is.character(x_value)
  
  # --- NEW BEHAVIOR HERE ---
  # If categorical → Chart(..., type="bubble")
  if (is_categorical) {
    call_args$type <- quote("bubble")   # <-- ONLY CHANGE
    new_call <- as.call(c(quote(Chart), call_args))
    return(eval(new_call, envir = parent.frame()))
  }
  
  # Otherwise numeric → X(..., type="vbs")
  call_args$type <- quote("vbs")
  new_call <- as.call(c(quote(X), call_args))
  eval(new_call, envir = parent.frame())
}


ScatterPlot <- function(...) {
  .deprecate_once("ScatterPlot", 'XY(..., type = "scatter")')
  XY(fun_call = match.call(), type = "scatter", ...)
}

sp <- function(...) {
  .deprecate_once("sp", 'XY(..., type = "scatter")')
  XY(fun_call = match.call(), type = "scatter", ...)
}


# legacy aliases ----------------------------------------------------------

av <-
function(...)

  ANOVA(...)

av_brief <-
function(..., brief=TRUE)

  ANOVA(..., brief=TRUE)

ca <-
function(...)

  CountAll(...)

cfa <-
function(...)

  corCFA(fun_call=match.call(), ...)

cr <-
function(...)

  Correlation(...)

cr_brief <-
function(..., brief=TRUE)

  cr(..., brief=TRUE)

cp <-
function(...)

  corProp(...)


db <-
function(..., brief=TRUE)

 details(..., brief=TRUE)

efa <-
function(...)

  corEFA(...)

lr <-
function(...)

  Logit(...)

model <-
function(...)

  Model(...)

mrg <-
function(...)

  Merge(...)


nt <-
function(...)

  Nest(...)

prop <-
function(...)

  Prop_test(...)


rd_lbl <-
function(..., var_labels=TRUE)

  Read(..., var_labels=TRUE, fun_call=match.call())

rd.cor <-
function(...)

  corRead(...)

rd <-
function(...)

  Read(..., fun_call=match.call())

Read2 <- 
function(..., sep=";", dec=",") 
         
  Read(..., fun_call=match.call(), sep=";", dec=",")


reg <-
function(...)

  Regression(fun_call=match.call(), ...)


reg_brief <-
function(..., brief=TRUE)

  Regression(..., fun_call=match.call(), brief=TRUE)

reord <-
function(...)

  corReorder(...)

reflect <-
function(...)

  corReflect(...)

ss <-
function(...)

  SummaryStats(...)

tt <-
function(...)

  ttest(...)

tt_brief <-
function(...)

 ttest(..., brief=TRUE)

ttp  <-
function(...) {

  ttestPower(...)

}


vl <-
function(...)

  VariableLabels(...)

wrt <-
function(...)

  Write(...)

wrt_r <-
function(..., format="R")

  Write(..., format="R")

wrt_x <-
function(..., format="Excel")

  Write(..., format="Excel")


