.stats <- function(x, y, by.var, facet.var = NULL, stat, y.name) {

  ## helper: aggregate over x and possibly multiple by / facet columns
  ##   - if both by.var and facet.var are NULL: tapply result (named vector/array)
  ##   - otherwise: data.frame with columns x, <grouping cols>, y
  agg_wrapper <- function(FUN) {

    # no grouping beyond x: original behavior
    if (is.null(by.var) && is.null(facet.var)) {
      out <- tapply(y, x, FUN)
      return(out)
    }

    # build data.frame: x + (columns from by.var) + (columns from facet.var) + y
    df <- data.frame(x = x)

    # by.var can be vector, matrix, or data.frame
    if (!is.null(by.var)) {
      if (is.data.frame(by.var)) {
        df <- cbind(df, by.var)
      } else if (is.matrix(by.var)) {
        df <- cbind(df, as.data.frame(by.var))
      } else {
        df$by.var <- by.var
      }
    }

    # facet.var can be vector, matrix, or data.frame
    if (!is.null(facet.var)) {
      if (is.data.frame(facet.var)) {
        df <- cbind(df, facet.var)
      } else if (is.matrix(facet.var)) {
        df <- cbind(df, as.data.frame(facet.var))
      } else {
        df$facet.var <- facet.var
      }
    }

    df$y <- y

    out <- stats::aggregate(y ~ ., data = df, FUN = FUN)

    ## Ensure we have canonical column names "x" and "y"
    if (!("x" %in% names(out)))
      stop(".stats(): aggregated result must contain a column named 'x'.")
    if (!("y" %in% names(out)))
      stop(".stats(): aggregated result must contain a column named 'y'.")

    out
  }

  # -------------------------------------------------------------------
  # Apply the wrapper for each supported stat, same logic as before
  # -------------------------------------------------------------------
  if (stat == "sum") {
    ylab <- paste("Sum of", y.name)
    out  <- agg_wrapper(function(z) sum(z, na.rm = TRUE))
  }

  if (stat == "mean") {
    ylab <- paste("Mean of", y.name)
    out  <- agg_wrapper(function(z) mean(z, na.rm = TRUE))
  }

  if (stat == "sd") {
    ylab <- paste("Standard Deviation of", y.name)
    out  <- agg_wrapper(function(z) stats::sd(z, na.rm = TRUE))
  }

  if (stat == "deviation") {
    ylab <- paste("Mean Deviation of", y.name)
    if (is.null(by.var) && is.null(facet.var)) {
      out <- tapply(y, x, mean, na.rm = TRUE)
      out <- out - mean(y, na.rm = TRUE)
    } else {
      cat("\n"); stop(call. = FALSE, "\n","------\n",
        "deviation for stat not meaningful with a by or facet variable\n\n")
    }
  }

  if (stat == "min") {
    ylab <- paste("Minimum of", y.name)
    out  <- agg_wrapper(function(z) min(z, na.rm = TRUE))
  }

  if (stat == "median") {
    ylab <- paste("Median of", y.name)
    out  <- agg_wrapper(function(z) stats::median(z, na.rm = TRUE))
  }

  if (stat == "max") {
    ylab <- paste("Maximum of", y.name)
    out  <- agg_wrapper(function(z) max(z, na.rm = TRUE))
  }

  ## Same missing-value safeguard as before.
  if (sum(is.na(out)) > 0) { # y and a summary table, then no stat
    cat("\n"); stop(call. = FALSE, "\n","------\n",
      "The summary table of the transformed data has some missing\n",
      "   values, likely as a result of too few data values in\n",
      "   some cells to be able to calculate the specified statistic\n\n")
  }

  return(list(out = out, ylab = ylab))
}
