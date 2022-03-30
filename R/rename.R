rename <-
  function(data, from, to) {

  # get name of data table
  df.name <- deparse(substitute(data))

  # if a tibble convert to data frame
  dfs <- .getdfs()
  if (!is.null(dfs)) {
    if (df.name %in% dfs) {  # tibble to df
      if (any(grepl("tbl", class(data), fixed=TRUE))) {
        data <- data.frame(data)
      }
    }
  }

  # new variable name
  nm.to <- deparse(substitute(to))

  # get existing, old variable name from its position in data frame
  data.vars <- as.list(seq_along(data))
  names(data.vars) <- names(data)
  ind.fr <- eval(substitute(from), envir=data.vars, parent.frame())
  nm.fr <- names(data)[ind.fr]

  cat("Change name of variable", nm.fr, "to", nm.to, "in data frame",
      df.name, "\n")

  if (length(ind.fr) > 0)
    names(data)[ind.fr] <- nm.to
  else {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Variable  ", from, "  does not exist in data frame", df.name, "\n\n")
  }

  return(data)

}

