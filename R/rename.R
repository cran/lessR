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

  # get existing, old variable name from its position in data frame
  data.vars <- as.list(seq_along(data))
  names(data.vars) <- names(data)
  ind.fr <- eval(substitute(from), envir=data.vars, parent.frame())
  nm.fr <- names(data)[ind.fr]

  # new variable name(s)
  # if converting a vector of names, then first element is "c", so remove
  nm.new <- sapply(substitute(to), deparse)
  nm.to <- character(length=0)
  if (length(nm.new) > 1)
    for (i in 2:length(nm.new)) nm.to[i-1] <- nm.new[i]
  else
    nm.to <- nm.new[1]

  if (length(ind.fr) > 0) {
  for(i in 1:length(ind.fr))
    names(data)[names(data) == nm.fr[i]] = nm.to[i]

  }

  cat("Change the following variable names for data frame", df.name, ":\n\n")
  for (i in 1:length(nm.fr))
    cat(nm.fr[i], "-->", nm.to[i], "\n")
  cat("\n")

  return(data)

}

