reshape_wide <-
  function(data, group, response, ID, prefix=NULL, sep="_") {
 
  # if a tibble, convert to data frame
  df.name <- deparse(substitute(data))  # is NULL if from shiny
  if (exists(df.name, envir=parent.frame())) {
    if (any(grepl("tbl", class(data), fixed=TRUE)))
      data <- data.frame(data)
  }

  data <- data[, c(ID, group, response)]
  dw <- reshape(data, direction="wide",
               idvar=ID, timevar=group, v.names=response,
               sep=sep)

  sep.nm <- sep
  pattern <- paste(response, sep.nm, sep="")

  if (is.null(prefix)) {
    if (!is.numeric(data[, group]))
      prefix <- FALSE
    else
      prefix <- TRUE
  }

  if (!prefix)
    names(dw) <- gsub(pattern, "", names(dw), fixed=TRUE)

  return(dw)

}

