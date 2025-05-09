reshape_wide <-
  function(data, widen, response, ID, prefix=NULL, sep="_", ...) {

  dots <- list(...)
  if (length(dots) > 0) {
    for (i in seq_along(dots)) {
      if (names(dots)[i] == "group") widen <- dots[[i]]
    }
  }
 
  # if a tibble, convert to data frame
  df.name <- deparse(substitute(data))  # is NULL if from shiny
  if (exists(df.name, envir=parent.frame())) {
    if (any(grepl("tbl", class(data), fixed=TRUE)))
      data <- data.frame(data)
  }

  # parameter variable names not quoted, so convert to char strings
  ID <- deparse(substitute(ID))
  widen <- deparse(substitute(widen))
  response <- deparse(substitute(response))
    
  data <- data[, c(ID, widen, response)]
  dw <- reshape(data, direction="wide",
               idvar=ID, timevar=widen, v.names=response,
               sep=sep)

  sep.nm <- sep
  pattern <- paste(response, sep.nm, sep="")

  if (is.null(prefix)) {
    if (!is.numeric(data[, widen]))
      prefix <- FALSE
    else
      prefix <- TRUE
  }

  if (!prefix)
    names(dw) <- gsub(pattern, "", names(dw), fixed=TRUE)

  return(dw)
}

