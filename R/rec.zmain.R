.rec.main <-
function(x, x.name, new.vars, old, new, ivar, dframe, dframe.name, brief) {

  n.values <- length(old)
  n.obs <- nrow(dframe)

  miss.old <- FALSE
  if (!is.null(old)) if (old[1] == "missing") miss.old <- TRUE

  miss.new <- FALSE
  if (!is.null(new)) {
    if (new[1] != "missing") {
      if (n.values != length(new)) { 
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "The same number of values must be specified for both\n",
        "old and new values.\n\n")
      }
    }
    else {
      for (i in 1:n.values) new[i] <- "missing"
      miss.new <- TRUE
    }
  }

  # text output
  if (ivar == 1) {
    cat("\nRecoding Specification\n")
    .dash(22)
    for (i in 1:n.values) cat("  ", old[i], "-->", new[i], "\n")
    if (miss.new)
      cat("\nR represents missing data with a NA for 'not assigned'.\n")
  }
  cat("\n")

  if (!brief) {
    old.unique <- sort(unique(x))
    n.unique <- length(old.unique)
    cat("\n")
    cat("---  Recode:", x.name, "---\n")
    cat("\n")

    if (class(x) == "numeric")
      cat("Unique values of", x.name, "in the data:", old.unique, "\n")
    else if (class(x) == "factor")
      cat("Unique values of", x.name, "in the data:", levels(x), "\n")
    cat("Number of unique values of", x.name, "in the data:", n.unique, "\n")
    cat("\n")

    # check to ensure that all values to recode exist in the data
    for (i in 1:n.values) {
      is.in <- FALSE
      for (j in 1:n.unique) 
        if (old[i] == old.unique[j]) is.in <- TRUE 
      if (!is.in) {
      cat(">>> Note: A value specified to recode, ", old[i], 
        ", is not in the data.\n\n", sep="")
      }
    }

    cat("Number of values of", x.name, "to recode:", n.values, "\n")
    cat("Number of observations (rows) to recode:", n.obs, "\n")

    if (is.null(new.vars))
      cat("\nReplace existing values of ", x.name,
          ", no value for option: new.vars\n", sep="")
    else
      cat("Recode to variable:", new.vars, "\n")

    cat("\n")
  } 

  if (class(x) == "factor") x <- as.character(x)

  new.x <- x

  # the recode
  for (i in 1:n.values) {
    for (j in 1:n.obs) {
      if (!miss.old) {
        if (!is.na(new.x[j])) 
          if (x[j] == old[i]) if (!miss.new) new.x[j] <- new[i] else new.x[j] <- NA
      } 
      else  # miss.old
        if (is.na(new.x[j])) new.x[j] <- new[1]
    } 
  }

  # insert transformation into dframe
  nm <- names(dframe)
  if (is.null(new.vars))
    dframe[, which(nm == x.name)] <- new.x
  else {
    dframe <- cbind(dframe, new.x)
    names(dframe) <- c(nm, new.vars)
  }

  # write the new data frame
  assign(dframe.name, dframe, pos=.GlobalEnv)
 
}
