.rec.main <-
function(x, x.name, new.var, old, new, ivar, n.obs, dname, quiet) {

  n.values <- length(old)

  miss_old <- FALSE
  if (!is.null(old)) if (old[1] == "missing") miss_old <- TRUE

  miss_new <- FALSE
  if (!is.null(new)) {
    if (new[1] != "missing") {
      if (n.values != length(new)) { 
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "The same number of values must be specified for both\n",
        "old and new values_\n\n")
      }
    }
    else {
      for (i in 1:n.values) new[i] <- "missing"
      miss_new <- TRUE
    }
  }

  # text output
  if (!quiet) {

    if (ivar == 1) {
      cat("\nRecoding Specification\n")
      .dash(22)
      for (i in 1:n.values) cat("  ", old[i], "-->", new[i], "\n")
      cat("\n")
      if (miss_new)
        cat("\nR represents missing data with a NA for 'not assigned'.\n\n")
      cat("Number of cases (rows) to recode:", n.obs, "\n")
      if (is.null(new.var))
        cat("\nReplace existing values of each specified variable",
            ", no value for option: new.var\n", sep="")
    }
    cat("\n")

    old.unique <- sort(unique(x))
    n.unique <- length(old.unique)
    cat("---  Recode:", x.name, "---------------------------------\n")
    
    if ("numeric" %in% class(x))  # R 4.0 results in two values: matrix, array
      cat("Unique values of", x.name, "in the data:", old.unique, "\n")
    else if ("factor" %in% class(x))
      cat("Unique values of", x.name, "in the data:", levels(x), "\n")
    cat("Number of unique values of", x.name, "in the data:", n.unique, "\n")

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

    if (!is.null(new.var)) cat("Recode to variable:", new.var, "\n")
  }  # end text 

  if ("factor" %in% class(x)) x <- as.character(x)

  new.x <- x

  # the recode
  for (i in 1:n.values) {
    for (j in 1:n.obs) {
      if (!miss_old) {
        if (!is.na(new.x[j])) 
          if (x[j] == old[i]) new.x[j] <- ifelse (!miss_new, new[i], NA)
      } 
      else  # miss_old
        if (is.na(new.x[j])) new.x[j] <- new[1]
    } 
  }

  new.x <- type.convert(new.x)  # new values are consistent with their type

  return(new.x)
 
}
