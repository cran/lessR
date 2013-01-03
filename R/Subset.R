Subset <-
function(rows, columns, data=mydata, holdout=FALSE, quiet=FALSE, ...) {

  dname <- deparse(substitute(data))

  n.vars <- ncol(data)
  n.obs <- nrow(data)

  if (!quiet) {
    cat("\n")
    .dash(17)
    cat("Before the subset\n")
    .dash(17)
    cat("\n")
    cat("Number of variables in ", dname, ": ", n.vars, "\n", sep="")
    cat("Number of cases (rows) in ", dname, ": ", n.obs, "\n\n", sep="")
    cat("First five rows of data for data frame:", dname, "\n")
    .dash(68)
    print(head(data, n=5))
    cat("\n")
  }

  if (missing(rows))
    r <- TRUE
  else {
    r <- eval(substitute(rows), envir=data, enclos=parent.frame())

    if (!is.numeric(r) && holdout) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Cannot have a hold out sample unless rows is numeric.\n\n")
    }

    if (is.logical(r))
      r <- r & !is.na(r)  # set missing for a row to FALSE

    else if (is.numeric(r)) {
      if (rows>1)
        n.obs.new <- round(rows,0)
      else
        n.obs.new <- round(r*n.obs,0)
      if (!quiet) {
        cat("\n")
        cat("Rows of data randomly extracted\n")
        .dash(42)
        if (rows <= 1) cat("Proportion of randomly retained rows: ", rows, "\n")
        cat("Number of randomly retained rows: ", n.obs.new, "\n")
      }
      rand.rows <- sample(1:n.obs, size=n.obs.new, replace=FALSE)
      rand.rows <- sort(rand.rows)
      r <- logical(length=n.obs)  # initial default is FALSE
      j <- 1
      for (i in 1:n.obs) {
        if (i == rand.rows[j]) {
          r[i] <- TRUE 
          if (j < length(rand.rows)) j <- j + 1
        }
      }
    }

    else {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Specified value for rows must be an expression or a number.\n\n")
    }
  }

  if (missing(columns)) 
    vars <- TRUE
  else {
    all.vars <- as.list(seq_along(data))
    names(all.vars) <- names(data)
    vars <- eval(substitute(columns), envir=all.vars, parent.frame())
 }

  data.sub <- data[r, vars, drop=FALSE]

  if (!quiet) {
    cat("\n\n")
    .dash(16)
    cat("After the subset\n")
    .dash(16)
    cat("\n")
    cat("Number of variables ", ": ", ncol(data.sub), "\n", sep="")
    cat("Number of cases (rows) ", ": ", nrow(data.sub), "\n\n", sep="")
    cat( "\n")
    cat("First five rows of data ")
    cat( "\n")
    .dash(68)
    print(head(data.sub, n=5))
    cat( "\n")
  }

  if (holdout)  {
    data.hold <- data[!r, vars, drop=FALSE]
    cat("\n\n")
    cat("Holdout Sample\n")
    .dash(70)
    cat("Deleted Rows:", nrow(data.hold), "\n")
    cat("Copy and paste this code into R to create the hold out sample\n",
        "from the original, unmodified data frame:", dname, "\n\n")
    cat(paste(dname,".hold", sep=""), "<- Subset(\n")
    for (i in 1:nrow(data.hold)) {
      if (i > 1) cat ("| ") else cat("  ")
      cat("row.names(", dname, ")==\"", row.names(data.hold)[i], "\"\n", sep="")
    }
    cat(")\n")
  }

  return(data.sub)

}
