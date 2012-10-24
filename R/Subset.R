Subset <-
function (rows, columns, brief=FALSE, keep=TRUE,
          dframe=mydata, validate=NULL, ...) {

  dname <- deparse(substitute(dframe))

  n.vars <- ncol(dframe)
  n.obs <- nrow(dframe)

  if (!brief && keep) {
    cat("\n")
    .dash(17)
    cat("Before the subset\n")
    .dash(17)
    cat("\n")
    cat("Number of variables in ", dname, ": ", n.vars, "\n", sep="")
    cat("Number of observations (rows) in ", dname, ": ", n.obs, "\n\n", sep="")
    cat("First five rows of data for data frame:", dname, "\n")
    .dash(68)
    print(head(dframe, n=5))
    cat("\n")
  }

  if (missing(rows)) {
    r <- TRUE
    validate <- FALSE
  }

  else {
    r <- eval(substitute(rows), envir=dframe, enclos=parent.frame())

    if (!is.null(validate)) if (!is.numeric(r) && validate) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Cannot have a hold out sample unless rows is numeric.\n\n")
    }

    if (is.logical(r)) {
      r <- r & !is.na(r)  # set missing for a row to FALSE
      validate <- FALSE
    }

    else if (is.numeric(r)) {
      if (is.null(validate)) validate <- TRUE
      if (rows>1)
        n.obs.new <- round(rows,0)
      else
        n.obs.new <- round(r*n.obs,0)
      cat("\n")
      cat("Rows of data randomly extracted\n")
      .dash(42)
      if (rows <= 1) cat("Proportion of randomly retained rows: ", rows, "\n")
      cat("Number of randomly retained rows: ", n.obs.new, "\n")
      rand.rows <- sample(1:n.obs, size=n.obs.new, replace=FALSE)
      rand.rows <- sort(rand.rows)
      if (n.obs.new < 1000) {
        cat("\nRows retained\n")
        .dash(13)
        cat(rand.rows, "\n")
      }
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
    all.vars <- as.list(seq_along(dframe))
    names(all.vars) <- names(dframe)
    vars <- eval(substitute(columns), envir=all.vars, parent.frame())
 }

  if (!is.null(validate)) if (validate) 
    hold.dframe <- dframe[!r, vars, drop=FALSE]
  dframe <- dframe[r, vars, drop=FALSE]

  n.vars <- ncol(dframe)
  n.obs <- nrow(dframe)

  if (!brief) {
    cat("\n\n")
    .dash(16)
    cat("After the subset\n")
    .dash(16)
    cat("\n")
    if (keep)
      txt <- paste("in", dname)
    else
      txt <- ""
    cat("Number of variables ", txt, ": ", n.vars, "\n", sep="")
    cat("Number of observations (rows) ", txt, ": ", n.obs, "\n\n", sep="")
    if (keep) {
      cat( "\n")
      cat("First five rows of data ")
      cat("for data frame:", dname)
      cat( "\n")
      .dash(68)
      print(head(dframe, n=5))
    }
  }

  if (keep) { 
    assign(dname, dframe, pos=.GlobalEnv) 
    if (!is.null(validate)) if (validate)  {
      hold.dname <- paste(dname,".hold",sep="")
      assign(hold.dname, hold.dframe, pos=.GlobalEnv)
      cat("\n\n")
      cat("Hold out validation sample created\n")
      .dash(38)
      cat("Name of validation sample: ", hold.dname, "\n",
          "Number of randomly retained rows: ", nrow(hold.dframe),
          "\n\n", sep="")
    }
  }
  else 
    return(dframe)

}
