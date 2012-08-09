Subset <-
function (rows, columns, brief=FALSE,
          save.dframe=TRUE, dframe=mydata, ...) {

  dname <- deparse(substitute(dframe))

  n.vars <- ncol(dframe)
  n.obs <- nrow(dframe)

  if (!brief) {
    cat("\n")
    .dash(17)
    cat("Before the subset\n")
    .dash(17)
    cat("\n")
    cat("Number of variables in ", dname, ": ", n.vars, "\n", sep="")
    cat("Number of observations (rows) in ", dname, ": ", n.obs, "\n\n", sep="")
    cat("First six rows of data for", "data frame:", dname, "\n")
    .dash(68)
    cat("\n")
    print(head(dframe))
    cat("\n")
  }

  if (missing(rows)) 
    r <- TRUE
  else {
    e <- substitute(rows)
    r <- eval(e, dframe, parent.frame())
    if (!is.logical(r)) 
      stop("'rows' must evaluate to logical")
    r <- r & !is.na(r)
  }

  if (missing(columns)) 
    vars <- TRUE
  else {
    all.vars <- as.list(seq_along(dframe))
    names(all.vars) <- names(dframe)
    vars <- eval(substitute(columns), all.vars, parent.frame())
 }

  dframe <- dframe[r, vars, drop=FALSE]

  n.vars <- ncol(dframe)
  n.obs <- nrow(dframe)

  if (!brief) {
    cat("\n\n")
    .dash(16)
    cat("After the subset\n")
    .dash(16)
    cat("\n")
    cat("Number of variables in ", dname, ": ", n.vars, "\n", sep="")
    cat("Number of observations (rows) in ", dname, ": ", n.obs, "\n\n", sep="")
    cat("First six rows of data for", "data frame:", dname, "\n")
    .dash(68)
    cat("\n")
    print(head(dframe))
    cat("\n")
  }

  if (save.dframe) assign(dname, dframe, pos=.GlobalEnv)

}
