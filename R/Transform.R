Transform <-
function (dframe=mydata, brief=FALSE, keep=TRUE, ...) {

  dname <- deparse(substitute(dframe))

  # transformations done here into vector e, e[1] the first trans, etc.
  e <- eval(substitute(list(...)), dframe, parent.frame())

  n.tvars <- length(e)
  n.obs <- nrow(dframe)

  if (!brief) {
    cat("\n")
    cat("Number of variables of", dname, "to transform:", n.tvars, "\n")
    cat("Number of observations (rows) of ", dname, ": ", n.obs, "\n", sep="")

    cat("\n")
    .dash(69)
    cat("Before Transformation, First five rows of data for",
        "data frame:", dname, "\n")
    .dash(69)
    print(head(get(dname, pos=.GlobalEnv), n=5))
    cat("\n")

    trs.all <- deparse(substitute(list(...)))[[1]]
    trs.all <- substr(trs.all, 6, nchar(trs.all)-1)
    no.break <- FALSE
    if (grepl("factor", trs.all)) no.break <- TRUE
    if (grepl("cut", trs.all)) no.break <- TRUE
    if (!no.break) {
      trs <- as.vector(strsplit(trs.all, ",")[[1]])
      if (n.tvars > 1) for (i in 2:n.tvars) trs[i] <- substr(trs[i], 2, nchar(trs[i]))
    }
    else
      trs <- trs.all  # displayed later if only one transformation
  }

  # vector of positions of existing vars in dframe, NA if not existing
  inx <- match(names(e), names(dframe))

  if (!brief) {
    cat("\n")
    .dash(22)
    cat("Transformation Summary\n")
    .dash(22)
    if (!no.break || n.tvars==1) {
      for (i in 1:n.tvars) {
        if (!is.na(inx[i])) txt <- "rewrite existing" else txt <- "      create new"
        cat(txt, "variable: ", trs[i], "\n")
      }
    }
    else {  # parsing too difficult, so just include variable names
       for (i in 1:n.tvars) {
        if (!is.na(inx[i])) txt <- "rewrite existing" else txt <- "      create new"
        cat(txt, "variable: ", names(e)[i], "\n")
      }
    }
  }

  # logical vector, TRUE if transformed var already exists, otherwise FALSE
  existing <- !is.na(inx)

  # inx[existing], e[existing] gives only positions of existing vars in dframe
  # corresponding FALSE values of existing are deleted
  # add existing var transformations to dframe
  if (any(existing))   # at least one existing var transformed
    dframe[inx[existing]] <- e[existing]

  # add new var transformations to dframe
  if (!all(existing))  # at least one new var created
    dframe <- do.call("data.frame", c(list(dframe), e[!existing]))

  if (!brief) {
    cat("\n\n")
    .dash(65)
    cat("After, First five rows of transformed data ")
    if (keep) cat("for data frame:", dname)
    cat( "\n")
    .dash(65)
    print(head(dframe[, names(e), drop=FALSE], n=5))
    cat("\n")
  }

  if (keep) 
    assign(dname, dframe, pos=.GlobalEnv)
  else
    return(dframe)
 
}
