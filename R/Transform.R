Transform <-
function (dframe=mydata, brief=FALSE, save.dframe=TRUE, ...) {

  dname <- deparse(substitute(dframe))

  # vector of transformation(s), e[1] the first trans, etc.
  e <- eval(substitute(list(...)), dframe, parent.frame())

  n.tvars <- length(e)
  n.obs <- nrow(dframe)

  if (!brief) {
    cat("\n")
    cat("Number of variables of", dname, "to transform:", n.tvars, "\n")
    cat("Number of observations (rows) of ", dname, ": ", n.obs, "\n", sep="")

    cat("\n")
    .dash(68)
    cat("Before Transformation, First six rows of data for",
        "data frame:", dname, "\n")
    .dash(68)
    cat("\n")
    print(head(get(dname, pos=.GlobalEnv)))
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
    cat("\nTransformation Summary\n")
    .dash(38)
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
  if (any(existing))   # at least one existing var transformed
    dframe[inx[existing]] <- e[existing]

  if (!all(existing))  # at least one new var created
    dframe <- do.call("data.frame", c(list(dframe), e[!existing]))

  if (save.dframe) assign(dname, dframe, pos=.GlobalEnv)

  if (!brief) {
    cat("\n\n")
    .dash(67)
    cat("After Transformation, First six rows of data for",
        "data frame:", dname, "\n")
    .dash(67)
    cat("\n")
    print(head(get(dname, pos=.GlobalEnv)))
    cat("\n")
  }
 
}
