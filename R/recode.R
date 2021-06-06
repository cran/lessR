recode <-
function(old_vars, new_vars=NULL, old, new, data=d,
         quiet=getOption("quiet"), ...) {

  # a dot in a parameter name to an underscore
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    change <- c("new.vars", "old.vars")
    for (i in 1:length(dots)) {
      if (names(dots)[i] %in% change) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }

  if (missing(old_vars)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Specify the variable to be recoded by listing it\n",
      "first or set according to:  old_vars\n\n")
  }

  if (missing(old)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Specify the values of the variable to be recoded with:  old\n\n")
  }

  if (missing(new)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Specify the new values of the recoded variable with:  new\n\n")
  }


  my.vars <- as.list(seq_along(data))
  names(my.vars) <- names(data)  # vars is ordinal position of variables in d
  vars <- eval(substitute(old_vars), envir=my.vars, enclos=parent.frame())

  if (!is.null(new_vars)) {
    old_vars.len <- 
      length(eval(substitute(old_vars), envir=my.vars, enclos=parent.frame()))
    new_vars.len <-
       length(eval(substitute(new_vars), envir=my.vars, enclos=parent.frame()))
    if (old_vars.len != new_vars.len) { 
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Number of specified existing variables: ", old_vars.len, "\n",
        "Number of specified transformed variables: ", new_vars.len, "\n\n",
        "The same number of variables must be specified for both\n",
        "existing and transformed variables.\n\n")
    }
  }

  for (ivar in 1:length(vars)) {
    if (is.factor(data[,vars[ivar]])) { 
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Variable to recode is a factor: ", names(data)[vars[ivar]], "\n",
        "Doing this recode would remove the factor attribute.\n\n",
        "Instead, recode with the levels argument of the function: factor.\n",
        "Enter  ?recode  and see last example to illustrate.\n\n")
    }
  }

  # get data frame name
  dname <- deparse(substitute(data))

  if (!quiet) {
    cat("\n")
    .dash(56)
    cat("First four rows of data to recode for data frame:", dname, "\n")
    .dash(56)
    print(head(data[, vars, drop=FALSE], n=4))
    cat("\n")
  } 

  for (ivar in 1:length(vars)) {
    # get actual variable name before potential call of data$x
    x.name <- names(my.vars)[vars[ivar]]
    options(xname = x.name)

    # get conditions and check for data existing
    xs <- .xstatus(x.name, dname, quiet)
    in.style <- xs$ig 

    if (in.style)
      if (is.function(old_vars)) 
        x.call <- as.vector(eval(substitute(data[,vars[ivar]])))
      else {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
            "Only variables in a data frame are processed.\n\n")
    }

    # see if variable exists in data frame, if x not in style Env or function call 
    .xcheck(x.name, dname, names(data))

    x.call <- as.vector(eval(substitute(data[,vars[ivar]])))

    n.obs <- nrow(data)
    new.x <- .rec.main(x.call, x.name, new_vars[ivar], old, new, ivar,
                        n.obs, dname, quiet)

    # insert transformation into data
    new.var <- new_vars[ivar]
    nm <- names(data)
    if (is.null(new.var))
      data[, which(nm == x.name)] <- new.x
    else {
      data <- cbind(data, new.x)
      names(data) <- c(nm, new.var)
    }

  }  # recode loop

  if (!quiet) {
    nn <- length(new_vars)
    new.index <- integer(length=nn)
    if (nn > 0) for (i in 1:nn)
      new.index[i] <- which(names(data)==new_vars[i])
    cat("\n\n")
    .dash(48)
    cat("First four rows of recoded data\n")
    .dash(48)
    print(head(data[, c(vars, new.index), drop=FALSE], n=4))
    cat("\n")
  } 
 
  return(data)

}
