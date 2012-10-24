Recode <-
function(old.vars, new.vars=NULL, old, new, brief=FALSE, keep=TRUE, dframe=mydata) {

  all.vars <- as.list(seq_along(dframe))
  names(all.vars) <- names(dframe)
  vars <- eval(substitute(old.vars), envir=all.vars, enclos=parent.frame())

  if (!is.null(new.vars)) {
    old.vars.len <- 
      length(eval(substitute(old.vars), envir=all.vars, enclos=parent.frame()))
    new.vars.len <-
       length(eval(substitute(new.vars), envir=all.vars, enclos=parent.frame()))
    if (old.vars.len != new.vars.len) { 
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Number of specified existing variables: ", old.vars.len, "\n",
        "Number of specified transformed variables: ", new.vars.len, "\n\n",
        "The same number of variables must be specified for both\n",
        "existing and transformed variables.\n\n")
    }
  }

  for (ivar in 1:length(vars)) {
    if (is.factor(dframe[,vars[ivar]])) { 
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Variable to recode is a factor: ", names(dframe)[vars[ivar]], "\n",
        "Doing this recode would remove the factor attribute.\n\n",
        "Instead, recode with the levels argument of the function: factor.\n",
        "Enter  ?Recode  and see last example to illustrate.\n\n")
    }
  }

  # get data frame name
  dframe.name <- deparse(substitute(dframe))

  if (!brief) {
    cat("\n")
    .dash(56)
    cat("First five rows of data to recode for data frame:", dframe.name, "\n")
    .dash(56)
    print(head(dframe[, vars, drop=FALSE], n=5))
    cat("\n")
  } 

  for (ivar in 1:length(vars)) {
    # get actual variable name before potential call of dframe$x
    x.name <- names(all.vars)[vars[ivar]]
    options(xname = x.name)

    # get conditions and check for dframe existing
    xs <- .xstatus(x.name, dframe.name)
    in.global <- xs$ig 

    if (in.global)
      if (is.function(old.vars)) 
        x.call <- as.vector(eval(substitute(dframe[,vars[ivar]])))
      else {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
            "Only variables in a data frame are processed.\n\n")
    }

    # see if variable exists in data frame, if x not in Global Env or function call 
    .xcheck(x.name, dframe.name, dframe)

    x.call <- as.vector(eval(substitute(dframe[,vars[ivar]])))

    .rec.main(x.call, x.name, new.vars[ivar], old, new, ivar,
             get(dframe.name, pos=.GlobalEnv), dframe.name, brief, keep )

  }

  if (!brief) {
    nn <- length(new.vars)
    new.index <- integer(length=nn)
    if (nn > 0) for (i in 1:nn)
      new.index[i] <- which(names(get(dframe.name, pos=.GlobalEnv))==new.vars[i])
    cat("\n\n")
    .dash(54)
    cat("First five rows of recoded data for data frame:", dframe.name, "\n")
    .dash(37+nchar(dframe.name))
    print(head(get(dframe.name, pos=.GlobalEnv)[, c(vars, new.index),
               drop=FALSE], n=5))
    cat("\n")
  } 

}
