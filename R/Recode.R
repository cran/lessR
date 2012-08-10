Recode <-
function(old.vars, new.vars=NULL, old, new, dframe=mydata, brief=FALSE) {

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
    # get actual variable name before potential call of dframe$x
    x.name <- names(all.vars)[vars[ivar]]
    options(xname = x.name)

    # get data frame name
    dframe.name <- deparse(substitute(dframe))

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
             get(dframe.name, pos=.GlobalEnv), dframe.name, brief)

  }

  if (!brief) {
    cat("\n")
    .dash(45)
    cat("First six rows of data for data frame:", dframe.name, "\n")
    .dash(45)
    cat("\n")
    print(head(get(dframe.name, pos=.GlobalEnv)))
    cat("\n")
  } 

}
