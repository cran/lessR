BarChart <-
function(x=NULL, by=NULL, dframe=mydata, n.cat=getOption("n.cat"), 
         count.names=NULL, text.out=TRUE, ...)  {


  is.df <- FALSE  # is data frame

  if (missing(x)) {
    x.name <- ""  # in case x is missing, i.e., data frame mydata
    is.df <- TRUE
    dframe <- eval(substitute(mydata))
  }
  else {
    # get actual variable name before potential call of dframe$x
    x.name <- deparse(substitute(x)) 
    options(xname = x.name)
    if (exists(x.name, where=1)) if (is.data.frame(x)) {
       is.df <- TRUE
       dframe <- x
    }
  }

  if (!is.df) {

    dframe.name <- deparse(substitute(dframe))

    # get conditions and check for dframe existing
    xs <- .xstatus(x.name, dframe.name)
    is.frml <- xs$ifr
    in.global <- xs$ig 

    # warn user that old formula mode no longer works
    if (is.frml) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Instead, of 'Y ~ X', now use the by option, 'Y, by=X' \n\n")
    }

    # see if the variable exists in data frame, if x not in Global Env 
    if (!in.global) .xcheck(x.name, dframe.name, dframe)


    if (!in.global) x.call <- eval(substitute(dframe$x))
    else {  # vars that are function names get assigned to global
      x.call <- x
      if (is.function(x.call)) x.call <- eval(substitute(dframe$x))
    }

    # evaluate by
    if (!missing(by)) {

      # get actual variable name before potential call of dframe$x
      y.name <- deparse(substitute(by)) 
      options(yname = y.name)

      # see if y exists from a function call
      # indicate a function call with sys.frame returns larger than 1 
      if (exists(y.name, where=parent.frame(n=1)) && sys.nframe() > 1) 
        in.call <- TRUE else in.call <- FALSE

      # get conditions and check for dframe existing
      if (!in.call) {
        xs <- .xstatus(y.name, dframe.name)
        in.global <- xs$ig 
      }
      else in.global <- FALSE

      # see if var exists in data frame, if x not in Global Env or function call 
      if (!in.global && !in.call) .xcheck(y.name, dframe.name, dframe)

      if (!in.global) y.call <- eval(substitute(dframe$by))
      else {  # vars that are function names get assigned to global
        y.call <- by
        if (is.function(y.call)) y.call <- eval(substitute(dframe$by))
      }

    }
    else y.call <- NULL


  # evaluate count.names
  #---------------------
  if (!missing(count.names)) {

    # get actual variable name before potential call of dframe$x
    count.names.name <- deparse(substitute(count.names)) 
    options(count.namesname = count.names.name)

    # get conditions and check for dframe existing
    xs <- .xstatus(count.names.name, dframe.name)
    in.global <- xs$ig 

    # see if var exists in data frame, if x not in Global Env or function call 
    if (!missing(x) && !in.global)
      .xcheck(count.names.name, dframe.name, dframe)

    if (!in.global) count.names.call <- eval(substitute(dframe$count.names))
    else {  # vars that are function names get assigned to global
      count.names.call <- count.names
      if (is.function(count.names.call)) 
        count.names.call <- eval(substitute(dframe$count.names))
    }
  }
  else
   count.names.call <- NULL

  }  # x not data frame

  if (is.df) bc.data.frame(dframe, n.cat, text.out, ...)

  else {
    bc.default(x.call, y.call, count.names=count.names.call, 
               text.out=text.out, ...)
  }

}

