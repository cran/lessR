SummaryStats <-
function(x=NULL, by=NULL, dframe=mydata, n.cat=getOption("n.cat"), 
         digits.d=NULL, brief=FALSE, ...)  {


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

  }  # x not data frame

  if (!is.df) nu <- length(unique(na.omit(x.call)))

  if (is.df) .ss.data.frame(dframe, n.cat, ...) 

  else if (is.numeric(x.call)  &&  nu > n.cat)
     .ss.numeric(x.call, y.call, dframe, digits.d, brief, ...)

  else if (is.numeric(x.call) && nu <= n.cat) {
    cat("\n>>> Variable is numeric, but only has", nu, "<= n.cat =", n.cat, "levels,",
      "so treat as categorical.\n",
      "   To obtain the numeric summary, decrease  n.cat  to indicate a lower\n",
      "   number of unique values such as with function: set.\n", 
      "   Perhaps make this variable a factor with R factor function.\n")
   .ss.factor(x.call, y.call, brief, digits.d, ...)
  }

  # ordered factors have two attributes, "ordered" and "factor"
  else if (is.factor(x.call))
     .ss.factor(x.call, y.call, brief, digits.d, ...)

  else if (is.character(x.call))
    if (nlevels(factor(x.call)) < length(x.call)) 
       .ss.factor(factor(x.call), by, brief, n.cat, digits.d, ...)
    else cat("\n Appears to contain unique Names or IDs", "\n")

  else {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "The variable to be analyzed must be numeric or a factor, or have\n",
        "character values that can be converted to a factor, or logical values\n",
        "that can be converted to numerical 0 and 1.\n")
  }

}


