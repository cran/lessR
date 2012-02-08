ss <-
function(x=NULL, by=NULL, dframe=mydata, ncut=4,  ...)  {


ss.main <-
function(x, by=NULL, ...) {

# an ordered factor has two attributes, so just take the first always
#   to avoid a warning message
  if (class(x)[1] == "data.frame") ss.data.frame(x, ncut, ...) 
  else if (class(x)[1] == "numeric") ss.numeric(x, by, dframe, ...)
  else if (class(x)[1] == "factor") ss.factor(x, by, ...)
  else UseMethod("ss")
}

x.name <<- ""  # in case x is missing, i.e., data frame mydata
if (!missing(x)) {
  # get actual variable name before potential call of dframe$x
  x.name <<- deparse(substitute(x)) 

# warn user that old formula mode no longer works
  if (grepl("~", x.name)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Instead, of 'Y ~ X', now use the by option, 'Y, by=X' \n\n")
  }

  # see if the variable exists in the Global Environment
  if (exists(x.name, where=1)) in.global <- TRUE  else in.global <- FALSE

  is.df <- FALSE
  if (in.global) if (class(x)[1] == "data.frame") is.df <- TRUE

  if (!is.df) {
    # see if the data frame exists (mydata default), if x not in Global Env
    dframe.name <- deparse(substitute(dframe))
    if (!in.global) {
      if (!exists(dframe.name)) {
        if (dframe.name == "mydata") 
          txtA <- ", the default data frame name, " else txtA <- " "
        txtB1 <- "So either create the data frame by reading with the rad function, or\n"
        txtB2 <- "  specify the actual data frame with the parameter: dframe\n"
        txtB <- paste(txtB1, txtB2, sep="")
        cat("\n"); stop(call.=FALSE, "\n","------\n",
            "Data frame ", dframe.name, txtA, "does not exist\n\n", txtB, "\n")
      }
    }

  # see if the variable exists in the data frame, if x not in Global Env 
  if (!missing(x) && !in.global) {
    if (!exists(x.name, where=dframe)) { 
      if (dframe.name == "mydata") {
        txt1 <- ", the default name \n\n"
        txt2 <- "So either make sure you are using the correct variable name, or\n"
        txt3 <- "  specify the actual data frame with the parameter: dframe\n"
        txt <- paste(txt1, txt2, txt3, sep="")
      }
      else txt <- " "
      cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Variable ", x.name, " does not exist either by itself ",
          "or in the data frame ", dframe.name, txt, "\n\n")
    }
  }
  if (in.global)
     x.call <- x else x.call <- eval(substitute(dframe$x))

# evaluate y
#-----------
if (!missing(by)) {

  y.name <<- deparse(substitute(by)) 

  # see if y exists in the Global Environment
  if (exists(y.name, where=.GlobalEnv)) in.global <- TRUE else in.global <- FALSE

  # see if y exists from a function call
  # indicate a function call with sys.frame returns larger than 1 
  if (exists(y.name, where=parent.frame(n=1)) && sys.nframe() > 1) 
    in.call <- TRUE else in.call <- FALSE
  if (!in.global && !in.call) {
  # see if y exists in the data frame, if y not in Global Env or function call 
    if (!exists(y.name, where=dframe)) { 
      if (dframe.name == "mydata") {
        txt1 <- ", the default name \n\n"
        txt2 <- "So either make sure you are using the correct variable name, or\n"
        txt3 <- "  specify the actual data frame with the parameter: dframe\n"
        txt <- paste(txt1, txt2, txt3, sep="")
      }
      else txt <- " "
      cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Variable ", y.name, " does not exist either by itself ",
          "or in the data frame ", dframe.name, txt, "\n\n")
    }
  }

  if (in.global)
     y.call <- by else y.call <- eval(substitute(dframe$by))
}

if (!missing(x)) 
  if (missing(by)) ss.main(x.call, ...)
  else ss.main(x.call, y.call, ...)
  }
}


if (missing(x) || is.df) {
  if (missing(x)) dframe <- eval(substitute(mydata)) else dframe <- x
  ss.main(dframe, ...)
}

if (exists("x.name", where=.GlobalEnv)) rm(x.name, envir=.GlobalEnv)
if (exists("y.name", where=.GlobalEnv)) rm(y.name, envir=.GlobalEnv)

}

