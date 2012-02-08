bc <-
function(x=NULL, by=NULL, dframe=mydata, ncut=4, ...)  {


bc.main <-
function(x, ...) {

# an ordered factor has two attributes, so just take the first always
#   to avoid a warning message
  if (class(x)[1] == "data.frame") bc.data.frame(x, ncut, ...) 
  else UseMethod("bc")
}


#-------------------------------------------------------------------------

# get actual variable name before potential call of dframe$x
if (!missing(x)) x.name <<- deparse(substitute(x)) 

# evaluate x
#-----------

if (!missing(x)) {
  # see if x exists in the Global Environment
  if (exists(x.name, where=.GlobalEnv)) in.global <- TRUE else in.global <- FALSE

  # see if x exists from a function call
  # indicate a function call with sys.frame returns larger than 1 
  if (exists(x.name, where=parent.frame(n=1)) && sys.nframe() > 1) 
    in.call <- TRUE else in.call <- FALSE

  # see if the data frame exists, if x not in Global Env or function call
  dframe.name <- deparse(substitute(dframe))
  if (!in.global && !in.call) {
    if (!exists(dframe.name)) {
      if (dframe.name == "mydata") 
        txtA <- ", the default data frame name, " else txtA <- " "
      txtB1 <- "So either create data frame by reading with the rad function, or\n"
      txtB2 <- "  specify the actual data frame with the parameter: dframe\n"
      txtB <- paste(txtB1, txtB2, sep="")
      cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Data frame ", dframe.name, txtA, "does not exist\n\n", txtB, "\n")
   }
  }

  # see if x exists in the data frame, if x not in Global Env or function call 
  if (!missing(x) && !in.global && !in.call) {
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
  if (in.global || in.call)
     x.call <- x else x.call <- eval(substitute(dframe$x))
}


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

  if (in.global || in.call)
     y.call <- by else y.call <- eval(substitute(dframe$by))
}

if (!missing(x)) 
  if (missing(by)) bc.main(x.call, ...)
  else bc.main(x.call, y.call, ...)
else
  bc.main(dframe, ...)


if (!missing(x)) rm(x.name, envir=.GlobalEnv)
if (!missing(by)) rm(y.name, envir=.GlobalEnv)

}

