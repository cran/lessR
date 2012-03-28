tt <-
function(x=NULL, y=NULL, dframe=mydata, ...)  {

  x.name <<- ""  # in case x is missing, i.e., data frame mydata

  # get actual variable name before potential call of dframe$x
  x.name <<- deparse(substitute(x)) 
  # see if the variable exists in the Global Environment
  if (exists(x.name, where=1)) in.global <- TRUE  else in.global <- FALSE


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

  if (grepl("~", x.name)) in.df <- FALSE  # formula
  else if (x.name == "NULL") in.df  <- FALSE  # from stats
  else {
    
    in.df <- FALSE
    # see if the variable exists in the data frame, if x not in Global Env 
    if (!in.global) {
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
      else in.df <- TRUE
    }
  }

  if (in.global) tt.default(x, y, ...)
  else 
   if (in.df) tt.default(eval(substitute(dframe$x)), ...)
  else
    if (is.null(x)) tt.default(...)  # analysis from stats
  else
    if (class(x) == "formula") tt.formula(x, y, dframe, ...)

  if (exists("x.name", where=.GlobalEnv)) rm(x.name, envir=.GlobalEnv)

}
