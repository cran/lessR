values <-
function(x, dframe=mydata, ...) {


# get actual variable name before potential call of dframe$x
x.name <- deparse(substitute(x)) 

# see if the variable exists in the Global Environment
if (exists(x.name, where=1)) in.global <- TRUE else in.global <- FALSE

# see if the variable exists from a function call
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

# see if variable exists in the data frame, if x not in Global Env or function call 
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
  print(x, ...)
else
  print(eval(substitute(dframe$x)), ...)

}
