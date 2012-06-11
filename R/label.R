label <- 
function(x, dframe=mylabels) {

  x.name <- deparse(substitute(x)) 
  options(xname = x.name)

  # get data frame name
  dframe.name <- deparse(substitute(dframe))

  # get conditions and check for dframe existing
  xs <- .xstatus(x.name, dframe.name)
  in.global <- xs$ig 

  # see if the data frame exists, if x not in Global Env or function call
  if (!in.global) {
    if (!exists(dframe.name)) {
      if (dframe.name == "mylabels") 
        txtA <- ", the default data frame name, " else txtA <- " "
      txtB1 <- "Create the labels by reading with rad.labels or rad.both\n"
      txtB <- paste(txtB1, sep="")
      cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Data frame ", dframe.name, txtA, "does not exist\n\n", txtB, "\n")
    }
  }

  gl <- .getlabels()
  lbl <- gl$xl
  if (length(lbl) == 0) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "For variable: ", x.name, "\n",
    "Either the variable or the label in mylabels or both do not exist.\n")
  }

  return(lbl)
  
}
