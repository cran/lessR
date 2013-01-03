label <- 
function(x, data=mydata) {

  x.name <- deparse(substitute(x)) 
  options(xname = x.name)

  # get data frame name
  dname <- deparse(substitute(data))
  options(dname = dname)

  # get conditions and check for data existing
  xs <- .xstatus(x.name, dname)
  in.global <- xs$ig 

  # see if the data frame exists, if x not in Global Env or function call
  if (!in.global) {
    if (!exists(dname)) {
      if (dname == "mydata") 
        txtA <- ", the default data frame name, " else txtA <- " "
      txtB1 <- "Create the labels by reading with Read and labels option\n"
      txtB <- paste(txtB1, sep="")
      cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Data frame ", dname, txtA, "does not exist\n\n", txtB, "\n")
    }
  }

  gl <- .getlabels()
  lbl <- gl$xl
  if (length(lbl) == 0) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "For variable: ", x.name, "\n",
    "Either the variable or the label in the data frame or both do not exist.\n")
  }

  return(lbl)
  
}
