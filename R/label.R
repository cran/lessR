label <- 
function(x, dframe=mylabels) {

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
      if (dframe.name == "mylabels") 
        txtA <- ", the default data frame name, " else txtA <- " "
      txtB1 <- "Create the labels by reading with the readLabels function\n"
      txtB <- paste(txtB1, sep="")
      cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Data frame ", dframe.name, txtA, "does not exist\n\n", txtB, "\n")
    }
  }

  irow <- which(row.names(mylabels)==x.name)
  if (length(mylabels[irow, "label"]) > 0) lbl <- mylabels[irow, "label"]
  else {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "For variable: ", x.name, "\n",
    "Either the variable or the label in mylabels or both do not exist.\n")
  }

  return(lbl)
  
}
