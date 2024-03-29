label <- 
function(x, value=NULL, data=d) {

  x.name <- deparse(substitute(x)) 
  options(xname = x.name)

  # get data frame name
  dname <- deparse(substitute(data))
  options(dname = dname)

  if (nchar(x.name)>0) if (!exists(x.name, where=data)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "For data frame: ", dname, "\n",
    "This variable does not exist: ", x.name, "\n")
  }

  # get conditions and check for data existing

  # see if the data frame exists, if x not in style Env or function call
  in.style <- .in.global(x.name, dname)
  if (!in.style) {
    if (!exists(dname)) {
      if (dname == "d") 
        txtA <- ", the default data frame name, " else txtA <- " "
      txtB1 <- "Create the labels by reading with Read and labels option\n"
      txtB <- paste(txtB1, sep="")
      cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Data frame ", dname, txtA, "does not exist\n\n", txtB, "\n")
    }
  }

  if (is.null(value)) {  # display an existing label
    if (!is.null(x.name)) {
      gl <- .getlabels()
      lbl <- gl$xl
      if (is.null(lbl)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        "The variable label does not exist for variable: ", x.name, "\n\n")
      }
      return(lbl)
    }
    else {
      l <- attr(data, which="variable.labels")
      for (i in 1:length(l))
        cat(names(l)[i], ": ", l[i], "\n", sep="")
       return(lbl)
    }
  }

  else {  # assign a label to a var in a data frame and return data frame
    l <- attr(data, which="variable.labels")
    lbl.len <- length(l)
    if (x.name %in% names(l)) { #cat("IS IN\n")
      lbl.index <- which(names(l) == x.name)
      indx <- lbl.index
    }
    else
      indx <- length(l) + 1
    l[indx] <- value
    names(l)[indx] <- x.name
    cat("\n")
    cat("Variable Name:",  names(l)[indx], "\n")
    cat("Variable Label:", l[indx], "\n")
    cat("\n")
    attr(data, which="variable.labels") <- l
    return(data) 
  }
  
  cat("\n")
}
