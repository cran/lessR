.tt.formula <-
function (my.formula, y=NULL, dframe, ...) {

# data frame existence check
  dframe.name <- deparse(substitute(dframe))
  if (!exists(dframe.name)) {
    if (dframe.name == "mydata") 
        txtA <- ", the default data frame name, " else txtA <- " "
      txtB1 <- "So either create data frame by reading with the rad function, or\n"
      txtB2 <- "  specify the actual data frame with the parameter: dframe\n"
      txtB <- paste(txtB1, txtB2, sep="")
      cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Data frame ", dframe.name, txtA, "does not exist\n\n", txtB, "\n")
  }

  nm <- all.vars(my.formula)  # names of vars in the model

# Y existence check
  if (!exists(nm[1], where=dframe)) { 
    if (dframe.name == "mydata") {
      txt1 <- ", the default name \n\n"
      txt2 <- "So either make sure you are using the correct variable name, or\n"
      txt3 <- "  specify the actual data frame with the parameter: dframe\n"
      txt <- paste(txt1, txt2, txt3, sep="")
    }
    else txt <- " "
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Response variable ", nm[1], " does not exist ",
        "in the data frame ", dframe.name, txt, "\n\n")
  }

# Y numeric check
  if (!is.numeric(dframe[1,nm[1]])) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "You specified ", nm[1], " as the response variable, the 1st variable listed.\n",
    "The response variable must have only numeric values.\n",
    "The first value of ", nm[1], " is ", dframe[1,nm[1]], ".\n",
    "Perhaps you reversed the order of the variables.\n\n")
  }

# X existence check
  if (!exists(nm[2], where=dframe)) { 
    if (dframe.name == "mydata") {
      txt1 <- ", the default name \n\n"
      txt2 <- "So either make sure you are using the correct variable name, or\n"
      txt3 <- "  specify the actual data frame with the parameter: dframe\n"
      txt <- paste(txt1, txt2, txt3, sep="")
    }
    else txt <- " "
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Grouping variable ", nm[2], " does not exist ",
        "in the data frame ", dframe.name, txt, "\n\n")
  }

# X levels = 2 check
  gu <- unique(dframe[,nm[2]])
  if (length(gu) != 2) {
    gu.out <- ""
    for (i in 1:length(gu)) gu.out <- paste(gu.out, gu[i], sep=" ")
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "Values of the grouping variable: ", gu.out, "\n",
    "Number of unique values: ", length(gu), "\n",
    "The grouping variable must have exactly two unique values.\n\n")
  }

  vectors <- split(dframe[,nm[1]], dframe[,nm[2]])

# save split variables in global environment
# g1 <- paste(nm[1], ".x", sep="") 
  if (mean(vectors[[1]], na.rm=TRUE) > mean(vectors[[2]], na.rm=TRUE)) {
    assign("group1", vectors[[1]], pos=.GlobalEnv)
    assign("group2", vectors[[2]], pos=.GlobalEnv)
  }
  else {
    assign("group2", vectors[[1]], pos=.GlobalEnv)
    assign("group1", vectors[[2]], pos=.GlobalEnv)
  }

# now that Y has been broken into two separate vectors, do the analysis  
  #tt.default(vectors[[1]], vectors[[2]], Ynm=nm[1], Xnm=nm[2], 
             #X1nm=names(vectors)[1], X2nm=names(vectors)[2], ...)

  return(list(x=vectors[[1]], y=vectors[[2]], Ynm=nm[1],
         Xnm=nm[2], X1nm=names(vectors)[1], X2nm=names(vectors)[2]))

}
