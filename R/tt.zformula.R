.tt.formula <-
function (my.formula, y=NULL, data, ...) {

# data frame existence check
  dname <- deparse(substitute(data))
  if (!exists(dname)) {
    if (dname == "mydata") 
        txtA <- ", the default data frame name, " else txtA <- " "
      txtB1 <- "So either create data frame by reading with the Read function, or\n"
      txtB2 <- "  specify the actual data frame with the parameter: data\n"
      txtB <- paste(txtB1, txtB2, sep="")
      cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Data frame ", dname, txtA, "does not exist\n\n", txtB, "\n")
  }

  nm <- all.vars(my.formula)  # names of vars in the model

# Y existence check
  if (!exists(nm[1], where=data)) { 
    if (dname == "mydata") {
      txt1 <- ", the default name \n\n"
      txt2 <- "So either make sure you are using the correct variable name, or\n"
      txt3 <- "  specify the actual data frame with the parameter: data\n"
      txt <- paste(txt1, txt2, txt3, sep="")
    }
    else txt <- " "
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Response variable ", nm[1], " does not exist ",
        "in the data frame ", dname, txt, "\n\n")
  }

# Y numeric check
  if (!is.numeric(data[1,nm[1]])) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "You specified ", nm[1], " as the response variable, the 1st variable listed.\n",
    "The response variable must have only numeric values.\n",
    "The first value of ", nm[1], " is ", data[1,nm[1]], ".\n",
    "Perhaps you reversed the order of the variables.\n\n")
  }

# X existence check
  if (!exists(nm[2], where=data)) { 
    if (dname == "mydata") {
      txt1 <- ", the default name \n\n"
      txt2 <- "So either make sure you are using the correct variable name, or\n"
      txt3 <- "  specify the actual data frame with the parameter: data\n"
      txt <- paste(txt1, txt2, txt3, sep="")
    }
    else txt <- " "
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Grouping variable ", nm[2], " does not exist ",
        "in the data frame ", dname, txt, "\n\n")
  }

# X levels = 2 check
  gu <- unique(na.omit(data[,nm[2]]))  # do not consider NA's
  if (length(gu) != 2) {
    gu.out <- ""
    for (i in 1:length(gu)) gu.out <- paste(gu.out, gu[i], sep=" ")
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "The grouping variable for this analysis: ", nm[2], "\n",
    "Values of the grouping variable: ", gu.out, "\n",
    "Number of unique values: ", length(gu), "\n",
    "The grouping variable must have exactly two unique values.\n",
    "Use ANOVA to analyze more than two groups.\n\n")
  }

  vectors <- split(data[,nm[1]], data[,nm[2]])

# now that Y has been broken into two separate vectors, do the analysis  
  return(list(x=vectors[[1]], y=vectors[[2]], Ynm=nm[1],
         Xnm=nm[2], X1nm=names(vectors)[1], X2nm=names(vectors)[2]))

}
