Sort <-
function(by, direction=NULL, dframe=mydata, ...) {

  n.obs <- nrow(dframe)

  all.vars <- as.list(seq_along(dframe))
  names(all.vars) <- names(dframe)

  by.col <- eval(substitute(by), envir=all.vars, enclos=parent.frame())
  n.sort <- length(by.col)

  cat("\nSort Specification\n")
  .dash(31)

  if (length(by.col) == 1  &&  by.col[1] == "row.names") {
    ord <- "order(row.names(dframe))"
    cat(" ", "row.names", "-->")
    if (!is.null(direction)) {
      if (direction[1] == "+") txt <- "ascending"
      else if (direction[1] == "-") txt <- "descending"
      else {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Value of direction, the sort direction specification: ", direction[1], "\n\n",
        "Only permissible values are + for ascending and - for descending.\n\n")
      }
    }
    else txt <- "+"
    cat(" ", txt, "\n") 
  }

  else {

    if (!is.null(direction)) {
      if (n.sort != length(direction)) { 
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Number of specified variables to sort: ", n.sort, "\n",
        "Number of + and - signs to indicate direction of sort: ", 
          length(direction), "\n\n",
        "The same number of values must be specified for both\n",
        "the list of values and the list of the sort direction.\n\n")
      }
    }
    else for (i in 1:n.sort) direction[i] <- "+"

    # console output
    for (i in 1:n.sort) {
      cat(" ", names(dframe)[by.col[i]], "-->")
      if (direction[i] == "+") txt <- "ascending"
      else if (direction[i] == "-") txt <- "descending"
      else {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Value of direction, the sort direction specification: ", direction[i], "\n\n",
        "Only permissible values are + for ascending and - for descending.\n\n")
      }
      cat(" ", txt, "\n") 
    }

    fvar <- matrix(nrow=n.obs, ncol=n.sort)  # max num of factors is n.sort
    i.fvar <- 0

    # construct the call to the order function
    ord <- ""
    for (i in 1:n.sort) { 
      if ("factor" %in% class(dframe[, by.col[i]])) {  # 2 attributes if ordered
        i.fvar <- i.fvar + 1  # another factor variable
        fvar[, i.fvar] <- as.character(dframe[, by.col[i]])
        for (i.row in 1:n.obs)  # replace value with factor integer prefixed 
          fvar[i.row, i.fvar] <- 
             paste(toString(as.numeric(dframe[, by.col[i]])[i.row]), 
                   as.character(fvar[i.row, i.fvar]), sep="")
        ord <- paste(ord, direction[i], "xtfrm(fvar[, ", toString(i.fvar), "]), ",
                     sep="")
      }
      else   # variable not a factor
        ord <- paste(ord, direction[i], 
                     "dframe[, by.col[", toString(i), "]], ", sep="")
    }
    ord <- paste("order(", ord, "...)", sep="")
  }

  # finish the console output
  .dash(31)
  cat("\n")

  # do the sort and return
  o <- eval(parse(text=ord))
  return(dframe[o, ])

}
