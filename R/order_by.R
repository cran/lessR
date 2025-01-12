order_by <-
function(data=d, by, direction=NULL, quiet=getOption("quiet"), ...) {


  if (missing(by)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Specify the variables to sort by first listing the data frame\n",
      "or preceding the variables with:  by\n\n")
  }

  dname <- deparse(substitute(data))
  n.obs <- nrow(data)

  all.vars <- as.list(seq_along(data))
  names(all.vars) <- names(data)

  if (!quiet) {
    cat("\nSort Specification\n")
  }

  # do special keywords: row.names, random
  if (deparse(substitute(by)) == "row.names") {
    ord <- "order(row.names(data)"
    cat(" ", "row.names", "-->")

    if (!is.null(direction)) {
      if (direction[1] == "+") txt <- "ascending"
      else if (direction[1] == "-") txt <- "descending"
      else {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Value of direction, the sort direction specification: ", direction[1],
        "\n\n",
        "Permissible values are + for ascending and - for descending.\n\n")
      }
    }
    else 
      txt <- "ascending"

    ord.txt <- "decreasing=FALSE"
    if (txt =="descending") ord.txt <- "decreasing=TRUE"
    ord <- paste(ord, ",", ord.txt, ",...)", sep="")
    cat(" ", txt, "\n") 
  }  # end row.names

  else if (deparse(substitute(by)) == "random") {
    cat(" ", "random\n")
    rand.rows <- sample(1:n.obs, size=n.obs, replace=FALSE)
    ord <- paste("order(", "rand.rows", ", ...)", sep="")
  }  # end sort random

  else {  # sort variable(s)

    # columns to sort
    by.col <- eval(substitute(by), envir=all.vars, enclos=parent.frame())
    n.sort <- length(by.col)

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
    else
      for (i in 1:n.sort) direction[i] <- "+"

    # console output
    if (!quiet) {
      for (i in 1:n.sort) {
        nm <- names(data)[by.col[i]]
        cat(" ", nm, "-->")
        if (direction[i] == "+") txt <- "ascending"
        else if (direction[i] == "-") txt <- "descending"
        else {
          cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Value of direction, the sort direction specification: ",
          direction[i], "\n\n",
          "Permissible values are + for ascending and - for descending.\n\n")
        }
        cat(" ", txt, "\n") 
      }
    }

    # construct the call to the order function
    ord <- ""
    for (i in 1:n.sort) {  # xtfrm() needed for factors
      ord <- paste(ord, direction[i], 
                   "xtfrm(data[,by.col[", toString(i),"]])", sep="")
      if (i < n.sort) ord <- paste(ord, ",", sep="")
        
    }
    ord <- paste("order(", ord, ")", sep="")
  }  # end sort variables


  # do the sort
  o <- eval(parse(text=ord))
  d <- data[o, ]

  cat("\n")
  return(d)

}

