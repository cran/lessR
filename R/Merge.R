Merge <-
function(data1, data2, by=NULL, quiet=getOption("quiet"), ...) {

  if (missing(data1)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Specify first data frame (table) to merge:  data1\n\n")
  }

  if (missing(data2)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Specify second data frame (table) to merge:  data2\n\n")
  }

  dname1 <- deparse(substitute(data1))
  dname2 <- deparse(substitute(data2))


  # do the merge
  if (by == "rows") {
    if (!identical(names(data1), names(data2))) { 
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "To do a vertical merge, both data sets must have the same variables.",
        "\n\n")
    }
    l <- attr(data1, which="variable.labels") # save variable labels
    myunits <- attr(data1, which="variable.units") # save variable units
    type <- "vertical"
    data <- rbind(data1, data2)
  }

  else {
    type <- "horizontal"
    l1 <- attr(data1, which="variable.labels") # save variable labels
    l2 <- attr(data2, which="variable.labels") # save variable labels
    myunits1 <- attr(data1, which="variable.units") # save variable units
    myunits2 <- attr(data2, which="variable.units") # save variable units
    data <- merge(data1, data2, by=by, ...)
    l <- c(l1, l2)
    myunits <- c(myunits1, myunits2)
  }


  # restore any variable labels, units
  if (!is.null(l)) attr(data, which="variable.labels") <- l
  if (!is.null(myunits)) attr(data, which="variable.units") <- myunits

  return(data)

}
