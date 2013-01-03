Merge <-
function(data1, data2, by=NULL, quiet=FALSE, ...) {

  dname1 <- deparse(substitute(data1))
  dname2 <- deparse(substitute(data2))

  if (!quiet) {
    cat("\n")
    .dash(17)
    cat("Before the merge\n")
    .dash(17)
    cat("\n")
    cat("First five rows of data for first data frame:", dname1, "\n")
    .dash(68)
    print(head(data1, n=5))
    cat("\n")
    cat("First five rows of data for second data frame:", dname2, "\n")
    .dash(68)
    print(head(data2, n=5))
    cat("\n")
  }


  # do the merge
  if (missing(by)) {
    if (!identical(names(data1), names(data2))) { 
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "To do a vertical merge, both data sets must have the same variables.\n\n")
    }
    type <- "vertical"
    mydata <- rbind(data1, data2)
  }

  else {
    type <- "horizontal"
    mydata <- merge(data1, data2, by=by)
  }


  if (!quiet) {
    cat("\n")
    .dash(16+nchar(type))
    cat("After the", type, "merge\n")
    .dash(16+nchar(type))
    cat("\n")
    cat("First five rows of data ")
    cat( "\n")
    .dash(68)
    print(head(mydata, n=5))
  }

  return(mydata)

}
