.bc.stat <-
function (x, y, by.var, stat, y.name) {

  if (stat == "sum") {
    ylab <- paste("Sum of", y.name)
    if (is.null(by.var))
      out <- tapply(y, x, sum, na.rm=TRUE)
    else 
      out <- aggregate(y ~ x + by.var, FUN=sum)
  }
  if (stat == "mean") {
    ylab <- paste("Mean of", y.name)
    if (is.null(by.var))
      out <- tapply(y, x, mean, na.rm=TRUE)
    else 
      out <- aggregate(y ~ x + by.var, FUN=mean)
  }
  if (stat == "sd") {
    ylab <- paste("Standard Deviation of", y.name)
    if (is.null(by.var))
      out <- tapply(y, x, sd, na.rm=TRUE)
    else 
      out <- aggregate(y ~ x + by.var, FUN=sd)
  }
  if (stat == "deviation") {
    ylab <- paste("Mean Deviation of", y.name)
    if (is.null(by.var)) {
      out <- tapply(y, x, mean, na.rm=TRUE)
      out <- out - mean(out, na.rm=TRUE)
    }
    else { 
      cat("\n"); stop(call.=FALSE, "\n","------\n",
      "deviation  value for  stat  not meaningful with a by variable\n\n")
    }
  }
  if (stat == "min") {
    ylab <- paste("Minimum of", y.name)
    if (is.null(by.var))
      out <- tapply(y, x, min, na.rm=TRUE)
    else 
      out <- aggregate(y ~ x + by.var, FUN=min)
  }
  if (stat == "median") {
    ylab <- paste("Median of", y.name)
    if (is.null(by.var))
      out <- tapply(y, x, median, na.rm=TRUE)
    else 
      out <- aggregate(y ~ x + by.var, FUN=median)
  }
  if (stat == "max") {
    ylab <- paste("Maximum of", y.name)
    if (is.null(by.var))
      out <- tapply(y, x, max, na.rm=TRUE)
    else 
      out <- aggregate(y ~ x + by.var, FUN=max)
  }

  if (sum(is.na(out)) > 0) { # y and a summary table, then no stat
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "The summary table of the transformed data has some missing\n",
      "   values, likely as a result of too few data values in\n",
      "   some cells to be able to calculate the specified statistic\n\n")
  }      

  return(list(out=out, ylab=ylab))
}
