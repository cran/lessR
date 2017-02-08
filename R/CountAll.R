CountAll <-
function(x=mydata, quiet=FALSE, ...)  {

  manage.gr <- .graphman()  # see if graphics are to be managed
  if (manage.gr)
    cat("\n>>> Note: Must have pdf=TRUE or run in RStudio.\n\n")


  dname <- deparse(substitute(x))
  options(dname = dname)
  
  if (missing(x))
    if (!exists(dname, where=.GlobalEnv)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Need to specify an existing data frame,\n",
      "or data frame  mydata  must exist.\n\n")
    }
    
  if (!quiet) {
    cat("\n")
    .dash(25,"-")
    cat(format(Sys.time(), "%a %b %d, %Y at %H:%M"), "\n")
    .dash(25,"-")
  }

  if (!quiet) {
    cat("\n\n\n")
    .dash(37,"+")
    cat("Histogram for Each Numeric Variable\n")
    .dash(37,"+")
  }
  Histogram(data=x, quiet=quiet, ...)
  
  if (!quiet) {
    cat("\n\n\n")
    .dash(39,"+")
    cat("Bar Chart for Each Non-numeric Variable\n")
    .dash(39,"+")
  }
  BarChart(data=x, quiet=quiet, ...)
  
}
