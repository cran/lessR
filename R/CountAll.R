CountAll <-
function(x=mydata)  {


  dframe.name <- deparse(substitute(x))
  
  if (is.null(x))
    if (!exists(dframe.name, where=.GlobalEnv)) {
      stop("Need to specify an existing data frame,\n",
           "or data frame mydata must exist.", sep="")
  }
    
  cat("\n")
  .dash(25,"-")
  cat(format(Sys.time(), "%a %b %d, %Y at %H:%M"), "\n")
  .dash(25,"-")

  cat("\n\n\n")
  .dash(37,"+")
  cat("Histogram for Each Numeric Variable\n")
  .dash(37,"+")
  Histogram(dframe=x)
  
  cat("\n\n\n")
  .dash(39,"+")
  cat("Bar Chart for Each Non-numeric Variable\n")
  .dash(39,"+")
  BarChart(dframe=x)
  
}
