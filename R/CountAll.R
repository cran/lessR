CountAll <-
function(x=mydata)  {

  if (is.null(x))
    if (!exists("mydata")) 
      stop("Need to specify an existing data frame or data frame mydata must exist.")
      
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
