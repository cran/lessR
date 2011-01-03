full <-
function(x=NULL)  {

  dashes <- function(ndash, cc) { for (i in 1:(ndash)) cat(cc); cat("\n") }

  if (is.null(x))
    if (!exists("mydata")) 
      stop("Need to specify an existing data frame or data frame mydata must exist.")
      
  cat("\n")
  dashes(25,"-")
  cat(format(Sys.time(), "%a %b %d, %Y at %H:%M"), "\n")
  dashes(25,"-")

  cat("\n\n\n")
  dashes(29,"+")
  cat("Data Summary of Each Variable\n")
  dashes(29,"+")
  describe(x)

  cat("\n\n\n")
  dashes(37,"+")
  cat("Histogram for Each Numerical Variable\n")
  dashes(37,"+")
  color.hist(x)
  
  cat("\n\n\n")
  dashes(39,"+")
  cat("Bar Chart for Each Non-numeric Variable\n")
  dashes(39,"+")
  color.barchart(x)
  
}
