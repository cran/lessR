full <-
function(x=mydata)  {

  dashes <- function(ndash, cc) { for (i in 1:(ndash)) cat(cc); cat("\n") }

  if (is.null(x))
    if (!exists("mydata")) 
      stop("Need to specify an existing data frame or data frame mydata must exist.")
      
  cat("\n")
  dashes(25,"-")
  cat(format(Sys.time(), "%a %b %d, %Y at %H:%M"), "\n")
  dashes(25,"-")

  cat("\n\n\n")
  dashes(37,"+")
  cat("Histogram for Each Numerical Variable\n")
  dashes(37,"+")
  hst(dframe=x)
  
  cat("\n\n\n")
  dashes(39,"+")
  cat("Bar Chart for Each Non-numeric Variable\n")
  dashes(39,"+")
  bc(dframe=x)
  
}
