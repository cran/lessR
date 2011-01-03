color.boxplot <-
function(x=NULL, ...)  {

  if (is.null(x)) {
    if (!exists("mydata")) 
      stop("Need to specify an existing data frame or data frame mydata must exist.")
    color.boxplot(mydata, ...) 
  }
  else UseMethod("color.boxplot")
	
}

