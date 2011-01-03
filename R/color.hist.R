color.hist <-
function(x=NULL, ...)  {

  if (is.null(x)) {
    if (!exists("mydata")) 
      stop("Need to specify an existing data frame or data frame mydata must exist.")
    color.hist(mydata, ...) 
  }
  else UseMethod("color.hist")
	
}

