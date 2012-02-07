ss.character <-
function(x, by=NULL, ...)  {

  if (nlevels(factor(x)) < length(x)) { 
    ss.factor(factor(x), by, ...)
  }
  else cat("\n Appears to contain unique Names or IDs", "\n")
  
}
