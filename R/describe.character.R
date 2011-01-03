describe.character <-
function(x, lbl=NULL, ...)  {

  if (is.null(lbl)) lbl <- deparse(substitute(x))
  if (nlevels(factor(x)) < length(x)) { 
    describe(factor(x), lbl=lbl, ...)
  }
  else cat("\n Appears to contain unique Names or IDs", "\n")
  
}
