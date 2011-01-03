describe.factor <-
function(x, lbl=NULL, ...)  {

  dashes <- function(ndash) for (i in 1:(ndash)) cat("-")

  cat("\n")
  if (is.null(lbl)) lbl <- deparse(substitute(x))
  dashes(nchar(lbl)); cat("\n")
  cat(lbl, "\n")
  dashes(nchar(lbl)); cat("\n")
  if (nlevels(x) < length(x)) { 
    x.table <- table(x); 
    names(x.table)[0]=" ";  # get rid of variable label
    print(x.table) 
  }
  else cat("\n Appears to contain unique Names or IDs", "\n")
  cat("\n")
  
}
