describe.data.frame <-
function(x, ...)  {

  dashes <- function(ndash) { for (i in 1:(ndash)) cat("-"); cat("\n") }

  for (i in 1:ncol(x)) {
    if (is.numeric(x[1,i])) 
      describe(x[,i], lbl=names(x)[i], ...)
    else {
      if (is.factor(x[,i])) {
        cat("\n")
        lbl <- names(x)[i]
        dashes(nchar(lbl))
        cat(lbl, "\n")
        dashes(nchar(lbl))
        if (nlevels(x[,i]) < length(x[,i])) {
          print(table(x[,i]))
        }
        else {
          cat("\n Appears to contain unique Names or IDs", "\n")
        }
        cat("\n\n")
      }
    }
  }

}
