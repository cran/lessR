corListVars <-
function (x=mycor) {

  cor.name <- deparse(substitute(x))
  if (!exists(cor.name, where=.GlobalEnv)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "No correlation matrix entered.\n\n",
      "Either enter the correct name, or calculate with: Correlation\n",
      "Or read the correlation matrix with: corRead\n\n")
  }

  NItems <- nrow(x)
  for (i in 1:NItems) cat(i, rownames(x)[i], "\n") 

}
