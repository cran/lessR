print.out_all <- function(x, ...) {

  for (ilist in 1:length(x)) {
    if (substr(names(x[ilist]),1,4) == "out_") {
      if (nchar(x[ilist]) > 0) {
        cat("\n\n")
        for (i in 1:length(x[[ilist]])) cat(x[[ilist]][i], "\n")
      }
    }
  }

  cat("\n")

}
