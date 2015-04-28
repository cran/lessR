.ss.data.frame <-
function(x, n.cat, brief, ...)  {

  for (i in 1:ncol(x)) {
    cat("\n\n")

    nu <- length(unique(na.omit(x[,i])))

    x.name <- names(x)[i]
    options(xname = x.name)

    if (is.numeric(x[,i]) && nu > n.cat) {
      stuff <- .ss.numeric(x[,i], brief=brief, ...)
      txsts <- stuff$tx
      txotl <- .outliers2(x[,i])
      class(txsts) <- "out_piece"
      class(txotl) <- "out_piece"
      output <- list(out_stats=txsts, out_outliers=txotl)
      class(output) <- "out_all"
      print(output)
    }

    else if (is.factor(x[,i]) || is.character(x[,i]) || nu <= n.cat) {
      stuff <- .ss.factor(x[,i], ...)
      txttl <- stuff$title
      txsts <- stuff$tx
      class(txttl) <- "out_piece"
      class(txsts) <- "out_piece"
      output <- list(out_title_freq=txttl, out_stats=txsts)
      class(output) <- "out_all"
      print(output)

      if (is.numeric(x[,i]) && nu <= n.cat)
        cat("\n>>> Variable is numeric, but only has", nu, "<= n.cat =", n.cat, "levels,",
        "so treat as categorical.\n",
        "   To obtain the numeric summary, decrease  n.cat  to indicate a lower\n",
        "   number of unique values such as with function: set.\n", 
        "   Perhaps make this variable a factor with the R factor function.\n")
    }

    else cat("\n>>> The following type of variable not processed: ", 
             class(x[,i]), "\n")

  }

}
