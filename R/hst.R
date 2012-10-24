hst <-
function(...) {

  cat("\n");   warning(call.=FALSE, "\n","------\n",
    " The abbreviations for basic stat functions will consist of",
    " just two characters.\n",
    " Now use  hs  instead of  hst, which will be removed in the future.\n\n",
     sep="")

  Histogram(...)

}

