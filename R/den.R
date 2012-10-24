den <-
function(...) {

  cat("\n");   warning(call.=FALSE, "\n","------\n",
    " The abbreviations for basic stat functions will consist of",
    " just two characters.\n",
    " Now use  dn  instead of  den, which will be removed in the future.\n\n",
     sep="")

  Density(...)

}

