rad.brief <-
function(..., quiet=TRUE) {

  cat("\n");   warning(call.=FALSE, "\n","------\n",
    " The abbreviations for basic stat functions will generally",
    " consist of just two characters.\n",
    " Now use  Read(..., quiet=TRUE)  instead of  rad.brief, to be removed in the future.\n\n",
     sep="")

  Read(..., quiet=TRUE)

 }
