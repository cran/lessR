rad2 <- 
function(..., sep=";", dec=",") {

  cat("\n");   warning(call.=FALSE, "\n","------\n",
    " The abbreviations for basic stat functions will generally",
    " consist of just two characters.\n",
    " Now use  rd2  instead of  rad2, which will be removed in the future.\n\n",
     sep="")
         
  Read(..., sep=";", dec=",")

}
