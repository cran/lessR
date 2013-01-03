rad.cor <-
function(...)  {

  cat("\n");   warning(call.=FALSE, "\n","------\n",
    " The abbreviations for basic stat functions will generally",
    " consist of just two characters.\n",
    " Now use  rd.cor  instead of  rad.cor, which will be removed in the future.\n\n",
     sep="")

  corRead(...)

}

