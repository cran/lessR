to <-
function(prefix, until, from=1, same.size=TRUE) {

  if (from > until) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
       "The value of  until  must be greater than the value of  from .\n\n")
  }

  if (same.size) {
    nc <- nchar(as.character(until))

    cstr <- ""
    for (ichar in (from:until)) { 
      cc <- format(sprintf("%s", ichar), width=nc, justify="right")
      cc <- as.character(.fmtc(cc), w=nc)
      for (i in 1:nchar(cc)) if (substr(cc,i,i) == " ") substr(cc,i,i) <- "0"
      cc <- paste(prefix, cc, sep="")
      cstr <- paste(cstr, cc)
    }

    substr(cstr,1,1) <- ""
    cstr <- strsplit(cstr, " ")[[1]]
  }
  else cstr <- paste(prefix, from:until, sep="")

  return(cstr)

}
