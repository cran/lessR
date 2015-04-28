tP <- function(var, d=NULL) {

  if (is.null(d))
    digits.d <- getOption("digits.d")
  else
    digits.d <- d

  if (!is.na(var))
    tx <- formatC(var, digits=digits.d, big.mark=",", format="f")
  else
    tx <- ""

  return(tx)

}

