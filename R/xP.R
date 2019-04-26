xP <- function(x, d=NULL, unit=NULL, semi=FALSE) {

  if (is.null(d))
    digits_d <- getOption("digits_d")
  else
    digits_d <- d

  if (!is.na(x)) {

    neg.flag <- FALSE
    if (!is.null(unit)) if (unit == "dollar"){
      digits_d <- 2
      if (x < 0) {
        neg.flag <- TRUE
        x <- abs(x)
    }
  }
    tx <- formatC(x, digits=digits_d, big.mark=",", format="f")

    if (!is.null(unit)) {
      if (unit != "dollar") {  # No $ for display of the value
        if (!semi)
          tx <- paste(tx, unit)
        else
          tx <- paste(tx, "\\:", unit)
      }
      else {
        if (!neg.flag)
          tx <- paste("$", tx, sep="")
        else  # Had removed minus sign, now add back in the front
          tx <- paste("-$", tx, sep="")
        }
    }
    
  }

  else
    tx <- ""

  return(tx)

}

