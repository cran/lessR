corPrint <- function(R, min_value=0)  {

  crs <- .prntbl(R, digits_d=2, cut=min_value, cc=NULL, cors=TRUE)

  tx <- character(length = 0)
  for (i in 1:length(crs)) tx[length(tx)+1] <- crs[i]
  class(tx) <- "out"

  return(tx)
}
