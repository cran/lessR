# insert  commas  and an  and  between the elements of a character vector  
xAnd <- function(x) {

  n <- length(x)
  lt <- x[1] 
  if (n > 1) {
    if (n > 2) for (i in 2:(n-1)) lt <- paste(lt, ", ", x[i], sep="")
    lt <- paste(lt, "and", x[n])
  }

  return(lt)
}

