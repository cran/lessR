tNum <- function(x, uc=FALSE) {
 
  x <- as.integer(round(x,0))

       if (x == 0) num.c <- "None"
  else if (x == 1) num.c <- "One"
  else if (x == 2) num.c <- "Two"
  else if (x == 3) num.c <- "Three"
  else if (x == 4) num.c <- "Four"
  else if (x == 5) num.c <- "Five"
  else if (x == 6) num.c <- "Six"
  else if (x == 7) num.c <- "Seven"
  else if (x == 8) num.c <- "Eight"
  else if (x == 9) num.c <- "Nine"
  else if (x == 10) num.c <- "Ten"
  else if (x == 11) num.c <- "Eleven"
  else if (x == 12) num.c <- "Twelve"
  else num.c <- .fmti(x)

  if (!uc) num.c <- tolower(num.c)

  return(num.c)
}

