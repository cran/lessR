describe.numeric <-
function(x, digits.d=NULL, lbl=NULL, ...) {

  dashes <- function(ndash) { for (i in 1:(ndash)) cat("-"); cat("\n") }
  
  max.dd <- function(x) {
  
      n.dec <-function(x) {
        xc <- as.character(x)
        nchar(xc)
        ipos <- 0
        for (i in 1:nchar(xc)) if (substr(xc,i,i)==".") ipos <- i
        if (ipos > 0) n.dec <- nchar(xc)-ipos else n.dec <- 0
        return(n.dec)
      }
       
      max.dd <- 0
      for (i in 1:length(x))
        if (!is.na(x[i])) if (n.dec(x[i]) > max.dd ) max.dd <- n.dec(x[i])   
      return(max.dd)
  }
  
  
  cat("\n")

  if (is.null(digits.d)) digits.d <- max.dd(x) + 1
  if (digits.d > 10) {
    cat("\nThese data contain", digits.d, "significant digits.\n")
    cat("Consider specifying a smaller number to display with the  digits  parameter.\n")
    cat("Example for Variables Y and X:  describe2(Y ~ X, digits=3)\n\n")
  }
  
  # width of mean field
  n.char <- nchar(format(sprintf("%.*f", digits.d, round(mean(x), digits.d))))
  w.m <- n.char + 2    
  
  if (is.null(lbl)) lbl <- deparse(substitute(x)) 
  dashes(nchar(lbl))
  cat(lbl, "\n")
  dashes(nchar(lbl))
  
  if (w.m < 6) w.m <- 6
  for (i in 1:4) cat(" ")
  cat("n"); for (i in 1:3) cat(" ")
  cat("Miss"); for (i in 1:3) cat(" ")
  cat("Mean"); for (i in 1:10) cat(" ")
  cat("SD"); for (i in 1:10) cat(" ")
  cat("Min"); for (i in 1:8) cat(" ")
  cat("Median"); for (i in 1:9) cat(" ")
  cat("Max");
  cat("\n")
  dashes(75)
  n.miss <- sum(is.na(x))
  n <- sum(!is.na(x))
  m <- round(mean(x, na.rm=TRUE), digits.d)
  s <- round(sd(x, na.rm=TRUE), digits.d)
  mn <- round(min(x, na.rm=TRUE), digits.d)
  md <- round(median(x, na.rm=TRUE), digits.d)
  mx <- round(max(x, na.rm=TRUE), digits.d)
  p.n <- format(sprintf("%i", n), width=5, justify="right")
  p.n.miss <- format(sprintf("%i", n.miss), width=5, justify="right")
  p.m <- format(sprintf("%.*f", digits.d, m), width=w.m, justify="right")
  p.s <- format(sprintf("%.*f", digits.d, s), width=12, justify="right")
  p.mn <- format(sprintf("%.*f", digits.d, mn), width=12, justify="right")
  p.md <- format(sprintf("%.*f", digits.d, md), width=12, justify="right")
  p.mx <- format(sprintf("%.*f", digits.d, mx), width=12, justify="right")
  cat(p.n, p.n.miss, p.m, p.s, p.mn, p.md, p.mx, "\n")
  dashes(75)

  cat("\n")

}
