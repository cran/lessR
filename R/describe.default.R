describe.default <-
function(x, digits=NULL, lbl=NULL, ...) {
  
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
      if (n.dec(x[i]) > max.dd) max.dd <- n.dec(x[i])   
    return(max.dd)
  }
  
  cat("\n")

  if (is.null(digits)) digits <- max.dd(x)
  if (digits > 10  && is.null(digits)) {
    cat("\nThese data contain", digits, "significant digits.\n")
    cat("Consider specifying a smaller number to display with the  digits  parameter.\n")
    cat("Example for Variables Y and X:  describe2(Y ~ X, digits=3)\n\n")
  }
  
  od <- digits + 1  #  out digits
  # width of mean field
  w.m <- 0
  m <- round(mean(x), od)
  n.char <- nchar(format(sprintf("%.*f", od, m)))
  if (n.char > w.m) w.m <- n.char
    
  
  if (is.null(lbl)) lbl <- deparse(substitute(x)) 
  dashes(nchar(lbl))
  cat(lbl, "\n")
  dashes(nchar(lbl))
  
  cat("\n")
  if (w.m < 6) w.m <- 6
  for (i in 1:3) cat(" ")
  cat("Mean"); for (i in 1:10) cat(" ")
  cat("SD"); for (i in 1:10) cat(" ")
  cat("Min"); for (i in 1:8) cat(" ")
  cat("Median"); for (i in 1:9) cat(" ")
  cat("Max");
  cat("\n")
  dashes(65)
    m <- round(mean(x), od)
    s <- round(sd(x), od)
    mn <- round(min(x), od)
    md <- round(median(x), od)
    mx <- round(max(x), od) 
    p.m <- format(sprintf("%.*f", od, m), width=w.m, justify="right")
    p.s <- format(sprintf("%.*f", od, s), width=12, justify="right")
    p.mn <- format(sprintf("%.*f", od, mn), width=12, justify="right")
    p.md <- format(sprintf("%.*f", od, md), width=12, justify="right")
    p.mx <- format(sprintf("%.*f", od, mx), width=12, justify="right")
    cat(p.m, p.s, p.mn, p.md, p.mx, "\n")
  dashes(65)

	cat("\n")

}