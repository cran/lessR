describe.formula <-
function (formula, data=mydata, ...) {
  
  
  max.dd <- function(x) {
  
    n.dec <- function(x) {    
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
  
  
  if ((length(formula) != 3) || (length(attr(terms(formula[-2]),"term.labels")) !=1)) 
  stop("'Formula' missing or incorrect.")
  
   m <- match.call(expand.dots = FALSE) 
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data <- as.data.frame(data)
  m[[1L]] <- as.name("model.frame")
  if (length(m) == 3) set.digits <- as.numeric(m[[3L]]$digits)
    else set.digits <- NULL
  m$... <- NULL
  mf <- eval(m, parent.frame())
  Ynm <- names(mf)[1]
  Xnm <- names(mf)[2]
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  
  
  response <- attr(attr(mf, "terms"), "response")
  if (!is.numeric(mf[[response]])) 
    stop("Response variable ", Ynm, " must be numeric")
  if (is.null(set.digits)) digits <- max.dd(mf[[response]]) else digits <- set.digits
  if (digits > 10  && is.null(set.digits)) {
    cat("\nThese data contain", digits, "significant digits.\n")
    cat("Consider specifying a smaller number to display with the  digits  parameter.\n")
    cat("Example for Variables Y and X:  describe2(Y ~ X, digits=3)\n\n")
  }
  od <- digits + 1  #  out digits
  
  g <- factor(mf[[-response]])   
  gu <- unique(g)
  max.char <- 0
  for (i in 1:nlevels(gu)) {
    if (nchar(levels(gu)[i]) > max.char) max.char <- nchar(levels(gu)[i])
  }
  
  
  DATA <- split(mf[[response]], g)
  attach(DATA, warn.conflicts=FALSE)
  
  # width of mean field
  w.m <- 0
  for (i in 1:length(gu)) {
    m <- round(mean(DATA[[i]]), od)
    n.char <- nchar(format(sprintf("%.*f", od, m)))
    if (n.char > w.m) w.m <- n.char
  }
  
  cat("\n")
  cat("------------------\n")
  cat(DNAME, "\n")
  cat("------------------\n")
  cat("\n")
  if (max.char <= nchar("Level")) n.blank <- max.char else n.blank <- max.char-1
  if (w.m < 6) w.m <- 6
  cat("Level"); for (i in 1:(round(((w.m/2)+max.char-2),0))) cat(" ")
  cat("Mean"); for (i in 1:10) cat(" ")
  cat("SD"); for (i in 1:10) cat(" ")
  cat("Min"); for (i in 1:8) cat(" ")
  cat("Median"); for (i in 1:9) cat(" ")
  cat("Max");
  cat("\n")
  for (i in 1:(65+n.blank)) cat("-"); cat("\n")
  for (i in 1:length(gu)) {
    lv <- levels(gu)[i]
    y <- DATA[[i]]
    m <- round(mean(y), od)
    s <- round(sd(y), od)
    mn <- round(min(y), od)
    md <- round(median(y), od)
    mx <- round(max(y), od) 
    p.lv <- format(lv, width=max.char+3)
    p.m <- format(sprintf("%.*f", od, m), width=w.m, justify="right")
    p.s <- format(sprintf("%.*f", od, s), width=12, justify="right")
    p.mn <- format(sprintf("%.*f", od, mn), width=12, justify="right")
    p.md <- format(sprintf("%.*f", od, md), width=12, justify="right")
    p.mx <- format(sprintf("%.*f", od, mx), width=12, justify="right")
    cat(p.lv, p.m, p.s, p.mn, p.md, p.mx, "\n")
  }
  for (i in 1:(65+n.blank)) cat("-"); cat("\n")
  cat("\n")
  
}
