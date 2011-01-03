describe.formula <-
function (formula, data=mydata, ...) {
  
  dashes <- function(ndash) for (i in 1:(ndash)) cat("-")
  
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
  

  if ((length(formula) != 3) || (length(attr(terms(formula[-2]),"term.labels")) !=1)) 
  stop("'Formula' missing or incorrect.")
  
  m <- match.call(expand.dots = FALSE) 
  if (is.matrix(eval(m$data, parent.frame()))) m$data <- as.data.frame(data)
  m[[1L]] <- as.name("model.frame")
  if (length(m) == 3) set.digits <- as.numeric(m[[3L]]$digits) else set.digits <- NULL
  m$... <- NULL

  # mf is the relevant Y~X columns of data from dataframe, all Y missing are deleted
  mf <- eval(m, parent.frame())
  Ynm <- names(mf)[1]
  Xnm <- names(mf)[2]
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  
  response <- attr(attr(mf, "terms"), "response")
  if (!is.numeric(mf[[response]])) 
    stop("Response variable ", Ynm, " must be numeric")
  if (is.null(set.digits)) digits.d <- max.dd(mf[[response]]) else digits.d <- set.digits
  if (digits.d > 10  && is.null(set.digits)) {
    cat("\nThese data contain", digits.d, "significant digits.\n")
    cat("Consider specifying a smaller number to display with the  digits  parameter.\n")
    cat("Example for Variables Y and X:  describe2(Y ~ X, digits=3)\n\n")
  }
  
  g <- factor(mf[[-response]])   
  cg <- as.character(g)
  for (i in 1:length(cg)) if (cg[i] == "") cg[i] <- "Null"
  for (i in 1:length(cg)) if (cg[i] == " ") cg[i] <- "Blank"
  rm(g)
  g <- factor(cg)

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
    m <- round(mean(DATA[[i]]), digits.d)
    n.char <- nchar(format(sprintf("%.*f", digits.d, m)))
    if (n.char > w.m) w.m <- n.char
  }
  w.m <- w.m + 2    
 
  cat("\n")
  cat("------------------\n")
  cat(DNAME, "\n")
  cat("------------------\n")
  if (max.char <= nchar("Level")) n.blank <- max.char else n.blank <- max.char-1
  if (w.m < 6) w.m <- 6
  for (i in 1:6) cat(" ")
  cat("non-missing"); for (i in 1:1) cat(" ")
  cat("   "); for (i in 1:1) cat(" ")
  cat("Mean"); for (i in 1:10) cat(" ")
  cat("SD"); for (i in 1:10) cat(" ")
  cat("Min"); for (i in 1:8) cat(" ")
  cat("Median"); for (i in 1:9) cat(" ")
  cat("Max");
  cat("\n")
  dashes(78+n.blank); cat("\n")
  for (i in 1:length(gu)) {
    lv <- levels(gu)[i]
    x <- DATA[[i]]
    n.miss <- sum(is.na(x))
    n <- sum(!is.na(x))
    m <- round(mean(x, na.rm=TRUE), digits.d)
    s <- round(sd(x, na.rm=TRUE), digits.d)
    mn <- round(min(x, na.rm=TRUE), digits.d)
    md <- round(median(x, na.rm=TRUE), digits.d)
    mx <- round(max(x, na.rm=TRUE), digits.d)
    p.lv <- format(lv, width=max.char+3)
    p.n <- format(sprintf("%i", n), width=5, justify="right")
    p.n.miss <- "    "
    p.m <- format(sprintf("%.*f", digits.d, m), width=w.m, justify="right")
    p.s <- format(sprintf("%.*f", digits.d, s), width=12, justify="right")
    p.mn <- format(sprintf("%.*f", digits.d, mn), width=12, justify="right")
    p.md <- format(sprintf("%.*f", digits.d, md), width=12, justify="right")
    p.mx <- format(sprintf("%.*f", digits.d, mx), width=12, justify="right")
    cat(p.lv, p.n, p.n.miss, p.m, p.s, p.mn, p.md, p.mx, "\n")    
  }
  dashes(78+n.blank); cat("\n")
  cat("\n")
  
}
