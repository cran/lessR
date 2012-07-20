corEFA <- 
function (x=mycor, n.fact, rotate=c("promax", "varimax"), min.load=.2) {

  rotate <- match.arg(rotate)

  if (is.na(min.load)) min.load <- -9999

  cor.name <- deparse(substitute(x))
  if (!exists(cor.name, where=.GlobalEnv)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "No correlation matrix entered.\n\n",
      "Either enter the correct name, or calculate with: Correlation\n",
      "Or read the correlation matrix with: corRead\n\n")
  }

  # EFA
  fa2 <- factanal(covmat=x, factors=n.fact, rotation="none")
  if (n.fact > 1) {
    if (rotate == "promax") rtt <- promax(loadings(fa2))
    if (rotate == "varimax") rtt <- varimax(loadings(fa2))
    ld <- loadings(rtt)
  }
  else ld <- loadings(fa2)

  # identify for each item its factor with highest loading
  n.items <- nrow(x)

  FacItems <- integer(length=n.items)
  for (i in 1:n.items) {
    max.ld <- 0
    FacItems[i] <- 0
    for (j in 1:n.fact) {
      if (abs(ld[i,j]) > max.ld  &&  abs(ld[i,j]) > min.load) {
        max.ld <- ld[i,j]
        FacItems[i] <- j
      }
    }
  }

  # present the MIMM
  cat("\n")
  .dash(36)
  cat("Multiple Indicator Measurement Model\n")
  .dash(36)
  cat("\n")

  cat("The following MIMM is suggested by the exploratory factor\n",
      "analysis. Each MIMM factor is defined by the items that\n",
      "have the highest loading on the corresponding exploratory\n",
      "factor.\n\n", sep="")
  cat("Copy and paste the following lessR instructions to analyze\n",
      "the model with a confirmatory factor analysis.\n\n", sep="")

  cat("crCFA(\n")
  Fac <- integer(length=n.fact)
  n.Fact <- integer(length=n.fact)
  for (i.fact in 1:n.fact) {
    n.Fact[i.fact] <- 0
    k <- 0
    for (j.item in 1:n.items) if (FacItems[j.item] == i.fact) {
      k <- k + 1
      Fac[k] <- j.item
      n.Fact[i.fact] <- n.Fact[i.fact] + 1
    }
    cat(paste("  F", as.character(i.fact), "=c(", sep=""))
    if (n.Fact[i.fact] > 0) {
      for (i in 1:n.Fact[i.fact]) {
        cat(Fac[i], sep="")
        if (i < n.Fact[i.fact]) cat(",")
      }
    }
    cat(")")
    if (i.fact < n.fact) cat(",")
    cat("\n")
  }

  cat(")\n\n")
  deleted <- integer(length=n.items)
  del.count <- 0
  for (i.item in 1:n.items) if (FacItems[i.item] == 0) {
    del.count <- del.count + 1
    deleted[del.count] <- i.item
  }
  if (del.count > 0) {
    cat("Any item with the absolute value of its highest loading\n",
        "from the exploratory factor analysis less than min.load = ", 
        min.load, "\n", "is deleted.\n\n", sep="")
    cat("Deleted items: ")
    for (i.item in 1:del.count) cat(deleted[i.item], " ", sep="")
    cat("\n")
  }

  # present the EFA
  cat("\n\n")
  .dash(30)
  cat("Exploratory Factor Analysis\n")
  .dash(30)
  cat("Extraction:", "maximum likelihood\n")
  if (n.fact > 1) cat("Rotation:", rotate, "\n")
  .dash(30)
  cat("\nLoadings between -0.10 and 0.10 are not listed\n")
  max.c <- nchar(as.character(nrow(ld)))
  for (i in 1:nrow(ld))
    rownames(ld)[i] <- paste(.fmtc(i,max.c), rownames(ld)[i])
  return(ld)
}
