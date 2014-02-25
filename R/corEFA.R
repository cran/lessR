corEFA <- 
function (x=mycor, n.factors, rotate=c("promax", "varimax"), 
          min.loading=.2, show.initial=FALSE, sort=TRUE, ...) {

  rotate <- match.arg(rotate)

  if (is.na(min.loading)) min.loading <- -9999

  if (missing(n.factors)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "The number of factors must be specified with:  n.factors\n\n")
  }

  cor.name <- deparse(substitute(x))
  if (!exists(cor.name, where=.GlobalEnv)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "No correlation matrix entered.\n\n",
      "Either enter the correct name, or calculate with: Correlation\n",
      "Or read the correlation matrix with: corRead\n\n")
  }

  n.items <- nrow(x)

  # EFA
  fa2 <- factanal(covmat=x, factors=n.factors, rotation="none", ...)
  if (show.initial || n.factors==1){
    cat("\n\n")
    .dash(30)
    cat("Exploratory Factor Analysis\n")
    .dash(30)
    cat("Extraction:", "maximum likelihood\n")
    .dash(30)
    cat("\nLoadings between -", min.loading, " and ", min.loading, 
        " are not listed\n", sep="")
    print(fa2, cutoff=min.loading, ...)
    ld <- fa2$loadings
  }

  if (n.factors > 1) {
    if (rotate == "promax") rtt <- promax(loadings(fa2))
    if (rotate == "varimax") rtt <- varimax(loadings(fa2))
    ld <- loadings(rtt)
    cat("\n\n")
    .dash(30)
    cat("Exploratory Factor Analysis\n")
    .dash(30)
    if (!show.initial) cat("Extraction:", "maximum likelihood\n")
    cat("Rotation:", rotate, "\n")
    .dash(30)
    cat("\nLoadings between -", min.loading, " and ", min.loading, 
        " are not listed\n", sep="")
    print(ld, cutoff=min.loading, sort=sort, ...)
  }


  # generate the MIMM code

  FacItems <- integer(length=n.items)  # factor with highest loading for item
  for (i in 1:n.items) {
    max.ld <- 0
    FacItems[i] <- 0
    for (j in 1:n.factors) {
      if (abs(ld[i,j]) > max.ld  &&  abs(ld[i,j]) > min.loading) {
        max.ld <- ld[i,j]
        FacItems[i] <- j
      }
    }
  }

  cat("\n\n")
  .dash(36)
  cat("Multiple Indicator Measurement Model\n")
  .dash(36)
  cat("\n")

  cat("Each MIMM factor is defined by the items that have the\n",
      "highest loading on the corresponding exploratory factor.\n\n", sep="")

  Fac <- integer(length=n.factors)
  n.Fact <- integer(length=n.factors)

  cat("lessR code for iterated centroid confirmatory factor analysis\n")
  .dash(10)

  cat("corCFA(\n")
  for (i.fact in 1:n.factors) {
    n.Fact[i.fact] <- 0
    k <- 0
    for (j.item in 1:n.items) if (FacItems[j.item] == i.fact) {
      k <- k + 1
      Fac[k] <- j.item
      n.Fact[i.fact] <- n.Fact[i.fact] + 1
    }
    cat(paste("  F", as.character(i.fact), " = c(", sep=""))
    if (n.Fact[i.fact] > 0) {
      for (i in 1:n.Fact[i.fact]) {
        cat(colnames(x)[Fac[i]], sep="")
        if (i < n.Fact[i.fact]) cat(",")
      }
    }
    cat(")")
    if (i.fact < n.factors) cat(",")
    cat("\n")
  }
  cat(")\n\n")

  cat("lavaan code for maximum likelihood confirmatory factor analysis\n")
  .dash(11)

  cat("library(lavaan)\n")
  cat("MeasModel <-\n")
  for (i.fact in 1:n.factors) {
    n.Fact[i.fact] <- 0
    k <- 0
    if (i.fact == 1) cat("\"") else cat(" ")
    for (j.item in 1:n.items) if (FacItems[j.item] == i.fact) {
      k <- k + 1
      Fac[k] <- j.item
      n.Fact[i.fact] <- n.Fact[i.fact] + 1
    }
    cat(paste("  F", as.character(i.fact), " =~ ", sep=""))
    if (n.Fact[i.fact] > 0) {
      for (i in 1:n.Fact[i.fact]) {
        cat(colnames(x)[Fac[i]], sep="")
        if (i < n.Fact[i.fact]) cat(" + ")
      }
    }
    if (i.fact == n.factors) cat(" \"")
    cat("\n")
  }
  cat("fit <- cfa(MeasModel, data=mydata)\n")
  cat("summary(fit, fit.measures=TRUE, standardized=TRUE)\n")

  # report any deleted items
  deleted <- integer(length=n.items)
  del.count <- 0
  for (i.item in 1:n.items) if (FacItems[i.item] == 0) {
    del.count <- del.count + 1
    deleted[del.count] <- i.item
  }
  if (del.count > 0) {
    cat("\n\n")
    cat("Deleted items\n")
    .dash(13)
    cat("Deletion threshold: min.loading = ", 
        min.loading, "\n\n", sep="")
    cat("Deleted items: ")
    for (i.item in 1:del.count) cat(colnames(x)[deleted[i.item]], " ", sep="")
    cat("\n\n")
  }
  else
    cat("\n")

}
