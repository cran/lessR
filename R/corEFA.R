corEFA <- 
function (x=mycor, n.factors, rotate=c("promax", "varimax"), 
          min.loading=.2, show.initial=FALSE, sort=TRUE, ...) {

  rotate <- match.arg(rotate)

  if (is.na(min.loading)) min.loading <- -9999

  cor.name <- deparse(substitute(x))
  if (!exists(cor.name, where=.GlobalEnv)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "No correlation matrix entered.\n\n",
      "Either enter the correct name, or calculate with: Correlation\n",
      "Or read the correlation matrix with: corRead\n\n")
  }

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


  # identify for each item its factor with highest loading
  n.items <- nrow(x)

  FacItems <- integer(length=n.items)
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

  # present the MIMM
  cat("\n\n")
  .dash(36)
  cat("Multiple Indicator Measurement Model\n")
  .dash(36)
  cat("\n")

  cat("The following MIMM and associated scales are suggested\n", 
      "by the exploratory factor analysis. Each MIMM factor is\n",
      "defined by the items that have the highest loading on\n",
      "the corresponding exploratory factor.\n\n", sep="")
  cat("Copy and paste the following lessR instructions to analyze\n",
      "the model with a confirmatory factor analysis.\n\n", sep="")

  cat("corCFA(\n")
  Fac <- integer(length=n.factors)
  n.Fact <- integer(length=n.factors)
  for (i.fact in 1:n.factors) {
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
        cat(colnames(x)[Fac[i]], sep="")
        if (i < n.Fact[i.fact]) cat(",")
      }
    }
    cat(")")
    if (i.fact < n.factors) cat(",")
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
    cat("Any item with the absolute value of its highest loading from\n",
        "the exploratory factor analysis less than min.loading = ", 
        min.loading, "\n", "is deleted.\n\n", sep="")
    cat("Deleted items: ")
    for (i.item in 1:del.count) cat(colnames(x)[deleted[i.item]], " ", sep="")
    cat("\n")
  }

}
