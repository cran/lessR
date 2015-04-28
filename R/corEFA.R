corEFA <- 
function (x=mycor, n.factors, rotate=c("promax", "varimax", "none"), 
          min.loading=.2, sort=TRUE, knitr.file=NULL, ...) {

  cl <- match.call()

  rotate <- match.arg(rotate)

  if (missing(n.factors)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "The number of factors must be specified with:  n.factors\n\n")
  }

  # cor matrix:  mycor as class out_all, mycor$cors, or stand-alone matrix
  cor.nm <- deparse(substitute(x))
  .cor.exists(cor.nm)  # see if matrix exists in one of the 3 locations
  if (class(x) == "out_all")
    x <- eval(parse(text=paste(cor.nm, "$cors", sep="")))  # go to $cors 
    

  tx <- character(length = 0)
  tx[length(tx)+1] <- "Exploratory Factor Analysis"
  tx[length(tx)+1] <- .dash2(30)
  tx[length(tx)+1] <- "Extraction: maximum likelihood"
  if (n.factors > 1)  tx[length(tx)+1] <- paste("Rotation:", rotate)
  tx[length(tx)+1] <- .dash2(30)
  txttl <- tx

  # EFA
  fa2 <- factanal(covmat=x, factors=n.factors, rotation="none", ...)
  if (rotate=="none" || n.factors==1) ld <- as.matrix(fa2$loadings)

  if (n.factors>1  &&  rotate!="none") {
    if (rotate == "promax") rtt <- promax(loadings(fa2))
    if (rotate == "varimax") rtt <- varimax(loadings(fa2))
    ld <- loadings(rtt)
  }

  n.ind <- nrow(ld)

  # sort option
  if (sort) {
    mx <- max.col(abs(ld))
    ind <- cbind(1L:n.ind, mx)
    mx[abs(ld[ind]) < 0.5] <- n.factors + 1
    ld.srt <- ld[order(mx, 1L:n.ind), ]
    ld.srt <- as.matrix(ld.srt)
  }

  # print loadings
  tx <- character(length = 0)
  tx[length(tx)+1] <-  paste("Loadings (except -", min.loading, " to ",
     min.loading, ")", sep="") 
  txld <- .prntbl(ld.srt, digits.d=3, cut=min.loading)
  for (i in 1:length(txld)) tx[length(tx)+1] <- txld[i]
  txld <- tx

  # print sum of squares by factor
  vx <- colSums(ld.srt^2)
  varex <- rbind(`SS loadings` = vx)
  varex <- rbind(varex, `Proportion Var` = vx/n.ind)
  if (n.factors > 1) 
    varex <- rbind(varex, `Cumulative Var` = cumsum(vx/n.ind))
  tx <- character(length = 0)
  tx[length(tx)+1] <- "Sum of Squares"
  txss <- .prntbl(varex, 3)
  for (i in 1:length(txss)) tx[length(tx)+1] <- txss[i]
  txss <- tx


  # generate the MIMM code

  FacItems <- integer(length=n.ind)  # factor with highest loading for item
  for (i in 1:n.ind) {
    max.ld <- 0
    FacItems[i] <- 0
    for (j in 1:n.factors) {
      if (abs(ld[i,j]) > max.ld  &&  abs(ld[i,j]) > min.loading) {
        max.ld <- ld[i,j]
        FacItems[i] <- j
      }
    }
  }


  Fac <- integer(length=n.factors)
  n.Fact <- integer(length=n.factors)

  tx <- character(length = 0)
  tx[length(tx)+1] <- "lessR code for iterated centroid confirmatory factor analysis"
  tx[length(tx)+1] <- .dash2(10)

  tx[length(tx)+1] <- "corCFA("
  for (i.fact in 1:n.factors) {
    n.Fact[i.fact] <- 0
    k <- 0
    for (j.item in 1:n.ind) if (FacItems[j.item] == i.fact) {
      k <- k + 1
      Fac[k] <- j.item
      n.Fact[i.fact] <- n.Fact[i.fact] + 1
    }
    tx[length(tx)+1] <- (paste("  F", as.character(i.fact), " = c(", sep=""))
    if (n.Fact[i.fact] > 0) {
      for (i in 1:n.Fact[i.fact]) {
        tx[length(tx)] <- paste(tx[length(tx)], colnames(x)[Fac[i]], sep="")
        if (i < n.Fact[i.fact]) tx[length(tx)] <- paste(tx[length(tx)], ", ", sep="")
      
      }
      tx[length(tx)] <- paste(tx[length(tx)], ")", sep="")
      if (i.fact < n.factors) tx[length(tx)] <- paste(tx[length(tx)], ",", sep="")
    }
  }
  tx[length(tx)+1] <- ")"
  txmgp <- tx


  tx <- character(length = 0)
  tx[length(tx)+1] <- "lavaan code for maximum likelihood confirmatory factor analysis"
  tx[length(tx)+1] <- .dash2(11)

  tx[length(tx)+1] <- "library(lavaan)"
  tx[length(tx)+1] <- "MeasModel <- "
  for (i.fact in 1:n.factors) {
    n.Fact[i.fact] <- 0
    k <- 0
    for (j.item in 1:n.ind) if (FacItems[j.item] == i.fact) {
      k <- k + 1
      Fac[k] <- j.item
      n.Fact[i.fact] <- n.Fact[i.fact] + 1
    }
    if (i.fact == 1)
      tx[length(tx)+1] <- "\""
    else
      tx[length(tx)+1] <- " "
    tx[length(tx)] <- paste(
      tx[length(tx)], "  F", as.character(i.fact), " =~ ", sep="")
    if (n.Fact[i.fact] > 0) {
      for (i in 1:n.Fact[i.fact]) {
        tx[length(tx)] <- paste(tx[length(tx)], colnames(x)[Fac[i]], sep="")
        if (i < n.Fact[i.fact])
          tx[length(tx)] <- paste(tx[length(tx)], " + ", sep="")
      }
    }
    if (i.fact == n.factors) tx[length(tx)] <- paste(tx[length(tx)], "\"")
  }
  tx[length(tx)+1] <- "fit <- cfa(MeasModel, data=mydata)"
  tx[length(tx)+1] <- "summary(fit, fit.measures=TRUE, standardized=TRUE)"
  txlvn <- tx


  # report any deleted items
  deleted <- integer(length=n.ind)
  del.count <- 0
  for (i.item in 1:n.ind) if (FacItems[i.item] == 0) {
    del.count <- del.count + 1
    deleted[del.count] <- i.item
  }

  tx <- character(length = 0)
  if (del.count > 0) {
    tx[length(tx)+1] <- "Deleted items"
    tx[length(tx)+1] <- .dash2(13)
    tx[length(tx)+1] <- paste("Deletion threshold: min.loading = ", 
        min.loading, "\n", sep="")
    tx[length(tx)+1] <- "Deleted items: "
    for (i.item in 1:del.count)
      tx[length(tx)] <- paste(tx[length(tx)], colnames(x)[deleted[i.item]], " ", sep="")
  }
  else
    tx <- ""
  txdel <- tx


  # knitr
  txkfl <- ""
  if (!is.null(knitr.file)) {
    if (grepl(".Rmd", knitr.file)) txt <- "" else txt <- ".Rmd"
    knitr.file <- paste(knitr.file, txt, sep="") 
    txknt <- .corfa.knitr(n.ind, n.factors)
    cat(txknt, file=knitr.file, sep="\n")
    txkfl <- .showfile2(knitr.file, "knitr instructions")
  }


  class(txttl) <- "out_piece"
  class(txld) <- "out_piece"
  class(txss) <- "out_piece"
  class(txmgp) <- "out_piece"
  class(txlvn) <- "out_piece"
  class(txdel) <- "out_piece"

  output <- list(out_title=txttl, out_loadings=txld, out_sum_squares=txss,
    out_ice=txmgp, out_lavaan=txlvn, out_deleted=txdel,
    converged=fa2$converged, n_factors=n.factors, ss_factors=vx, loadings=ld.srt,
    call=cl)

  class(output) <- "out_all"
  return(output)

}

