corEFA <- 
function (x=mycor, n_factors, rotate=c("promax", "varimax", "none"), 
          min_loading=.2, sort=TRUE, Rmd=NULL, ...) {


  # a dot in a parameter name to an underscore
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    change <- c("n.factors", "min.loading")
    for (i in 1:length(dots)) {
      if (names(dots)[i] %in% change) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }

  cl <- match.call()

  rotate <- match.arg(rotate)

  if (missing(n_factors)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "The number of factors must be specified with:  n_factors\n\n")
  }

  # cor matrix:  mycor as class out_all, mycor$R, or stand-alone matrix
  cor.nm <- deparse(substitute(x))
  .cor.exists(cor.nm)  # see if matrix exists in one of the 3 locations
  if (class(x) == "out_all")
    x <- eval(parse(text=paste(cor.nm, "$R", sep="")))  # go to $R 
    

  title_efa <- "  EXPLORATORY FACTOR ANALYSIS"

  txer <- ""
  if (is.null(options()$knitr.in.progress)) {
    tx <- character(length = 0)
    tx[length(tx)+1] <- "Extraction: maximum likelihood"
    if (n_factors > 1)  tx[length(tx)+1] <- paste("Rotation:", rotate)
    txer <- tx
  }

  # EFA
  fa2 <- factanal(covmat=x, factors=n_factors, rotation="none", ...)
  if (rotate=="none" || n_factors==1) ld <- as.matrix(fa2$loadings)

  if (n_factors>1  &&  rotate!="none") {
    if (rotate == "promax") rtt <- promax(loadings(fa2))
    if (rotate == "varimax") rtt <- varimax(loadings(fa2))
    ld <- loadings(rtt)
  }

  n.ind <- nrow(ld)

  # sort option
  if (sort) {
    mx <- max.col(abs(ld))
    ind <- cbind(1L:n.ind, mx)
    mx[abs(ld[ind]) < 0.5] <- n_factors + 1
    ld.srt <- ld[order(mx, 1L:n.ind), ]
    ld.srt <- as.matrix(ld.srt)
  }

  # print loadings
  tx <- character(length = 0)
  tx[length(tx)+1] <-  paste("Loadings (except -", min_loading, " to ",
     min_loading, ")", sep="") 
  txld <- .prntbl(ld.srt, digits_d=3, cut=min_loading)
  for (i in 1:length(txld)) tx[length(tx)+1] <- txld[i]
  txld <- tx

  # print sum of squares by factor
  vx <- colSums(ld.srt^2)
  varex <- rbind(`SS loadings` = vx)
  varex <- rbind(varex, `Proportion Var` = vx/n.ind)
  if (n_factors > 1) 
    varex <- rbind(varex, `Cumulative Var` = cumsum(vx/n.ind))
  tx <- character(length = 0)
  tx[length(tx)+1] <- "Sum of Squares"
  txss <- .prntbl(varex, 3)
  for (i in 1:length(txss)) tx[length(tx)+1] <- txss[i]
  txss <- tx


  title_cfa <- "  CONFIRMATORY FACTOR ANALYSIS CODE"

  # generate the MIMM code
  FacItems <- integer(length=n.ind)  # factor with highest loading for item
  Fac <- integer(length=n_factors)
  n.Fact <- integer(length=n_factors)

  for (i in 1:n.ind) {
    max.ld <- 0
    FacItems[i] <- 0
    for (j in 1:n_factors) {
      if (abs(ld[i,j]) > max.ld  &&  abs(ld[i,j]) > min_loading) {
        max.ld <- ld[i,j]
        FacItems[i] <- j
      }
    }
  }


  tx <- character(length = 0)

  tx[length(tx)+1] <- "MeasModel <- "
  for (i.fact in 1:n_factors) {
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
    if (i.fact == n_factors) tx[length(tx)] <- paste(tx[length(tx)], "\n\"")
  }

  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "fit <- lessR::cfa(MeasModel)\n"

  tx[length(tx)+1] <- "library(lavaan)"
  tx[length(tx)+1] <- "fit <- lavaan::cfa(MeasModel, data=d)"
  tx[length(tx)+1] <- "summary(fit, fit.measures=TRUE, standardized=TRUE)"
  txcfa <- tx


  # report any deleted items
  deleted <- integer(length=n.ind)
  del.count <- 0
  for (i.item in 1:n.ind) if (FacItems[i.item] == 0) {
    del.count <- del.count + 1
    deleted[del.count] <- i.item
  }

  txdel <- ""
  tx <- character(length = 0)
  if (del.count > 0) {
    tx[length(tx)+1] <- paste("Deletion threshold: min_loading = ", 
        min_loading, sep="")
    tx[length(tx)+1] <- "Deleted items: "
    for (i.item in 1:del.count)
      tx[length(tx)] <- paste(tx[length(tx)], colnames(x)[deleted[i.item]], " ", sep="")
    txdel <- tx
  }


  # knitr
  txkfl <- ""
  if (!is.null(Rmd)) {
    if (!grepl(".Rmd", Rmd)) Rmd <- paste(Rmd, ".Rmd", sep="")
    txknt <- .corfa.Rmd(n.ind, n_factors)
    cat(txknt, file=Rmd, sep="\n")
    txkfl <- .showfile2(Rmd, "R Markdown instructions")
  }


  class(title_efa) <- "out"
  class(txer) <- "out"
  class(txld) <- "out"
  class(txss) <- "out"
  class(title_cfa) <- "out"
  class(txcfa) <- "out"
  class(txdel) <- "out"

  output <- list(
    out_title_efa=title_efa, out_type=txer, out_loadings=txld, out_ss=txss,
    out_title_cfa=title_cfa, out_cfa_code=txcfa,
    out_deleted=txdel,

    converged=fa2$converged, n_factors=n_factors, ss_factors=vx,
    loadings=ld.srt, call=cl)

  class(output) <- "out_all"
  return(output)

}

