.reg2Relations <- 
function(lm.out, dname, n.keep, show_R,
         cor, collinear, subsets, best_sub, max.sublns, 
         in.data.frame, sterrs, MSW) {

  nm <- all.vars(lm.out$terms)  # names of vars in the model
  n.vars <- length(nm)
  n.pred <- n.vars - 1L
  n.obs <- nrow(lm.out$model)

  # -------------------------
  # Relations among Variables
  # -------------------------
  tx <- character(length = 0)

  # correlations
  if (cor) {
  
    if (in.data.frame) {

      if (show_R) {
        cv <- paste("\"",nm[1],"\"", sep="")
        for (i in 2:n.vars) cv <- paste(cv, ",\"", nm[i], "\"", sep="")
        tx[length(tx)+1] <- .dash2(68)
        tx[length(tx)+1] <- paste("> ",
                   "cor(", dname, "[c(", cv, ")])", "\n", sep="")
        tx[length(tx)+1] <- .dash2(68)
      }

      if (is.null(options()$knitr.in.progress))
        tx[length(tx)+1] <- "-- Correlation Matrix\n"
      crs <- cor(lm.out$model[c(nm)])
      txcrs <- .prntbl(crs, 2, cc=NULL)
      for (i in 1:length(txcrs)) tx[length(tx)+1] <- txcrs[i]
    }

    else {  # not in data frame
      tx[length(tx)+1] <- ">>> No correlations reported, some variables not"
      tx[length(tx)] <- paste(tx[length(tx)], "in the data frame.")
      crs <- numeric(length=0)
      crs <- NA
    }
  
    txcor <- tx

  }  # cor

  else
    crs <- NA
  

  # -------------------------
  # collinearity    

  txcln <- ""
  if (collinear) {
    tx <- character(length = 0)

    if (is.null(options()$knitr.in.progress)) {
      tx[length(tx)+1] <- "-- Collinearity"
      tx[length(tx)+1] <- ""
    }

    vif <- numeric(length = 0)
    tol <- numeric(length = 0)
    for (i in 1:n.pred) {
      v <- var(lm.out$model[i+1])
      vif[i] <- (v * (n.keep-1) * sterrs[i+1]^2) / MSW
      tol[i] <- 1 / vif[i]
    }

    out <- cbind(tol, vif)
    colnames(out) <- c("Tolerance", "      VIF")
    rownames(out) <- nm[2:length(nm)]

    txcln <- .prntbl(out, 3, cc=NULL)
    for (i in 1:length(txcln)) tx[length(tx)+1] <- txcln[i]

    txcln <- tx
  } 

    else {  # !collinear
      vif <- NA
      tol <- NA
    }

  # -------------------------
  # all possible subsets of predictor variables    
  txsbs <- ""
  if (subsets) {
    tx <- character(length = 0)

    if (is.null(options()$knitr.in.progress)) {
      tx[length(tx)+1] <- "-- Best Subset Regression Models"
      if (n.pred > 5)
        tx[length(tx)+1] <- "up to 10 subsets of each number of predictors"
      tx[length(tx)+1] <- ""
    }

    X <- data.frame(lm.out$model[nm[seq(2,n.vars)]])
    Y <- numeric(length=n.keep)  # convert response to atomic vector for leaps
    for (i in 1:n.keep) Y[i] <- lm.out$model[nm[1]][i,1]
    lp.out <- leaps(X, Y, method=best_sub)  # leaps function
    md <- lp.out$which  # md is logical
    rownames(md) <- 1:nrow(md)  # matrix md does not have proper row names
    # gives 0, 1
    if (best_sub == "adjr2") {
      models <- data.frame(md, lp.out$adjr2, lp.out$size-1)
      names(models) <- c(names(X), "R2adj", "X's")
      mod.srt <- models[order(models$R2adj, decreasing=TRUE),]

    }
    else {
      models <- data.frame(md, lp.out$Cp, lp.out$size-1)
      names(models) <- c(names(X), "   Cp", "X's")
      mod.srt <- models[order(models$'   Cp', decreasing=FALSE),]
    }
    best_sub <- ifelse(best_sub == "adjr2", "R2adj", best_sub)
    names(mod.srt)[ncol(mod.srt)-1L] <- paste("   ", best_sub, sep="")
    lines <- min(max.sublns, nrow(mod.srt))

    # width of data columns
    max.ln <- integer(length=ncol(X))
    for (i in 1:ncol(X)) {
      ln.nm <- nchar(colnames(X)[i])
      max.ln[i] <- ln.nm + 1
      if (max.ln[i] < 4) max.ln[i] <- 4L
    }

    tx[length(tx)+1] <- ""  # build the line of variable names
    for (i in 1:(n.pred)) {
      if (i <= n.pred) ww <- max.ln[i]
      tx[length(tx)] <- paste(tx[length(tx)],
          .fmtc(names(mod.srt)[i], w=ww), sep="")
      nms <- tx[length(tx)]
    }
    for (i in 1:2) {
      if (i == 1) ww <- 9L
      if (i == 2) ww <- 7L
      tx[length(tx)] <- paste(tx[length(tx)], .fmtc(names(mod.srt)[n.pred+i],
                              w=ww), sep="")
    }
    for (i in 1:lines) {
      if (lines > 40) if (i %% 30 == 0) tx[length(tx)+1] <- nms
      tx[length(tx)+1] <- ""
      for(j in 1:n.pred)
        tx[length(tx)] <- paste(tx[length(tx)], .fmti(mod.srt[i,j],
          w=max.ln[j]), sep="")
      tx[length(tx)] <- paste(tx[length(tx)], .fmt(mod.srt[i,n.pred+1],
          d=3, w=8)) # R2adj
      tx[length(tx)] <- paste(tx[length(tx)], .fmti(mod.srt[i,n.pred+2],
          w=6))  # num X's
    }

    if (nrow(mod.srt) > max.sublns)
      tx[length(tx)+1] <- paste(
        "\n>>> Only first", lines, "of", nrow(mod.srt), "rows printed\n",
        "   To indicate more, add subset=n, where n is the number of lines")

    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste("[based on Thomas Lumley's leaps function",
                        "from the leaps package]")

    txsbs <- tx
  }  # end subsets

  return(list(out_cor=txcor, out_collinear=txcln, out_subsets=txsbs, crs=crs, tol=tol, vif=vif))

}
