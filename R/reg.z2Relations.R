.reg2Relations <- 
function(lm.out, dname, n.keep, show.R,
         cor, collinear, subsets, numeric.all, in.data.frame) {

  nm <- all.vars(lm.out$terms)  # names of vars in the model
  n.vars <- length(nm)
  n.pred <- n.vars - 1
  n.obs <- nrow(lm.out$model)

  # -------------------------
  # Relations among Variables
  # -------------------------
  tx <- character(length = 0)

  tx[length(tx)+1] <- "  RELATIONS AMONG VARIABLES"

  # correlations
  txall <- ""
  if (cor) {
    tx[length(tx)+1] <- "" 
    tx[length(tx)+1] <- "Correlations\n"
  
    if (numeric.all && in.data.frame) {

      if (show.R) {
        cv <- paste("\"",nm[1],"\"", sep="")
        for (i in 2:n.vars) cv <- paste(cv, ",\"", nm[i], "\"", sep="")
        tx[length(tx)+1] <- .dash2(68)
        tx[length(tx)+1] <- paste("> ", "cor(", dname, "[c(", cv, ")])", "\n", sep="")
        tx[length(tx)+1] <- .dash2(68)
      }

      tx <- "Correlation Matrix"
      crs <- cor(lm.out$model[c(nm)])
      txcrs <- .prntbl(crs, 2, cc=" ")
      for (i in 1:length(txcrs)) tx[length(tx)+1] <- txcrs[i]

    }

  else {  # not all numeric
    tx[length(tx)+1] <- ">>> No correlations reported, not all variables are"
    if (!in.data.frame) tx[length(tx)] <- paste(tx[length(tx)], "in the data frame.\n")
    if (!numeric.all) tx[length(tx)] <- paste(tx[length(tx)], "numeric.\n")
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

    tx[length(tx)+1] <- "Collinearity"

    if (numeric.all) {

      VIF <- vif(lm.out)
      tol <- 1/VIF

      out <- cbind(VIF, tol)
      colnames(out) <- c("VIF", "Tolerance")
      rownames(out) <- nm[2:length(nm)]

      txcln <- .prntbl(out, 3, cc=" ")
      for (i in 1:length(txcln)) tx[length(tx)+1] <- txcln[i]

    }
    else { 
      tx[length(tx)+1] <- ">>> No collinearity analysis, not all variables are numeric.\n"
      VIF <- NA
      tol <- NA
    }

    txcln <- tx
  } 

    else {  # !collinear
      VIF <- NA
      tol <- NA
    }

  # -------------------------
  # all possible subsets of predictor variables    
  txall <- ""

  if (subsets) {
    tx <- character(length = 0)

    tx[length(tx)+1] <- "All Possible Subset Regressions"

    if (numeric.all) {
      X <- data.frame(lm.out$model[nm[seq(2,n.vars)]])
      Y <- numeric(length=n.keep)  # convert response to an atomic vector for leaps
      for (i in 1:n.keep) Y[i] <- lm.out$model[nm[1]][i,1]
      lp.out <- leaps(X, Y, method="adjr2")  # leaps function
      md <- lp.out$which  # md is logical
      #md <- matrix(as.integer(md), nrow=nrow(md), ncol=ncol(md), byrow=TRUE)
      rownames(md) <- 1:nrow(md)  # matrix md does not have proper row names
      models <- data.frame(md, lp.out$adjr2, lp.out$size-1)  # gives 0, 1
      #models <- data.frame(cbind(md, lp.out$adjr2, lp.out$size-1))  
      names(models) <- c(names(X), "R2adj", "X's")
      mod.srt <- models[order(models$R2adj, decreasing=TRUE),]
      names(mod.srt)[ncol(mod.srt)-1] <- "   R2adj"
      lines <- min(50, nrow(mod.srt))

      # width of data columns
      max.ln <- integer(length=0)
      for (i in 1:ncol(X)) {
        ln.nm <- nchar(colnames(X)[i])
        max.ln[i] <- ln.nm + 1
        if (max.ln[i] < 4) max.ln[i] <- 4
      }

      tx[length(tx)+1] <- ""
      tx[length(tx)+1] <- ""
      for (i in 1:(n.pred+2)) {
        if (i <= n.pred) ww <- max.ln[i]
        if (i == n.pred+1) ww <- 9
        if (i == n.pred+2) ww <- 7
        tx[length(tx)] <- paste(tx[length(tx)], .fmtc(names(mod.srt)[i], w=ww), sep="")
      }

      for (i in 1:lines) {
        tx[length(tx)+1] <- ""
        for(j in 1:n.pred) {
          tx[length(tx)] <- paste(tx[length(tx)], .fmti(mod.srt[i,j],  w=max.ln[j]), sep="")
        }
         tx[length(tx)] <- paste(tx[length(tx)], .fmt(mod.srt[i,n.pred+1], d=3, w=8))
         tx[length(tx)] <- paste(tx[length(tx)], .fmti(mod.srt[i,n.pred+2], w=6))
      }

      if (lines > 50)
        tx[length(tx)+1] <- "\n>>> Only first 50 rows printed.\n\n"

      #out <- cbind(
      #colnames(out) <- paste(colnames(crs), "R2adj", "X's")

      }

    else  # not numeric.all
      tx[length(tx)+1] <- ">>> No subset analysis reported, not all variables are numeric.\n"

    txall <- tx
  }

  return(list(txcor=txcor, txcln=txcln, txall=txall, crs=crs, tol=tol, VIF=VIF))

}
