.reg1anvBasic <-
function(lm.out, dname="mydata", digits.d=NULL, show.R=FALSE) {

  nm <- all.vars(lm.out$terms)  # names of vars in the model
  n.vars <- length(nm)
  n.pred <- n.vars - 1
  n.obs <- nrow(lm.out$model)

  tx <- character(length = 0)

  # ANOVA 
  smc <- anova(lm.out)
  SSE <- smc[n.vars,2]  # pull out before it becomes a character string

  if (show.R) {
    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <-.dash2(68)
    tx[length(tx)+1] <- paste("> ","anova(model)", "\n",sep="")
    tx[length(tx)+1] <-.dash2(68)
  }

  tx[length(tx)+1] <- "Analysis of Variance"

  # lessR standard print function for tables
  #tx2 <- .prntbl(smc, digits.d)
  #for (i in 1:length(tx2)) tx[length(tx)+1] <- tx2[i]

  # width of column 1
  max.c1 <- 0 
  for (i in 1:n.vars) {
    c1 <- nchar(rownames(smc)[i])
    if (c1 > max.c1) max.c1 <- c1 
   }

  # width of data columns
  max.ln <- integer(length=0)
  for (i in 1:4) {
    ln.nm <- nchar(colnames(smc)[i])
    max.ln[i] <- ln.nm + 1
    for (j in 1:nrow(smc)) {
      xjc <- .fmt(smc[j,i], d=digits.d)
      if (nchar(xjc) > max.ln[i]) max.ln[i] <- nchar(xjc)
    }
    max.ln[i] <- max.ln[i] + 1
    if (max.ln[i] < 9) max.ln[i] <- 9
  }

  df.lbl <- .fmtc("     df", max.ln[1]+1)
  SS.lbl <- .fmtc(" Sum Sq", max.ln[2]+1)
  MS.lbl <- .fmtc("Mean Sq", max.ln[3]+1)
  fv.lbl <- .fmtc("F-value", max.ln[4]+1)
  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- paste(eval(format("", width=max.c1-5)), df.lbl, SS.lbl,
                             MS.lbl, fv.lbl, "   p-value", sep="")

  for (i in 1:(n.vars)) {
    rlb <- .fmtc(rownames(smc)[i], max.c1)
    df <- .fmti(smc[i,1], max.ln[1]-5)
    SS <- .fmt((smc[i,2]), digits.d, max.ln[2])
    MS <- .fmt((smc[i,3]), digits.d, max.ln[3])
    fv <- .fmt((smc[i,4]), digits.d, max.ln[4])
    pv <- .fmt((smc[i,5]), digits.d, 9)
    if (i < n.vars)
      tx[length(tx)+1] <- paste(rlb, df, SS, MS, fv, pv)
    else
      tx[length(tx)+1] <- paste(rlb, df, SS, MS) 
  } 

  return(list(tx=tx, SSE=smc[n.vars,2]))

}
