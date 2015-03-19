.reg3txtResidual <-
function(lm.out, cook, digits.d=NULL, res.sort="cooks", res.rows=NULL, show.R=FALSE) {

  nm <- all.vars(lm.out$terms)  # names of vars in the model
  n.vars <- length(nm)
  n.keep <- nrow(lm.out$model)
  
  tx <- character(length = 0)

# ----------------------------------------------
# text output

  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "  ANALYSIS OF RESIDUALS AND INFLUENCE"
  tx[length(tx)+1] <- ""

  if (show.R) {
    tx[length(tx)+1] <- .dash2(68)
    tx[length(tx)+1] <- paste("> ", "fitted(model)", sep="", "\n")
    tx[length(tx)+1] <- paste("> ", "resid(model)", sep="", "\n")
    tx[length(tx)+1] <- paste("> ", "rstudent(model)", sep="", "\n")
    tx[length(tx)+1] <- paste("> ", "dffits(model)", sep="", "\n")
    tx[length(tx)+1] <- paste("> ", "cooks.distance(model)", sep="", "\n")
    tx[length(tx)+1] <- .dash2(68)
  }

  tx[length(tx)+1] <- "Data, Fitted, Residual, Studentized Residual, Dffits, Cook's Distance"
  if (res.sort == "cooks")
    tx[length(tx)+1] <- "   [sorted by Cook's Distance]"
  if (res.sort == "rstudent")  
    tx[length(tx)+1] <- "   [sorted by Studentized Residual, ignoring + or - sign]"
  if (res.sort == "dffits")  
    tx[length(tx)+1] <- "   [sorted by dffits, ignoring + or - sign]"
  if (res.rows < n.keep)
    txt <- "cases (rows) of data, or do res.rows=\"all\"]"
  else
    txt="]"
  tx[length(tx)+1] <- paste("   [res.rows = ", res.rows, ", out of ", n.keep, " ", txt, sep="")


  fit <- lm.out$fitted.values
  res <- lm.out$residuals
  #cook <- cooks.distance(lm.out)
  
  # text output
  out <- cbind(fit, res, rstudent(lm.out), dffits(lm.out), cook)
  out <- cbind(lm.out$model[c(nm[seq(2,n.vars)],nm[1])], out)
  out <- data.frame(out)
  names(out)[n.vars+1] <- "fitted"
  names(out)[n.vars+2] <- "residual"
  names(out)[n.vars+3] <- "rstudent"
  names(out)[n.vars+4] <- "dffits"
  names(out)[n.vars+5] <- "cooks"
  if (res.sort != "off") {
    if (res.sort == "cooks") o <- order(out$cooks, decreasing=TRUE)
    if (res.sort == "rstudent") o <- order(abs(out$rstudent), decreasing=TRUE)
    if (res.sort == "dffits") o <- order(abs(out$dffits), decreasing=TRUE)
    out <- out[o,]
  }

  tx2 <- .prntbl(out[1:res.rows,], digits.d)
  for (i in 1:length(tx2)) tx[length(tx)+1] <- tx2[i]

  if (res.rows > 5  &&  res.sort == "cooks") {
    label.top <- numeric(length=0)
    out.top <- numeric(length=0)
    for (i in 1:5) {
      label.top[i] <- rownames(out)[i]
      out.top[i] <- out[i,ncol(out)]  # cook's distance
    }
      names(out.top) <- label.top
  }
  else
    out.top <- NA

  return(list(tx=tx, cooks.max=out.top))

}
