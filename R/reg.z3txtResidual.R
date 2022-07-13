.reg3txtResidual <-
function(lm.out, cook, digits_d=NULL, res_sort="cooks", res_rows=NULL,
         show_R=FALSE) {

  nm <- all.vars(lm.out$terms)  # names of vars in the model
  n.vars <- length(nm)
  n.pred <- n.vars - 1L
  n.keep <- nrow(lm.out$model)
  
  tx <- character(length = 0)

# ----------------------------------------------
# text output

  if (show_R) {
    tx[length(tx)+1] <- .dash2(68)
    tx[length(tx)+1] <- paste("> ", "fitted(model)", sep="", "\n")
    tx[length(tx)+1] <- paste("> ", "resid(model)", sep="", "\n")
    tx[length(tx)+1] <- paste("> ", "rstudent(model)", sep="", "\n")
    tx[length(tx)+1] <- paste("> ", "dffits(model)", sep="", "\n")
    tx[length(tx)+1] <- paste("> ", "cooks.distance(model)", sep="", "\n")
    tx[length(tx)+1] <- .dash2(68)
  }

  tx[length(tx)+1] <- paste("-- Data, Fitted, Residual, Studentized Residual,",
                      "Dffits, Cook's Distance")
  if (res_sort == "cooks")
    tx[length(tx)+1] <- "   [sorted by Cook's Distance]"
  if (res_sort == "rstudent")  
    tx[length(tx)+1] <- "   [sorted by Studentized Residual, ignoring + or - sign]"
  if (res_sort == "dffits")  
    tx[length(tx)+1] <- "   [sorted by dffits, ignoring + or - sign]"
  if (res_rows < n.keep)
    txt <- "rows of data, or do res_rows=\"all\"]"
  else
    txt="]"
  tx[length(tx)+1] <- paste("   [res_rows = ", res_rows, ", out of ", n.keep, " ", txt, sep="")


  fit <- lm.out$fitted.values
  res <- lm.out$residuals
  #cook <- cooks.distance(lm.out)
  
  # text output
  out <- data.frame(fit, res, rstudent(lm.out), dffits(lm.out), cook,
                    stringsAsFactors=TRUE)
  out <- data.frame(lm.out$model[nm[1]], out, stringsAsFactors=TRUE)
  if (n.pred > 0) out <- data.frame(lm.out$model[c(nm[seq(2,n.vars)])], out)

  #out <- data.frame(out)
  names(out)[n.vars+1] <- "fitted"
  names(out)[n.vars+2] <- "resid"
  names(out)[n.vars+3] <- "rstdnt"
  names(out)[n.vars+4] <- "dffits"
  names(out)[n.vars+5] <- "cooks"
  if (res_sort != "off") {
    if (res_sort == "cooks") {
      o <- order(out$cooks, decreasing=TRUE)
      clmn <- 0L
    }
    if (res_sort == "rstudent") {
      o <- order(abs(out$rstdnt), decreasing=TRUE)
      clmn <- 2L
    }
    if (res_sort == "dffits") {
      o <- order(abs(out$dffits), decreasing=TRUE)
      clmn <- 1L
    }
    out <- out[o,]
  }

  tx2 <- .prntbl(out[1:res_rows,], digits_d)
  for (i in 1:length(tx2)) tx[length(tx)+1] <- tx2[i]

  if (res_rows > 5  &&  res_sort != "off") {
    label.top <- numeric(length=5)
    out_top <- numeric(length=5)
    for (i in 1:5) {
      label.top[i] <- rownames(out)[i]
      out_top[i] <- out[i,(ncol(out)-clmn)]
    }
      names(out_top) <- label.top
  }
  else
    out_top <- NA

  return(list(tx=tx, resid.max=out_top))

}
