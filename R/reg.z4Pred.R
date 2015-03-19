.reg4Pred <-
function(lm.out, brief, 
         n.keep, digits.d, show.R,
         new.data, pred.sort, pred.rows, scatter.3D, scatter.coef,
         numeric.all, in.data.frame, X1.new, 
         X2.new, X3.new, X4.new, X5.new) {

  nm <- all.vars(lm.out$terms)  # names of vars in the model
  n.vars <- length(nm)
  n.pred <- n.vars - 1
  n.obs <- nrow(lm.out$model)

  tx <- character(length = 0)

# --------------------
# prediction intervals
# --------------------
     
  tx[length(tx)+1] <- "  FORECASTING ERROR"
  tx[length(tx)+1] <- ""

  if (show.R) {
    txt <- "predict(model, interval=\"prediction\")"
    tx[length(tx)+1] <- paste("> ", txt, sep="", "\n")
    txt <- "predict(model, interval=\"confidence\")"
    tx[length(tx)+1] <- paste("> ", txt, sep="", "\n")
    tx[length(tx)+1] <- .dash2(68)
  }

  tx[length(tx)+1] <- "Data, Fitted Values, Confidence and Prediction Intervals"
  tx[length(tx)+1] <- "   [sorted by lower bound of prediction interval]"
  if (pred.rows < n.keep  &&  !new.data) 
    tx[length(tx)+1] <- "   [to save space only some intervals printed, or do pred.rows=\"all\"]"
  
  if (!new.data) {
    c.int <- data.frame(predict(lm.out, interval="confidence"))
    p.int <- suppressWarnings(data.frame(predict(lm.out, interval="prediction")))
    p.width <- p.int$upr - p.int$lwr
    out <- cbind(lm.out$model[c(nm[seq(2,n.vars)],nm[1])],
                 c.int, p.int$lwr, p.int$upr, p.width)
  }
  else {
    Xnew.val <- list(X1.new)
    if (n.vars > 2) for (i in 2:(n.pred)) {
      pp <- eval(parse(text=paste("X", toString(i),".new",sep="")))
      Xnew.val <- c(Xnew.val, list(pp))
    }
    Xnew <- expand.grid(Xnew.val)
    for (i in 1:(n.pred)) names(Xnew)[i] <- nm[i+1]
    c.int <- data.frame(predict(lm.out, interval="confidence", newdata=Xnew))
    p.int <- data.frame(predict(lm.out, interval="prediction", newdata=Xnew))
    p.width <- p.int$upr - p.int$lwr
    Ynew <- character(length = nrow(Xnew))
    Ynew <- ""
    out <- cbind(Xnew, Ynew, c.int, p.int$lwr, p.int$upr, p.width)
    names(out)[n.vars] <- nm[1]
  }

  out <- data.frame(out)
  if (pred.sort == "predint") {
    o <- order(out[,n.vars+4])  # lower bound of prediction interval
    out <- out[o,]
  }

  names(out)[n.vars+1] <- "fitted"
  names(out)[n.vars+2] <- "ci:lwr"
  names(out)[n.vars+3] <- "ci:upr"
  names(out)[n.vars+4] <- "pi:lwr"
  names(out)[n.vars+5] <- "pi:upr"
  names(out)[n.vars+6] <- " width"

  if (!new.data) {
    if (pred.rows == n.keep) {
      tx2 <- .prntbl(out, digits.d)
      for (i in 1:length(tx2)) tx[length(tx)+1] <- tx2[i]
    }
    else {
      piece.rows <- round(pred.rows/3,0)
      if (piece.rows < 1) piece.rows <- 1
      tx2 <- .prntbl(out[1:piece.rows,], digits.d)
      for (i in 1:length(tx2)) tx[length(tx)+1] <- tx2[i]
      tx[length(tx)+1] <- paste("\n... for the middle", piece.rows, "rows of sorted data ...")
      n.mid <- n.keep/2
      tx2 <- .prntbl(out[(n.mid-(piece.rows/2)):(n.mid+((piece.rows/2)-1)),], digits.d)
      for (i in 1:length(tx2)) tx[length(tx)+1] <- tx2[i]
      tx[length(tx)+1] <- paste("\n... for the last", piece.rows, "rows of sorted data ...")
      tx2 <- .prntbl(out[(nrow(out)-(piece.rows-1)):nrow(out),], digits.d)
      for (i in 1:length(tx2)) tx[length(tx)+1] <- tx2[i]
    }
  }
  else { 
    tx2 <- .prntbl(out, digits.d)   # want row.names=FALSE
    for (i in 1:length(tx2)) tx[length(tx)+1] <- tx2[i]
  }

  return(list(cint=c.int, pint=p.int, tx=tx))  # need these in 5Plot next
}
