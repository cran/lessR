.reg4Pred <-
function(lm.out, 
         n.keep, digits_d, show_R,
         new.data, pred_sort, pred_rows, scatter_coef,
         in.data.frame, X1_new, X2_new, X3_new, X4_new, X5_new, X6_new) {

  nm <- all.vars(lm.out$terms)  # names of vars in the model
  n.vars <- length(nm)
  n.pred <- n.vars - 1L
  n.obs <- nrow(lm.out$model)

  tx <- character(length = 0)

# --------------------
# prediction intervals
# --------------------
     
  if (show_R) {
    txt <- "predict(model, interval=\"prediction\")"
    tx[length(tx)+1] <- paste("> ", txt, sep="", "\n")
    txt <- "predict(model, interval=\"confidence\")"
    tx[length(tx)+1] <- paste("> ", txt, sep="", "\n")
    tx[length(tx)+1] <- .dash2(68)
  }

  if (!new.data) {
    c.int <- data.frame(predict(lm.out, interval="confidence"),
                        stringsAsFactors=TRUE)
    p.int <- suppressWarnings(predict(lm.out,
                                      interval="prediction", se.fit=TRUE))
    s.prederr <- sqrt(p.int$residual.scale^2 + p.int$se.fit^2)
    p.width <- p.int$fit[,"upr"] - p.int$fit[,"lwr"]
    out <- cbind(lm.out$model[nm[1]],
             p.int$fit[,"fit"], s.prederr, p.int$fit[,"lwr"],
             p.int$fit[,"upr"], p.width)
    if (n.pred > 0) out <- cbind(lm.out$model[c(nm[seq(2,n.vars)])], out)
    out <- data.frame(out, stringsAsFactors=TRUE)
  }
  else {
    Xnew.val <- list(X1_new)
    if (n.vars > 2) for (i in 2:(n.pred)) {
      pp <- eval(parse(text=paste("X", toString(i),"_new",sep="")))
      Xnew.val <- c(Xnew.val, list(pp))
    }
    Xnew <- expand.grid(Xnew.val)
    for (i in 1:(n.pred)) names(Xnew)[i] <- nm[i+1]
    c.int <- data.frame(predict(lm.out, interval="confidence", newdata=Xnew),
                        stringsAsFactors=TRUE)
    p.int <- predict(lm.out, interval="prediction", newdata=Xnew,
                                 se.fit=TRUE)
    s.prederr <- sqrt(p.int$residual.scale^2 + p.int$se.fit^2)
    p.width <- p.int$fit[,"upr"] - p.int$fit[,"lwr"]
    Ynew <- character(length = nrow(Xnew))  # Y becomes non-numeric
    Ynew <- ""
    out <- data.frame(Xnew, Ynew, p.int$fit[,"fit"], s.prederr,
                      p.int$fit[,"lwr"], p.int$fit[,"upr"],
                      p.width, stringsAsFactors=FALSE)
    names(out)[n.vars] <- nm[1]
  }

  if (pred_sort == "predint") {
    o <- order(out[,n.vars+3])  # lower bound of pred interval
    out <- out[o,]
  }

  names(out)[n.vars+1] <- "pred"
  names(out)[n.vars+2] <- "s_pred"
  names(out)[n.vars+3] <- "pi.lwr"
  names(out)[n.vars+4] <- "pi.upr"
  names(out)[n.vars+5] <- "width"

  # manually do calc for pi:upr to verify
  #tcut <- qt(.025, df=n.obs-n.vars, lower.tail=FALSE)
  #s.upper <- out$fitted + tcut*s.prederr
 
  # min and max of pred interval widths
  predmm <- numeric(length=2)
  predmm[1] <- min(out[n.vars+5])
  predmm[2] <- max(out[n.vars+5])

  min_row <- which(out[n.vars+5] == min(out[n.vars+5]))
  if (length(min_row) > 1) min_row <- min_row[1]
  min_nm <- rownames(out[min_row,])

  max.row <- which(out[n.vars+5] == max(out[n.vars+5]))
  if (length(max.row) > 1) max.row <- max.row[1]
  max.side <- ifelse (max.row < n.keep/2, max.side <- TRUE, max.side <- FALSE)
  max.nm <- rownames(out[max.row,])

  names(predmm) <- c(min_nm, max.nm)


  if (!new.data) {

    if (pred_rows == nrow(out)) {
      r <- 1:nrow(out)
      tx2 <- .prntbl(out[r,], digits_d, cc=NULL)
    }

    else {
      piece.rows <- round(pred_rows/3,0)
      if (piece.rows < 1) piece.rows <- 1
      pr2 <- floor(piece.rows/2)

      if (max.side)
        r1 <- max(max.row-pr2,1):(max.row+pr2)
      else
        r1 <- 1:piece.rows

      r2 <- (min_row-pr2):(min_row+pr2)

      if (!max.side)
        r3 <- (max.row-pr2):(min(max.row+pr2,nrow(out)))
      else {
        r3 <- (nrow(out)-(piece.rows-1)):nrow(out)
      }

      r <- c(r1,r2,r3)
      b1 <- which(r == r2[1])
      b2 <- which(r == r3[1])
      tx2 <- .prntbl(out[r,], digits_d, brk=c(b1,b2), cc=NULL)
    }
  }

  else   # new data 
    tx2 <- .prntbl(out, digits_d)   # prefer to have row.names=FALSE

  for (i in 1:length(tx2)) tx[length(tx)+1] <- tx2[i]


  p.int <- data.frame(cbind(p.int$fit[,"lwr"], p.int$fit[,"upr"]),
                      stringsAsFactors=TRUE)
  names(p.int) <- c("lwr", "upr")

  # need in 5Plot next
  return(list(cint=c.int, pint=p.int, out_predict=tx, predmm=predmm)) 
}
