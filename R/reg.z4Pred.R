.reg4Pred <-
function(lm.out, nm, my.formula, brief, 
         n.vars, n.pred, n.obs, n.keep, digits.d, explain, show.R, pre, line,
         new.data, pred.sort, pred.rows, scatter.3D, scatter.coef,
         numeric.all, in.data.frame, X1.new, 
         X2.new, X3.new, X4.new, X5.new) {

# --------------------
# prediction intervals
# --------------------
     
  cat( "\n\n", "  FORECASTING ERROR", "\n")

  if (show.R) {
    txt <- "predict(model, interval=\"prediction\")"
    cat(line, pre, txt, sep="", "\n")
    txt <- "predict(model, interval=\"confidence\")"
    cat(pre, txt, sep="", "\n")
    .dash(68)
  }
  else cat("\n")
  
  if (explain) {
    .dash(68)
    cat("The 'standard deviation of the residuals', assumed to be the same\n",
        "for each set of values of the predictor variables, estimates the\n",
        "modeling error. However, even for predictions from the current data\n",
        "from which the model is estimated, the forecasts are based on future\n",
        "responses from the collection of new data. That is, the sampling\n",
        "error of the sample regression line must also be considered in the\n",
        "assessment of forecasting error. The amount of sampling error varies\n",
        "depending on the values of the predictor variables.\n",
        "\n",
        "The 95% confidence interval around each fitted value of the sample\n",
        "regression model is given below, as well as the likely range of\n",
        "forecasting error, the 95% prediction interval, the expected range\n",
        "in which the actual future value of the response variable, ", nm[1], ", \n",
        "will likely be found.  This forecasting error depends on both modeling\n", 
        "error and sampling error.\n", sep="")
    .dash(68)
    cat("\n")
  }

  cat("Data, Fitted Values, Confidence and Prediction Intervals\n")
  cat("   [sorted by lower bound of prediction interval]\n")
  if (pred.rows < n.keep  &&  !new.data) 
    cat("   [to save space only some intervals printed, or pred.rows=\"all\"]\n")
  .dash(66)
  
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
    p.int <- suppressWarnings(data.frame(predict(lm.out,
                              interval="prediction", newdata=Xnew)))
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

  for (i in 1:(n.vars+6))
    if (is.numeric(out[,i])) if (!is.integer(out[,i]))
      out[,i] <- .fmt(out[,i],digits.d-1)

  if (!new.data) {
    if (pred.rows == n.keep)
      print(out)
    else {
      print(out[1:pred.rows,])
      cat("\n... for the middle", pred.rows, "rows of sorted data ...\n\n")
      n.mid <- n.keep/2
      print(out[(n.mid-(pred.rows/2)):(n.mid+((pred.rows/2)-1)),])
      cat("\n... for the last", pred.rows, "rows of sorted data ...\n\n")
      print(out[(n.keep-(pred.rows-1)):n.keep,])
    }
  }
  else 
    print(out, digits=digits.d, row.names=FALSE)

  .dash(68)

  return(list(cint=c.int, pint=p.int))  # need these in 5Plot next
}
