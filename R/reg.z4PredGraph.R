.reg4PredGraph <-
function(nm, mydframe, my.formula, brief, res.rows,
         n.vars, n.pred, n.obs, n.keep, digits.d, explain, show.R, pre, line,
         new.data, pred.sort, pred, pred.all, scatter.3d, scatter.coef,
         numeric.all, in.data.frame, colors, graphics.save, X1.new, 
         X2.new, X3.new, X4.new, X5.new) {

# --------------------
# prediction intervals
# --------------------
if (pred) {
     
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
  if (n.keep > 50 && pred.all == FALSE && !new.data) 
    cat("   [to save space only some intervals printed, do pred.all=TRUE to see all]\n")
  .dash(68)
  
  if (!new.data) {
    c.int <- data.frame(predict(lm.out, interval="confidence"))
    p.int <- suppressWarnings(data.frame(predict(lm.out, interval="prediction")))
    out <- cbind(lm.out$model[c(nm[seq(2,n.vars)],nm[1])],c.int,p.int$lwr,p.int$upr)
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
    p.int <- suppressWarnings(data.frame(predict(lm.out, interval="prediction", newdata=Xnew)))
    Ynew <- character(length = nrow(Xnew))
    Ynew <- ""
    out <- cbind(Xnew, Ynew, c.int, p.int$lwr, p.int$upr)
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
  if (n.keep < 50  || pred.all == TRUE || new.data)
    print(out, digits=digits.d)
  else {
    print(out[1:5,], digits=digits.d)
    cat("\n... for the middle 5 rows of sorted data ...\n\n")
    n.mid <- round(n.keep/2)
    print(out[(n.mid-2):(n.mid+2),], digits=digits.d)
    cat("\n... for the last 5 rows of sorted data ...\n\n")
    print(out[(n.keep-4):n.keep,], digits=digits.d)
  }
  .dash(68)
}

# ------------
# Scatterplots
# ----------
  if (res.rows > 0) dev.set(which=5) else dev.set(which=3)

  if (n.pred == 1) {  # scatterplot, if one predictor variable
    if ( (pred==FALSE) || is.factor(lm.out$model[,nm[2]]) || !is.null(X1.new) ) 
     do.int <- FALSE
    else do.int <- TRUE
    if (!do.int) {
      ctitle <- "Scatterplot and Regression Line"
      y.min <- min(lm.out$model[,nm[1]])
      y.max <- max(lm.out$model[,nm[1]])
    }
    else {
      ctitle <- "Regression Line,\nConfidence and Prediction Intervals"
      y.min <- min(p.int$lwr)
      y.max <- max( max(p.int$upr),  max(lm.out$model[,nm[1]]) )
    }
    if (!is.factor(lm.out$model[,nm[2]])) fl <- "ls" else fl <- "none"
    x.values <- lm.out$model[,nm[2]]
    y.values <- lm.out$model[,nm[1]] 
    .plt.main(x.values, y.values, type="p", 
       cex=.8, fit.line=fl, xlab=nm[2], ylab=nm[1],
       ylim=c(y.min,y.max), main=ctitle, text.out=FALSE, colors=colors,
       col.line=NULL, col.area=NULL, col.box="black",
       col.pts=NULL, col.fill=NULL, trans.pts=NULL,
       pch=NULL, col.grid=NULL, col.bg=NULL,
       cex.axis=.85, col.axis="gray30",
       col.ticks="gray30", xy.ticks=TRUE,
       center.line=NULL, col.fit.line="grey55",
       time.start=NULL, time.by=NULL, time.reverse=FALSE,
       col.bubble=NULL, bubble.size=.25, col.flower=NULL,
       ellipse=FALSE, col.ellipse="lightslategray", fill.ellipse=TRUE,
       ncut=4, kind="default")

    if (do.int) {
      if (colors == "gray") {
        col.ci <- "gray60"
        col.pi <- "gray30"
      }
      else {
        col.ci <- "lightsteelblue"
        col.pi <- "darksalmon"
      }
      lines(lm.out$model[,nm[2]], c.int$lwr, col=col.ci, lwd=2)
      lines(lm.out$model[,nm[2]], c.int$upr, col=col.ci, lwd=2)
      lines(lm.out$model[,nm[2]], p.int$lwr, col=col.pi, lwd=2)
      lines(lm.out$model[,nm[2]], p.int$upr, col=col.pi, lwd=2)
    }
  }
  else {  # scatterplot matrix for multiple regression
    if (numeric.all && in.data.frame) {
      if (scatter.coef) {
        panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
          usr <- par("usr"); on.exit(par(usr))
          par(usr = c(0, 1, 0, 1))
          r <- cor(x, y)
          txt <- format(c(r, 0.123456789), digits=digits)[1]
          txt <- paste(prefix, txt, sep="")
          if(missing(cex.cor)) cex.cor <- .9/strwidth(txt)
          text(0.5, 0.5, txt, cex=2)  # or cex = cex.cor * r
        }
        suppressWarnings(pairs(lm.out$model[c(nm)], 
          lower.panel=panel.smooth, col.smooth="grey50", upper.panel=panel.cor))
      }
      else suppressWarnings(pairs(lm.out$model[c(nm)], 
                            panel=panel.smooth, col.smooth="grey50"))
    }
    else {
      cat("\n\n>>> No scatterplot matrix reported because not all variables are ")
      if (!in.data.frame) cat("in the data frame.\n")
      if (!numeric.all) cat("numeric.\n")
      dev.off()
    }
  }

  if (scatter.3d) {  # 3d scatterplot option for 2-predictor models
    if (is.numeric(lm.out$model[,nm[2]]) && is.numeric(lm.out$model[,nm[3]]))
       proceed.3d <- TRUE
    else {
      proceed.3d <- FALSE
      cat("\n>>> No 3d scatterplot because both predictor variables must be numeric.\n")
    }
    cat("\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n", 
          "Note: As of 5/2012 there is a bug in the Mac OS X implementation of the rgl\n",
           "package needed to generate this scatterplot.  So it is disabled for now.\n",
           "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n", sep="")
    proceed.3d <- FALSE
    if (proceed.3d) { 
      cat("1. Click and drag to rotate plot.\n",
          "2. Press the right mouse button and drag a rectangle around any points to be\n",
          "   identified, and then release. Repeat for each set of points to be identified.\n",
          "3. To exit, right-click in a blank area of the 3d-scatterplot.\n", sep="")
      suppressMessages(scatter3d(my.formula, id.method="identify", data=lm.out$model))  # car
    }
  }

}
