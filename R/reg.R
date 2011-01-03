reg <-
function(my.formula, dframe=mydata, graph=TRUE, cor=TRUE,
         res.rows=NULL, res.sort=c("cooks","rstudent","off"), 
         pred=TRUE, pred.sort=c("predint", "off"), sig.digits=NULL) {
         
  mydframe <- deparse(substitute(dframe))  # get dataframe name for cor before sort
  
  # produce actual argument, such as from an abbreviation, and flag if not exist
  res.sort <- match.arg(res.sort)
  pred.sort <- match.arg(pred.sort)
    
  options(show.signif.stars=FALSE, scipen=30)
      
  pre <- "> "
  line <- "--------------------------------------------------------------------\n"

  nm <- all.vars(my.formula)  # names of vars in the model
  n.vars <- length(nm)
  n.obs <- nrow(dframe)
    
  if (is.null(res.rows)) if (n.obs < 25) res.rows <- n.obs else res.rows <- 25 
  if (res.rows == "all") res.rows <- n.obs  # turn off resids with res.rows=0 call
  
  if (n.vars > 2) graph <- FALSE
  
  if (graph == TRUE) {
    o <- order(dframe[,nm[2]], decreasing=FALSE)
    dframe <- dframe[o,]
  }
  
  
  # reg analysis, all analysis done on data in model construct lm.out$model
  #   this model construct contains only model vars, with Y listed first
  lm.out <<- lm(my.formula, data=dframe)
  
  cat( "\n\n\n", "  THE MODEL", "\n")

  cv <- paste(nm[1]," ~ ", sep="")
  cv <- paste(cv, nm[2], sep="")
  if (n.vars > 2) for (i in 3:n.vars) cv <- paste(cv, " + ", nm[i], "", sep="")
  cat(line, pre, "model <- lm(", cv, ")", "\n", line,"\n", sep="")
  
  cat("Response Variable:  ", nm[1], "\n")
  for (i in 2:n.vars) {
    if (n.vars >2) txt <- paste("Predictor Variable ", toString(i-1), ": ", sep="")
      else txt <- "Predictor Variable: "
    cat(txt, nm[i], "\n")
  }
  
  cat("\nNumber of observations (rows) of data: ", n.obs, "\n")
  
    
  cat( "\n\n\n", "  BASIC ANALYSIS", "\n")
  
  cat(line, pre, "summary(model)", "\n", line, sep="")
  print(summary(lm.out))
  
  cat("\n", line, pre, "confint(model)", "\n", line, "\n", sep="")
  print(confint(lm.out))
  
  cat("\n\n", line, pre, "anova(model)", "\n", line, "\n", sep="")
  print(anova(lm.out))
  

  # correlations
  if (cor == TRUE) {
      cat( "\n\n\n", "  CORRELATIONS", "\n")
  
    # check for all numeric vars
    do.cor <- TRUE
    for (i in 1:n.vars) {
      if (!is.numeric(dframe[1,which(names(mydata) == nm[i])])) {
        cat("No correlations reported,", nm[i], "is not numeric.\n")
        do.cor <- FALSE
      }
    }
      
    if (do.cor == TRUE) {
      cv <- paste("\"",nm[1],"\"", sep="")
      for (i in 2:n.vars) cv <- paste(cv, ",\"", nm[i], "\"", sep="")
      cat(line, pre, "cor(", mydframe, "[c(", cv, ")])", "\n", line, "\n", sep="")

      print(cor(dframe[c(nm)]), digits=2)
    }
  }


  # residual analysis
  if (res.rows > 0) {
  
    cat( "\n\n\n", "  ANALYSIS OF RESIDUALS", "\n")
  
    cat(line, sep="")
    cat(pre, "fitted(model)", sep="", "\n")
    cat(pre, "resid(model)", sep="", "\n")
    cat(pre, "rstudent(model)", sep="", "\n")
    cat(pre, "cooks.distance(model)", sep="", "\n")
    cat(line, "\n")
    
    cat("Data, Fitted, Residuals, Studentized Residuals, Cook's Distances\n")
    if (res.sort == "cooks") cat("   [sorted by Cook's Distance]\n")
    if (res.sort == "rstudent")  
      cat("   [sorted by Studentized Residual, ignoring + or - sign]\n")
    txt <- "observations (rows) of data]"
    cat("   [res.rows = ", res.rows, " out of ", n.obs, " ", txt, sep="", "\n")
    cat(line)
    
    out <- cbind(fitted(lm.out),resid(lm.out),rstudent(lm.out),cooks.distance(lm.out))
    if (!is.null(sig.digits)) out <- signif(out, sig.digits)
    out <- cbind(lm.out$model[c(nm[seq(2,n.vars)],nm[1])],out)
    out <- data.frame(out)
    names(out)[n.vars+1] <- "fitted"; names(out)[n.vars+2] <- "residual";
    names(out)[n.vars+3] <- "rstudent"; names(out)[n.vars+4] <- "cooks";
    if (res.sort != "off") {
      if (res.sort == "cooks") o <- order(out$cooks, decreasing=TRUE)
      if (res.sort == "rstudent")  o <- order(abs(rstudent(lm.out)), decreasing=TRUE)
      out <- out[o,]
    }
    print(out[1:res.rows,])
    rm(out)
  }
  

  # prediction intervals
  if (pred == TRUE) {
      
    cat( "\n\n\n", "  FORECASTING ERROR", "\n")

    txt <- "predict(model, interval=\"prediction\")"
    cat(line, pre, txt, sep="", "\n")
    txt <- "predict(model, interval=\"confidence\")"
    cat(pre, txt, sep="", "\n")
    cat(line, "\n")
    cat("Data, Fitted Values, Confidence and Prediction Intervals\n")
    cat(line)
    
    ci <- data.frame(predict(lm.out, interval="confidence"))
    pi <- suppressWarnings(data.frame(predict(lm.out, interval="prediction")))
    if (!is.null(sig.digits)) {
      ci <- signif(ci, sig.digits)
      pi <- signif(pi, sig.digits)
    }
    out <- cbind(lm.out$model[c(nm[seq(2,n.vars)],nm[1])],ci,pi$lwr,pi$upr)
    out <- data.frame(out)
    if (pred.sort == "predint") {
      o <- order(out[,n.vars+4])  # lower bound of prediction interval
      out <- out[o,]
    }
    names(out)[n.vars+1] <- "fitted"
    names(out)[n.vars+2] <- "ci:lwr"; names(out)[n.vars+3] <- "ci:upr";
    names(out)[n.vars+4] <- "pi:lwr"; names(out)[n.vars+5] <- "pi:upr";
    print(out)
    cat(line, "\n")
    cat("Note: Predictions from current data, from which the model is estimated,\n")
    cat("      refer to _future_ responses based on the collection of new data.\n")
  }
  
  
  # scatterplot, if one predictor variable
  if (n.vars == 2) {
    if (pred == FALSE) ctitle <- "Scatterplot and Regression Line"
      else ctitle <- "Regression Line, Confidence and Prediction Intervals"
    plot(lm.out$model[,nm[2]], lm.out$model[,nm[1]], pch=19, cex=.8, col="gray70", 
      xlab=nm[2], ylab=nm[1], main=ctitle)
    abline(lm.out$coef)
    if (pred == TRUE) {
      lines(lm.out$model[,nm[2]], ci$lwr, col="lightsteelblue", lwd=2)
      lines(lm.out$model[,nm[2]], ci$upr, col="lightsteelblue", lwd=2)
      lines(lm.out$model[,nm[2]], pi$lwr, col="darksalmon", lwd=2)
      lines(lm.out$model[,nm[2]], pi$upr, col="darksalmon", lwd=2)
    }
  }
  
  cat("\n")

}

