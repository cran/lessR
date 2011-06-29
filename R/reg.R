reg <-
function(my.formula, dframe=mydata, sig.digits=4,
         res.rows=NULL, res.sort=c("cooks","rstudent","off"), 
         pred=TRUE, pred.all=FALSE, pred.sort=c("predint", "off"),
         cor=TRUE, subsets=TRUE, collinear=TRUE, relations=TRUE,
         cook.cut=1, results=c("full", "brief"), scatter.cor=FALSE,
         X1.new=NULL, X2.new=NULL, X3.new=NULL, X4.new=NULL, 
         X5.new=NULL, show.R=FALSE) {
         
  mydframe <- deparse(substitute(dframe))  # get dataframe name for cor before sort
  
  # produce actual argument, such as from an abbreviation, and flag if not exist
  res.sort <- match.arg(res.sort)
  pred.sort <- match.arg(pred.sort)
  results <- match.arg(results)
    
  if (relations) 
    { cor <- TRUE; subsets <- TRUE; collinear <- TRUE }
  else 
    { cor <- FALSE; subsets <- FALSE; collinear <- FALSE }
  if (!cor & !subsets & !collinear) relations <- FALSE

  if (results == "brief") {
    if (is.null(res.rows)) res.rows <- 0
    pred <- FALSE
    subsets <- FALSE
    collinear <- FALSE
    show.R <- FALSE
   }
    
  options(show.signif.stars=FALSE, scipen=30)
      
  pre <- "> "
  line <- "--------------------------------------------------------------------\n"

  nm <- all.vars(my.formula)  # names of vars in the model
  n.vars <- length(nm)
  n.obs <- nrow(dframe)

  if ( !is.null(X1.new) && (n.vars-1)>5 ) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Cannot have new data if more than 5 predictor variables.\n\n")
  }
  new.data <- FALSE
  if ( (n.vars-1) <= 5 ) { 
    for (i in 1:(n.vars-1)) {
      pp <- eval(parse(text=paste("X", toString(i),".new",sep="")))
      if (!is.null(pp)) new.data <- TRUE
    }
    if (new.data) for (i in 1:(n.vars-1)) {
      pp <- eval(parse(text=paste("X", toString(i),".new",sep="")))
      if (is.null(pp)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Specified new data values for one predictor variable, so do for all.\n\n")
      }
    }
  }

    
  if (is.null(res.rows)) if (n.obs < 25) res.rows <- n.obs else res.rows <- 25 
  if (res.rows == "all") res.rows <- n.obs  # turn off resids with res.rows=0 call
  
  if (n.vars == 2) { # order values of the one predictor variable for scatterplot
    o <- order(dframe[,nm[2]], decreasing=FALSE)
    dframe <- dframe[o,]
  }

  in.data.frame <- TRUE
  for (i in 1:n.vars) {
    if (!(nm[i] %in% names(dframe))) {
        cat("\n\n\n>>>> Note: ", nm[i], "is not in the data frame.\n")
        in.data.frame <- FALSE
      }
  }
  
  graphics.off()  # start graphics with clean start

  # reg analysis, all analysis done on data in model construct lm.out$model
  #   this model construct contains only model vars, with Y listed first
  lm.out <<- lm(my.formula, data=dframe)
  
  cat( "\n\n\n", "  BACKGROUND", "\n")

  if(show.R) {
    cv <- paste(nm[1]," ~ ", sep="")
    cv <- paste(cv, nm[2], sep="")
    if (n.vars > 2) for (i in 3:n.vars) cv <- paste(cv, " + ", nm[i], "", sep="")
    cat(line, pre, "model <- lm(", cv, ")", "\n", line, sep="")
  }
  cat("\n")

  cat("Data Frame: ", mydframe, "\n\n")
  
  cat("Response Variable:  ", nm[1], "\n")
  for (i in 2:n.vars) {
    if (n.vars > 2) txt <- paste("Predictor Variable ", toString(i-1), ": ", sep="")
      else txt <- "Predictor Variable: "
    cat(txt, nm[i], "\n")
  }
  
  cat("\nNumber of observations (rows) of data: ", n.obs, "\n")

  
  
    
  cat( "\n\n\n", "  BASIC ANALYSIS", "\n")
  
  if (show.R) cat(line, pre, "summary(model)", "\n", line, sep="")
  sm <- summary(lm.out)
  cat("\n")
  cat("Estimate: Model coefficients, y-intercept or a slope coefficient\n")
  cat("Hypothesis Test:\n")
  cat("    Standard error, t-value and p-value of each estimate\n")
  cat("    Null hypothesis is corresponding population coefficient is 0\n")
  cat("\n")
  print(sm$coefficients, digits=sig.digits)
  cat("\n\n")
  if (show.R) cat("\n",line,pre,"confint(model)","\n",line,"\n",sep="") else cat("\n")
  cat("Confidence Interval: About each model coefficient\n")
  cat("\n")
  print(confint(lm.out), digits=sig.digits)
  cat("\n\n\n")
  cat("Model fit\n")
  cat("\n")
  cat("Standard deviation of residuals: ", signif(sm$sigma,4),
    "for", sm$df[2], "degrees of freedom", "\n")
  cat("If normal, approximate 95% range of residuals about\n")
  cat("  each fitted value is 4*", signif(sm$sigma,4), 
    " or ", signif(4*sm$sigma,4), sep="", "\n\n")
  cat("R-squared: ", signif(sm$r.squared,3), 
    "    Adjusted R-squared: ", signif(sm$adj.r.squared,3), "\n")
  cat("\n")
  cat("F-statistic for hypothesis test of population R-squared=0: ", 
    signif(sm$fstatistic[1],4), "\n") 
  cat("Degrees of freedom: ", sm$fstatistic[2], "and", sm$fstatistic[3],"\n")
  cat("p-value: ",
    signif(1-pf(sm$fstatistic[1],sm$fstatistic[2],sm$fstatistic[3]),6),"\n")
  cat("\n\n")
  if (show.R) cat("\n\n",line, pre,"anova(model)","\n",line,"\n",sep="") else cat("\n")
  print(anova(lm.out))
    
  
  # check for all numeric vars  in.data.frame <- TRUE
  numeric.all <- TRUE
  for (i in 1:n.vars) {
      if (in.data.frame && !is.numeric(dframe[1,which(names(dframe) == nm[i])])) {
        cat("\n\n\n>>>> Note: ", nm[i], "is not a numeric variable.\n")
        numeric.all <- FALSE
      }
    }



  if (relations) cat( "\n\n\n", "  RELATION AMONG VARIABLES", "\n")

  # correlations
  if (cor) {
    cat( "\nCorrelations\n")
  
    if (numeric.all && in.data.frame) {
      if (show.R) {
        cv <- paste("\"",nm[1],"\"", sep="")
        for (i in 2:n.vars) cv <- paste(cv, ",\"", nm[i], "\"", sep="")
        cat(line, pre, "cor(", mydframe, "[c(", cv, ")])", "\n", line, "\n", sep="")
      }
      else cat("\n") 
      print(cor(dframe[c(nm)]), digits=2)
    }
    else {
      cat("\n>>> No correlations reported because not all variables are ")
      if (!in.data.frame) cat("in the data frame.\n")
      if (!numeric.all) cat("numeric.\n")
    }
  }
  

  # collinearity    
  if (collinear && n.vars>2) {
    cat( "\n\n", "  Collinearity", "\n\n")
    if (numeric.all) {
      check.car <- suppressWarnings(require(car, quietly=TRUE))
      if (check.car) {
        cat("Tolerances, usually should be > approximately 0.20 or so\n\n")
        print(1/(vif(lm.out)), digits=3)
        cat("\n\nVariance Inflation Factors, usually should be < approximately 5\n\n")
        print(vif(lm.out), digits=4)
      }
      else {
        cat("\n>>> Obtaining the collinearity analysis requires package car.", "\n")
        cat(">>> This analysis is not provided here, but all other output is unaffected.", "\n")
        cat(">>> To obtain the car package, run one time only: install.packages('car')", "\n")
      }
     }
     else cat("\n>>> No collinearity analysis because not all variables are numeric.\n")
   }

  # all possible subsets of predictor variables    
  if (subsets && n.vars>2) {
    cat( "\n\n", "  Predictor Variable Subsets", "\n\n")
    cat("Warning: This analysis only describes the data, so does not literally\n")
    cat("         generalize to the population. Only use as a descriptive heuristic.\n\n")
    cat("A 1 means the predictor variable is in the model, a 0 means it is out.\n\n")
    if (numeric.all) {
      check.leaps <- suppressWarnings(require(leaps, quietly=TRUE))
      if (check.leaps) {
        X <- data.frame(lm.out$model[nm[seq(2,n.vars)]])
        Y <- numeric(length=n.obs)  # convert response to an atomic vector for leaps
        for (i in 1:n.obs) Y[i] <- lm.out$model[nm[1]][i,1]
        leaps.out <- leaps(X, Y, method="adjr2")
        models <- data.frame(cbind(leaps.out$which,leaps.out$adjr2), row.names=NULL)
        names(models) <- c(names(X),"R2adj")
        print(models[order(models$R2adj, decreasing=TRUE),], digits=3)
      }
      else {
        cat("\n>>> Analyzing subsets of predictor variables requires package leaps.", "\n")
        cat(">>> This analysis is not provided, but all other output is unaffected.", "\n")
        cat(">>> To get the leaps package, run once only: install.packages('leaps')", "\n")
      }
    }
    else cat("\n>>> No subset analysis reported because not all variables are numeric.\n")
  }




  # residual analysis
  if (res.rows > 0) {
  
    cat( "\n\n\n", "  ANALYSIS OF RESIDUALS AND INFLUENCE", "\n")
  
    if (show.R) {
      cat(line, sep="")
      cat(pre, "fitted(model)", sep="", "\n")
      cat(pre, "resid(model)", sep="", "\n")
      cat(pre, "rstudent(model)", sep="", "\n")
      cat(pre, "cooks.distance(model)", sep="", "\n")
      cat(line, "\n")
    }
    else cat("\n")
    
    cat("Data, Fitted, Residuals, Studentized Residuals, Cook's Distances\n")
    if (res.sort == "cooks") cat("   [sorted by Cook's Distance]\n")
    if (res.sort == "rstudent")  
      cat("   [sorted by Studentized Residual, ignoring + or - sign]\n")
    txt <- "observations (rows) of data]"
    cat("   [res.rows = ", res.rows, " out of ", n.obs, " ", txt, sep="", "\n")
    cat(line)
    
    out <- cbind(fitted(lm.out),resid(lm.out),rstudent(lm.out),
      cooks.distance(lm.out))
    out <- cbind(lm.out$model[c(nm[seq(2,n.vars)],nm[1])],out)
    out <- data.frame(out)
    names(out)[n.vars+1] <- "fitted"
    names(out)[n.vars+2] <- "residual"
    names(out)[n.vars+3] <- "rstudent"
    names(out)[n.vars+4] <- "cooks"
    if (res.sort != "off") {
      if (res.sort == "cooks") o <- order(out$cooks, decreasing=TRUE)
      if (res.sort == "rstudent") o <- order(abs(rstudent(lm.out)),
        decreasing=TRUE)
      out <- out[o,]
    }
    print(out[1:res.rows,], digits=sig.digits)
    rm(out)
  
  
    # plot of residuals, residuals vs fitted
    res <- residuals(lm.out)
    color.density(res, main="Evaluate Normality of Residuals", 
      xlab="Residuals", text.out=FALSE)

    fit <- fitted(lm.out)
    cook <- cooks.distance(lm.out)
    max.cook <- max(cook)
    if (max.cook < cook.cut) {
      cook.cut <- floor(max.cook*100)/100
      txt <- paste("The point with the largest Cook's Distance, ", round(max.cook,2), 
        ", is displayed in red", sep="")
    }
    else
      txt <- paste("Points with Cook's Distance >", cook.cut, "are displayed in red")
    dev.new()
    color.plot(fit, res, type="p", fit.line="lowess", text.out=FALSE,
      main="Plot of Residuals vs Fitted Values", xlab="Fitted Values",
        ylab="Residuals", sub=txt)
    abline(h=0, lty="dotted", col="gray70")
    res.c <- res[which(cook>=cook.cut)]
    fit.c <- fit[which(cook>=cook.cut)]
    if (length(fit.c) > 0) {
      points(fit.c, res.c, col="red")
      text(fit.c, res.c, names(fit.c), pos=1, cex=.8)
    }

    rm(fit, res, cook, res.c, fit.c)
    
  }



  # prediction intervals
   if (pred) {
      
    cat( "\n\n\n", "  FORECASTING ERROR", "\n")

    if (show.R) {
      txt <- "predict(model, interval=\"prediction\")"
      cat(line, pre, txt, sep="", "\n")
      txt <- "predict(model, interval=\"confidence\")"
      cat(pre, txt, sep="", "\n")
      cat(line, "\n")
    }
    else cat("\n")
    
    cat("Data, Fitted Values, Confidence and Prediction Intervals\n")
    cat("   [sorted by lower bound of prediction interval]\n")
    if (n.obs > 50 && pred.all == FALSE && !new.data) 
      cat("   [to save space only some intervals printed, do pred.all=TRUE to see all]\n")
    cat(line)
    
    if (!new.data) {
      c.int <- data.frame(predict(lm.out, interval="confidence"))
      p.int <- suppressWarnings(data.frame(predict(lm.out, interval="prediction")))
      out <- cbind(lm.out$model[c(nm[seq(2,n.vars)],nm[1])],c.int,p.int$lwr,p.int$upr)
    }
    else {
      Xnew.val <- list(X1.new)
      if (n.vars > 2) for (i in 2:(n.vars-1)) {
        pp <- eval(parse(text=paste("X", toString(i),".new",sep="")))
        Xnew.val <- c(Xnew.val, list(pp))
      }
      Xnew <- expand.grid(Xnew.val)
      for (i in 1:(n.vars-1)) names(Xnew)[i] <- nm[i+1]
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
    if (n.obs < 50  || pred.all == TRUE || new.data)
      print(out, digits=sig.digits)
    else {
      print(out[1:5,], digits=sig.digits)
      cat("\n... for the middle 5 rows of sorted data ...\n\n")
      n.mid <- round(n.obs/2)
      print(out[(n.mid-2):(n.mid+2),], digits=sig.digits)
      cat("\n... for the last 5 rows of sorted data ...\n\n")
      print(out[(n.obs-4):n.obs,], digits=sig.digits)
    }
    cat(line, "\n")
    cat("Note: Predictions from current data, from which the model is estimated,\n")
    cat("      refer to future responses from the collection of new data.\n")
  }
  
  
  
  
  # scatterplot, if one predictor variable
  dev.new() 
  if (n.vars == 2) {
    if ( (pred==FALSE) || is.factor(lm.out$model[,nm[2]]) || !is.null(X1.new) ) 
     do.int <- FALSE
    else do.int <- TRUE
    if (!do.int) {
      ctitle <- "Scatterplot and Regression Line"
      y.min <- min(lm.out$model[,nm[1]])
      y.max <- max(lm.out$model[,nm[1]])
    }
    else {
      ctitle <- "Regression Line, Confidence and Prediction Intervals"
      y.min <- min(p.int$lwr)
      y.max <- max( max(p.int$upr),  max(lm.out$model[,nm[1]]) )
    }
    if (!is.factor(lm.out$model[,nm[2]])) fl <- "ls" else fl <- "none"
    color.plot(lm.out$model[,nm[2]], lm.out$model[,nm[1]], type="p", 
      cex=.8, fit.line=fl, xlab=nm[2], ylab=nm[1],
      ylim=c(y.min,y.max), main=ctitle, text.out=FALSE)
    if (do.int) {
      lines(lm.out$model[,nm[2]], c.int$lwr, col="lightsteelblue", lwd=2)
      lines(lm.out$model[,nm[2]], c.int$upr, col="lightsteelblue", lwd=2)
      lines(lm.out$model[,nm[2]], p.int$lwr, col="darksalmon", lwd=2)
      lines(lm.out$model[,nm[2]], p.int$upr, col="darksalmon", lwd=2)
    }
  }
  else   # scatterplot matrix for multiple regression
    if (numeric.all && in.data.frame) {
      if (scatter.cor) {
        panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
          usr <- par("usr"); on.exit(par(usr))
          par(usr = c(0, 1, 0, 1))
  #        r <- abs(cor(x, y))
          r <- cor(x, y)
          txt <- format(c(r, 0.123456789), digits=digits)[1]
          txt <- paste(prefix, txt, sep="")
          if(missing(cex.cor)) cex.cor <- .9/strwidth(txt)
  #        text(0.5, 0.5, txt, cex = cex.cor * r)
          text(0.5, 0.5, txt, cex=2)
        }
        suppressWarnings(pairs(dframe[c(nm)], 
          lower.panel=panel.smooth, col.smooth="grey50", upper.panel=panel.cor))
      }
      else suppressWarnings(pairs(dframe[c(nm)], panel=panel.smooth, col.smooth="grey50"))
    }
    else {
      cat("\n\n>>> No scatterplot matrix reported because not all variables are ")
      if (!in.data.frame) cat("in the data frame.\n")
      if (!numeric.all) cat("numeric.\n")
    }

  
  cat("\n")

}

