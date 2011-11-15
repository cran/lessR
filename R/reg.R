reg <-
function(my.formula, dframe=mydata, sig.digits=4,
         res.rows=NULL, res.sort=c("cooks","rstudent","dffits","off"), 
         pred=TRUE, pred.all=FALSE, pred.sort=c("predint", "off"),
         subsets=TRUE, cooks.cut=1, results=c("full", "brief"), 
         scatter.coef=FALSE, scatter.3d=NULL, graphics.save=FALSE,
         X1.new=NULL, X2.new=NULL, X3.new=NULL, X4.new=NULL, 
         X5.new=NULL, text.width=100, show.R=FALSE, explain=FALSE) {
         
  dash <- function(n.dash) { for (i in 1:(n.dash)) cat("-"); cat("\n") }
  
  mydframe <- deparse(substitute(dframe))  # get dataframe name for cor before sort
 
  # produce actual argument, such as from an abbreviation, and flag if not exist
  res.sort <- match.arg(res.sort)
  pred.sort <- match.arg(pred.sort)
  results <- match.arg(results)

  op <- options()  # save current options to reset at end of reg
  options(show.signif.stars=FALSE, scipen=30, width=text.width)

  # output
  cor <- TRUE
  collinear <- TRUE 

  if (results == "brief") {
    if (is.null(res.rows)) res.rows <- 0
    pred <- FALSE
    cor <- FALSE
    collinear <- FALSE
    show.R <- FALSE
   }
      
  pre <- "> "
  line <- "--------------------------------------------------------------------\n"
  
  
  if(graphics.save) pdf("regOut.pdf")
  else  graphics.off()  # graphics get clean start


  nm <- all.vars(my.formula)  # names of vars in the model
  n.vars <- length(nm)
  n.pred <- n.vars - 1
  n.obs <- nrow(dframe)
  
  if(n.pred > 1) {
    collinear <- TRUE
    subsets <- TRUE
  }
  else {
    collinear <- FALSE
    subsets <- FALSE
  }
  
  if(is.null(scatter.3d)) if (n.pred==2) scatter.3d <- TRUE else scatter.3d <- FALSE

  if ( scatter.3d && (n.pred)!=2 ) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
       "Can have a 3d scatterplot only with exactly two predictor variables.\n\n")
  }
  
  if ( !is.null(X1.new) && (n.pred)>5 ) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Cannot specify new data if more than 5 predictor variables.\n\n")
  }

  # check new.data option for consistency  
  new.data <- FALSE
  if ( (n.pred) <= 5 ) { 
    for (i in 1:(n.pred)) {
      pp <- eval(parse(text=paste("X", toString(i),".new",sep="")))
      if (!is.null(pp)) new.data <- TRUE
    }
    if (new.data) for (i in 1:(n.pred)) {
      pp <- eval(parse(text=paste("X", toString(i),".new",sep="")))
      if (is.null(pp)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Specified new data values for one predictor variable, so do for all.\n\n")
      }
    }
  }

    
  if (is.null(res.rows)) if (n.obs < 25) res.rows <- n.obs else res.rows <- 25 
  if (res.rows == "all") res.rows <- n.obs  # turn off resids with res.rows=0 call
 
  # order values of the one predictor variable for scatterplot
  #   so that the prediction/confidence intervals can be drawn
  if (n.vars == 2) { 
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

  # reg analysis, all analysis done on data in model construct lm.out$model
  #   this model construct contains only model vars, with Y listed first
  lm.out <<- lm(my.formula, data=dframe)

  if (!explain) 
    cat("\nAdd the following option to see explanations of the output:  explain=TRUE\n")



# ----------
# Background
# ----------

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



# --------------
# Basic Analysis
# --------------
  cat( "\n\n\n", "  BASIC ANALYSIS", "\n")
  
  sm <- summary(lm.out)
  if (show.R) cat(line, pre, "summary(model)", "\n", line, sep="")
  cat("\n")
  
  if (explain) {
    dash(68)
    cat("Estimates\n",
        "  Intercept: Fitted value of response variable, ", nm[1], ", when the\n",
        "             values of the predictor variable", sep="")
    if (n.pred == 1) cat(" is ") else cat("s are ")
    cat("set to zero.\n")
    cat("  Slope Coefficient: Average change in the value of response variable,\n",
        "             ", nm[1], ", for a one-unit increase in the value of the\n",
        "             corresponding predictor variable", sep="")
    if (n.pred == 1) cat(".\n") 
    else cat(", with the values of all\n",
             "             remaining predictor variables held constant.\n", sep="")
    cat("\n")
    cat("Hypothesis Tests\n")
    cat("    Standard error, t-value and p-value of each estimate\n")
    cat("    Null hypothesis: Corresponding population coefficient is 0\n")
    dash(68)
   }
  else cat("Estimates, Hypothesis Tests\n")
  cat("\n")
  print(sm$coefficients, digits=sig.digits)
  
  cat("\n\n\n")
  if (show.R) cat("\n",line,pre,"confint(model)","\n",line,"\n",sep="")
  if (explain) {
    dash(68)
    cat("95% Confidence Intervals\n",
        "    Each interval is constructed about the\n",
        "      corresponding estimated model coefficient.\n",
        "    The margin of error is half the width of the interval.\n")
    dash(68)
  }
  else cat("Confidence Intervals\n")
  cat("\n")
  print(confint(lm.out), level=0.95, digits=sig.digits)
  
  cat("\n\n\n")
  cat("Model fit\n")
  cat("\n")
  if (explain) {
    dash(68)
    cat("The \'standard deviation of the residuals\' is also called\n",
        "the \'standard error of estimate\'.  Are the residuals typically\n",
        "close to their mean of zero, or are they scattered with\n",
        "relatively large positive and negative values?\n",
        "\n",
        "For any normal distribution, about 95% of the values are within\n",
        "two standard deviations of the mean, for a range of four.\n", sep="")
    dash(68)
    cat("\n")
  }
  cat("Standard deviation of residuals: ", signif(sm$sigma,4),
    "for", sm$df[2], "degrees of freedom", "\n")
  cat("If normal, the approximate 95% range of residuals about\n")
  cat("  each fitted value is 4*", signif(sm$sigma,4), 
    " or ", signif(4*sm$sigma,4), sep="", "\n\n")
  if (explain) {
    dash(68)
    cat("R-squared: Proportion of the overall variability of response variable\n",
        nm[1], " that is accounted for by the model. The unexplained\n",
        "variability is the variability of the residuals.\n",  
        "\n",
        "Adjusted R-squared: Adjusts R-squared downward according to the\n",
        "degrees of freedom. Unlike R-squared, the adjusted version increases\n",
        "when a new predictor variable is added to the model only if the\n",
        "new variable improves the model more than would be expected by\n",
        "chance.\n", sep="")
    dash(68)
    cat("\n")
  }
  cat("R-squared: ", signif(sm$r.squared,3), 
    "    Adjusted R-squared: ", signif(sm$adj.r.squared,3), "\n")
  cat("\n")
  cat("F-statistic for hypothesis test of population R-squared=0: ", 
    signif(sm$fstatistic[1],4), "\n") 
  cat("Degrees of freedom: ", sm$fstatistic[2], "and", sm$fstatistic[3],"\n")
  cat("p-value: ",
    signif(1-pf(sm$fstatistic[1],sm$fstatistic[2],sm$fstatistic[3]),6),"\n")
  cat("\n")
  
  if (show.R) cat("\n\n",line, pre,"anova(model)","\n",line,"\n",sep="") else cat("\n")
  anv <- anova(lm.out)
  if (explain) {
    dash(68)
    cat("The ANOVA table presents the details of the explained and unexplained\n",
        "variation. \n",
        "\n",
        "The sum of the squared residuals, the value minimized by the OLS\n",
        "estimation procedure, is ", anv$'Sum Sq'[n.vars], ".\n", sep="")
    dash(68)
    cat("\n")
  }
  cat("\n")
  print(anv)
    
  
  # check for all numeric vars  in.data.frame <- TRUE
  numeric.all <- TRUE
  
  for (i in 1:n.vars) {
      if (in.data.frame && !is.numeric(dframe[1,which(names(dframe) == nm[i])])) {
        cat("\n\n\n>>>> Note: ", nm[i], "is not a numeric variable.\n")
        numeric.all <- FALSE
      }
    }



# -------------------------
# Relations among Variables
# -------------------------

  cat( "\n\n\n", "  RELATIONS AMONG VARIABLES", "\n")

  # correlations
  if (cor) {
    cat("\n")
    if (explain) {
      dash(68)
      cat("Correlations among the variables in the model.\n",
          "\n",
          "The correlations of response variable ", nm[1], " with the predictor\n",
          "variables should be high. The correlations of the predictor variables\n",
          "with each other should be small.\n", sep="")
      dash(68)
    }
    else cat("Correlations\n")
  
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
  if (collinear) {
    if (explain) {
      cat("\n\n")
      dash(68)
      cat("Collinearity analysis.\n",
          "\n",
          "The predictor variables should not be collinear, where one variable \n",
          "is linearly dependent on the others.\n",
          "\n",
          "Tolerances usually should be > approximately 0.20 or so.\n", 
          "Variance Inflation Factors, usually should be < approximately 5.\n", sep="")
      dash(68)
    }
    else cat( "\n\n", "Collinearity", "\n", sep="")
    cat("\n")
   if (numeric.all) {
      check.car <- suppressWarnings(require(car, quietly=TRUE))
      if (check.car) {
        cat("  Tolerances\n\n")
        print(1/(vif(lm.out)), digits=3)
        cat("\n\n  Variance Inflation Factors\n\n")
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
  if (subsets) {
    cat("\n\n")
    if (explain) {
      dash(68)
      cat("Analysis of all possible subsets of the predictor variable.\n",
         "\n",
          "Assess fit for models that correspond to all possible combinations\n",
          "of predictor variables.\n",
          "\n",
          "Warning: This analysis only describes the data, so does not literally\n",
          "  generalize to the population. Only use as a descriptive heuristic.\n",
          "\n",
          "A 1 means the predictor variable is in the model, a 0 means it is out.\n", sep="")
      dash(68)
    }
    else cat("Predictor Variable Subsets", "\n")
    cat("\n")
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



# -----------------
# residual analysis
# -----------------
  if (res.rows > 0) {
  
    cat( "\n\n\n", "  ANALYSIS OF RESIDUALS AND INFLUENCE", "\n")
  
    if (show.R) {
      cat(line, sep="")
      cat(pre, "fitted(model)", sep="", "\n")
      cat(pre, "resid(model)", sep="", "\n")
      cat(pre, "rstudent(model)", sep="", "\n")
      cat(pre, "dffits(model)", sep="", "\n")
      cat(pre, "cooks.distance(model)", sep="", "\n")
      cat(line, "\n")
    }
    else cat("\n")
    
    if (explain) {
      dash(68)
      cat("The identification of observations that have a large residual\n",
          "and/or undue influence on the estimation of the model helps\n",
          "detect potential outliers.  Each of the following statistics is\n",
          "calculated for a single observation (row of data).\n",
         "\n",          
          "residual: Value of the response variable ", nm[1], " minus its\n",
          "    fitted value.\n",
          "\n",
          "rstudent: Studentized residual, standardized value of the residual\n",
          "    from a model estimated without the observation present.\n",
          "\n",
          "dffits: The influence of an observation on its own fitted value.\n",
         "\n",
          "cooks: Cook's Distance, the aggregate influence of the observation\n",
          "    on the estimation of the model coefficients.\n", sep="")
      dash(68)
      cat("\n")
    }

    cat("Data, Fitted, Residual, Studentized Residual, Dffits, Cook's Distance\n")
    if (res.sort == "cooks") cat("   [sorted by Cook's Distance]\n")
    if (res.sort == "rstudent")  
      cat("   [sorted by Studentized Residual, ignoring + or - sign]\n")
   if (res.sort == "dffits")  
      cat("   [sorted by dffits, ignoring + or - sign]\n")
    txt <- "observations (rows) of data]"
    cat("   [res.rows = ", res.rows, " out of ", n.obs, " ", txt, sep="", "\n")
    dash(68)
    
    out <- cbind(fitted(lm.out),resid(lm.out),rstudent(lm.out),dffits(lm.out),
      cooks.distance(lm.out))
    out <- cbind(lm.out$model[c(nm[seq(2,n.vars)],nm[1])],out)
    out <- data.frame(out)
    names(out)[n.vars+1] <- "fitted"
    names(out)[n.vars+2] <- "residual"
    names(out)[n.vars+3] <- "rstudent"
    names(out)[n.vars+4] <- "dffits"
    names(out)[n.vars+5] <- "cooks"
    if (res.sort != "off") {
      if (res.sort == "cooks") o <- order(out$cooks, decreasing=TRUE)
      if (res.sort == "rstudent") o <- order(abs(out$rstudent),
        decreasing=TRUE)
      if (res.sort == "dffits") o <- order(abs(out$dffits),
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
    if (max.cook < cooks.cut) {
      cooks.cut <- floor(max.cook*100)/100
      txt <- paste("The point with the largest Cook's Distance, ", round(max.cook,2), 
        ", is displayed in red", sep="")
    }
    else
      txt <- paste("Points with Cook's Distance >", cooks.cut, "are displayed in red")
    if (!graphics.save) dev.new()
    color.plot(fit, res, type="p", fit.line="lowess", text.out=FALSE,
      main="Residuals vs Fitted Values", xlab="Fitted Values",
        ylab="Residuals", sub=txt)
    abline(h=0, lty="dotted", col="gray70")
    res.c <- res[which(cook>=cooks.cut)]
    fit.c <- fit[which(cook>=cooks.cut)]
    if (length(fit.c) > 0) {
      points(fit.c, res.c, col="red")
      text(fit.c, res.c, names(fit.c), pos=1, cex=.8)
    }

    rm(fit, res, cook, res.c, fit.c)
    
  }



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
      dash(68)
    }
    else cat("\n")
    
    if (explain) {
      dash(68)
      cat("The 'standard deviation of the residuals', assumed to be the same\n",
          "value for each set of values of the predictor variables, estimates the\n",
          "modeling error. However, even for predictions from the current data\n",
          "from which the model is estimated, the forecasts are based on future\n",
          "responses from the collection of new data. That is, the sampling\n",
          "error of the sample regression line must also be considered. This\n",
          "sampling error varies depending on the values of the predictor variables.\n",
          "\n",
          "The 95% confidence interval around each fitted value of the sample\n",
          "regression model is given below, as well as the likely range of\n",
          "forecasting error, the 95% prediction interval, the expected range\n",
          "in which the actual future value of the response variable, ", nm[1], ", \n",
          "will likely be found.  This forecasting error depends on both modeling\n", 
          "error and sampling error.\n", sep="")
      dash(68)
      cat("\n")
    }

    cat("Data, Fitted Values, Confidence and Prediction Intervals\n")
    cat("   [sorted by lower bound of prediction interval]\n")
    if (n.obs > 50 && pred.all == FALSE && !new.data) 
      cat("   [to save space only some intervals printed, do pred.all=TRUE to see all]\n")
    dash(68)
    
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
    dash(68)
  }
  


# ------------
# Scatterplots
# ------------

  if (!graphics.save) dev.new() 
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

  if (scatter.3d) {  # 3d scatterplot option for 2-predictor models
    check1.3d <- suppressWarnings(require(car, quietly=TRUE))
    check2.3d <- suppressWarnings(require(rgl, quietly=TRUE))
    if (check1.3d && check2.3d) 
      scatter3d(my.formula, id.method="identify", data=dframe)
    else {
      if (!check1.3d) {
        cat("\n>>> Creating a 3d scatterplot requires package car.", "\n")
        cat(">>> This analysis is not provided, but other output is unaffected.", "\n")
        cat(">>> To get this package, run once only:", "\n")
        cat("      install.packages('car')", "\n")
      }
      if (!check2.3d) {
        cat("\n>>> Creating a 3d scatterplot requires package rgl.", "\n")
        cat(">>> This analysis is not provided, but other output is unaffected.", "\n")
        cat(">>> To get this package, run once only:", "\n")
        cat("      install.packages('rgl')", "\n")
      }
    }
  }
  

  if (graphics.save) {
    cat("\n\n")
    dash(68)
    if (getwd() == "/")
      workdir <- "top level (root) of your file system"
    else
      workdir <- getwd()
    cat("regression graphics written at current working directory\n")
    cat("       regOut.pdf", "at:  ", workdir, "\n")
    dev.off()
  }
  cat("\n")
  options(op)  # restore options going into reg

}
