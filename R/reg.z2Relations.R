.reg2Relations <- 
function(lm.out, nm, dname,
         n.vars, n.pred, n.obs, n.keep, digits.d, explain, show.R, pre, line,
         cor, collinear, subsets, numeric.all, in.data.frame) {

# -------------------------
# Relations among Variables
# -------------------------

    cat( "\n\n\n", "  RELATIONS AMONG VARIABLES", "\n")

    # correlations
    if (cor) {
      cat("\n")
      if (explain) {
        .dash(68)
        cat("Correlations among the variables in the model.\n",
            "\n",
            "The correlations of response variable ", nm[1], " with the predictor\n",
            "variables should be high. The correlations of the predictor variables\n",
            "with each other should be small.\n", sep="")
        .dash(68)
      }
      else cat("Correlations\n")
    
      if (numeric.all && in.data.frame) {
        if (show.R) {
          cv <- paste("\"",nm[1],"\"", sep="")
          for (i in 2:n.vars) cv <- paste(cv, ",\"", nm[i], "\"", sep="")
          cat(line, pre, "cor(", dname, "[c(", cv, ")])", "\n", line, "\n", sep="")
        }
        else cat("\n") 
        print(cor(lm.out$model[c(nm)]), digits=2)
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
      .dash(68)
      cat("Collinearity analysis.\n",
          "\n",
          "The predictor variables should not be collinear, where one variable \n",
          "is linearly dependent on the others.\n",
          "\n",
          "Tolerances usually should be > approximately 0.20 or so.\n", 
          "Variance Inflation Factors, usually should be < approximately 5.\n", sep="")
      .dash(68)
    }
    else cat( "\n\n", "Collinearity", "\n", sep="")
    cat("\n")
    if (numeric.all) {
      tol <- 1/(vif(lm.out)) 
      tol <- data.frame(tol, vif(lm.out)) 
      names(tol) <- c("Tolerance", "    VIF")
      print(round(tol,3))
    }
    else cat("\n>>> No collinearity analysis because not all variables are numeric.\n")
  }

  # all possible subsets of predictor variables    
  if (subsets) {
    cat("\n\n")
    if (explain) {
      .dash(68)
      cat("Analysis of all possible subsets of the predictor variable.\n",
         "\n",
          "Assess fit for models that correspond to all possible combinations\n",
          "of predictor variables.\n",
          "\n",
          "Warning: This analysis only describes the data, so does not literally\n",
          "  generalize to the population. Only use as a descriptive heuristic.\n",
          "\n",
          "A 1 means the predictor variable is in the model, a 0 means it is out.\n", sep="")
      .dash(68)
    }
    else cat("All Possible Subset Regressions", "\n")
    cat("\n")
    if (numeric.all) {
      X <- data.frame(lm.out$model[nm[seq(2,n.vars)]])
      Y <- numeric(length=n.keep)  # convert response to an atomic vector for leaps
      for (i in 1:n.keep) Y[i] <- lm.out$model[nm[1]][i,1]
      lp.out <- leaps(X, Y, method="adjr2")  # leaps function
      md <- lp.out$which
      rownames(md) <- 1:nrow(md)  # matrix md does not have proper row names
      models <- data.frame(cbind(md, lp.out$adjr2, lp.out$size-1))  # cbind gives 0, 1
      names(models) <- c(names(X), "R2adj", "X's")
      mod.srt <- models[order(models$R2adj, decreasing=TRUE),]
      names(mod.srt)[ncol(mod.srt)-1] <- "   R2adj"
      if (nrow(mod.srt) < 51)
        print(mod.srt, digits=3, row.names=FALSE)
      else {
        print(mod.srt[1:50, ], digits=3, row.names=FALSE)
        cat("\n>>> Only first 50 rows printed.\n\n")
      }
    }
    else cat("\n>>> No subset analysis reported because not all variables are numeric.\n")
  }


}
