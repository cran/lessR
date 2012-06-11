.reg2Relations <- 
function(nm, mydframe,
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
          cat(line, pre, "cor(", mydframe, "[c(", cv, ")])", "\n", line, "\n", sep="")
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
      cat("\n  Tolerances\n\n")
      print(1/(vif(lm.out)), digits=3)  # car function
      cat("\n\n  Variance Inflation Factors\n\n")
      print(vif(lm.out), digits=4)
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
    else cat("Predictor Variable Subsets", "\n")
    cat("\n")
    if (numeric.all) {
      X <- data.frame(lm.out$model[nm[seq(2,n.vars)]])
      Y <- numeric(length=n.keep)  # convert response to an atomic vector for leaps
      for (i in 1:n.keep) Y[i] <- lm.out$model[nm[1]][i,1]
      leaps.out <- leaps(X, Y, method="adjr2")  # leaps function
      models <- data.frame(cbind(leaps.out$which,leaps.out$adjr2), row.names=NULL)
      names(models) <- c(names(X),"R2adj")
      print(models[order(models$R2adj, decreasing=TRUE),], digits=3)
    }
    else cat("\n>>> No subset analysis reported because not all variables are numeric.\n")
  }


}
