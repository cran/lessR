.reg.knitr <-
function(nm, dname, n.vars, n.pred, pred.rows, digits.d, explain, pvalues) {

  response <- nm[1]
  pred<- character(length=0)
  for (i in 1:n.pred) pred[i] <- nm[i+1]
  predictors <- pred[1] 
  if (n.pred > 1) {
    if (n.pred > 2)for (i in 2:(n.pred-1)) predictors <- paste(predictors, ", ", pred[i], sep="")
    predictors <- paste(predictors, "and", pred[n.pred])
  }
  
  tx <- character(length = 0)

  tx[length(tx)+1] <- "---"
  tx[length(tx)+1] <- "title: 'Regression Analysis'"
  tx[length(tx)+1] <- "output: word_document"
  tx[length(tx)+1] <- "---"

  ref <- getOption("data.file")
  if (ref %in% c("Employee", "Reading", "Cars93", "Jackets", "Learn", "Mach4"))
    ref  <- paste(ref, "\"", ", format=\"lessR", sep="")
  if (n.pred == 1)
    txt <- "predictor variable, "
  else
    txt <- "following predictor variables: "
  tx[length(tx)+1] <- paste(
    "The purpose of this analysis is to explain the values of the response variable, ",
    response, ", in terms of the values of the ", txt, predictors, ".", sep="")

  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "## The Data"

  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- paste(
    "First read the data, obtained from the brief version of `Read`. Here we read from ",
    "a data file that is included with the `lessR` package.")
  tx[length(tx)+1] <- "```{r}"
  tx[length(tx)+1] <- paste(dname, " <- rd.brief(\"", ref, "\")", sep="")
  tx[length(tx)+1] <- "```"
  tx[length(tx)+1] <- paste(
    "Data from the following variables are available for analysis: `r names(", dname, ")`.",
    sep="")

  cv <- paste("$$\\hat Y_{", nm[1], "} = b_0 + b_1 X_{", nm[2], "}", sep="")
  if (n.vars > 2)
    for (i in 3:n.vars) cv <- paste(cv, " + b_", i, " X_{", nm[i], "}", sep="")
  cv <- paste(cv, "$$", sep="")



  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "## Basic Analysis"
  
  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "### The Estimated Model"
  tx[length(tx)+1] <- ""

  if (n.pred > 1)
    txt <- "variables"
  else
    txt <- "variable"
  tx[length(tx)+1] <- paste(
    "The model of interest has ", n.pred, " predictor ", txt, ". ",
    "To understand the variable ", response, " in terms of ",
    predictors, ", analyze this model with regression analysis.",
    sep="")

  cv <- paste("$$\\hat Y_{", nm[1], "} = b_0 + b_1 X_{", nm[2], "}", sep="")
  if (n.vars > 2)
    for (i in 3:n.vars) cv <- paste(cv, " + b_", i, " X_{", nm[i], "}", sep="")
  cv <- paste(cv, "$$", sep="")
  tx[length(tx)+1] <- cv

  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- paste(
    "First obtain the least squares estimates of the model, the values of each regression ",
    "coefficient, $b_i$. Do so with the `lessR` function `Regression`, here abbreviated as ",
    "`reg`.", sep="")
  cv <- paste(nm[1]," ~ ", nm[2], sep="")
  if (n.vars > 2) for (i in 3:n.vars) cv <- paste(cv, " + ", nm[i], "", sep="")
  tx[length(tx)+1] <- "```{r}"
  tx[length(tx)+1] <- "# Generate graphics later at specific locations with `regPlot` function"
  tx[length(tx)+1] <- paste("r <- reg(", cv, ", graphics=FALSE)", sep="")
  tx[length(tx)+1] <- "```"

  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- paste(
    "The first part of the output specifies the analysis ",
    "in terms of the variables and data.",
    sep="")
  tx[length(tx)+1] <- "```{r, echo=FALSE}"
  tx[length(tx)+1] <-"r$out_background" 
  tx[length(tx)+1] <- "```"

  tx[length(tx)+1] <- paste("\n",
    "Of the `r r$n.obs` cases presented for analysis, `r r$n.keep` are retained, ",
    "so the number of deleted cases due to missing data is `r r$n.obs - r$n.keep`.",
    sep="")

  tx[length(tx)+1] <- paste("\n",
    "Express the obtained model via the estimated ",
    "regression coefficients and their corresponding hypothesis tests and ",
    "confidence intervals. Each hypothesis test specifies that the corresponding population ",
    "coefficient is 0, that is, $H_0: \\beta_i=0$.",
    sep="")

  if (explain) {
    tx[length(tx)+1] <- paste(
      "The *intercept* is the fitted value of response variable, ", nm[1], ", when the ",
      "values of ", predictors, "are zero.")
    if (n.pred > 1) {
        txt <-  ", with the values of all remaining predictor variables held constant"
      }
    else
      txt <- ""
    tx[length(tx)+1] <- paste(
      "Each *slope coefficient* is the average change in the value of response variable, ",
      nm[1], ", for a one-unit increase in the value of the corresponding predictor variable",
      txt, ".", sep="")
      tx[length(tx)+1] <- ""
      tx[length(tx)+1] <- paste(
          "Each 95% confidence interval is constructed about the corresponding",
          "estimated intercept or slope coefficient. ",
          "The margin of error is half the width of the interval.")
  }
  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "```{r, echo=FALSE}"
  tx[length(tx)+1] <- "r$out_coefs"
  tx[length(tx)+1] <- "```"

  tx[length(tx)+1] <- paste(
    "This estimated model is the specific linear function that provides the fitted values of ",
    response, " from the values of ", predictors, ".",
    sep="")
  cv <- paste("$$\\hat Y_{", nm[1], "} = `r round(r$coefs[1],", digits.d, ")` ",
     " + `r round(r$coefs[2],", digits.d, ")` X_{", nm[2], "}", sep="")
  if (n.vars > 2)
    for (i in 3:n.vars)
      cv <- paste(cv, " + `r round(r$coefs[", i, "],", digits.d, ")`", " X_{", nm[i], "}", sep="")
  cv <- paste(cv, "$$", sep="")
  tx[length(tx)+1] <- cv

  if (n.pred > 1) {
    txt2 <- " with the values of the remaining variables held constant"
  }
  else {
    txt2 <- ""
  }
  if (length(which(pvalues[2:length(pvalues)] > 0.05)) > 0) {
    if (length(which(pvalues[2:length(pvalues)] > 0.05)) > 1)
      txt1 <- "these predictor variables "
    else
      txt1 <- "this predictor variable "
    tx[length(tx)+1] <- paste(
      "Note that the following predictor variables have *p*-values larger than $\\alpha=0.05$: ", 
      "`r names(which(r$pvalues[2:length(r$pvalues)] > 0.05))`. Each null hypothesis of no ",
      "relationship could not be rejected, so there is a reasonable possibility ",
      "that ", txt1, "may not contribute to explaining the values of ", response, txt2, ". ",
      "These variables will likely demonstrate little contribution to $R^2_{adj}$ ",
      "shown in the all possible subset regressions that follow later.",
      sep="")
  }

  n.sig <- length(which(pvalues[2:length(pvalues)] <= 0.05)) 
  if (n.sig > 0) {
    if (length(which(pvalues[2:length(pvalues)] > 0.05)) > 1) {
      txt1 <- "These predictor variables each have "
      txt2 <- "their corresponding confidence intervals"
      txt3 <- "these coefficients"
    }
    else {
      txt1 <- "This predictor variable has "
      txt2 <- "its corresponding confidence interval"
      txt3 <- "this coefficient"
    }
    tx[length(tx)+1] <- paste("\n",
      txt1, "a *p*-value less than or equal to $\\alpha=0.05$: ", 
      "`r names(which(r$pvalues[2:length(r$pvalues)] <= 0.05))`. To extend the results ",
      "beyond this sample, interpret the meaning of ", txt3, " in terms of ", txt2, ". ",
      sep="")
      if (n.pred > 1) tx[length(tx)+1] <- paste(
        "When examining the ",
        "subsequent analysis of all possible subset models, these ", n.sig, " variables ",
        "likely form the most parsimonious model in terms of their cumulative ",
        "contribution to obtained value of $R^2_{adj}$.", 
        sep="")

    for (i in 1:n.sig) { 
      j <- which(pvalues[2:length(pvalues)] <= 0.05)[i] 
      if (n.pred > 1) {
        txt <- paste(", with the values of the other predictor variables ",
        " held constant", sep="")
      }
      else
        txt <- ""
      if (i == 1 && n.pred > 1) tx[length(tx)+1] <- ""
      if (n.pred > 1) tx[length(tx)+1] <- paste("*", pred[j], "*: ", sep="")
      tx[length(tx)+1] <- paste(
        "With 95% confidence, for each unit increase in the value of ", pred[j], ", ",
        "the average value of ", response, " changes somewhere in the range from  ",
        "`r round(r$cilb[", j+1, "],", digits.d, ")`", " to ",
        "`r round(r$ciub[", j+1, "],", digits.d, ")`", txt, ".  ",
        sep="") 
    }
  }

  
  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "### Model Fit"

  tx[length(tx)+1] <- paste(
    "To estimate a model does not necessarily result in a useful model. ",
    "One important consideration is how well the model fits the data, that is, ",
    "to what extent do the ",
    "values of ", response, " fitted by the model match the actual ",
    "data values of ", response, "?",
    sep="")

  tx[length(tx)+1] <- "```{r, echo=FALSE}"
  tx[length(tx)+1] <-"r$out_fit" 
  tx[length(tx)+1] <- "```"

  tx[length(tx)+1] <- paste(
      "One consideration is the standard deviation of the residuals, ",
      "$s_e=`r round(r$se,", digits.d, ")`$, which reflects ",
      "the unexplained variability of ", response, ", ",
      "the variability of the data values of ", response, " about the ",
      "corresponding fitted values", ". ",
      sep="") 

  if (explain) {
    tx[length(tx)+1] <- paste(
        "Are the residuals typically ",
        "close to their mean of zero, or are they scattered with ",
        "relatively large positive and negative values? ",
        "For a normal distribution, about 95% of the values are within ",
        "two standard deviations of the mean, for a range of four standard deviations.",
        sep="")
  }

  if (explain) {
    tx[length(tx)+1] <- paste("\n",
        "$R^2$, R-squared, is the proportion of the overall variability of response variable ",
        response, " that is accounted for by the model beyond the null model, that is the model ",
        "where the fitted value is just the mean of ", response, " for all the data values. ",
        "$R^2_{adj}$, adjusted R-squared, adjusts R-squared downward according to the ",
        "degrees of freedom. Unlike R-squared, the adjusted version increases ",
        "when a new predictor variable is added to the model only if the ",
        "new variable improves the model more than would be expected by ",
        "chance.", sep="")
  }
  tx[length(tx)+1] <- paste(
    "From this analysis we have $R^2 =`r round(r$Rsq,2)`$, compared to the ",
    "adjusted value of $R^2_{adj} =`r round(r$Rsqadj,2)`$.")

  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- paste(
    "The ANOVA table presents the amount of the explained and unexplained ",
    "variation. The sum of the squared residuals, the value minimized by the ",
    "OLS estimated coefficients, is ",
    " SSE = `r format(round(r$SSE,", digits.d, "), big.mark=\",\",scientific=FALSE)`.", sep="")

  tx[length(tx)+1] <- "```{r, echo=FALSE}"
  tx[length(tx)+1] <-"r$out_anova" 
  tx[length(tx)+1] <- "```"




  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "## Relations Among the Variables"

  if (n.pred > 1)
    txt <- paste("The model has multiple predictor variables, so we have a scatter plot matrix. ",
            " Each scatter plot in the matrix also contains a non-linear best-fit curve.")
  else
    txt <- ""
  tx[length(tx)+1] <- paste("Visually summarize all the relationships among the variables in the ",
                      "model with the scatterplot. ", txt, sep="") 
  
  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "```{r, echo=FALSE}"
  tx[length(tx)+1] <- "regPlot(r, 1, pred.intervals=FALSE)"
  tx[length(tx)+1] <- "```"
  if (explain) {
    tx[length(tx)+1] <- ""
    if (n.pred > 1) txt <- "s" else txt <- ""
    tx[length(tx)+1] <- paste(
      "The correlations of response variable ", response, " with the predictor variable", txt,
      " ", predictors, " should be high.",
      sep="")
    if (n.pred > 1)
      tx[length(tx)+1] <-  paste("The correlations of the predictor variables ",
        "with each other should be small.\n", sep="")
  }
  tx[length(tx)+1] <- "```{r, echo=FALSE}"
  tx[length(tx)+1] <-"r$out_cor" 
  tx[length(tx)+1] <- "```"


  if (n.pred >1) {

    tx[length(tx)+1] <- paste(
      "The collinearity analysis assesses the extent that the ",
      "predictor variables are linearly dependent on each other, of which the prime ",
      "example is a high pairwise correlation, but any linear dependency amongst two or more ",
      "predictor variables suffices to indicate collinearity. Redundancy indicated by ",
      "collinearity results in unstable estimates and the inclusion of more ",
      "predictor variables than is needed. Parsimony is always preferred.",
      sep="")

    if (explain) {
    tx[length(tx)+1] <- paste("\n",
      "Tolerance and the Variance Inflation Factor (VIF) indicate collinearity. ",
      "They are computed for each predictor variable by regressing that predictor ",
      "onto all of the remaining predictor variables. Tolerance is 1 minus the ",
      "resulting $R^2$. A high $R^2$ that results from this regression that involves ",
      "just predictor variables is the primary indicator of collinearity for that predictor, ",
      "and so tolerance should be high, approximately larger than 0.20 or so.", 
      "The VIF is the reciprocal of tolerance and so usually should be less than ",
      "approximately 5.",
      sep="")
    }

    tx[length(tx)+1] <- "```{r, echo=FALSE}"
    tx[length(tx)+1] <-"r$out_collinear" 
    tx[length(tx)+1] <- "```"

    tx[length(tx)+1] <- paste(
      "Especially when collinearity is present, can a simpler model be more effective? ",
      "To answer, assess the fit for models that correspond to all possible combinations ",
      "of the predictor variables. A 1 means the predictor variable is in the model, a 0 ",
      "means it is excluded from the model.",
      sep="")

    tx[length(tx)+1] <- paste( "\n",
      "Note that this analysis only describes the available data, so does not literally ",
      "generalize to the population. This subset analysis is a descriptive heuristic that can ",
      "effectively help eliminate unnecessary predictor variables from your model, but all ",
      "resulting inferential statistics such as *p*-values are no longer valid. Ultimately the model ",
      "requires cross-validation on a new data set.",
      sep="")

    tx[length(tx)+1] <- "```{r, echo=FALSE}"
    tx[length(tx)+1] <-"r$out_subsets" 
    tx[length(tx)+1] <- "```"

  }  # end n.pred > 1


  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "## Analysis of Residuals and Influence"

  tx[length(tx)+1] <- paste(
    "Values of ", response, " fitted by the estimated model do not ",
    "equal the corresponding data value. Hence the residuals. What are the largest ",
    "residuals? Even more important to understand are the indices derived from the ",
    "residuals, such as Cook's distance, an indicator of the influence calculated for ",
    "each row of data on the estimated coefficients.",
    sep="")

  if (explain) {
    tx[length(tx)+1] <- paste(
      "The identification of cases that have a large residual ",
      "and/or undue influence on the estimation of the model helps ",
      "detect potential outliers.  Each of the following statistics is ",
      "calculated for a single case (row of data). ",
      sep="")
    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste(
      "*residual*: Value of the response variable ", response, " minus its fitted value.  \n",
      "*rstudent*: Externally Studentized residual, standardized value of the ",
      "    residual from a model estimated without the case present.  \n",
      "*dffits*: The influence of the case on its own fitted value.  \n",
      "*cooks*: Cook's Distance, the aggregate influence of the case ",
      "    on the estimation of the model coefficients.\n",
      sep="")
  }

  tx[length(tx)+1] <- "```{r, echo=FALSE}"
  tx[length(tx)+1] <-"r$out_residuals" 
  tx[length(tx)+1] <- "```"


  tx[length(tx)+1] <- paste(
    "From this analysis we have the five largest Cook's distances: ",
    "$`r round(r$cooks.max,2)`$.")


  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "## Prediction Intervals"
  
  if (explain) {
    tx[length(tx)+1] <- paste(
      "The standard deviation of the residuals, assumed to be the same ",
      "for each set of values of the predictor variables, estimates the ",
      "modeling error. However, even for predictions of future values of ", response, " ", 
      "from the current data from which the model is estimated, the forecasts are based on future ",
      "responses from the collection of new data. That is, the sampling ",
      "error of the sample regression line must also be considered in the ",
      "assessment of forecasting error. The amount of sampling error varies ",
      "depends on the values of the predictor variables.",
      sep="")
    tx[length(tx)+1] <- paste("\n",
      "The 95% confidence interval around each fitted value of the sample ",
      "regression model is given below, as well as the likely range of ",
      "forecasting error, the 95% prediction interval, the expected range ",
      "in which the actual future value of the response variable, ", nm[1], ",  ",
      "will likely be found.  This forecasting error depends on both modeling ", 
      "error and sampling error.\n",
      sep="")
  }

  tx[length(tx)+1] <- paste(
    "Each row of data values includes a fitted value based on the model estimated ",
    "from the sample data. Each row also contains a corresponding 95% prediction ",
    "interval on the likely range of the values of the response variable for those ",
    "same values of the corresponding predictor variables. ",
    "This interval applies to new data beyond the original data in the sample of ",
    " data from which the model parameters were estimated. ",
    sep="")

  tx[length(tx)+1] <- "```{r, echo=FALSE}"
  tx[length(tx)+1] <-"r$out_predict" 
  tx[length(tx)+1] <- "```"

  if (n.pred == 1 && pred.rows > 0) {
  tx[length(tx)+1] <- paste(
    "The confidence interval for the regression line, ",
    "and the much larger prediction intervals for the individual data points ",
    "can now be illustrated with an enhancement of the original scatter plot.",
    sep="")
  tx[length(tx)+1] <- "```{r, echo=FALSE}"
  tx[length(tx)+1] <- "regPlot(r, 1)"
  tx[length(tx)+1] <- "```"
  }




  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "## Model Validity"
  tx[length(tx)+1] <- paste(
    "The residuals should be normally distributed. Violation of normality does not ",
    "bias the estimates, but it does render the inferential tests invalid.",
    sep="")
  tx[length(tx)+1] <- "```{r, echo=FALSE}"
  tx[length(tx)+1] <- "regPlot(r, 2)"
  tx[length(tx)+1] <- "```"
  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "The residuals should not correlate with any other variable, including the fitted values."
  tx[length(tx)+1] <- "```{r, echo=FALSE}"
  tx[length(tx)+1] <- "regPlot(r, 3)"
  tx[length(tx)+1] <- "```"

  return(tx)

}
