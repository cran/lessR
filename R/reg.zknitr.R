.reg.knitr <-
function(nm, dname, fun.call, n.vars, res.rows, pred.rows, res.sort,
         digits.d, explain, interpret, results, pvalues, tolerances,
         resid.max, numeric.all) {

  fncl <- .fun.call.deparse(fun.call) 
  fc <- .rm.arg("knitr.file", fncl) 

  # set parameters
  n.pred <- n.vars - 1
  d <- digits.d
  Y <- nm[1]
  pred <- character(length=0)
  for (i in 1:n.pred) pred[i] <- nm[i+1]
  X <- tAnd(pred)

  if (n.pred > 1) {
    pl <- "s" 
    et <- "Each "
    cnst <- ", with the values of all remaining predictor variables held constant"
  }
  else {
    pl <- ""
    et <- "The "
    cnst <- ""
  }

  if (explain) show <-  "" else show <- ", echo=FALSE"
  

  tx <- character(length = 0)

  tx[length(tx)+1] <- "---"
  tx[length(tx)+1] <- "output: html_document"
  tx[length(tx)+1] <- "---"

  if (n.pred > 1)
    tx[length(tx)+1] <- paste("# Multiple Regression of ", Y, sep="")
  else
    tx[length(tx)+1] <- paste("# Regression of ", Y, " on ", X, sep="")

  tx[length(tx)+1] <- paste(
"_", format(Sys.time(), "%a %b %d, %Y at %H:%M"), "_",
sep="")

  tx[length(tx)+1] <- paste("\n",
"_Output Options: explain=", explain, ", interpret=", interpret,
", results=", results, "_",
sep="")

  tx[length(tx)+1] <- "```{r echo=FALSE}"
  tx[length(tx)+1] <- "suppressPackageStartupMessages(library(lessR))  # load lessR"
  tx[length(tx)+1] <- "```"


  tx[length(tx)+1] <- paste(
"The purpose of this analysis is to account for the values of the _response ",
"variable_, ", Y, ", in terms of the values of the ", 
"_predictor variable", pl, "_ ", X, ".", 
sep="")







  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "## The Data"

  rdcall <- getOption("read.call")
  if (is.null(rdcall)) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
       "To generate a knitr output file, first read the data for this\n",
       "regression analysis with the lessR function Read.\n\n")
  }

  #ref <- .get.arg("ref", rdcall)
  #if (nchar(ref) == 0) {
      #cat("\n"); stop(call.=FALSE, "\n","------\n",
       #"To generate a knitr output file, need to specify a file name\n",
       #"to Read the data for this regression analysis.\n\n")
  #}


  if (explain) {
    tx[length(tx)+1] <- paste("Read the data. ")
    ref <- .get.arg("ref", rdcall)  # only works for Read, not rd or rd.brief
    if (ref %in% c("Employee", "Reading", "Cars93", "Jackets", "Learn", "Mach4")) {
      ref  <- paste(ref, "\"", ", format=\"lessR", sep="")
      tx[length(tx)+1] <- paste(
"Here read from a data file included with the `lessR` package.",
sep="")
    }
  }

  if (explain) tx[length(tx)+1] <- paste(
"The corresponding data values for the ",
"variables in the model comprise the _training ",
"data_, from which the model is estimated. ",
sep="")

  if (results) {
    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste("```{r", show, "}", sep="")
    tx[length(tx)+1] <- paste(dname, " <- ", rdcall, sep="")
    tx[length(tx)+1] <- "```"
  }

  tx[length(tx)+1] <- paste(
"Data from the following variables are available for analysis: ",
"`r tAnd(names(", dname, "))`. ",
sep="")







  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "## The Model"

  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "### Specified Model"

  tx[length(tx)+1] <- paste(
"Express ", Y, " as a linear function of ", tNum(n.pred), " ", 
"predictor variable", pl, ": ", X, ". ",
sep="")

  if (explain) tx[length(tx)+1] <- paste(
"Within the context of the model, indicate the response variable with a Y, ",
"subscripted by the variable name, $Y_{", Y, "}$. Write each predictor variable as ",
"a subscript to an X. ",
"From the training data compute $\\hat Y_{", Y, "}$, the _fitted value_ of the ",
"response variable from the model for ",
"a specific set of values for ", X, ". ",
sep="")

  cv <- paste("$$\\hat Y_{", Y, "} = b_0 + b_1 X_{", nm[2], "}", sep="")
  if (n.vars > 2)
    for (i in 3:n.vars) cv <- paste(cv, " + b_", i-1, " X_{", nm[i], "}", sep="")
  cv <- paste(cv, "$$", sep="")
  tx[length(tx)+1] <- cv

    if (explain) tx[length(tx)+1] <- paste(
"The _intercept_, $b_0$, indicates ",
"the fitted value of ", Y, ", when the values of ", X, " are zero.", sep="")

    if (n.pred > 1) {
        txt2 <- paste("through $b_", n.pred, "$", sep="")
      }
    else {
      txt2 <- ""
    }

      if (explain) tx[length(tx)+1] <- paste(
"Applied to the training data, ", tolower(et), "_slope coefficient_, ",
"$b_1$ ", txt2, ", is the ",
"average change in the value of ",
"response variable, ", Y, ", for a one-unit increase in the value of ",
"the corresponding predictor variable",
cnst, ".",
sep="")

    if (explain) tx[length(tx)+1] <- paste("\n",
"To compute $\\hat Y_{", Y, "}$ from the values ",
"of ", X, " for a specific row of data ",
"requires the estimated values of ", 
"the coefficients of the model, the values of ",
"each regression coefficient, $b_j$. ",
"This estimation is based on the _residual_, the difference between the ",
"actual value of ", Y, " for each row of data, ",
"and the corresponding value fitted by the model. ", 
"Here write the model with the subscript _i_ for the $i^{th}$ row of data, ",
"to emphasize that the equation applies to each row of training data. ",
"The name of the response variable is understood ",
"and so is omitted for simplicity. ",
sep="")

    if (explain) tx[length(tx)+1] <- paste(
"$$e_i = Y_i - \\hat Y_i$$",
sep="")

    if (explain) tx[length(tx)+1] <- paste(
"Estimate the coefficients ",
"with ordinary least squares (OLS). ",
"That is, choose the estimates that minimize the sum of the squared ",
"residuals, $\\sum e^2_i$, across all the rows of training data. ",
"Accomplish the estimation and related computations with the `lessR` ",
"function `Regression`.",
sep="")

  loc <- regexec("graphics = FALSE", fc)
  if (loc == -1) fc <- sub(")$", ", graphics=FALSE)", fc)

  if (results) {
    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste("```{r", show, "}", sep="")
    tx[length(tx)+1] <- "# Generate graphics later with `regPlot` function"
    tx[length(tx)+1] <- paste("r <-", fc)
    tx[length(tx)+1] <- "```"
  }

  if (explain) tx[length(tx)+1] <- paste(
"The output begins with a specification of the variables ",
"in the model and a brief description of the data. ",
sep="")

  if (results) {
    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste("```{r", show, "}", sep="")
    tx[length(tx)+1] <-"r$out_background" 
    tx[length(tx)+1] <- "```"
  }

  if (interpret) {
    tx[length(tx)+1] <- paste("\n",
"Of the `r r$n.obs` cases presented for analysis, `r r$n.keep` are retained, ",
"so the number of deleted cases due to missing data is `r r$n.obs - r$n.keep`.",
sep="")
  }




  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "### Estimated Model"

  if (explain) tx[length(tx)+1] <- paste(
"The analysis of the model begins with the estimation of each sample ",
"regression coefficient, $b_j$, from the training data. ",
"Of greater interest is each corresponding population value, $\\beta_j$. ",
sep="")

  cv <- paste("$$\\hat Y_{", Y, "} = \\beta_0 + \\beta_1 X_{", nm[2], "}", sep="")
  if (n.vars > 2)
    for (i in 3:n.vars) cv <- paste(cv, " + \\beta_", i-1, " X_{", nm[i], "}", sep="")
  cv <- paste(cv, "$$", sep="")
  if (explain)tx[length(tx)+1] <- cv

  if (explain) tx[length(tx)+1] <- paste(
"The associated inferential analyses for each estimate are the ",
"hypothesis test and confidence interval.",
sep="")

  if (explain) tx[length(tx)+1] <- paste(
"Each _t_-test evaluates the _null hypothesis_ that ",
"the corresponding _individual_ population regression ",
"coefficient is 0, here for the $j^{th}$ coefficient. ",
sep="")

  if (explain) tx[length(tx)+1] <- paste(
"$$H_0: \\beta_j=0$$\n",
"$$H_1: \\beta_j \\ne 0$$",
sep="")

  if (explain) tx[length(tx)+1] <- paste(
"The confidence interval provides ",
"the range of likely values of the corresponding $\\beta_j$. ",
"Each 95% confidence interval is the margin of error on ",
"either side of the corresponding ",
"estimated intercept or slope coefficient, $b_j$. ",
sep="")
  
  if (results) {
    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste("```{r", show, "}", sep="")
    tx[length(tx)+1] <- "r$out_estimates"
    tx[length(tx)+1] <- "```"
  }

  tx[length(tx)+1] <- paste(
"This estimated model is the specific linear function that provides a ",
"fitted value of ", Y, " from the value", pl, " of ", X, ".",
sep="")

  cv <- paste("$$\\hat Y_{", Y, "} = `r tP(r$coefficients[1],", d, ")` + ",
    "`r tP(r$coefficients[2],", d, ")` X_{", nm[2], "}", sep="")
  if (n.vars > 2)
    for (i in 3:n.vars)
      cv <- paste(cv, " + `r tP(r$coefficients[", i, "],", d, ")`", " X_{",
                  nm[i], "}", sep="")
  cv <- paste(cv, "$$", sep="")
  tx[length(tx)+1] <- cv

  gt05 <- length(which(pvalues[2:length(pvalues)] > 0.05)) 
  if (gt05 > 0) {
    if (gt05  > 1) {
      txt1 <- "these"
      txt3 <- "have _p_-values"
      txt5 <- "Each "
      pl2 <- "s"
    }
    else {
      txt1 <- "this"
      txt3 <- "has a _p_-value"
      txt5 <- "The "
      pl2 <- ""
    }

    if (interpret) tx[length(tx)+1] <- paste(
tNum(gt05, uc=TRUE), " predictor variable", pl2, " ", txt3, " larger than ",
"$\\alpha$ = 0.05: ", 
"`r tAnd(names(which(r$pvalues[2:length(r$pvalues)] > 0.05)))`. ", 
sep="")

    if (interpret) tx[length(tx)+1] <- paste(
txt5, "null hypothesis of no ",
"relationship could not be rejected, so there is a reasonable possibility ",
"that ", txt1, " predictor variable", pl2, " may not contribute to ",
"explaining the values of ", Y, cnst, ". ",
sep="")
  }  #  p > .05

  n.sig <- length(which(pvalues[2:length(pvalues)] <= 0.05)) 
  if (n.sig > 0) {
    if (length(which(pvalues[2:length(pvalues)] <= 0.05)) > 1) {
      txt1 <- "These predictor variables each have "
      txt2 <- "their"
      txt3 <- "these coefficients"
      txt4 <- "these"
      pl3 <- "s"
    }
    else {
      txt1 <- "This predictor variable has "
      txt2 <- "its"
      txt3 <- "this coefficient"
      txt4 <- "this"
      pl3 <- ""
    }

    if (interpret) tx[length(tx)+1] <- paste("\n",
txt1, "a _p_-value less than or equal to $\\alpha$ = 0.05: ", 
"`r tAnd(names(which(r$pvalues[2:length(r$pvalues)] <= 0.05)))`. ",
sep="")

    if (interpret && n.pred > 1  && n.sig < n.pred)
      tx[length(tx)+1] <- paste(
"The possibility should be further explored in the rest of the ",
"analysis that ", txt4," ", tNum(n.sig), " variable", pl3, " ", 
"likely form an equally effective ",
"but more parsimonious model in terms of ", txt2, " ",
"cumulative contribution to explaining the values of ", Y, ", ", 
"compared to the current model with ", tNum(n.pred), " predictor variables. ", 
sep="")

    if (interpret) tx[length(tx)+1] <- paste("\n",
"To extend the ",
"results beyond this sample, interpret the meaning of ", txt3, " ",
"in terms of ", txt2, " corresponding confidence interval", pl3, ". ",
sep="")

    if (interpret) {
      for (i in 1:n.sig) { 
        j <- which(pvalues[2:length(pvalues)] <= 0.05)[i] 
        if (i == 1 && n.pred > 1) tx[length(tx)+1] <- ""
        if (n.pred > 1) tx[length(tx)+1] <- paste(
  "* _", pred[j], "_: ",
  sep="")
        tx[length(tx)+1] <- paste(
  "With 95% confidence, for each additional unit of the value of ",
  pred[j], ", ",
  "on average, the value of ", Y, " changes somewhere between ",
  "`r tP(r$cilb[", j+1, "],", d, ")`", " to ",
  "`r tP(r$ciub[", j+1, "],", d, ")`",
  sep="") 
        remn <- sub(pred[j], "", pred)  # leaves an empty vector value
        remain <- character(length=0)
        for (k in 1:n.pred)  # collate into a single string
          if (nchar(remn[k]) > 0) remain[length(remain)+1] <- remn[k]
        remain <- tAnd(remain)
        if (n.pred > 1)
          tx[length(tx)] <- paste(tx[length(tx)], ", with the values of ",
            remain, " held constant",
  sep="")
        tx[length(tx)] <- paste(tx[length(tx)], ".", sep="")
      }  #  i in n.sig
    }  # end interpret

  }  #  n.sig > 0
  




  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "## Model Fit"

  if (explain) tx[length(tx)+1] <- paste(
"An estimated model is not necessarily a useful model. ",
sep="")

  if (explain) tx[length(tx)+1] <- paste(
"How well does the model fit the training data? ",
"That is, to what extent do the ",
"values of ", Y, " fitted by the model match the actual ",
"data values of ", Y, "? Are the residuals typically ",
"close to their mean of zero, or are they scattered with ",
"relatively large positive and negative values? ",
sep="")



  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "### Partitioning of Variance"


  if (explain) tx[length(tx)+1] <- paste("\n",
"The analysis of fit depends on the adequacy of the model to account for ",
"the variability of the data values of $Y_{", Y, "}$. ",
"The core component of variability is the _sum of squares_, short for ",
"the sum of some type of squared deviations. ",
"Base the total variability of ", Y,  " ",
"on the deviations of its data values ",
"from its mean, ",
"$Y_{", Y, "} - \\bar Y_{", Y, "}$, and then the resulting sums of squares, $SS_{", Y, "}$. ",
sep="")

  if (explain) tx[length(tx)+1] <- paste("\n",
"The analysis of the residuals, ",
"$e = Y_{", Y, "} - \\hat Y_{", Y, "}$, is based on their corresponding sum of squares, ", 
"the value minimized by the least squares estimation procedure, ",
"$\\sum e^2_i$ = $SS_{Residual}$. ",
"It represents variation _not_ accounted for by $\\hat Y_{", Y, "}$. ",
"The corresponding Model (or Regression) sum of squares ",
"is the analysis of the deviations of the fitted values about ",
"the mean, ",
"$\\hat Y_{", Y, "} - \\bar Y_{", Y, "}$. ", 
sep="")

  if (explain) tx[length(tx)+1] <- paste("\n",
"The ANOVA partitions this total sum of squares ",
"into the residual variability, $\\sum e^2_i$, and the Model ",
"sum of squares, $SS_{Model}$. ",
"The analysis of variance table (ANOVA) provides the ",
"analysis of variability from these various sources of variation. ",
sep="")

  if (results) {
    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste("```{r", show, "}", sep="")
    tx[length(tx)+1] <-"r$out_anova" 
    tx[length(tx)+1] <- "```"
  }

  if (results) tx[length(tx)+1] <- paste(
"$$SS_{",Y,"} = SS_{Model} + SS_{Residual} = ",
"`r tP(r$anova_model[\"ss\"],", d, ")` + ",
"`r tP(r$anova_residual[\"ss\"],", d, ")` = ",
"`r tP(r$anova_total[\"ss\"],", d, ")` $$",
sep="")

  if (explain && n.pred > 1) tx[length(tx)+1] <- paste("\n",
"The ANOVA further partitions ",
"the overall model sums of squares by predictor variable. ",
sep="")

  cv <- paste("$$SS_{Model} = SS_{", nm[2], "}", sep="")
  if (n.vars > 2)
    for (i in 3:n.vars) cv <- paste(cv, " + SS_{", nm[i], "}", sep="")

  if (explain && n.pred > 1) tx[length(tx)+1] <- paste(
cv, " = `r tP(r$anova_model[\"ss\"],", d, ")`$$",
sep="")

  if (explain && n.pred > 1) tx[length(tx)+1] <- paste("\n",
"The sum of squares for a predictor variable ",
"is called a _sequential sums of squares_ ",
"because, unless the predictor variables are uncorrelated, its value depends ",
"on the variables listed before it in the specification of the model. ",
sep="")

  if (explain && n.pred > 1) tx[length(tx)+1] <- paste("\n",
"This fundamental relationship of these sums of squares components ",
"is the basis for assessing fit.",
sep="")



  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "### Fit Indices"

  if (explain) tx[length(tx)+1] <- paste("\n",
"From the ANOVA two types of primary indicators of fit are derived: ",
"standard deviation of the residuals and several ",
"$R^2$ statistics. ",
sep="")

  if (results) {
    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste("```{r", show, "}", sep="")
    tx[length(tx)+1] <-"r$out_fit" 
    tx[length(tx)+1] <- "```"
  }

  if (explain) tx[length(tx)+1] <- paste("\n",
"The _standard deviation of the residuals_, $s_e$, directly assesses ",
"the variability of the data values of ", Y, " about the ",
"corresponding fitted values for the training data, the particular ",
"data set from which the model was estimated. ",
"The mean squares in the ANOVA table are variances, a ",
"sum of squares divided by the corresponding degrees of freedom, _df_. ",
"By definition of the standard deviation, ",
"$s_e$ is the square root of the mean square of ",
"the residuals. ",
sep="")

  if (explain) tx[length(tx)+1] <- "$$s_e = "
  if (explain) tx[length(tx)] <- paste(tx[length(tx)], 
"\\sqrt{MS_{Residual}} = ",
"\\sqrt{`r tP(r$anova_residual[\"ms\"],", d, ")`} = ",
sep="")
  if (explain) tx[length(tx)] <- paste(tx[length(tx)],
"`r tP(r$se,", d, ")`$$",
sep="")

  if (interpret) tx[length(tx)+1] <- paste(
"To interpret $s_e$ = `r tP(r$se,", d, ")`, consider the estimated range ",
"of 95% of the values of a normally distributed variable, which ",
"depends on the corresponding 2.5% cutoff from the $t$-distribution ",
"for df=`r r$anova_residual[\"df\"]`: ",
"`r tP(-qt(0.025, df=r$anova_residual[\"df\"]),3)`. ",
sep="")

    if (interpret) tx[length(tx)+1] <- paste(
"$$95\\% \\;  Range: 2 * t_{cutoff} * s_e = ",
"2 * `r tP(-qt(0.025, df=r$anova_residual[\"df\"]),3)` * `r tP(r$se,", d, ")` = `r tP(r$resid_range,", d, ")`$$",
sep="")

    if (interpret) tx[length(tx)+1] <- paste("\n",
"This range of the residuals for the fitted values is the lower limit ",
"of the range of forecasting error presented later. ",
sep="")

  if (explain) tx[length(tx)+1] <- paste("\n",
"A second type of fit index is ",
"$R^2$, the proportion of the overall variability of ",
"response variable ", Y, " that is accounted for by the model, ",
"expressed either in terms of $SS_{Residual}$ or $SS_{Model}$. ",
sep="")

  if (explain) tx[length(tx)+1] <- "$$R^2 = "
  if (explain) tx[length(tx)] <- paste(tx[length(tx)], 
"1 - \\frac{SS_{Residual}}{SS_{", Y, "}} = ",
"\\frac{SS_{Model}}{SS_{", Y, "}} = ",
"\\frac{`r tP(r$anova_model[\"ss\"],", d, ")`} ",
"{`r tP(r$anova_total[\"ss\"],", d, ")`} = ",
sep="")
  if (explain) tx[length(tx)] <- paste(tx[length(tx)],
"`r tP(r$Rsq,3)` $$ ",
sep="")

  if (explain) tx[length(tx)+1] <- paste(
"Unfortunately when any new predictor variable is added ",
"to a model, useful or not, $R^2$ necessarily increases. ",
"Use the adjusted version, $R^2_{adj}$, to more appropriately ",
"compare ",
"models estimated from the same training data with different ",
"numbers of predictors. ",
"$R^2_{adj}$ helps to avoid overfitting a model because it only ",
"increases if a new predictor variable added to the model ",
"improves the fit more than would be expected by ",
"chance. The adjustment considers the ",
"number of predictor variables relative to the number of rows of data ",
"(cases). Accomplish this adjustment with the degrees of freedom, ",
"to shift from the Sum of Squares ",
"to the corresponding Mean Squares. ",
sep="")

  if (explain) tx[length(tx)+1] <- "$$R^2_{adj} = "
  if (explain) tx[length(tx)] <- paste(tx[length(tx)], 
"1 - \\frac{SS_{Residual} \\; / \\; `r r$anova_residual[\"df\"]`}{SS_{", Y, "} \\; / \\; `r r$anova_total[\"df\"]`} = ",
"1 - \\frac{MS_{Residual}}{MS_{", Y, "}} = ",
"1 - \\frac{`r tP(r$anova_residual[\"ms\"],", d, ")`} ",
"{`r tP(r$anova_total[\"ms\"],", d, ")`} = ", 
sep="")
  if (explain) tx[length(tx)] <- paste(tx[length(tx)],
"`r tP(r$Rsqadj,3)`$$",
sep="")

  if (interpret) tx[length(tx)+1] <- paste(
"From this analysis compare $R^2$ = `r tP(r$Rsq,3)` to the ",
"adjusted value of $R^2_{adj}$ = `r tP(r$Rsqadj,3)`, a difference of ", 
"`r tP((r$Rsq-r$Rsqadj), 3)`. A large difference indicates that too many ",
"predictor variables in the model for the available data yielded an overfitted ",
"model. ",
sep="")

  if (explain)  tx[length(tx)+1] <- paste("\n",
"Both $R^2$ and $R^2_{adj}$ describe the fit of the model to the training ",
"data. To assess the fit of the model to forecasting from new data, apply ",
"the concept of the _predictive residual_ (PRE). ",
"Calculate each residual from the ",
"model estimated from all the remaining cases in the training data, ",
"what is called a _leave-one-out_ cross validation procedure. ",
"$SS_{PRE}$, or PRESS, ",
"is the sum of squares of all the predictive residuals in a data set. ",
"From $SS_{PRE}$ define the predictive $R^2$. ",
sep="")

  if (explain) tx[length(tx)+1] <- "$$R^2_{PRESS} = "
  if (explain) tx[length(tx)] <- paste(tx[length(tx)], 
"1 - \\frac{SS_{PRE}}{SS_{", Y, "}} = ",
"1 - \\frac{`r tP(r$PRESS,", d, ")`} ",
"{`r tP(r$anova_total[\"ss\"],", d, ")`} = ",
sep="")
  if (explain) tx[length(tx)] <- paste(tx[length(tx)],
"`r tP(r$RsqPRESS,3)` $$ ",
sep="")

  if (interpret) tx[length(tx)+1] <- paste("\n",
"Because an estimated model at least to some extent overfits the training data, ",
"$R^2_{PRESS}$ = `r tP(r$RsqPRESS,3)` is lower than both $R^2$ and $R^2_{adj}$. ",
sep="")




  tx[length(tx)+1] <- ""
  if (explain && n.pred > 1) 
    tx[length(tx)+1] <- "### Hypothesis Tests of Multiple Coefficients"

  if (explain && n.pred > 1) tx[length(tx)+1] <- paste(
"The ANOVA table presents the overall ",
"hypothesis test that evaluates if _all_ the predictor variables ",
"as a set -- ", X, " -- ", 
"are related to ", Y, " as specified by the model. ",
sep="")

  cv <- paste("\\beta_{", nm[2], "} = \\beta_{", nm[3], "}", sep="")
  if (n.vars > 2)
    for (i in 3:n.vars)
      cv <- paste(cv, " = \\beta_{", nm[i], "}", sep="")
  cv <- paste(cv, "= 0", sep="")

  if (explain  &&  n.pred > 1)  tx[length(tx)+1] <- paste(
"$$\n",
"\\begin{aligned}\n",
"H_0&: \\;", cv, " \\\\\\\\ \n",
"H_1&: \\; at \\; least \\;  one \\;  \\beta_j \\ne 0\n",
"\\end{aligned}\n",
"$$",
sep="")

  if (explain  &&  n.pred > 1) tx[length(tx)+1] <- paste(
"From the sums of squares for the Model and Residuals, ",
"with degrees of freedom of ",
"`r as.integer(r$anova_model[\"df\"])` ",
"and `r as.integer(r$anova_residual[\"df\"])`, ",
"the test statistic is _F_ = `r tP(r$anova_model[\"fvalue\"],", d, ")`, ", 
"with a _p_-value of `r tP(r$anova_model[\"pvalue\"],", d, ")`." ,  
sep="")

  if (explain && n.pred > 1) tx[length(tx)+1] <- paste("\n",
"Progressing through the table of the sequential sums of squares for each ",
"predictor variable, from the last up until the first entry ",
"forms a sequence of increasingly restrictive _nested models_ that ",
"contain successively fewer variables. ",
"For example, the _p_-value of ",
"`r r$pvalues[r$n.vars]` ",
"for the last variable entered into the model, ",
"`r all.vars(r$formula)[r$n.vars]`, is the same for both the ANOVA ",
"table and its regression slope coefficient because in both ",
"cases the effects of all other predictor variables are partialled out. ",
sep="")

  if (explain && n.pred > 2) tx[length(tx)+1] <- paste("\n",
"To help identify predictors that contribute little beyond ",
"those variables previously included in the model, ",
"list the more important variables first in the model specification. ",
"Add together the sequential sum of squares from the ANOVA table ", 
"for variables listed last in the table to form a nested model. ",
"Then test if the designated ",
"_subset_ of the regression coefficients are all equal to zero. ",
"Or, more conveniently, use the `lessR` function `Nest`. ",
sep="")

  if (explain && n.pred > 2) tx[length(tx)+1] <- paste("\n",
"To illustrate, consider the hypothesis test that ",
"the slope coefficients for the last two variables, ",
nm[n.vars-1], " and ", nm[n.vars], ", are both equal to zero. ",
sep="")

  cv <- paste("\\beta_{", nm[n.vars-1], "} = \\beta_{", nm[n.vars], "}", sep="")
  cv <- paste(cv, "= 0", sep="")

  if (explain && n.pred > 2) tx[length(tx)+1] <- paste(
"$$\n",
"\\begin{aligned}\n",
"H_0&: \\;", cv, " \\\\\\\\ \n",
"H_1&: \\; at \\; least \\;  one \\;  \\beta_{", nm[n.vars-1], "}, \\beta_{", nm[n.vars], "} \\ne 0\n",
"\\end{aligned}\n",
"$$",
sep="")

  if (explain && n.pred > 2) tx[length(tx)+1] <- paste("\n",
"The analysis compares nested models to evaluate ",
"if the additional variables in the full model provide ",
"a detectable increase in fit beyond that of the reduced model. ",
"To use `Nest`, specify the response variable ", Y, ", the variables in ",
"the reduced model, and then the additional variables in the full model. ",
sep="")

  cv <- "n <- Nest("
  cv <- paste(cv, nm[1], ", c(", sep="")
  for (i in 2:(n.vars-2)) {
    txt <- ","
    if (i == n.vars-2) txt <- ")"
    cv <- paste(cv, nm[i], txt, sep="")
  }
  cv <- paste(cv,", c(", nm[n.vars-1], ", ", nm[n.vars],"))", sep="")

  if (results && n.pred > 2) {
    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste("```{r", show, "}", sep="")
    tx[length(tx)+1] <- cv 
    tx[length(tx)+1] <- "```"
  }

  if (explain && n.pred > 2) tx[length(tx)+1] <- paste("\n",
"First verify that the reduced and full models are properly specified. ",
sep="")

  if (explain && n.pred > 2) {
    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste("```{r", show, "}", sep="")
    tx[length(tx)+1] <-"n$out_models" 
    tx[length(tx)+1] <- "```"
  }

  if (explain && n.pred > 2) tx[length(tx)+1] <- paste("\n",
"Evidence for accepting the reduced model is to have the test ",
"_not_ be significant, which means that the evaluated ",
"coefficients from the additional variables in the full model ",
"are not detected to be different from 0, and so ",
"perhaps need not be included in the model. ", 
sep="")

  if (explain && n.pred > 2) {
    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste("```{r", show, "}", sep="")
    tx[length(tx)+1] <-"n$out_anova" 
    tx[length(tx)+1] <- "```"
  }

  tx[length(tx)+1] <- "`r reject <- \"Reject the null hypothesis of the tested regression coefficients equal to 0 because of the small _p_-value of\"`"

  tx[length(tx)+1] <- "`r accept <- \"No difference of the coefficients from zero detected because of the relatively large _p_-value of\"`"

  if (explain && n.pred > 2) tx[length(tx)+1] <- paste("\n",
"`r if ((n$anova_tested[5]< 0.05)) reject else accept`",
sep="")
  if (explain && n.pred > 2) tx[length(tx)+1] <- "`r tP(n$anova_tested[5],3)`."

  if (explain && n.pred > 2)tx[length(tx)+1] <- paste(
"Realize that if the reduced model was constructed solely from analyzing ", 
"the initial regression output, the analysis is post-hoc, so the _p_-value ",
"is an interesting heuristic, but cannot be literally interpreted.",
sep="")









  tx[length(tx)+1] <- ""
  if (n.pred > 1)
    tx[length(tx)+1] <- "## Relations Among the Variables"
  else
    tx[length(tx)+1] <- paste("## Relation Between ", Y, " and ", X, sep="")

  if (numeric.all) {      

    tx[length(tx)+1] <- ""
    if (n.pred > 1) tx[length(tx)+1] <- "### Scatter Plot"

    if (explain) tx[length(tx)+1] <- paste("\n",
"How do the variables in the model relate to each other? ",
"The correlation", pl, " of response variable ", Y, " with predictor ",
"variable", pl, " ", X, " should be high. ",
sep="")

    if (explain  &&  n.pred > 1) tx[length(tx)+1] <- paste(
"The correlations of the predictor variables ",
"with each other should be relatively small. ",
sep="")

      if (n.pred == 1) tx[length(tx)+1] <- paste(
"The correlation of ", Y, " with ", X, " in the training data is $r$ = ",
"`r tP(r$cor[2,1],3)`. ",
sep="")


      if (n.pred > 1)
        txt <- "all the relationships among the variables"
      else
        paste("the relationship of ", Y, " and ", X, sep="")
    
      if (explain) tx[length(tx)+1] <- paste("\n",
"Visually summarize ", txt, " in the model ", 
"with the scatterplot. ", 
sep="") 

      if (explain  &&  n.pred > 1) tx[length(tx)+1] <- paste(
"The model has multiple predictor variables, so the ",
"different scatter plots are presented in a scatter plot matrix. ",
"Each scatter plot in the matrix also contains a non-linear best-fit ",
"curve. ",
sep="")

    if (results && n.pred > 1) tx[length(tx)+1] <- paste(
"Express the linear numeric variable relationships among the variables ",
"in the model with their correlations. ",
sep="")

    if (results) {
      tx[length(tx)+1] <- ""
      tx[length(tx)+1] <- paste("```{r", show, "}", sep="")
      if (n.pred > 1)
        tx[length(tx)+1] <- "regPlot(r, 1)  # 1: scatter plot matrix"
      else
        tx[length(tx)+1] <- "regPlot(r, 1, pred.intervals=FALSE)  # 1: scatter plot "
      tx[length(tx)+1] <- "```"
    }


    if (n.pred >1) {

      tx[length(tx)+1] <- ""
      tx[length(tx)+1] <- "### Collinearity Analysis"

      if (explain) tx[length(tx)+1] <- paste(
"The collinearity analysis assesses the extent that the ",
"predictor variables are linearly dependent upon each other. ",
sep="")

      if (explain) tx[length(tx)+1] <- paste(
"Although collinearity does not diminish the fit of the model, ",
"or forecasting efficacy, ",
"it may indicate an overly complex model. ",
"Also, collinear variables have large ",
"standard errors of their estimated slope coefficients, ",
"so the estimates are unstable across different samples. ",
"The unique effects of collinear variables indicated by their ",
"slope coefficients ",
"cannot be statistically disentangled without a very ",
"large sample size. ",
sep="")

      if (explain) tx[length(tx)+1] <- paste(
"A primary example is a high pairwise correlation. In general, any linear ",
"dependency among two or more predictor variables indicates ",
"collinearity. ",
sep="")

      if (explain) tx[length(tx)+1] <- paste("\n",
"To assess collinearity for predictor variable $X_j$ ",
"regress that predictor ",
"onto all of the remaining predictor variables. A high ",
"resulting $R^2_j$ ",
"indicates collinearity for that predictor. ",
"Usually express this result in terms of ",
"the _tolerance_ of the predictor, 1 minus $R^2_j$, ",
"the proportion of variance for $X_j$ _not_ due do the ",
"remaining predictor variables. ",
"Because each $R^2_j$ should be low, presumably at least less than 0.8, ",
"tolerance should be high, at least larger ",
"than approximately 0.20. ",
"An equivalent statistic is the _variance inflation factor_ (VIF), ",
"which indicates the extent collinearity inflates the variance of ",
"the estimated coefficient. VIF is the reciprocal of tolerance, so ",
"usually should be at least less than approximately 5.",
sep="")

      if (results) {
        tx[length(tx)+1] <- ""
        tx[length(tx)+1] <- paste("```{r", show, "}", sep="")
        tx[length(tx)+1] <-"r$out_collinear" 
        tx[length(tx)+1] <- "```"
      }

      l20 <- length(which(tolerances < 0.20))
      if (interpret  &&  l20 > 1)
        hh <- "s have tolerances"
      else
        hh <- " has a tolerance"
    
      if (interpret && l20 > 0)
        tx[length(tx)+1] <- paste(
"Collinearity is indicated. ", 
tNum(l20, uc=TRUE), " variable", hh, " less than the ", 
"cutoff of 0.20: ",
"`r tAnd(names(which(r$tolerances < 0.20)))`. ",
sep="")

      l2030 <- length(which(tolerances >= 0.20 & tolerances < 0.30))
      if (l2030 > 1)
        hh <- "s have tolerances"
      else
        hh <- " has a tolerance"

      if (interpret  &&  l2030 > 0) 
        tx[length(tx)+1] <- paste(
tNum(l2030, uc=TRUE), " variable", hh, " greater than the ", 
"cutoff of 0.20, but still somewhat low, less than 0.30: ",
"`r tAnd(names(which(r$tolerances >= 0.20 & r$tolerances < 0.30)))`. ",
sep="")

      if (interpret && length(which(tolerances < 0.30)) == 0)
        tx[length(tx)+1] <- paste(
"No collinearity exists according to the tolerance cutoff of 0.30. ",
sep="")

      pl2 <- ifelse (length(which(tolerances == min(tolerances))) > 1, "s", "")
      vrb <- ifelse (length(which(tolerances == min(tolerances))) > 1,
                     "are", "is")

      if (results  &&  n.pred > 1) tx[length(tx)+1] <- paste(
"The predictor variable", pl2, " with the lowest tolerance ", vrb, " ",
"`r tAnd(names(which(r$tolerances == min(r$tolerances))))` at ",
"`r tP(min(r$tolerances),3)`.",
sep="")



      tx[length(tx)+1] <- ""
      tx[length(tx)+1] <- "### All Possible Subset Models"

      if (explain) tx[length(tx)+1] <- paste(
"Especially when collinearity is present, can a simpler model be ",
"more, or at least almost, as effective as the current model? ",
sep="")

      if (explain) tx[length(tx)+1] <- paste("\n",
"To investigate, assess the fit for the models that ",
"correspond to all possible combinations of the predictor variables. ",
"Each row of the analysis defines a different model. ",
"A 1 means the predictor variable is in the model, a 0 ",
"means it is excluded from the model.",
sep="")

      if (results) {
        tx[length(tx)+1] <- ""
        tx[length(tx)+1] <- paste("```{r", show, "}", sep="")
        tx[length(tx)+1] <-"r$out_subsets" 
        tx[length(tx)+1] <- "```"
      }

      if (explain) tx[length(tx)+1] <- paste("\n",
"The goal of this analysis is _parsimony_, to obtain the ",
"most explanatory power, here assessed with $R^2_{adj}$, with ",
"the least number of predictor variables, presumably guided also ",
"by the content and meaningfulness of the variables in the model. ",
sep="")

      if (explain) tx[length(tx)+1] <- paste("\n",
"Note that this analysis only describes the available data. ",
"This subset analysis is a ",
"descriptive heuristic that can effectively help eliminate unnecessary ",
"predictor variables from your model, but all resulting inferential ",
"statistics such as _p_-values are no longer valid. Ultimately the model ",
"requires cross-validation on a new data set.",
sep="")

    }  # end n.pred > 1

  }  # end numeric.all

  else
     tx[length(tx)+1] <- 
       "No analysis due to non-numeric variables"






  
  if (res.rows >0) {

    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- "## Analysis of Residuals and Influence"

    if (explain) tx[length(tx)+1] <- paste(
"Values of ", Y, " fitted by the estimated model do not ",
"equal the corresponding data values. Which cases contribute the most ",
"to this lack of fit? ",
sep="")

    if (explain) tx[length(tx)+1] <- paste(
"The identification of cases that have a large residual ",
"and/or undue influence on the estimation of the model helps ",
"detect potential outliers.  ",
"In addition to the data values, fitted value and residual, ",
"the analysis provides the following ",
"values for each single case (row of data). ",
sep="")

    if (explain) tx[length(tx)+1] <- paste("\n",
"* _residual_: Value of the response variable ", Y, " minus its ",
"fitted value, $e = Y_{", Y, "} - \\hat Y_{", Y, "}$ \n",
"* _rstudent_: Externally Studentized residual, standardized value of the ",
"residual from a model estimated without the case present \n",
"* _dffits_: Standardized difference between a fitted value with and without ",
"the case present \n",
"* _cooks_: Cook's Distance, the aggregate influence of the case ",
"on all the fitted values with each fitted value calculated with the case deleted \n",
sep="")

    if (results) {
      tx[length(tx)+1] <- ""
      tx[length(tx)+1] <- paste("```{r", show, "}", sep="")
      tx[length(tx)+1] <-"r$out_residuals" 
      tx[length(tx)+1] <- "```"
    }

    if (res.sort != "off") {
      cv[1] <- paste("`r tP(r$resid.max[1],2)`", sep="")
      for (i in 2:4)
        cv <- paste(cv, ", `r tP(r$resid.max[", i, "],2)`", sep="")
      cv <- paste(cv, " and `r tP(r$resid.max[5],2)`.", sep="")
      

      if (res.sort == "cooks") txt <- "Cook's distances"
      else if (res.sort == "rstudent") txt <- "Studentized residuals" 
      else if (res.sort == "dffits") txt <- "dffits values" 
 
      if (results) tx[length(tx)+1] <- paste(
"From this analysis the five largest ", txt, ": ", cv,
sep="")

      if (interpret  && res.sort == "cooks") tx[length(tx)+1] <- paste("\n",
"An informal criterion ",
"for evaluating the size of Cook\'s distance is a cutoff value of 1 ",
"to indicate too large of a large size. ",
sep="")

    if (length(which(resid.max > 1)) > 1)
      hh <- "s have values"
    else
      hh <- " has a value"
  
    lbl <- tRow(resid.max)
    if (interpret  && res.sort == "cooks"  && (length(which(resid.max > 1)) > 0))
      tx[length(tx)+1] <- paste(
"The following case", hh, " more than the ", 
"cutoff of 1: ", tAnd(lbl[which(resid.max > 1)]), ". ",
"For larger sample sizes this guideline should be reduced as the ",
"influence of any case tends to diminish as the sample size increases. ",
"The best basis for understanding a high Cook's distance value is to ",
"also consider the substantive nature of the underlying data values ",
"of the case and to verify if they were sampled from the same population ",
"as the remaining cases. ",
sep="")


    if (interpret  && res.sort == "cooks"  && (length(which(resid.max > 1)) == 0))
      tx[length(tx)+1] <- paste(
"No cases have a Cook's distance larger than 1 in this analysis. ", 
sep="")


    }  # res.sort is on

  }  # res.rows > 0



  if (pred.rows > 0) {

    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- "## Prediction Intervals"

    if (explain) tx[length(tx)+1] <- paste(
"Prediction is from _new_ data values of ", X, ". Unfortunately, ",  
"prediction is not perfect. ",
"The range of values likely to contain the ",
"actual data value for ", Y, " predicted from specific values of ", X, " ",
"quantifies the _forecasting error_. ",
sep="")
    
    if (explain) tx[length(tx)+1] <- paste("\n",
"The standard deviation of the residuals, $s_e$, assumed to be the same ",
"for all sets of values of the predictor variables, specifies the ",
"modeling error of the fitted values from the training data. ",
"However, for predictions of future values of ", Y, ", new data ",
"are collected. So sampling ",
"error of a value on the regression line, ",
"indicated with $s_{\\hat Y}$, must also be considered in the ",
"assessment of forecasting error, which results in the _standard ",
"error of forecast_. ",
sep="")

    if (explain) tx[length(tx)+1] <- paste("\n",
"$$s_f = \\sqrt{s^2_e + s^2_{\\hat Y}}$$",
sep="")

    if (explain) tx[length(tx)+1] <- paste("\n",
"Unlike modeling error, the amount of ",
"sampling error varies ",
"depending on the values of the predictor variables, so each ",
"row of data has its own value, $s_f$. ",
"Each prediction interval ",
"is the margin of error, the _t_-cutoff multiplied by the corresponding ",
"$s_f$, added and subtracted on either side ",
"of $\\hat Y_{", Y, "}$.",
sep="")

    if (results) tx[length(tx)+1] <- paste("\n",
"The analysis provides each row of data values with a fitted value based ",
"on the model estimated ",
"from the training data, as well as the standard error of forecast. From ",
"these values obtain the lower and upper bounds of the corresponding ",
"95% prediction interval. ",
sep="")

    if (results) {
      tx[length(tx)+1] <- ""
      tx[length(tx)+1] <- paste("```{r", show, "}", sep="")
      tx[length(tx)+1] <-"r$out_predict" 
      tx[length(tx)+1] <- "```"
    }

    if (explain  &&  n.pred == 1  &&  pred.rows > 0)
      tx[length(tx)+1] <- paste(
"The confidence intervals for the points on the regression line, ",
"and the much larger prediction intervals for the individual data points, ",
"can now be illustrated with an enhancement of the original scatter plot.",
sep="")

    if (results  &&  n.pred == 1) {
      tx[length(tx)+1] <- ""
      tx[length(tx)+1] <- paste("```{r", show, "}", sep="")
      tx[length(tx)+1] <- paste(
"regPlot(r, 1)  # 1: scatter plot with prediction intervals",
sep="")
      tx[length(tx)+1] <- "```"
    }

    if (interpret) tx[length(tx)+1] <- paste("\n",
"The size of the prediction intervals vary from a minimum of ",
"`r tP(r$pred_min_max[1], ", d, ")` for `r tAnd(tRow(r$pred_min_max[1]))` ",
"to a maximum of ",
"`r tP(r$pred_min_max[2], ", d, ")` for `r tAnd(tRow(r$pred_min_max[2]))`. ",
sep="")

    }







  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "## Model Validity"

  if (explain) tx[length(tx)+1] <- paste(
"The residuals should be independent, normal random variables with a ",
"mean of zero and constant variance. ",
sep="")

  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "### Distribution of Residuals"

  if (explain) tx[length(tx)+1] <- paste(
"For the inferential analyses to be valid, ",
"the residuals should be normally distributed. ",
sep="")

  if (explain) {
    tx[length(tx)+1] <- paste(
"Violation of normality does not bias the estimates, but ",
"it does render the inferential tests invalid. ",
sep="")
  }

  if (results) {
    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste("```{r", show, "}", sep="")
    tx[length(tx)+1] <- "regPlot(r, 2)  # 2: distribution of residuals"
    tx[length(tx)+1] <- "```"
    tx[length(tx)+1] <- ""
  }


  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "### Fitted Values vs Residuals"

  if (explain) tx[length(tx)+1] <- paste(
"The residuals should represent random variation, free of ",
"any pattern or structure. ",
"They should satisfy the _homoscedasticity_ assumption, ",
"randomly scattered about 0, with approximately ",
"the same level of variability across the range of the fitted values ",
"within a horizontal band around the zero-line. Otherwise they ",
"demonstrate _heteroskedasticity_. ",
sep="")
  
  if (results) {
    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste("```{r", show, "}", sep="")
    tx[length(tx)+1] <- "regPlot(r, 3)  # 3: scatter plot of fitted with residuals"
    tx[length(tx)+1] <- "```"
  }

  return(tx)

}
