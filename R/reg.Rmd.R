.reg.Rmd <-
function(nm, dname, fun_call, n_res_rows, n_pred_rows, res_sort, ancova=FALSE,
         digits_d, results, explain, interpret, code,
         pvalues, tolerances, resid.max, numeric.all, X1_new,
         new.val=matrix(nrow=n.vars-1, ncol=2, byrow=TRUE),
         Rmd_data, Rmd_custom, Rmd_dir, Rmd_labels) {

  # simplify function call
  fncl <- .fun_call.deparse(fun_call) 
  if (regexec("Rmd", fncl)[1] > 0)     fc <- .rm.arg("Rmd", fncl) 
  if (regexec("explain", fc)[1] > 0)   fc <- .rm.arg.ns("explain", fc) 
  if (regexec("interpret", fc)[1] > 0) fc <- .rm.arg.ns("interpret", fc) 
  if (regexec("results", fc)[1] > 0)   fc <- .rm.arg.ns("results", fc) 

  n.vars <- length(nm)
  n.pred <- n.vars - 1


# ---------------------------
# functions to be interpreted
# ---------------------------

lbls <- function(the.lbl) {
  paste("\n------\n\n", "<span style='color:maroon;'>**", the.lbl,
                          "**</span>\n", sep="")
}

eq.model_b <- function() {
  cv <- paste("<span style='color:maroon;'>", "eq.model_b", "</span>")
  cv <- paste(cv, "$$\\hat Y_{", Y, "} = b_0 + b_1 X_{", nm[2], "}", sep="")
  if (n.vars > 2)
    for (i in 3:n.vars)
      cv <- paste(cv, " + b_", i-1, " X_{", nm[i], "}", sep="")
  cv <- paste(cv, "$$", sep="")
}

eq.model_beta <- function() {
  cv <- paste("<span style='color:maroon;'>", "eq.model_beta", "</span>")
  cv <- paste(cv, 
              "$$\\hat Y_{", Y, "} = \\beta_0 + \\beta_1 X_{", nm[2], "}", 
               sep="")
  if (n.vars > 2)
    for (i in 3:n.vars)
      cv <- paste(cv, " + \\beta_", i-1, " X_{", nm[i], "}", sep="")
  cv <- paste(cv, "$$", sep="")
}

eq.model_est <- function() {
  cv <- paste("<span style='color:maroon;'>", "eq.model_est", "</span>")
  cv <- paste(cv,
"$$\\hat Y_{", Y, "} = `r xP(r$coefficients[1],", d_d, ")` ", 
"`r ifelse(sign(r$coefficients)==1, \"+\", \"-\")[2]` ",
"`r xP(abs(r$coefficients[2]),", d_d, ")` X_{", nm[2], "}", 
sep="")
  if (n.vars > 2) {
    for (i in 3:n.vars)
      cv <- paste(cv,
"`r ifelse(sign(r$coefficients)==1, \"+\", \"-\")[", i, "]` ",
" `r xP(abs(r$coefficients[", i, "]),", d_d, ")`",
" X_{", nm[i], "}", "\n",
sep="")
  }
  cv <- paste(cv, "$$")
  return(cv)
}

eq.resid <- function() {
  cv <- paste("<span style='color:maroon;'>", "eq.resid", "</span>")
  cv <- paste(cv, "$$e_i = Y_i - \\hat Y_i$$", sep="")
}

in.read <- function() {
  out <- paste("<span style='color:maroon;'>", "in.read", "</span>\n")
  out <- paste(out, "```{r", show, "}", "\n", sep="")
  out <- paste(out, dname, " <- ", rdcall, "\n", sep="")
  out <- paste(out, "```", "\n", sep="")
}

in.reg <- function() {
  locTRUE <- regexec("graphics = TRUE", fc)
  if (locTRUE == -1) {
    loc <- regexec("graphics = FALSE", fc)
    if (loc == -1) fc <- sub(")$", ", graphics = FALSE)", fc)
  }
  out <- paste("<span style='color:maroon;'>", "in.reg", "</span>\n")
  out <- paste(out, "```{r", show, "}", "\n", sep="")
  out <- paste(out, "r <-", fc, "\n", sep="")
  out <- paste(out, "```", "\n", sep="")
}

out.bck <- function() {
  out <- paste("<span style='color:maroon;'>", "out.bck", "</span>\n")
  out <- paste(out, "```{r", show, "}", "\n", sep="")
  out <- paste(out, "r$out_background", "\n", sep="")
  out <- paste(out, "```", "\n", sep="")
}

out.ANOVA <- function() {
  out <- paste("<span style='color:maroon;'>", "out.ANOVA", "</span>\n")
  out <- paste(out, "```{r", show, "}", "\n", sep="")
  out <- paste(out, "r$out_anova", "\n", sep="")
  out <- paste(out, "```", "\n", sep="")
}

out.estimates <- function() {
  out <- paste("<span style='color:maroon;'>", "out.estimates", "</span>\n")
  out <- paste(out, "```{r", show, "}", "\n", sep="")
  out <- paste(out, "r$out_estimates", "\n", sep="")
  out <- paste(out, "```", "\n", sep="")
}

plot.scatter <- function() {
  out <- "\n"
  out <- paste(out, "```{r echo=FALSE}\n", sep="")
  out <- paste(out, "regPlot(r, 1, ancova, pred.intervals=FALSE)\n", sep="")
  out <- paste(out, "```", "\n", sep="")
}

plot.sp_pred <- function() {
  out <- "\n"
  out <- paste(out, "```{r echo=FALSE}\n", sep="")
  out <- paste(out, "regPlot(r, 1, ancova, pred.intervals=TRUE)\n", sep="")
  out <- paste(out, "```", "\n", sep="")
}

plot.resids_dist <- function() {
  out <- "\n"
  out <- paste(out, "```{r echo=FALSE}\n", sep="")
  out <- paste(out, "regPlot(r, 2, ancova)\n", sep="")
  out <- paste(out, "```", "\n", sep="")
}

plot.fitted_resids <- function() {
  out <- "\n"
  out <- paste(out, "```{r echo=FALSE}\n", sep="")
  out <- paste(out, "regPlot(r, 3, ancova)\n", sep="")
  out <- paste(out, "```", "\n", sep="")
}

out.collinear <- function() {
  out <- paste("<span style='color:maroon;'>", "out.collinear", "</span>\n")
  out <- paste(out, "```{r", show, "}", "\n", sep="")
  out <- paste(out, "r$out_collinear", "\n", sep="")
  out <- paste(out, "```", "\n", sep="")
}

eq.decomp <- function() {
  out <- paste("<span style='color:maroon;'>", "eq.decomp", "</span>")
  out <- paste(out, "$$SS_{", `Y`, "} = SS_{Model} + SS_{Residual}")
  out <- paste(out, " = `r xP(r$anova_model[\"ss\"],", d_d, ")`", sep="")
  out <- paste(out, " + `r xP(r$anova_residual[\"ss\"],", d_d, ")`", sep="")
  out <- paste(out, 
             "= `r xP(r$anova_total[\"ss\"],", d_d,",", uYq, ", semi=TRUE)` $$")
}

eq.model_decomp <- function() {
  out <- paste("<span style='color:maroon;'>", "eq.model_decomp", "</span>")
  out <- paste(out, "$$SS_{Model}")
  for (i in 2:n.vars) out <- paste(out, " + SS_{", nm[i], "}", sep="")
  out <- paste(out, " = `r xP(r$anova_model[\"ss\"],", d_d, ")`$$", sep="")
  out <- sub("+", "=", out, fixed=TRUE)
}

eq.se <- function() {
  out <- paste("<span style='color:maroon;'>", "eq.se", "</span>")
  out <- paste(out, "$$s_e = ")
  out <- paste(out, "\\sqrt{MS_{Residual}} = ")
  out <- paste(out, "\\sqrt{`r xP(r$anova_residual[\"ms\"],", d_d, ")`} = ",
               sep="")
  out <- paste(out, "`r xP(r$se,", d_d, ",", uYq, ", semi=TRUE)`$$", sep="")
}

eq.se_range <- function() {
  out <- paste("<span style='color:maroon;'>", "eq.se_range", "</span>")
  out <- paste(out, "$$95\\% \\; Range \\; of \\; Variation:",
               "2 * t_{cutoff} * s_e = ")
  out <- paste(out, "2 * `r xP(-qt(0.025, df=r$anova_residual[\"df\"]),3)` * `r xP(r$se,", d_d, ")`")
  out <- paste(out, "= `r xP(r$resid_range,", d_d, ",", uYq, ", semi=TRUE)`$$", sep="")
}

eq.R2 <- function() {
  out <- paste("<span style='color:maroon;'>", "eq.R2", "</span>")
  out <- paste(out, "$$R^2 = 1 - \\frac{SS_{Residual}}{SS_{", Y, "}} = ")
  out <- paste(out, "1 - \\frac{`r xP(r$anova_residual[\"ss\"],", d_d, ")`} ")
  out <- paste(out, "{`r xP(r$anova_total[\"ss\"],", d_d, ")`} = ")
  out <- paste(out, "`r xP(r$Rsq,3)` $$ ") 
}

eq.R2adj <- function() {
  out <- paste("<span style='color:maroon;'>", "eq.R2adj", "</span>")
  out <- paste(out, "$$R^2_{adj} = 1 - \\frac{MS_{Residual}}{MS_{", Y, "}} = ")
  out <- paste(out, "1 - \\frac{SS_{Residual} \\; / \\; 
                    `r r$anova_residual[\"df\"]`} {SS_{", Y, "} \\; / \\;
                    `r r$anova_total[\"df\"]`} = ")
  out <- paste(out, "1 - \\frac{`r xP(r$anova_residual[\"ms\"],", d_d, ")`} ")
  out <- paste(out, "{`r xP(r$anova_total[\"ms\"],", d_d, ")`} = ")
  out <- paste(out, "`r xP(r$Rsqadj,3)` $$ ") 
}

eq.R2press <- function() {
  out <- paste("<span style='color:maroon;'>", "eq.R2press", "</span>")
  out <- paste(out, "$$R^2_{PRESS} = 1 - \\frac{SS_{PRE}}{SS_{", Y, "}} = ")
  out <- paste(out, "1 - \\frac{`r xP(r$PRESS,", d_d, ")`} ",
                    "{`r xP(r$anova_total[\"ss\"],", d_d, ")`} = ", sep="")
  out <- paste(out, "`r xP(r$RsqPRESS,3)` $$ ") 
}

eq.multNull <- function() {
  cv <- paste("\\beta_{", nm[2], "} = \\beta_{", nm[3], "}", sep="")
  if (n.vars > 2)
    for (i in 4:n.vars)
      cv <- paste(cv, " = \\beta_{", nm[i], "}", sep="")
  out <- paste("<span style='color:maroon;'>", "eq.multNull", "</span>")
  out <- paste(out, "\n",
"\\begin{aligned}\n",
"H_0&: \\;", cv, "= 0\\\\",
"H_1&: \\; at \\; least \\;  one \\;  \\beta_j \\ne 0",
"\\end{aligned}"
) 
}

eq.mult2Null <- function() {
  cv <- paste("\\beta_{", nm[n.vars-1], "} = \\beta_{", nm[n.vars], "}", sep="")
  out <- paste("<span style='color:maroon;'>", "eq.mult2Null", "</span>")
  out <- paste(out, "\n$$\n",
"\\begin{aligned}\n",
"H_0&: \\;", cv, "=0\\\\",
"H_1&: \\; at \\; least \\;  one \\;  \\beta_j \\ne 0",
"\\end{aligned}\n",
"$$") 
}

in.nest <- function() {
  cv <- "n <- Nest("
  cv <- paste(cv, nm[1], ", c(", sep="")
  for (i in 2:(n.vars-2)) {
    txt <- ","
    if (i == n.vars-2) txt <- ")"
    cv <- paste(cv, nm[i], txt, sep="")
  }
  cv <- paste(cv,", c(", nm[n.vars-1], ", ", nm[n.vars],"))", sep="")
  out <- paste("<span style='color:maroon;'>", "in.nest", "</span>\n")
  out <- paste(out, "```{r", show, "}", "\n", sep="")
  out <- paste(out, cv, "\n", sep="") 
  out <- paste(out, "```", "\n", sep="")
}

out.fit <- function() {
  out <- paste("<span style='color:maroon;'>", "out.fit", "</span>\n")
  out <- paste(out, "```{r", show, "}", "\n", sep="")
  out <- paste(out, "r$out_fit", "\n")
  out <- paste(out, "```", "\n", sep="")
}

out.nest <- function() {
  out <- paste("<span style='color:maroon;'>", "out.nest", "</span>\n")
  out <- paste(out, "```{r", show, "}", "\n", sep="")
  out <- paste(out, "n$out_models", "\n")
  out <- paste(out, "```", "\n", sep="")
}

out.av_nest <- function() {
  out <- paste("<span style='color:maroon;'>", "out.av_nest", "</span>\n")
  out <- paste(out, "```{r", show, "}", "\n", sep="")
  out <- paste(out, "n$out_anova", "\n")
  out <- paste(out, "```", "\n", sep="")
}

out.predict <- function() {
  out <- paste("<span style='color:maroon;'>", "out.predict", "</span>\n")
  out <- paste(out, "```{r", show, "}", "\n", sep="")
  out <- paste(out, "r$out_predict", "\n")
  out <- paste(out, "```", "\n", sep="")
}

out.subsets <- function() {
  out <- paste("<span style='color:maroon;'>", "out.subsets", "</span>\n")
  out <- paste(out, "```{r", show, "}", "\n", sep="")
  out <- paste(out, "r$out_subsets", "\n")
  out <- paste(out, "```", "\n", sep="")
}

out.residuals <- function() {
  out <- paste("<span style='color:maroon;'>", "out.residuals", "</span>\n")
  out <- paste(out, "```{r", show, "}", "\n", sep="")
  out <- paste(out, "r$out_residuals", "\n")
  out <- paste(out, "```", "\n", sep="")
}

eq.s_pred <- function() {
  out <- paste("<span style='color:maroon;'>", "eq.s_pred", "</span>")
  out <- paste(out, "$$s_{\\hat Y_{p,i}} = \\sqrt{s^2_e + s^2_{\\hat Y_{p,i}}}$$")
}

intr.tolerance <- function() {
      out <- ""
      l20 <- length(which(tolerances < 0.20))
      if (l20 > 1)
        hh <- "s have tolerances"
      else
        hh <- " has a tolerance"

      if (l20 > 0)
        out <- paste(
"Collinearity is indicated. ", 
xU(xNum(l20)), " variable", hh, " less than the ", 
"cutoff of 0.20: ",  # r$tolerances has no names, so get them at r$model
"`r xAnd(names(r$model[which(r$tolerances < 0.20) + 1]))`.", sep="")

      l2030 <- length(which(tolerances >= 0.20 & tolerances < 0.30))
      if (l2030 > 1)
        hh <- "s have tolerances"
      else
        hh <- " has a tolerance"

      if (l2030 > 0) 
         out <- paste(out,
xU(xNum(l2030)), " variable", hh, " greater than the ", 
"cutoff of 0.20, but still somewhat low, less than 0.30: ",
"`r xAnd(names(r$model[which(r$tolerances >= 0.20 & r$tolerances < 0.30) + 1]))`. ",
sep="")

      if (length(which(tolerances < 0.30)) == 0)
        out <- paste(out,
"No collinearity exists according to the tolerance cutoff of 0.30. ",
sep="")

      pl2 <- ifelse (length(which(tolerances == min(tolerances))) > 1, "s", "")
      vrb <- ifelse (length(which(tolerances == min(tolerances))) > 1,
                     "are", "is")

      out <- paste(out, " ",
"The predictor variable", pl2, " with the lowest tolerance ", vrb, " ",
"`r xAnd(names(r$model[which(r$tolerances == min(r$tolerances)) + 1]))` at ",
"`r xP(min(r$tolerances),3)`.",
sep="")
}

intr.max_influence <- function() {
    cv <- character(10)
    cv[1] <- paste("`r xP(r$resid.max[1],3)`", sep="")
    for (i in 2:4)
      cv <- paste(cv, ", `r xP(r$resid.max[", i, "],3)`", sep="")
    cv <- paste(cv, " and `r xP(r$resid.max[5],3)`.", sep="")
    

    if (res_sort == "cooks") txt <- "Cook's distances"
    else if (res_sort == "rstudent") txt <- "Studentized residuals" 
    else if (res_sort == "dffits") txt <- "dffits values" 
 
      out <- paste(
"From this analysis, the largest ", txt, ": ", cv,
sep="")
}

intr.threshold_influence <- function() {
    lbl <- xRow(resid.max)
    if (res_sort == "cooks"  && (length(which(resid.max > 1)) > 0))
      out <- paste(
"The following case", hh, " more than the ", 
"cutoff of 1: ", xAnd(lbl[which(resid.max > 1)]), ". ",
sep="")

    if (res_sort == "cooks"  && (length(which(resid.max > 1)) == 0))
      out <- paste(
"No cases have a Cook's distance larger than 1 in this analysis. ", 
sep="")
}

in.new_data <- function() {
  out <- paste("<span style='color:maroon;'>", "in.new_data", "</span>  \n")
  for (i in 1:n.pred) {
      out <- paste(out,
        pred[i], ": ", new.val[i,1], ", ", new.val[i,2], "  \n", sep="")
  }
  return(out)
}

in.reg_new <- function() {
  locTRUE <- regexec("graphics = TRUE", fc)
  if (locTRUE == -1) {
    loc <- regexec("graphics = FALSE", fc)
    if (loc == -1) fc <- sub(")$", ", graphics = FALSE)", fc)
  }
  cv <- ",\n        "
  for (i in 1:n.pred)
    cv <- paste(cv,
" X", i, "_new=c(", new.val[i,1], ",", new.val[i,2], ")",
ifelse(i == n.pred, "", ","), 
sep="")
#   cv <- paste(cv, ",\n         graphics = FALSE", sep="")
    fc <- sub(")", paste(cv, ")", sep=""), fc, fixed=TRUE)
  out <- paste("<span style='color:maroon;'>", "in.reg_new", "</span>\n")
  out <- paste(out, "```{r", show, "}", "\n", sep="")
  out <- paste(out, "r <-", fc, "\n", sep="")
  out <- paste(out, "```", "\n", sep="")
}


intr.CI_nosig <- function() {
  gt05 <- length(which(pvalues[2:length(pvalues)] > 0.05)) 
  if (gt05 > 0) {
    if (gt05  > 1) {
      txt1 <- "these"
      txt3 <- "have _p_-values"
      txt5 <- "each "
      pl2 <- "s"
    }
    else {
      txt1 <- "this"
      txt3 <- "has a _p_-value"
      txt5 <- "the "
      pl2 <- ""
    }
    out <- paste(xU(xNum(gt05)), " predictor variable", pl2, " ", txt3,
           " larger than ", "$\\alpha$ = 0.05: ", 
           "_`r xAnd(names(which(r$pvalues[2:length(r$pvalues)] > 0.05)))`_. ", 
           sep="")
    out <- paste(out, xU(txt5), "null hypothesis of no ",
"relationship could not be rejected, so there is a reasonable possibility ",
"that ", txt5, " predictor variable may not contribute to ",
"explaining the values of ", Y, cnst, ". ", sep="")
  }  #  p > .05
}

intr.CI_sig <- function() {
  n.sig <- length(which(pvalues[2:length(pvalues)] <= 0.05)) 
  if (n.sig > 0) {
    if (length(which(pvalues[2:length(pvalues)] <= 0.05)) > 1) {
      txt1 <- "These predictor variables each have "
      txt2 <- "their"
      txt3 <- "these corresponding slope coefficients"
      txt4 <- "these"
      pl3 <- "s"
    }
    else {
      txt1 <- "This predictor variable has "
      txt2 <- "its"
      txt3 <- "this corresponding slope coefficient"
      txt4 <- "this"
      pl3 <- ""
    }
    out <- paste("\n",
txt1, "a _p_-value less than or equal to $\\alpha$ = 0.05: ", 
"_`r xAnd(names(which(r$pvalues[2:length(r$pvalues)] <= 0.05)))`_. ",
sep="")

      if (n.pred > 1  && n.sig < n.pred)
        out <- paste(out,
"The possibility should be further explored in the remainder of this ",
"analysis that ", txt4," ", xNum(n.sig), " variable", pl3, " ", 
"may form an equally effective ",
"but more parsimonious model in terms of ", txt2, " ",
"cumulative contribution to explaining the values of ", Y, ", ", 
"compared to the current model with ", xNum(n.pred), " predictor variables. ", 
sep="")

      out <- paste(out, "\n\n",
"To extend the ",
"results beyond this sample to the population from which the sample ",
"was obtained, interpret the meaning of ", txt3, " ",
"in terms of ", txt2, " confidence interval", pl3, ". ",
sep="")

      # response variable labels
      txtY <- Y
      if (!is.null(l)) {
        txtY <- l[which(row.names(l) == Y), "label"] 
        if (length(txtY) == 0) txtY <- Y
      }

      # response variable units
      if (!is.null(l)  &&  ncol(l) == 2) {
        uY <- l[which(row.names(l) == Y), "unit"]
        if (length(uY) == 0)  uY <- ""
        uYq <- paste("\"", uY, "\"", sep="")  # unit with quotes
        if (uY != "dollar") {
          if (uY != "")
            txtY <- paste("the value of", uY, "of", txtY)
          else
            txtY <- paste("the value of", txtY)
        }
      }
      else
        uYq <- ""

      # identify significant predictors
      for (i in 1:n.sig) { 
        j <- which(pvalues[2:length(pvalues)] <= 0.05)[i] 
        if (i == 1 && n.pred > 1) tx[length(tx)+1] <- ""
        if (n.pred > 1)
          out <- paste(out, "\n\n", "- _", pred[j], "_: ", sep="")

        # significant predictor j variable labels
        if (!is.null(l)) {
          txtX <- l[which(row.names(l) == pred[j]), "label"]
          if (length(txtX) == 0) txtX <- pred[j]
        } 
        else
          txtX <- pred[j]

        # significant predictor j variable units
        if (!is.null(l)  &&  ncol(l) == 2) {
          uX <- l[which(row.names(l) == pred[j]), "unit"]
          if (length(uX) == 0) uX <- "unit"
        }
        else
          uX <- "unit" 

        txt <- paste(uX, " of ", txtX)

        out <- paste(out,
"With 95% confidence, for each additional ",
txt,
", on average, ", txtY, " changes somewhere between ",
"`r xP(r$cilb[", j+1, "],", d_d, ",", uYq, ")`", " to ",
"`r xP(r$ciub[", j+1, "],", d_d, "," ,uYq, ")`",
sep="") 
        remn <- sub(pred[j], "", pred)  # leaves an empty vector value
        remain <- character(length=0)
        for (k in 1:n.pred)  # collate into a single string
          if (nzchar(remn[k])) remain[length(remain)+1] <- remn[k]
        remain <- xAnd(remain)
        if (n.pred > 1)
          out <- paste(out, ", with the values of ",
            remain, " held constant", sep="")
        out <- paste(out, ".", sep="")
      }  #  i in n.sig

  }  #  n.sig > 0, i.e., at least one predictor significant

  else
    uYq <- ""

  return(out)
}

  # ----------------------------------------------------------
  # set values for symbols to be interpreted in input markdown
  # ----------------------------------------------------------

  # input markdown is sent to .RmdParse(), which processes the input
  #  including providing the values for all defined symbols, and then
  #  returns the processed input to this function, which then outputs
  #  to the R markdown document

  uYq <- ""  # initialize in case interpret=FALSE, so not used

  # internal
  d_d <- digits_d  # needed here for internal to function calls
  pred <- character(length=0)
  for (i in 1:n.pred) pred[i] <- nm[i+1]

  # for users
  Y <- nm[1]
  X <- xAnd(pred)
  W_n.vars <- xNum(n.vars)
  W_n.pred <- xNum(n.pred)

  if (n.pred > 1) {
    et <- "each "
    et.c <- "Each "
    pl <- "s" 
    cnst <- ", with the values of all remaining predictor variables held constant"
    mult <- paste("through $b_", n.pred, "$", sep="")
  }
  else {
    et <- "the "
    et.c <- "The"
    pl <- ""
    cnst <- ""
    mult <- ""
  }


  # --------------------------------------
  # get variable labels and units if exist
  # --------------------------------------

  l.name <- "l"
  if (l.name %in% ls(name=.GlobalEnv)) {
    l <- get(l.name, pos=.GlobalEnv)
    n.lbls <- length(which(row.names(l) %in% nm))
    var.lbl <- character(length=0)
    # var.unit <- character(length=0)
    for (i in 1:n.lbls) {
      i.row <- which(row.names(l) == nm[i])
      if (length(i.row) > 0) if (is.numeric(i.row)) if (all(!is.na(l[i.row, ])))
        var.lbl[i] <- l[i.row,1]

      myunits <- NULL
      #myunits <- attr(get(dname, pos=.GlobalEnv), which="variable.units")
      #if (!is.null(myunits))
    }
  }
  else {
    l <- NULL
    var.lbl <- NULL
    myunits <- NULL
  }
 

  # ------------------------------------------
  # flags to use default input or custom input
  # ------------------------------------------

  alt1Intro <-  ifelse ("1Intro" %in% Rmd_custom, TRUE, FALSE)
  alt2Data <-  ifelse ("2Data" %in% Rmd_custom, TRUE, FALSE)
  alt3Model <-  ifelse ("3Model" %in% Rmd_custom, TRUE, FALSE)
  alt4Fit <- ifelse ("4Fit" %in% Rmd_custom, TRUE, FALSE)
  alt5Rel <- ifelse ("5Rel" %in% Rmd_custom, TRUE, FALSE)
  alt6Infl <- ifelse ("6Infl" %in% Rmd_custom, TRUE, FALSE)
  alt7Pred <- ifelse ("7Pred" %in% Rmd_custom, TRUE, FALSE)
  alt8Valid <- ifelse ("8Valid" %in% Rmd_custom, TRUE, FALSE)


  # ----------------------
  # create R markdown file
  # ----------------------

  # front matter
  # ------------
  tx <- character(length = 0)

  tx[length(tx)+1] <- "---"
  tx[length(tx)+1] <- "output:"
  tx[length(tx)+1] <- "  html_document:"
  tx[length(tx)+1] <- "    toc: true"
  tx[length(tx)+1] <- "    toc_depth: 4"
  tx[length(tx)+1] <- "    fig_height: 4.5"
  tx[length(tx)+1] <- "    fig_width: 5.5"
# tx[length(tx)+1] <- ""
  if (n.pred > 1)
    tx[length(tx)+1] <- paste("title: \"Multiple Regression of ", nm[1],
                              "\"", sep="")
  else
    tx[length(tx)+1] <- paste("title: \"Regression of ", Y, " on ", X, "\"",
                               sep="")
  tx[length(tx)+1] <- "---"


  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "***"

  v <- packageVersion("lessR")
  tx[length(tx)+1] <- paste(
"_", format(Sys.time(), "%a %b %d, %Y at %H:%M"), " &nbsp; with ",
"lessR version ", v, "_",
sep="")

  tx[length(tx)+1] <- paste("\n",
"_Output Options: explain=", explain, ", interpret=", interpret,
", results=", results, ", code=", code, "_",
sep="")

  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "***"


  tx[length(tx)+1] <- "```{r echo=FALSE}"
  tx[length(tx)+1] <- "suppressPackageStartupMessages(library(lessR))  # lessR"
  tx[length(tx)+1] <- "```"

  txtY <- ""
  if (!is.null(var.lbl)) {
    if (!is.na(var.lbl[1])) {
      Ylbl <- var.lbl[1]
      txtY <- paste(", ", Ylbl, sep="")
    }
  }

  txtX <- ""
  if (n.pred == 1) {
    if (!is.null(var.lbl)) {
      if (!is.na(var.lbl[2])) {
        Xlbl <- var.lbl[2]
        txtX <- paste(", ", Xlbl, sep="")
      }
    }
  }  # end n.pred==1



  show <- ifelse (code, "", ", echo=FALSE")
  tx[length(tx)+1] <- paste("`r results  <- ", results, "`", sep="")
  tx[length(tx)+1] <- paste("`r explain  <- ", explain, "`", sep="")
  tx[length(tx)+1] <- paste("`r interpret  <- ", interpret, "`", sep="")


# 1 - Intro
# ---------

    if (Rmd_labels) tx[length(tx)+1] <- lbls("1Intro.txt")
    if (!alt1Intro) {
      sf <- system.file("Rmd/reg/1Intro.txt", package="lessR")
      tx[length(tx)+1] <- .RmdParse(sf, NULL, n.pred, explain, results, interpret)
    }
    else  # read and process alternate text
      tx[length(tx)+1] <- .RmdParse("1Intro.txt", Rmd_dir, n.pred,
                                 explain, results, interpret)



# 2 - Data
# --------


  if (is.null(Rmd_data)) 
    rdcall <- getOption("read.call")  # the last Read statement
  else 
    rdcall <- paste("Read(\"", Rmd_data, "\")", sep="")
  if (is.null(rdcall)) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
       "To generate an R markdown file, first read the data for this\n",
       "regression analysis with the lessR function Read(), or\n",
       "specify a path name for  Rmd_data.\n\n",
       "ex:  d <- Read(\"\")\n\n")
  }

  if (code) {
    ref <- .get.arg("ref", rdcall)  # only works for Read, not rd or rd_brief
    if (ref %in% c("Employee", "Reading", "Cars93", "Jackets", "Learn", 
                   "Mach4", "BodyMeas", "StockPrice", "WeightLoss",
                   "Anova_1way", "Anova_2way")) {
      tx[length(tx)+1] <- paste(
"Here read from a data file included with the `lessR` package.",
sep="")
    } 
  }

  # get dname resolved to its value and pass the value into markdown
  tx[length(tx)+1] <- paste("`r data.in <- ", dname, "`", sep="")

  if (Rmd_labels) tx[length(tx)+1] <- lbls("2Data.txt")
  if (!alt2Data) {
    sf <- system.file("Rmd/reg/2Data.txt", package="lessR")
    tx[length(tx)+1] <- .RmdParse(sf, NULL, n.pred, explain, results, interpret)
  }
  else  # read and process alternate text
    tx[length(tx)+1] <- .RmdParse("2Data.txt", Rmd_dir, n.pred,
                               explain, results, interpret)



# 3 - Model
# ---------

  if (!alt3Model) {
    if (Rmd_labels) tx[length(tx)+1] <- lbls("3Model.txt")
    sf <- system.file("Rmd/reg/3Model.txt", package="lessR")
    tx[length(tx)+1] <- .RmdParse(sf, NULL, n.pred, explain, results, interpret)
  }
  else
    tx[length(tx)+1] <- .RmdParse("3Model.txt", Rmd_dir, n.pred,
                               explain, results, interpret)



# 4 - Fit
# -------

  if (Rmd_labels) tx[length(tx)+1] <- lbls("4Fit.txt")
  if (!alt4Fit) {
    sf <- system.file("Rmd/reg/4Fit.txt", package="lessR")
    tx[length(tx)+1] <- .RmdParse(sf, NULL, n.pred, explain, results, interpret)
  }
  else
    tx[length(tx)+1] <- .RmdParse("4Fit.txt", Rmd_dir, n.pred, explain,
                               results, interpret)


# 5 - Relations
# -------------

  if (numeric.all) {      

  if (nchar(uYq) == 0) uYq <- NULL

  tx[length(tx)+1] <- paste("`r d_d <-", d_d, "`")

  if (n.pred==1) {
    if (Rmd_labels) tx[length(tx)+1] <- lbls("5Relations1.txt")
    if (!alt5Rel) {
      sf <- system.file("Rmd/reg/5Relations1.txt", package="lessR")
      tx[length(tx)+1] <- .RmdParse(sf, NULL, n.pred, explain, results, interpret)
    }
    else
      tx[length(tx)+1] <- .RmdParse("5Relations1.txt", Rmd_dir, n.pred,
                                 explain, results, interpret)
  }

  else {  # multiple regression`
    if (Rmd_labels) tx[length(tx)+1] <- lbls("5RelationsM.txt")
    if (!alt5Rel) {
      sf <- system.file("Rmd/reg/5RelationsM.txt", package="lessR")
      tx[length(tx)+1] <- .RmdParse(sf, NULL, n.pred, explain, results, interpret)
    }
    else
      tx[length(tx)+1] <- .RmdParse("5RelationsM.txt", Rmd_dir, n.pred,
                                 explain, results, interpret)
  }

  }  # end numeric.all

  else {
   tx[length(tx)+1] <- "## Relations"
   tx[length(tx)+1] <- 
       "No analysis due to non-numeric variables."
  }



  
  if (n_res_rows > 0) {


# 6 - Influence
# -------------

    if (length(which(resid.max > 1)) > 1)
      hh <- "s have values"
    else
      hh <- " has a value"

  if (Rmd_labels) tx[length(tx)+1] <- lbls("6Influence.txt")
  if (!alt6Infl) {
    sf <- system.file("Rmd/reg/6Influence.txt", package="lessR")
    tx[length(tx)+1] <- .RmdParse(sf, NULL, n.pred, explain, results, interpret,
                               res_sort=res_sort)
  }
  else
    tx[length(tx)+1] <- .RmdParse("6Influence.txt", Rmd_dir, n.pred,
                               explain, results, interpret,
                               res_sort=res_sort)
  }  # n_res_rows > 0




#7 - Prediction
# -------------

  if (!is.null(uYq)) if (nchar(uYq) == 0) uYq <- NULL

  if (n_pred_rows > 0  && n.pred <= 6  &&  numeric.all  &&  is.null(X1_new)) {
    tx[length(tx)+1] <- paste("`r uYq <- ", paste("\"", uYq, "\"`", sep=""), sep="")

    if (Rmd_labels) tx[length(tx)+1] <- lbls("7Prediction.txt")
    if (!alt7Pred) {
      sf <- system.file("Rmd/reg/7Prediction.txt", package="lessR")
      tx[length(tx)+1] <- .RmdParse(sf, NULL, n.pred, explain, results, interpret)
    }
    else
      tx[length(tx)+1] <- .RmdParse("7Prediction.txt", Rmd_dir, n.pred,
                                 explain, results, interpret)
  }



#8 - Validity
# -----------

  if (Rmd_labels) tx[length(tx)+1] <- lbls("8Validity.txt")
  if (!alt8Valid) {
    sf <- system.file("Rmd/reg/8Validity.txt", package="lessR")
    tx[length(tx)+1] <- .RmdParse(sf, NULL, n.pred, explain, results, interpret)
  }
  else
    tx[length(tx)+1] <- .RmdParse("8Validity.txt", Rmd_dir, n.pred,
                               explain, results, interpret)



  return(tx)

}
