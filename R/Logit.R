Logit <-
function(my_formula, data=d, filter=NULL,
         digits_d=4, text_width=120, 

         brief=getOption("brief"),

         res_rows=NULL, res_sort=c("cooks","rstudent","dffits","off"), 
         pred=TRUE, pred_all=FALSE, prob_cut=0.5, cooks_cut=1, 

         X1_new=NULL, X2_new=NULL, X3_new=NULL, X4_new=NULL, 
         X5_new=NULL, X6_new=NULL, 

         pdf_file=NULL, width=5, height=5, ...) {
 

  # a dot in a parameter name to an underscore
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    change <- c("digits.d", "text.width", "res.rows", "res.sort", "pred.all",
                "cooks.cut", "X1.new", "X2.new", "X3.new", "X4.new", 
                "X5.new", "X6.new", "pdf.file")
    for (i in 1:length(dots)) {
      if (names(dots)[i] %in% change) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }

  if (missing(my_formula)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Specify a model by listing it first or set according to:  my_formula\n\n")
  }


  # let deprecated mydata work as default
  dfs <- .getdfs() 
  mydata.ok <- FALSE
  if (!is.null(dfs)) {
    if ("mydata" %in% dfs  &&  !("d" %in% dfs)) {
      d <- mydata
      df.name <- "mydata"
      mydata.ok <- TRUE
      options(dname = df.name)
    }
  }

  if (!mydata.ok) {
    df.name <- deparse(substitute(data))  # get name of data table
    options(dname = df.name)
  }
 
  # if a tibble convert to data frame
  if (!is.null(dfs)) {
    if (df.name %in% dfs) {  # tibble to df
      if (any(grepl("tbl", class(data), fixed=TRUE))) {
        data <- data.frame(data, stringsAsFactors=FALSE)
      }
    }
  }


  # convert character variables to factors (for DV)
  n_col <- apply(data, 2, function(x) sum(!is.na(x)))  # num values per variable
  nu.col <- apply(data, 2, function(x) length(unique(na.omit(x))))  # num unique
  fnu.col <- logical(length=ncol(data))  # logical vector, initial values to FALSE

  for (i in 1:ncol(data)) 
    if (is.character(data[,i])) if (nu.col[i] != n_col[i]) fnu.col[i] <- TRUE 
  data[fnu.col] <- lapply(data[fnu.col], as.factor) 

  # produce actual argument, such as from an abbreviation, and flag if not exist
  res_sort <- match.arg(res_sort)

  max_new <- 6

  show_R <- FALSE
  explain <- FALSE

  old.opt <- options()
  on.exit(options(old.opt))

  options(width=text_width)

  # output
  cor <- TRUE

  if (brief) {
    if (is.null(res_rows)) res_rows <- 0
    pred <- FALSE
    relate <- FALSE
    show_R <- FALSE
   }
   else relate <- TRUE
      
  pre <- "> "
  line <- "--------------------------------------------------------------------\n"
  
  if (!exists(df.name)) {
    txtC <- "Function reg requires the data exist in a data frame\n"
    if (df.name == "d") 
      txtA <- ", the default data frame name, " else txtA <- " "
    txtB1 <- "Either create the data frame, such as with data.frame function, or\n"
    txtB2 <- "  specify the actual data frame with the parameter: data\n"
    txtB <- paste(txtB1, txtB2, sep="")
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        txtC, "Data frame ", df.name, txtA, "does not exist\n\n", txtB, "\n")
  }

  nm <- all.vars(my_formula)  # names of vars in the model
  n.vars <- length(nm)
  n.pred <- n.vars - 1

  if (!missing(filter)) {  # subset rows
    r <- eval(substitute(filter), envir=data, enclos=parent.frame())
    r <- r & !is.na(r)  # set missing for a row to FALSE
    data <- data[r,,drop=FALSE]
  }
  n.obs <- nrow(data)
  
  if (n.pred > 1) collinear <- TRUE else collinear <- FALSE

  is.bin <- TRUE
  if (is.factor(data[,nm[1]])) { 
     if (nlevels(data[,nm[1]]) != 2) is.bin  <- FALSE
  }
  else {
    for (i in 1:n.obs)
      if (!is.na(data[i,nm[1]]))
        if (data[i,nm[1]]!=0 && data[i,nm[1]]!=1) is.bin <- FALSE
  }
  if (!is.bin) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Response variable: ", nm[1], "\n",
      "If numeric, can only have values of 0 or 1.\n",
      "If a factor, can only have two levels.\n\n")
  }
  
  if ( !is.null(X1_new)  &&  (n.pred) > max_new ) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "No new data for prediction if more than", max_new,
          "predictor variables.\n\n")
  }

  # check new.data option for consistency  
  new.data <- FALSE
  if ( (n.pred) <= max_new ) { 
    for (i in 1:(n.pred)) {
      pp <- eval(parse(text=paste("X", toString(i),"_new",sep="")))
      if (!is.null(pp)) new.data <- TRUE
    }
    if (new.data) for (i in 1:(n.pred)) {
      pp <- eval(parse(text=paste("X", toString(i),"_new",sep="")))
      if (is.null(pp)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Specified new data values for one predictor variable, so do for all.\n\n")
      }
    }
  }
 
  # sort values of the one predictor variable for scatterplot
  #   so that the prediction/confidence intervals can be drawn
  if (n.pred == 1) { 
    o <- order(data[,nm[2]], decreasing=FALSE)
    data <- data[o,]
  }

  in.data.frame <- TRUE
  for (i in 1:n.vars) {
    if (!(nm[i] %in% names(data))) {
      cat("\n\n\n>>> Note: ", nm[i], "is not in the data frame.\n")
      in.data.frame <- FALSE
    }
  }

  # -----------------------------------------------------------
  # logit analysis
  #   all subsequent analysis done on data in model construct lm.out$model
  #   this model construct contains only model vars, with Y listed first
  lm.out <- glm(my_formula, data=data, family="binomial")
  # -----------------------------------------------------------

  n.keep <- nrow(lm.out$model)
    
  if (is.null(res_rows)) if (n.keep < 20) res_rows <- n.keep else res_rows <- 20 
  if (res_rows == "all")
    res_rows <- n.keep  # turn off resids with res_rows=0 call
  
  cat("\n")
  if (sys.nframe() == 1) {  # only accurate if not called from model
    cat("Data Frame: ", df.name, "\n\n")
  }

  for (i in 1:n.vars) {
    ind <- i
    .varlist(n.pred, ind, nm[i], "Predictor", n.obs, n.keep)
  }
  
  
  cat( "\n\n\n", "  BASIC ANALYSIS", "\n\n")

  cat("Model Coefficients\n")

  sm <- summary(lm.out)
  sm1 <- sm$coefficients
  # conf intervals from standard errors, not confint.glm that requires MASS
  ci <- suppressMessages(confint.default(lm.out, level=0.95)) 
  smc <- cbind(sm1, ci)

  buf <- 0 
  for (i in 1:length(n.vars)) {
    lng.lbl <- nchar(rownames(smc)[i])
    if (lng.lbl > buf) buf <- lng.lbl 
   }

  max.num <- integer(length=0)
  for (icol in 1:6) {
    max.num[icol] <- 0 
    for (i in 1:n.vars) {
      ln.nm <- nchar(as.character(trunc(smc[i,icol]))) + digits_d + 1
      if (ln.nm > max.num[icol]) max.num[icol] <- ln.nm
    }
    if (max.num[icol] < 9) max.num[icol] <- 9 
  }

  est.lbl <- .fmtc("Estimate", max.num[1]+1)
  ste.lbl <- .fmtc("  Std Err", max.num[2]+2)
  t.lbl <-  "  z-value"
  p.lbl <-  "  p-value"
  lb.lbl <- .fmtc("Lower 95%", max.num[5]+3)
  ub.lbl <- .fmtc("Upper 95%", max.num[6]+3)
  cat("\n", rep(" ", buf), est.lbl, ste.lbl, t.lbl, p.lbl, lb.lbl, ub.lbl, sep="", "\n")
  for (i in 1:(nrow(smc))) {
    rlb <- .fmtc(rownames(smc)[i], buf)
    ub <- .fmt(smc[i,6], digits_d, max.num[6])
    est <- .fmt(smc[i,1], digits_d, max.num[1])
    ste <- .fmt(smc[i,2], digits_d, max.num[2]+1)
    tvl <- .fmt(smc[i,3], 3, 8)
    pvl <- .fmt(smc[i,4], 3, 8)
    lb <- .fmt(smc[i,5], digits_d, max.num[5])
    ub <- .fmt(smc[i,6], digits_d, max.num[6])
    cat(rlb, est, ste, tvl, pvl, " ", lb, " ", ub, "\n")
  }

  # odds ratios and 95% CI
  OR <- coef(lm.out)
  orci <- exp(cbind(OR, ci))
  cat("\n\nOdds ratios and confidence intervals\n\n")
  max.num <- 9
  OR.lbl <- .fmtc("Odds Ratio", max.num+1)
  lb.lbl <- .fmtc("Lower 95%", max.num+3)
  ub.lbl <- .fmtc("Upper 95%", max.num+3)
  cat(rep(" ",13), OR.lbl, lb.lbl, ub.lbl, sep="", "\n")
  for (i in 1:(nrow(orci))) {
    rlb <- .fmtc(rownames(orci)[i], buf)
    or.est <- .fmt(orci[i,1], digits_d, max.num)
    lb <- .fmt(orci[i,2], digits_d, max.num)
    ub <- .fmt(orci[i,3], digits_d, max.num)
    cat(rlb, " ", or.est, " ", lb, " ", ub, "\n")
  }

  # model fit
  cat("\n\n")
  cat("Model Fit\n")
  cat("\n")
  cat("    Null deviance:", .fmt(sm$null.deviance,3), "on",
      .fmti(sm$df.null), "degrees of freedom\n")
  cat("Residual deviance:", .fmt(sm$deviance,3),  "on",
      .fmti(sm$df.residual), "degrees of freedom\n\n")
  cat("AIC:", sm$aic, "\n\n") 
  cat("Number of iterations to convergence:", sm$iter, "\n\n")

  # check for all numeric vars (except Y)  in.data.frame <- TRUE
  numeric.all <- TRUE
  for (i in 2:n.vars) {
    if (in.data.frame && !is.numeric(data[1,which(names(data) == nm[i])])) {
      cat("\n\n\n>>> Note: ", nm[i], "is not a numeric variable.\n")
      numeric.all <- FALSE
    }
  }
 
  # collinearity    
  if (collinear) {
    cat( "\n", "Collinearity", "\n", sep="")
    cat("\n")
    if (numeric.all) {

    # need to run reg on a numeric of Y to get usual 
      if (is.factor(data[,nm[1]]))  {
        Y <- as.numeric(data[,nm[1]])
        m.f <- paste("Y ~", nm[2])
        if (n.pred > 1)
          for (i in 2:n.pred) m.f <- paste(m.f, "+", nm[i+1])
        m.d <- data.frame(Y, data)
      }
      else {
        m.f <- my_formula
        m.d <- data
      }

      r.out <- lm(as.formula(m.f), data=m.d)
      MSW <- anova(r.out)[n.vars,3]
      sterrs <- summary(r.out)$coefficients[,2]

      vif <- numeric(length = 0)
      tol <- numeric(length = 0)
      for (i in 1:n.pred) {
        v <- var(r.out$model[i+1])
        vif[i] <- (v * (n.keep-1) * sterrs[i+1]^2) / MSW
        tol[i] <- 1 / vif[i]
      }

      out <- cbind(tol, vif)
      colnames(out) <- c("Tolerance", "      VIF")
      rownames(out) <- nm[2:length(nm)]
      print(round(out,3))
 
    }
    else cat("\n>>> No collinearity analysis because not all variables are numeric.\n")
  }
 
  if (res_rows > 0)
    .logit3Residual(lm.out, nm, df.name,
         n.vars, n.pred, n.obs, n.keep, digits_d, pre, line,
         res_sort, res_rows, cooks_cut)
 
  if (pred)
    .logit4Pred(lm.out, nm, df.name, my_formula, brief, res_rows,
         n.vars, n.pred, n.obs, n.keep, digits_d, pre, line,
         new.data, pred, pred_all, prob_cut, 
         numeric.all, in.data.frame, X1_new, 
         X2_new, X3_new, X4_new, X5_new, X6_new,
         pdf_file, width, height)

  invisible(lm.out)

}
