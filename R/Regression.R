Regression <-
function(my.formula, data=mydata, digits.d=NULL, standardize=FALSE,
         text.width=120, brief=getOption("brief"), explain=TRUE, show.R=FALSE,

         res.rows=NULL, res.sort=c("cooks","rstudent","dffits","off"), 
         pred.rows=NULL, pred.sort=c("predint", "off"),
         subsets=NULL, cooks.cut=1, 

         scatter.coef=TRUE, scatter.3D=FALSE, graphics=TRUE, knitr.file=NULL,

         X1.new=NULL, X2.new=NULL, X3.new=NULL, X4.new=NULL, 
         X5.new=NULL,

         pdf=FALSE, pdf.width=5, pdf.height=5, refs=FALSE, ...) {


  if (missing(my.formula)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Specify a model by listing it first or set according to:  my.formula\n\n")
  }

  dname <- deparse(substitute(data))  # get data frame name for cor before sort
  options(dname = dname)

  # produce actual argument, such as from an abbreviation, and flag if not exist
  res.sort <- match.arg(res.sort)
  pred.sort <- match.arg(pred.sort)

  old.opt <- options()
  on.exit(options(old.opt))

  options(width=text.width)

  # output
  cor <- TRUE  # do even if only one pred variable

  if (brief) {
    if (is.null(res.rows)) res.rows <- 0
    if (is.null(pred.rows)) pred.rows <- 0
    relate <- FALSE
   }
   else relate <- TRUE
  
  if (!exists(dname)) {
    txtC <- "Function reg requires the data exist in a data frame\n"
    if (dname == "mydata") 
      txtA <- ", the default data frame name, " else txtA <- " "
    txtB1 <- "Either create the data frame, such as with data.frame function, or\n"
    txtB2 <- "  specify the actual data frame with the parameter: data\n"
    txtB <- paste(txtB1, txtB2, sep="")
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        txtC, "Data frame ", dname, txtA, "does not exist\n\n", txtB, "\n")
  }

  nm <- all.vars(my.formula)  # names of vars in the model
  n.vars <- length(nm)
  n.pred <- n.vars - 1
  n.obs <- nrow(data)

  predictors <- character(length=0)
  for (i in 2:n.vars) predictors[i-1] <- nm[i]

  # check that variables exist in the data
  for (i in 1:n.vars) if (!(nm[i] %in% names(data))) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
       "Variable ", nm[i], " is not in the data table ", dname, ".\n\n",
       "Either the variable name is misspelled ",
       "or the wrong data table is specified.\n\n")
  }

  # check that variables are not function calls
  v.str <- deparse(attr(terms.formula(my.formula), which="variables"))
  v.str <- substr(v.str, 6, nchar(v.str)-1)  # remove "list(" and ending ")"
  if (grepl("(", v.str, fixed=TRUE))  {
    txtA <- paste("The reference to a variable in the lessR Regression function can\n",
      "only be a variable name that refers to a variable in a data frame.\n\n", sep="")
    txtB <- "For example, this does not work:\n  > reg(Salary ~ log(Years))\n\n"
    txtC <- "Instead use Transform to first add the new variable to mydata:\n"
    txtD <- "  > mydata <- Transform(YearsLog = log(Years))\n"
    txtE <- "  > reg(Salary ~ YearsLog)"
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        txtA, txtB, txtC, txtD, txtE, "\n")
  }
  
  if(n.pred > 1) {
    collinear <- TRUE
    if (is.null(subsets)) subsets <- TRUE
  }
  else {
    collinear <- FALSE
    subsets <- FALSE
  }

  if ( scatter.3D && (n.pred)!=2 ) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
       "Can have a 3D scatterplot only with exactly two predictor variables.\n\n")
  }
  
  if ( !is.null(X1.new) && (n.pred)>5 ) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "No new data for prediction if more than 5 predictor variables.\n\n")
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
 
  # sort values of the one predictor variable for scatterplot
  #   so that the prediction/confidence intervals can be drawn
  if (n.pred == 1) { 
    o <- order(data[,nm[2]], decreasing=FALSE)
    data <- data[o,]
  }

  if (is.null(digits.d)) digits.d <- .getdigits(data[,nm[1]], 3)

  # standardize option
  if (standardize) {
    for (i in 1:n.vars) {
      data[,nm[i]] <- round(scale(data[,nm[i]]), digits.d)
    }
  }

  in.data.frame <- TRUE
  for (i in 1:n.vars) {
    if (!(nm[i] %in% names(data))) {
      cat("\n\n\n>>> Note: ", nm[i], "is not in the data frame.\n")
      in.data.frame <- FALSE
    }
  }

  # keep track of generated graphic, see if manage graphics
    if (graphics) {
      plot.i <- 0
      plot.title  <- character(length=0)
      manage.gr <- .graphman()
    }

  # --------------------------------------------------------
  # reg analysis
  #   all analysis done on data in model construct lm.out$model
  #   this model construct contains only model vars, with Y listed first
  #assign("lm.out", lm(my.formula, data=data), pos=.GlobalEnv)
  lm.out <- lm(my.formula, data=data)

  if (lm.out$rank < n.vars) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
       "The attempted solution is singular. Too much linear dependence.\n\n")
  }
 
  cook <- cooks.distance(lm.out)
 
  # replace a factor with indicator variables in data frame
  #mm <- model.matrix(my.formula, data=data)
  #mf.out <- data.frame(lm.out$model[,1], mm[,2:ncol(mm)])
  #names(mf.out)[1] <- nm[1]
      

  n.keep <- nrow(lm.out$model)  # lm.out$model is the data with deleted


  # basic analysis
  bck <- .reg1bckBasic(lm.out, dname, digits.d, show.R, n.obs, n.keep)
  tx1bck <- bck$tx

  est <- .reg1modelBasic(lm.out, dname, digits.d, show.R)
  tx1est <- est$tx

  fit <- .reg1fitBasic(lm.out, dname, digits.d, show.R)
  tx1fit <- fit$tx

  anv <- .reg1anvBasic(lm.out, dname, digits.d, show.R)
  tx1anv <- anv$tx 

  # check for all numeric vars  in.data.frame <- TRUE
  numeric.all <- TRUE
  for (i in 1:n.vars) {
      if (in.data.frame && !is.numeric(data[1,which(names(data) == nm[i])])) {
        #cat("\n>>> Note: ", nm[i], "is not a numeric variable.\n",
            #"   No scatter plot(s) generated.\n")
        numeric.all <- FALSE
      }
    }

  # relationship analysis
  tx2rel <- ""; tx2cor <- ""; tx2cln <- ""; tx2all <- ""
  if (relate) {
    rel <- .reg2Relations(lm.out, dname, n.keep, show.R,
         cor, collinear, subsets, numeric.all, in.data.frame)
    tx2cor <- rel$txcor
    tx2cln <- rel$txcln
    tx2all <- rel$txall
    if (!is.matrix(rel$crs)) crs <- round(rel$crs,3) else crs <- NA
    if (!is.vector(rel$tol)) tol <- round(rel$tol,3) else tol <- NA
    if (!is.vector(rel$VIF)) VIF <- round(rel$VIF,3) else VIF <- NA
  }
  else { # not relate
    crs <- NA_real_; tol <- NA; VIF <- NA
  }
  
  # residual analysis  
  if (is.null(res.rows)) if (n.keep < 20) res.rows <- n.keep else res.rows <- 20 
  if (res.rows == "all") res.rows <- n.keep  # turn off resids with res.rows=0

  tx3res <- ""
  cooks.max <- NA
  if (res.rows > 0) {

    # text output
    res <- .reg3txtResidual(lm.out, cook, digits.d, res.sort, res.rows, show.R)
    tx3res <- res$tx
    if (!is.na(res$cooks.max[1])) cooks.max <- round(res$cooks.max,3)

    if (graphics) {
      if (!pdf && manage.gr) {  # set up graphics system
        if (numeric.all || n.pred==1)
          .graphwin(3) 
        else
          .graphwin(2)  # no scatter plot matrix if not all numeric
      }

      if (manage.gr && !pdf) dev.set(which=3)
      plt <- .reg3dnResidual(lm.out, pdf, pdf.width, pdf.height, manage.gr, ...)
      for (i in (plot.i+1):(plot.i+plt$i)) plot.title[i] <- plt$ttl[i-plot.i]
      plot.i <- plot.i + plt$i 

      if (manage.gr && !pdf) dev.set(which=4)
      plt <- .reg3resfitResidual(lm.out, cook, cooks.cut,
                 pdf, pdf.width, pdf.height, manage.gr, ...)
      for (i in (plot.i+1):(plot.i+plt$i)) plot.title[i] <- plt$ttl[i-plot.i]
      plot.i <- plot.i + plt$i
    }

   }  # res.rows > 0

 
  # prediction interval analysis   
  # scatter plot(s)
  if (is.null(pred.rows)) if (n.keep < 25) pred.rows <- n.keep else pred.rows <- 10 
  if (pred.rows == "all") pred.rows <- n.keep  # turn off preds with pred.rows=0

  tx3prd <- ""
  if (pred.rows > 0) {
    prd <- .reg4Pred(lm.out, brief,
         n.keep, digits.d, show.R,
         new.data, pred.sort, pred.rows, scatter.3D, scatter.coef,
         numeric.all, in.data.frame,
         X1.new, X2.new, X3.new, X4.new, X5.new)
    tx3prd <- prd$tx
  }

    if (graphics) {
      if (manage.gr && !pdf) {
        if (res.rows > 0)  # already did two plots 
          dev.set(which=5) 
        else {
          .graphwin(1)  #  only plot is a scatterplot
          dev.set(which=3)
        }
      }
   
      if ((numeric.all || n.pred==1) && in.data.frame) {
        splt <- .reg5Plot(lm.out, res.rows, pred.rows, scatter.coef, scatter.3D,
           X1.new, numeric.all, in.data.frame, prd$cint, prd$pint,
           pdf, pdf.width, pdf.height, manage.gr, ...)

        for (i in (plot.i+1):(plot.i+splt$i)) plot.title[i] <- splt$ttl[i-plot.i]
        plot.i <- plot.i + splt$i
      } 
    }


  # cites
  txcte <- ""
  if (collinear || subsets || scatter.3D) {
    tx <- character(length = 0)
    did <- FALSE
    if (!brief)  {
      if (collinear) {
        txt <- "[Collinearity analysis with Nilsson and Fox's vif function"
        tx[length(tx)+1] <- paste(txt, "from the car package]") 
        did <- TRUE
      }
      if (subsets) {
        txt <- "[Subsets analysis with Thomas Lumley's leaps function"
        tx[length(tx)+1] <- paste(txt, "from the leap's package]") 
        did <- TRUE
      }
    }
    if (scatter.3D) {
      tx[length(tx)+1] <- ""
      txt <- "[3D scatterplot with John Fox's scatter3d function"
      tx[length(tx)+1] <- paste(txt, "from the car package]") 
        did <- TRUE
    }

  if (did) txcte <- tx else txcte=""
  }


# ----------
# References
# ----------
  txref <- ""
  tx <- character(length = 0)
  if (refs) {
    tx[length(tx)+1] <- "  REFERENCES"

    tx[length(tx)+1] <- paste("\n",
        "Function Regression is from David Gerbing's lessR package.\n",
        "  To obtain the reference: Enter citation(\"lessR\")")
    tx[length(tx)+1] <- paste("\n",
        "Collinearity analysis is from the vif function in\n",
        "John Fox's car package.\n",
        "  To obtain the reference: Enter citation(\"car\")")
    tx[length(tx)+1] <- paste("\n",
        "Best model subset analysis is from Thomas Lumley's leaps function\n",
        "in his package leaps.\n",
        "  To obtain the reference: Enter citation(\"leaps\")")
    tx[length(tx)+1] <- paste("\n",
        "All analyses based on R.\n",
        "  To obtain the reference: Enter citation()")
    txref <- tx
  }


  # knitr
  txkfl <- ""
  txknt <- .reg.knitr(nm, dname, n.vars, n.pred, pred.rows, digits.d, explain,
    est$pvalues)
  if (!is.null(knitr.file)) {
    if (grepl(".Rmd", knitr.file)) 
      txt <- ""
    else
      txt <- ".Rmd"
    knitr.file <- paste(knitr.file, txt, sep="") 
    cat(txknt, file=knitr.file, sep="\n")
    txkfl <- .showfile2(knitr.file, "knitr instructions")
  }

  # display list of plots if more than 1
  txplt <- ""
  if (graphics) {
    if (plot.i > 1) txplt <- .plotList2(plot.i, plot.title)

    dev.set(which=2)  # reset graphics window for standard R functions
  }

  class(tx1bck) <- "out_piece"
  class(tx1est) <- "out_piece"
  class(tx1fit) <- "out_piece"
  class(tx1anv) <- "out_piece"
  class(tx2cor) <- "out_piece"
  class(tx2cln) <- "out_piece"
  class(tx2all) <- "out_piece"
  class(tx3res) <- "out_piece"
  class(tx3prd) <- "out_piece"
  class(txcte) <- "out_piece"
  class(txplt) <- "out_piece"
  class(txref) <- "out_piece"
  class(txkfl) <- "out_piece"
  class(txknt) <- "out_piece"
  
  output <- list(out_background=tx1bck, out_coefs=tx1est, out_fit=tx1fit,
    out_anova=tx1anv, out_cor=tx2cor, out_collinear=tx2cln, out_subsets=tx2all,
    out_residuals=tx3res, out_predict=tx3prd, out_cite=txcte, out_ref=txref,
    out_knitr.file=txkfl, out_plots=txplt, formula=my.formula, n.vars=bck$n.vars,
    n.obs=bck$n.obs, n.keep=n.keep, 
    coefs=est$estimates, sterrs=est$sterrs, tvalues=est$tvalues,
    pvalues=est$pvalues, cilb=est$cilb, ciub=est$ciub, SSE=anv$SSE, se=fit$se,
    Rsq=fit$Rsq, Rsqadj=fit$Rsqadj, cor=crs, tolerances=tol, VIF=VIF,
    cooks.max=cooks.max,
    residuals=lm.out$residuals, fitted.values=lm.out$fitted, 
    cooks.distance=cook, model=lm.out$model, terms=lm.out$terms,
    knitr=txknt)
  class(output) <- "out_all"
  return(output)
  
}
