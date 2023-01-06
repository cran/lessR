Regression <-
function(my_formula, data=d, rows=NULL,
         digits_d=NULL, n_cat=getOption("n_cat"),

         Rmd=NULL, Rmd_browser=TRUE, 
         Rmd_format=c("html", "word", "pdf", "odt", "none"),
         Rmd_data=NULL, Rmd_custom=NULL, Rmd_dir=path.expand("~/reg"),
         Rmd_labels=FALSE,
         results=getOption("results"), explain=getOption("explain"),
         interpret=getOption("interpret"), code=getOption("code"), 

         text_width=120, brief=getOption("brief"), show_R=FALSE,
         plot_errors=FALSE,

         n_res_rows=NULL, res_sort=c("cooks","rstudent","dffits","off"), 
         n_pred_rows=NULL, pred_sort=c("predint", "off"),
         subsets=NULL, best_sub=c("adjr2", "Cp"), cooks_cut=1, 

         scatter_coef=TRUE, mod=NULL, 

         X1_new=NULL, X2_new=NULL, X3_new=NULL, X4_new=NULL, 
         X5_new=NULL, X6_new=NULL,

         kfold=0, seed=NULL,
         new_scale=c("none", "z", "center", "0to1", "robust"),
         scale_response=FALSE,

         quiet=getOption("quiet"),
         graphics=TRUE, pdf=FALSE, width=6.5, height=6.5, refs=FALSE,
         fun_call=NULL, ...) {


  # produce actual argument, such as from an abbreviation, flag if not exist
  res_sort <- match.arg(res_sort)
  pred_sort <- match.arg(pred_sort)
  new_scale <- match.arg(new_scale)
  best_sub <- match.arg(best_sub)

  old.opt <- options()
  on.exit(options(old.opt))

  options(width=text_width)

  # allow for more than one value, so cannot use match.arg
  # then all 5 get selected, then default to "html"
  if (missing(Rmd_format)  &&  length(Rmd_format) == 5) Rmd_format <- "html"

  # a dot in a parameter name to an underscore
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (length(grep(".", names(dots)[i], fixed=TRUE)) > 0) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
     }
    }
  }

  dots <- list(...)  # check for deprecated parameters
  if (!is.null(dots)) if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (names(dots)[i] == "res_rows") n_res_rows <- dots[[i]] 
      if (names(dots)[i] == "pred_rows") n_pred_rows <- dots[[i]] 
      if (names(dots)[i] == "standardize") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "standardized  parameter  no longer used\n\n",
          "Instead use  recode=\"z\" . Also have \"0to1\" and\n",
          "  \"robust\" (median and IQR replace mean and sd) options.\n")
      }
      if (names(dots)[i] == "knitr.file") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "knitr.file  no longer used\n",
          "Instead use  Rmd  for R Markdown file\n\n")
      }
      if (names(dots)[i] == "quiet") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "quiet  option not available for Regression\n\n")
      }
    }
  }


  if (is.null(fun_call)) fun_call <- match.call()

  if (!is.null(mod)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Parameter  mod  not yet implemented. Coming soon.\n\n")
  }

  if (missing(my_formula)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Specify a model by listing it first or specify with:  my_formula\n\n")
  }

  if (!is.null(Rmd) && brief) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "To create an R Markdown File requires the full version of Regression\n\n")
  }

  # let deprecated mydata work as default
  dfs <- .getdfs() 
  mydata.ok <- FALSE
  if (!is.null(dfs)) {
    if ("mydata" %in% dfs  &&  !("d" %in% dfs)) {
      d <- mydata
      d.name <- "mydata"
      mydata.ok <- TRUE
      options(dname = d.name)
    }
  }

  if (!mydata.ok) {
    d.name <- deparse(substitute(data))  # get name of data table
    options(dname = d.name)
  }
 
  # if a tibble convert to data frame
  if (!is.null(dfs)) {
    if (d.name %in% dfs) {  # tibble to df
      if (any(grepl("tbl", class(data), fixed=TRUE))) {
        data <- data.frame(data, stringsAsFactors=FALSE)
      }
    }
  }


  max_new <- 6

  # output
  cor <- TRUE  # do even if only one pred variable

  if (brief) {
    if (is.null(n_res_rows)) n_res_rows <- 0L
    if (is.null(n_pred_rows) && is.null(X1_new)) n_pred_rows <- 0L
  }
  relate <- ifelse (brief, FALSE, TRUE)

  if (kfold > 0) {
    graphics <- FALSE
    relate <- FALSE 
    n_res_rows <- 0
    n_pred_rows <- 0
  }

  if (!mydata.ok) .nodf(d.name)  # does data frame exist?

  nm <- all.vars(my_formula)  # names of vars in the model
  n.vars <- length(nm)
  n.pred <- n.vars - 1L  # n.pred==0 means null model, y ~ 1

  if (!missing(rows)) {  # subset rows
    r <- eval(substitute(rows), envir=data, enclos=parent.frame())
    if (!any(r)) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "No rows of data with the specified value of\n",
        "rows = ", deparse(substitute(rows)), "\n\n")
    }
    r <- r & !is.na(r)  # set missing for a row to FALSE
    data <- data[r,,drop=FALSE]
  }
  n.obs <- nrow(data)

  predictors <- character(length=n.pred)
  for (i in 2:n.vars) predictors[i-1] <- nm[i]

  # do variables nm exist in d.name?
  # for (i in 1:n.vars) .xcheck(nm[i], d.name, names(data)) 
  .xcheck(nm, d.name, names(data)) 

  # check that variables are not function calls
  v.str <- deparse(attr(terms.formula(my_formula), which="variables"),
                   width.cutoff=500L)
  v.str <- substr(v.str, 6, nchar(v.str)-1)  # remove "list(" and ending ")"
  if (grepl("(", v.str, fixed=TRUE))  {
    txtA <- paste("The reference to a variable in the lessR Regression ",
                  "function can\n",
      "only be a variable name that refers to a variable in a data frame.\n\n",
      sep="")
    txtB <- "For example, this does not work:\n  > reg(Salary ~ log(Years))\n\n"
    txtC <- "Instead use Transform to first add the new variable to d:\n"
    txtD <- "  > d <- Transform(YearsLog = log(Years))\n"
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

  in.data.frame <- TRUE
  for (i in 1:n.vars) {
    if (!(nm[i] %in% names(data))) {
      cat("\n\n\n>>> Note: ", nm[i], "is not in the data frame.\n")
      in.data.frame <- FALSE
    }
  }

  # check for all numeric vars in data.frame <- TRUE
  numeric.all <- TRUE
  for (i in 1:n.vars) {
    if (in.data.frame && !is.numeric(data[1,which(names(data) == nm[i])]))
      numeric.all <- FALSE
  }
    
  if (nrow(data) < 3) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Need more than ", nrow(data), " rows of data.\n\n", sep="")
  } 

  if (!is.null(X1_new)  &&  (n.pred > max_new)) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "No new data for prediction if more than", max_new,
          "predictor variables.\n\n")
  }
   
  if (!is.numeric(data[,nm[1]])) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Response variable, ", nm[1], ", must be numeric.\n\n", sep="")
  }
 
  if (!is.null(X1_new) && !numeric.all) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "All variables must be numeric to use new data for prediction.\n\n")
  }

  # check new.data option for consistency  
  new.data <- FALSE
  if (n.pred > 0  &&  n.pred <= max_new) { 
    for (i in 1:(n.pred)) {
      pp <- eval(parse(text=paste("X", toString(i), "_new", sep="")))
      if (!is.null(pp)) new.data <- TRUE
    }
    if (new.data) for (i in 1:(n.pred)) {
      pp <- eval(parse(text=paste("X", toString(i), "_new", sep="")))
      if (is.null(pp)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Specified new data values for one predictor variable,\n",
          " so do for all.\n\n")
      }
    }
  }
 
  # sort values of the one predictor variable for scatterplot
  #   so that the prediction/confidence intervals can be drawn
  if (n.pred == 1)
    data <- data[order(data[,nm[2]], decreasing=FALSE), ]

  if (is.null(digits_d)) digits_d <- .getdigits(data[,nm[1]], 3)
  options(digits_d=digits_d) 


  # rescale option (if not K-Fold, then do in reg.zKfold.R)
  transf <- NULL  # string to label output in .reg1bckBasic
  if (new_scale != "none") {
    if (new_scale == "z") transf <- "Standardized"
    if (new_scale == "center") transf <- "Centered"
    if (new_scale == "0to1") transf <- "Min-Max (0 to 1)"
    if (new_scale == "robust") transf <- "Robust Version of Standardized"
      if (kfold == 0) {
        i.start <- ifelse (scale_response, 1, 2)
        for (i in i.start:n.vars)  {  # do not rescale y
          unq.x <- length(unique(data[,nm[i]])) 
          if (unq.x > 2  &&  is.numeric(data[,nm[i]]))
            data[,nm[i]] <- rescale(data[,nm[i]], data=NULL,
                                    kind=new_scale, digits_d)
        }
      cat("\nRescaled Data, First Six Rows\n")
      print(data[1:6, nm])
      cat("\n")
      }
  }

  # keep track of generated graphic, see if manage graphics
  if (graphics) {
    plot.i <- 0L
    plot.title  <- character(length=0)
    manage.gr <- .graphman()
  }

  # non-numeric, non-factor concert to factor
  # not needed for lm(), but elsewhere
  if (n.pred > 0) {
    for (i.pred in 2:n.vars) {
      if (!is.numeric(data[,nm[i.pred]])) {
        if (!is.factor(data[,nm[i.pred]])) 
          data[,nm[i.pred]] <- as.factor(data[,nm[i.pred]])
      }
    }
  }


  # --------------------------------------------------------
  # OLS regression analysis
  # --------------------------------------------------------
  #   lmo: lm out
  #   all analysis done on data in model construct lmo$model
  #   this model construct contains only model vars, with Y listed first
  #assign("lmo", lm(my_formula, data=data), pos=.GlobalEnv)
  lmo <- lm(my_formula, data=data, ...)
  # --------------------------------------------------------

  if (lmo$rank < n.vars) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
       "The attempted solution is singular. Too much linear dependence.\n\n")
  }

  n.keep <- nrow(lmo$model)  # lmo$model is the data with deleted
 
  # replace a factor with indicator variables in data frame
  #mm <- model.matrix(my_formula, data=data)
  #mf.out <- data.frame(lmo$model[,1], mm[,2:ncol(mm)])
  #names(mf.out)[1] <- nm[1]


  if (kfold == 0) {
  
    # for ancova to work, the variable must be non-numeric or a factor
    ancova <- FALSE  # ancova for one cont and one cat predictors
    if (n.pred == 2) {
      if (is.numeric(data[,nm[2]]) && is.factor(data[,nm[3]]))
        ancova <- TRUE
      if (is.numeric(data[,nm[3]]) && is.factor(data[,nm[2]]))
        ancova <- TRUE
    }

    title_bck <- "\n  BACKGROUND"
    bck <- .reg1bckBasic(lmo, d.name, digits_d, show_R, n.obs, n.keep,
                         transf)

    title_basic <- "\n  BASIC ANALYSIS"
    est <- .reg1modelBasic(lmo, digits_d, show_R)

    anv <- .reg1anvBasic(lmo, ancova, digits_d, show_R)

    sy <- sd(data[,nm[1]], na.rm=TRUE)
    fit <- .reg1fitBasic(lmo, anv$tot["ss"], sy, digits_d, show_R)

    txkfl <- ""
    title_kfold <- paste("\n  K-FOLD CROSS-VALIDATION", sep="")
    m_se <- NA;  m_MSE <- NA;  m_Rsq <- NA

    title_rel <- "\n  RELATIONS AMONG THE VARIABLES"
    rel <- list()  # allow for rel not being processed and returned
    if (relate  &&  n.pred > 0) {
      max.sublns <- 50
      if (subsets > 1) {
        max.sublns <- subsets
        subsets <- TRUE
      }
      else
        rel$out_subsets <- ""
      rel <- .reg2Relations(lmo, d.name, n.keep, show_R,
           cor, collinear, subsets, best_sub, max.sublns, numeric.all,
           in.data.frame, est$sterrs, anv$MSW)
      if (is.matrix(rel$crs)) crs <- round(rel$crs,3) else crs <- NA
      if (is.vector(rel$tol)) tol <- round(rel$tol,3) else tol <- NA
      if (is.vector(rel$vif)) vif <- round(rel$vif,3) else vif <- NA
    }
    else { # not relate or n.pred > 0
      rel$out_cor <- ""; rel$out_collinear <- "";  rel$out_subsets <- ""
      rel$crs <- NA_real_; rel$tol <- NA; rel$vif <- NA
    }
  
    title_res <- "\n  RESIDUALS AND INFLUENCE"
    if (is.null(n_res_rows)) n_res_rows <- ifelse (n.keep < 20, n.keep, 20) 
    if (n_res_rows == "all") n_res_rows <- n.keep  # turn off resids if no res

    if (n_res_rows > 0) {

      cook <- round(cooks.distance(lmo), 5)
      res <- .reg3txtResidual(lmo, cook, digits_d, res_sort, n_res_rows,
                              show_R)
      if (!is.na(res$resid.max[1])) resid.max <- round(res$resid.max,3)

      if (graphics  &&  n.pred > 0) {
        if (!pdf && manage.gr) {  # set up graphics system
          if (numeric.all || n.pred==1)
            .graphwin(3, width, height) 
          else
            .graphwin(2, width, height)  # no sp matrix if not all numeric
        }

        if (manage.gr && !pdf) dev.set(which=3)
        plt <- .reg3dnResidual(lmo, pdf, width, height, manage.gr, ...)
        for (i in (plot.i+1):(plot.i+plt$i)) plot.title[i] <- plt$ttl[i-plot.i]
        plot.i <- plot.i + plt$i 

        
        if (manage.gr && !pdf) dev.set(which=4)
        fr <- .reg3resfitResidual(lmo, cook, cooks_cut,
                   pdf, width, height, manage.gr)
        for (i in (plot.i+1):(plot.i+fr$i)) plot.title[i] <- fr$ttl[i-plot.i]
        crfitres <- fr$crfitres
        plot.i <- plot.i + fr$i
      } # graphics

    }  # end n_res_rows > 0
    else {
      res <- list()
      res$out_residuals <- ""
      resid.max <- NA
      cook <- NA
    }

    if (is.null(n_pred_rows)) n_pred_rows <- ifelse (n.keep < 25, n.keep, 10) 
    if (n_pred_rows == "all") n_pred_rows <- n.keep  # no preds with n_pred_rows=0

    a <- "\n  PREDICTION ERROR"
    if (n_pred_rows > 0) {
      a <- paste(a, "\n\n-- Data, Predicted, Standard Error of Prediction,",
                          "95% Prediction Intervals")
      a <- paste(a, "\n   [sorted by lower bound of prediction interval]")
      if (n_pred_rows < n.keep  &&  !new.data) 
        a <- paste(a, "\n   [to see all intervals add n_pred_rows=\"all\"]")
      a <- paste(a, "\n", .dash2(46))
    }
    title_pred <- a

    if (n_pred_rows > 0  ||  !is.null(X1_new)) {  # if requested, do X1_new, etc.
      prd <- .reg4Pred(lmo,
           n.keep, digits_d, show_R,
           new.data, pred_sort, n_pred_rows, scatter_coef,
           in.data.frame, X1_new, X2_new, X3_new, X4_new, X5_new, X6_new)
    }
    else {
      prd <- list()
      prd$out_predict <- ""
      prd$predmm <- NA
    }
  }  # end kfold==0

  else {
    txkfl <- ""
    title_kfold <- paste("\n  ", kfold, "-FOLD CROSS-VALIDATION", sep="")
    m_se <- NA;  m_MSE <- NA;  m_Rsq <- NA

    Kfld <- .regKfold(data[,nm], my_formula, kfold, new_scale, scale_response,
                      nm, predictors, n.vars,
                      n.keep, seed, digits_d, show_R)
    txkfl <- Kfld$tx
    m_se <- Kfld$m_se; m_MSE <- Kfld$m_MSE; m_Rsq <- Kfld$m_Rsq
  }

  title_eqs <- ""
  txeqs <- ""
  if (graphics) {
    if (manage.gr && !pdf) {
      if (n_res_rows > 0  &&  n.pred > 0)  # already did two plots 
        dev.set(which=5) 
      else {
        .graphwin(1, width, height)  #  only plot is a scatterplot
        dev.set(which=3)
      }
    }
 
    # Plot scatterplot, maybe ANCOVA
    ancovaOut <- .reg5Plot(lmo, n_res_rows, n_pred_rows, scatter_coef, 
       X1_new, ancova, numeric.all, in.data.frame, prd$cint, prd$pint,
       plot_errors, digits_d, n_cat, pdf, width, height, manage.gr,
       quiet, ...)

    tx <- ""
    if (!is.null(ancovaOut$txeqs)) {
      title_eqs <- paste("\n  MODELS OF", nm[1], "FOR LEVELS OF", ancovaOut$cat)

      # test interaction
      tx[1] <- "-- Test of Interaction"
      tx[length(tx)+1] <- " "

      lm2.out <- lm(lmo$model[,nm[1]] ~ 
                    lmo$model[,nm[2]] * lmo$model[,nm[3]])
      a.tbl <- anova(lm2.out)
      a <- round(a.tbl[3,], 3)  # 3rd row is interaction row
      a.int <- paste(nm[2], ":", nm[3],
                     "  df: ", a[1,1], "  df resid: ", a.tbl[nrow(a.tbl),1],
                     "  SS: ", a[1,2], "  F: ",  a[1,4], "  p-value: ", a[1,5],
                     sep="") 

      tx[length(tx)+1] <- a.int 
      tx[length(tx)+1] <- " "
      tx[length(tx)+1] <- paste(
          "-- Assume parallel lines, no interaction of", ancovaOut$cat, "with",
          ancovaOut$cont, "\n")
      n.levels <- length(unique(na.omit(data[,ancovaOut$cat])))
      for (i.level in 1:n.levels)
        tx[length(tx)+1] <- ancovaOut$txeqs[i.level]

      tx[length(tx)+1] <- paste("\n", 
        "-- Visualize Separately Computed Regression Lines")
      tx[length(tx)+1] <- paste("\n", "Plot(", ancovaOut$cont, ", ", nm[1],
           ", by=", ancovaOut$cat, ", fit=\"lm\")", sep="")   
    }  # end ancova 
    txeqs <- tx

    if (ancovaOut$i > 0) {
      for (i in (plot.i+1):(plot.i+ancovaOut$i))
        plot.title[i] <- ancovaOut$ttl[i-plot.i]
      plot.i <- plot.i + ancovaOut$i
    } 
  }  # end graphics



  # ----------
  # References
  # ----------
  txref <- ""
  tx <- character(length = 0)
  if (refs) {
    tx[length(tx)+1] <- "\n  REFERENCES"

    tx[length(tx)+1] <- paste("\n",
        "Function Regression() is from David Gerbing's lessR package.\n",
        "  To obtain the reference: Enter citation(\"lessR\")")
    tx[length(tx)+1] <- paste("\n",
        "Best model subset analysis is from Thomas Lumley's leaps function\n",
        "in his package leaps.\n",
        "  To obtain the reference: Enter citation(\"leaps\")")
    tx[length(tx)+1] <- paste("\n",
        "All analyses based on R.\n",
        "  To obtain the reference: Enter citation()")
    txref <- tx
  }


  # R Markdown
  txRmd <- ""
  txWeb <- ""
  txWrd <- ""
  txpdf <- ""
  txodt <- ""
  txrtf <- ""
  if (!is.null(Rmd)) {

    # get some (generally) unique values for each pred to demo X1_new ...
    new.val <- matrix(nrow=n.pred, ncol=2, byrow=TRUE)
    if (n.pred <= max_new  &&  numeric.all  &&  is.null(X1_new)) {
      for (i in 1:n.pred) {
        v <- sort(data[,nm[i+1]])

        # get new lower value
        min_v <- min(v, na.rm=TRUE)
        test.v <- round(quantile(v, prob=.25)[1])
        while(test.v %in% v)
          if (test.v > min_v) test.v <- test.v - 1 else break 
        new.val[i,1] <- ifelse (test.v == min_v, round(test.v-1), round(test.v))
        if (min_v==0  &&  test.v==0) new.val[i,1] <- 0  # don't go neg here
      
        # get new upper value
        max.v <- max(v, na.rm=TRUE)
        test.v <- round(quantile(v, prob=.75)[1])
        while(test.v %in% v) 
          if (test.v < max.v) test.v <- test.v + 1 else break 
        new.val[i,2] <- ifelse (test.v == max.v, round(test.v+1), round(test.v))
      }
    }  # new.val
    
    # generate and write Rmd file
# in case want to move an r output structure to .reg.Rmd
#   r <- c(bck, anv, est, fit, rel, res, prd)
#   vars <- all.vars(my_formula)
#   r$vars <- vars

    txknt <- .reg.Rmd(nm, d.name, fun_call, n_res_rows, n_pred_rows,
        res_sort, ancova, digits_d, results, explain, interpret, code,
        est$pvalues, tol, resid.max, numeric.all, X1_new, new.val,
        Rmd_data, Rmd_custom, Rmd_dir, Rmd_labels)
    if (!grepl(".Rmd", Rmd)) Rmd <- paste(Rmd, ".Rmd", sep="")
    cat(txknt, file=Rmd, sep="\n")  # Rmd is now file of markdown instructions
    txRmd <- .showfile2(Rmd, "R Markdown file")

    if (!requireNamespace("rmarkdown", quietly=TRUE)) {
      stop("Package \"rmarkdown\" needed for this regression output\n",
           "Please install it:  install.packages(\"rmarkdown\")\n\n",
           call. = FALSE)
    }
    if (rmarkdown::pandoc_available()) {
      pandocYN <- TRUE
      Rmd_format <- tolower(Rmd_format)

      # render Rmd R markdown to current working dir for each specified doc type
      for (i in 1:length(Rmd_format)) {
        if (Rmd_format[i] != "none") {
          Rmd_format[i] <- paste(Rmd_format[i], "_document", sep="")
          rmarkdown::render(Rmd, quiet=TRUE, output_format=Rmd_format[i])
        }
        do_Web <- ifelse (grepl("html", Rmd_format[i]), TRUE, FALSE)
        do_Wrd <- ifelse (grepl("word", Rmd_format[i]), TRUE, FALSE)
        do_pdf <- ifelse (grepl("pdf", Rmd_format[i]), TRUE, FALSE)
        do_odt <- ifelse (grepl("odt", Rmd_format[i]), TRUE, FALSE)
        do_rtf <- ifelse (grepl("rtf", Rmd_format[i]), TRUE, FALSE)
        if (do_Web) {
          fname <- sub("Rmd", "html", Rmd, fixed=TRUE)
          txWeb <- .showfile2(fname, "rendered HTML file for a web browser")
          if (Rmd_browser) browseURL(fname)
        }
        if (do_Wrd) {
          fname <- sub("Rmd", "docx", Rmd, fixed=TRUE)
          txWrd <- .showfile2(fname, "rendered MS Word file")
        }
        if (do_pdf) {
          fname <- sub("Rmd", "pdf", Rmd, fixed=TRUE)
          txpdf <- .showfile2(fname, "rendered pdf file")
        }
        if (do_odt) {
          fname <- sub("Rmd", "odt", Rmd, fixed=TRUE)
          txodt <- .showfile2(fname, "rendered odt file")
        }
        if (do_rtf) {
          fname <- sub("Rmd", "rtf", Rmd, fixed=TRUE)
          txrtf <- .showfile2(fname, "rendered rtf file")
        }
      }
    }  # is pandoc
    else {
      message("\n",
      "R Markdown (Rmd) file created\n",
      "However, need  pandoc  installed to render web, Word, or pdf output\n",
      "<< Re-run analysis in RStudio, which makes the process automatic >>\n",
      "Otherwise can download from pandoc.org and run manually\n\n")
    }
  }  # end Rmd not null

  # display list of plots if more than 1
  txplt <- ""
  if (graphics) {
    if (plot.i > 1) txplt <- .plotList2(plot.i, plot.title)
    if (n.pred > 0) dev.set(which=2)  # reset graphics for standard R functions
  }
  
  # suggestion for Rmd option
  txsug <- ""
  if (getOption("suggest")) {
    # function call for suggestions
    fncl <- .fun_call.deparse(fun_call) 
    fncl <- gsub(")$", "", fncl)  # get function call less closing ) 
    fncl <- gsub(" = ", "=", fncl)
    fncl <- gsub("reg.brief", "reg", fncl)
    
    fc <- ""
    if (!grepl("Rmd", fncl)) {
      txsug <- ">>> Suggestion\n"
      fc <- paste(fc, ", Rmd=\"eg\"", sep="")
      if (nzchar(fc)) {
        fc <- paste(fncl, fc, ") ", sep="")
        txsug <- paste(txsug, 
           "# Create an R markdown file for interpretative output ",
           "with  Rmd = \"file_name\"\n", sep="")
        txsug <- paste(txsug, fc, sep="")
      }
    }
  }
  
  if (kfold == 0) { 
    class(txsug) <- "out"
    class(title_bck) <- "out"
    class(bck$out_background) <- "out"
    class(title_kfold) <- "out"
    class(txkfl) <- "out"
    class(title_basic) <- "out"
    class(est$out_estimates) <- "out"
    class(fit$out_fit) <- "out"
    class(anv$out_anova) <- "out"
    class(title_rel) <- "out"
    class(rel$out_cor) <- "out"
    class(rel$out_collinear) <- "out"
    class(rel$out_subsets) <- "out"
    class(title_res) <- "out"
    class(res$out_residuals) <- "out"
    class(title_pred) <- "out"
    class(prd$out_predict) <- "out"
    class(txplt) <- "out"
    class(title_eqs) <- "out"
    class(txeqs) <- "out"
    class(txref) <- "out"
    class(txRmd) <- "out"
    class(txWrd) <- "out"
    class(txpdf) <- "out"
    class(txodt) <- "out"
    class(txrtf) <- "out"

    output <- list(
      out_suggest=txsug,
      
      call=fun_call, formula=my_formula, vars=all.vars(my_formula),

      out_title_bck=title_bck, out_background=bck$out_background,

      out_title_basic=title_basic, out_estimates=est$out_estimates,
      out_fit=fit$out_fit, out_anova=anv$out_anova,

      out_title_eqs=title_eqs, out_eqs=txeqs,

      out_title_kfold=title_kfold, out_kfold=txkfl,

      out_title_rel=title_rel, out_cor=rel$out_cor,
      out_collinear=rel$out_collinear, out_subsets=rel$out_subsets,

      out_title_res=title_res, out_residuals=res$out_residuals,
      out_title_pred=title_pred, out_predict=prd$out_predict,

      out_ref=txref, out_Rmd=txRmd, out_Word=txWrd, out_pdf=txpdf,
                     out_odt=txodt, out_rtf=txrtf, out_plots=txplt,
      n.vars=bck$n.vars, n.obs=bck$n.obs, n.keep=n.keep, 
      coefficients=est$estimates, sterrs=est$sterrs, tvalues=est$tvalues,
      pvalues=est$pvalues, cilb=est$cilb, ciub=est$ciub,
      anova_model=anv$mdl, anova_residual=anv$rsd, anova_total=anv$tot, 
      se=fit$se, resid_range=fit$range,
      Rsq=fit$Rsq, Rsqadj=fit$Rsqadj, PRESS=fit$PRESS, RsqPRESS=fit$RsqPRESS,
      m_se=m_se, m_MSE=m_MSE, m_Rsq=m_Rsq,
      cor=rel$crs, tolerances=rel$tol, vif=rel$vif,
      resid.max=resid.max, pred_min_max=prd$predmm, 
      residuals=lmo$residuals, fitted=lmo$fitted, 
      cooks.distance=cook, model=lmo$model, terms=lmo$terms
    )
  }

  # kfold > 0
  else {
    class(txkfl) <- "out"
    output <- list(out_title_kfold=title_kfold, out_kfold=txkfl)
  }


  # -----------------------------
  class(output) <- "out_all"

  in.knitr <- ifelse (is.null(options()$knitr.in.progress), FALSE, TRUE)
  if (in.knitr)
    return(output)    
  else
    if (!quiet) return(output)

}
