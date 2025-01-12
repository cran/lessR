Regression <-
function(my_formula, data=d, filter=NULL,
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

         scatter_coef=TRUE, mod=NULL, mod_transf=c("center", "z", "none"),

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
  mod_transf <- match.arg(mod_transf)

  mod.mis <- ifelse (missing(mod), TRUE, FALSE)

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
      df.name <- "mydata"
      mydata.ok <- TRUE
      options(dname = df.name)
    }
  }

  if (!mydata.ok) {
    df.name <- deparse(substitute(data))  # get name of data table
    options(dname = df.name)
  }

  # if a tibble, convert to data frame
  if (any(grepl("tbl", class(data), fixed=TRUE)))
    data <- data.frame(data)

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

  if (!mydata.ok) .nodf(df.name)  # does data frame exist?

  nm <- all.vars(my_formula)  # names of vars in the model
  n.vars <- length(nm)
  n.pred <- n.vars - 1L  # n.pred==0 means null model, y ~ 1

  if (!missing(filter)) {  # subset rows
    r <- eval(substitute(filter), envir=data, enclos=parent.frame())
    if (!any(r)) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "No rows of data with the specified value of\n",
        "filter = ", deparse(substitute(filter)), "\n\n")
    }
    r <- r & !is.na(r)  # set missing for a row to FALSE
    if (any(r))
      data <- data[r,,drop=FALSE]
  }
  n.obs <- nrow(data)

  predictors <- character(length=n.pred)
  for (i in 2:n.vars) predictors[i-1] <- nm[i]

  # do variables nm exist in df.name?
  .xcheck(nm, df.name, names(data))

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

  # set digits_d
  if (is.null(digits_d)) digits_d <- .getdigits(data[,nm[1]], 3)
  options(digits_d=digits_d)

  in.data.frame <- TRUE
  for (i in 1:n.vars) {
    if (!(nm[i] %in% names(data))) {
      cat("\n\n\n>>> Note: ", nm[i], "is not in the data frame.\n")
      in.data.frame <- FALSE
    }
  }

  # sort values of the one predictor variable for scatterplot
  #   so that the prediction/confidence intervals can be drawn
  if (n.pred == 1)
    data <- data[order(data[,nm[2]], decreasing=FALSE), ]

  # reduce data to just model variables with no missing data
  data <- data[, c(nm), drop=FALSE]
  data <- data[complete.cases(data), , drop=FALSE]
  n.keep <- nrow(data)

  # for ancova to work, the variable must be non-numeric or a factor
  ancova <- FALSE  # ancova for one cont and one cat predictors
  if (n.pred == 2) {
  # non-numeric, non-factor concert to factor
    for (i.pred in 2:3) {
      if (!is.numeric(data[,nm[i.pred]])) {
        if (!is.factor(data[,nm[i.pred]]))
          data[,nm[i.pred]] <- as.factor(data[,nm[i.pred]])
      }
    }
    if (is.numeric(data[,nm[2]]) && is.factor(data[,nm[3]]))
      ancova <- TRUE
    if (is.numeric(data[,nm[3]]) && is.factor(data[,nm[2]]))
      ancova <- TRUE
  }
  if (ancova)
    d.ancova <- data  # save original 3 cols of data for ancova
  else
    d.ancova <- NULL

  # check for all numeric vars in data.frame <- TRUE
  numeric.all <- TRUE
  for (i in 1:n.vars) {
    if (in.data.frame && !is.numeric(data[1,which(names(data) == nm[i])])) {
      cat("\n>>> ", nm[i], "is not numeric.",
                 "Converted to indicator variables.\n")
      numeric.all <- FALSE
    }
  }

  # if not all numeric, construct the indicator variables
  if (!numeric.all) {
    md <- data.frame(model.matrix(my_formula, data=data))
    md[,1] <- data[,nm[1]]
    names(md)[1] <- nm[1]
    data <- md
    rm(md)
    nm <- names(data)
    n.vars <- ncol(data)
    n.pred <- n.vars - 1
    my_formula <- DF2formula(data)

    # if true integer, then convert from type double to integer
    rows <- min(50, nrow(data))  # save some time scanning
    fnu.col <- logical(length=ncol(data))
    for (i in 1:ncol(data))
      if (.is.integer(data[1:rows,i]))
        fnu.col[i] <- TRUE
     data[fnu.col] <- lapply(data[fnu.col], as.integer) # move to integer
  }

  if (nrow(data) < n.pred + 2) {
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

  # rescale option (if not K-Fold, do in reg.zKfold.R)
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

  # moderator variable option
  if (!mod.mis) {

    if (n.pred > 2) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Parameter  mod  currently only works with 2 predictor variables.\n\n")
    }

    w.nm <- deparse(substitute(mod))
    w.ind <- which(names(data) == w.nm)

    if (!is.numeric(data[,w.ind])) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Variable for parameter  mod  must be numeric.\n\n")
    }

    data <- na.omit(data)  # lm() default, process data here that lm processes

    # scale the two predictor variables
    if (mod_transf != "none") {
      is.z <- ifelse (mod_transf == "z", TRUE, FALSE)
      for (i in 1:2)
        data[,nm[1+i]] <- scale(data[,nm[1+i]], center=TRUE, scale=is.z)
     }

    # name the new interaction variable
    x.nm <- ifelse (nm[2]==w.nm, nm[3], nm[2])
    xy.nm <- paste(w.nm, ".", x.nm, sep="")
    nm <- c(nm, xy.nm)

    # create new interaction variable
    new.col <- ncol(data) + 1
    data[, new.col] <- data[,nm[2]] * data[,nm[3]]
    names(data)[new.col] <- xy.nm

    # create the new regression model with the added interaction term
    my_formula <- as.formula(paste(nm[1], "~", paste(nm[2:4], collapse="+")))
    n.vars <- length(nm)
    n.pred <- n.vars - 1L

  }  # end mod


  # keep track of generated graphic, see if manage graphics
  if (graphics) {
    plot.i <- 0L
    plot.title  <- character(length=0)
    manage.gr <- .graphman()
  }



  # --------------------------------------------------------
  # OLS regression analysis
  # --------------------------------------------------------
  #   lmo: lm out
  #   all analysis done on data in model construct lmo$model
  lmo <- lm(my_formula, data=data, ...)
  # --------------------------------------------------------

  if (lmo$rank < n.vars) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
       "The attempted solution is singular. Too much linear dependence.\n\n")
  }

  # singularity check
  coef <- lmo$coefficients
  if (anyNA(coef)) {
    bad <- names(coef)[which(is.na(coef))]
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Variable redundant with a prior predictor in the model: ", bad, "\n\n",
      "Delete that variable, or corresponding prior redundant variables,\n",
      "  from the model.\n\n")
  }


  # -----------------------------------
  if (kfold == 0) {
    i.which <- 3

    title_bck <- "\n  BACKGROUND"
    bck <- .reg1bckBasic(lmo, df.name, digits_d, show_R, n.obs, n.keep,
                         transf)

    title_basic <- "\n  BASIC ANALYSIS"
    est <- .reg1modelBasic(lmo, digits_d, show_R)

    if (!ancova)
      anv <- .reg1anvBasic(lmo, digits_d, show_R)
    else
      anv <- .reg1ancova(lmo, d.ancova, digits_d, show_R)

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
      rel <- .reg2Relations(lmo, df.name, n.keep, show_R,
           cor, collinear, subsets, best_sub, max.sublns,
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
        if (!pdf && manage.gr) {  # set up graphics system outside of RStudio
          if (n.pred==1)
            .graphwin(3, width, height)
          else
            .graphwin(2, width, height)  # no sp matrix if not all numeric
        }

        if (manage.gr && !pdf) dev.set(which=i.which)
        plt <- .reg3dnResidual(lmo, pdf, width, height, manage.gr, ...)
        for (i in (plot.i+1):(plot.i+plt$i)) plot.title[i] <- plt$ttl[i-plot.i]
        plot.i <- plot.i + plt$i


        i.which <- i.which + 1
        if (manage.gr && !pdf) dev.set(which=i.which)
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
    if (n_pred_rows == "all") n_pred_rows <- n.keep  # no preds if n_pred_rows=0

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

    if (n_pred_rows>0 || !is.null(X1_new)) {  # if requested, do X1_new, etc.
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
  # -----------------------------------

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

  # Plot scatterplot, maybe ANCOVA info
  txmdl <- ""
  if (graphics) {
    if (manage.gr && !pdf) {
      if (n_res_rows > 0  &&  n.pred > 0)
        i.wh <- 5  # already did two plots
      else {
        .graphwin(1, width, height)  #  only plot is a scatterplot
        i.wh <- 3
      }
      dev.set(which=i.wh)
    }

    if (!ancova) {
      ancovaOut <- .reg5Plot(lmo, n_res_rows, n_pred_rows, scatter_coef,
         X1_new, in.data.frame, prd$cint, prd$pint,
         plot_errors, digits_d, n_cat, pdf, width, height, manage.gr,
         quiet, ...)
    }
    else {
      ancovaOut <- .reg5ancova(lmo, d.ancova, digits_d,
                               pdf, width, height, manage.gr, quiet, ...)
      txmdl <- ancovaOut$txmdl
      for (i in (plot.i+1):(plot.i+ancovaOut$i))
        plot.title[i] <- ancovaOut$ttl[i-plot.i]
      plot.i <- plot.i + ancovaOut$i
    }
  }  # end graphics

  title_mod <- ""
  out_mod <- ""
  if (!mod.mis && graphics) {

    if (graphics) {
      if (manage.gr && !pdf)  # set up graphics system
          .graphwin(4, width, height)

      title_mod <- "\n  MODERATION ANALYSIS"

      mo <- .reg6mod(lmo, w.nm, x.nm, digits_d, pdf, width, height, manage.gr)

      if (manage.gr && !pdf) {
        i.which <- i.which + 1
      }

      for (i in (plot.i+1):(plot.i+mo$i)) plot.title[i] <- mo$ttl[i-plot.i]
      plot.i <- plot.i + mo$i

    }  # end graphics
  }  # end mod
  else {
    mo <- list()
    mo$out_mod <- ""
  }


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
    if (n.pred <= max_new  &&  is.null(X1_new)) {
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

    txknt <- .reg.Rmd(nm, df.name, fun_call, n_res_rows, n_pred_rows,
        res_sort, d.ancova, digits_d, results, explain, interpret, code,
        est$pvalues, tol, resid.max, X1_new, new.val,
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
    class(title_mod) <- "out"
    class(mo$out_mod) <- "out"
    class(title_rel) <- "out"
    class(rel$out_cor) <- "out"
    class(rel$out_collinear) <- "out"
    class(rel$out_subsets) <- "out"
    class(title_res) <- "out"
    class(res$out_residuals) <- "out"
    class(title_pred) <- "out"
    class(prd$out_predict) <- "out"
    class(txplt) <- "out"
    class(txmdl) <- "out"
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

      out_title_mod=title_mod, out_mod=mo$out_mod,

      out_mdls=txmdl,

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
