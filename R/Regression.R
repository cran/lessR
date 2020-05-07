Regression <-
function(my_formula, data=d, rows=NULL, kfold=0,
         digits_d=NULL, standardize=FALSE,

         Rmd=NULL, Rmd_browser=TRUE, 
         Rmd_format=c("html", "word", "pdf", "odt", "none"),
         Rmd_data=NULL,
         results=getOption("results"), explain=getOption("explain"),
         interpret=getOption("interpret"), document=getOption("document"), 
         code=getOption("code"), 

         text_width=120, brief=getOption("brief"), show_R=FALSE,

         res_rows=NULL, res_sort=c("cooks","rstudent","dffits","off"), 
         pred_rows=NULL, pred_sort=c("predint", "off"),
         subsets=NULL, cooks_cut=1, 

         scatter_coef=TRUE, graphics=TRUE, scatter_3D=FALSE,

         X1_new=NULL, X2_new=NULL, X3_new=NULL, X4_new=NULL, 
         X5_new=NULL, X6_new=NULL,

         quiet=getOption("quiet"),
         pdf=FALSE, width=6.5, height=6.5, refs=FALSE,
         fun_call=NULL, ...) {


  # allow for more than one value, so cannot use match.arg
  # then all 5 get selected, then default to "html"
  if (missing(Rmd_format)  &&  length(Rmd_format) == 5) Rmd_format <- "html"

  # a dot in a parameter name to an underscore
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    change <- c("digits.d", "Rmd.format", "Rmd.browser", "text.width",
                "res.rows", "res.sort", "pred.rows", "pred.sort",
                "cooks.cut", "scatter.coef", "X1.new", "X2.new",
                "X3.new", "X4.new", "X5.new", "X6.new", "fun.call")
    for (i in 1:length(dots)) {
      if (names(dots)[i] %in% change) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
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

  dots <- list(...)  # check for deprecated parameters
  if (length(dots) > 0) {
    for (i in 1:length(dots)) {
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


  # produce actual argument, such as from an abbreviation, flag if not exist
  res_sort <- match.arg(res_sort)
  pred_sort <- match.arg(pred_sort)

  old.opt <- options()
  on.exit(options(old.opt))

  options(width=text_width)

  max_new <- 6

  # output
  cor <- TRUE  # do even if only one pred variable

  if (brief) {
    if (is.null(res_rows)) res_rows <- 0L
    if (is.null(pred_rows)  &&  is.null(X1_new)) pred_rows <- 0L
    relate <- FALSE
  }
  else
    relate <- TRUE

  if (kfold > 0) {
    graphics <- FALSE
    relate <- FALSE 
    res_rows <- 0
    pred_rows <- 0
  }

  if (!mydata.ok) .nodf(df.name)  # does data frame exist?

  nm <- all.vars(my_formula)  # names of vars in the model
  n.vars <- length(nm)
  n.pred <- n.vars - 1L

  if (!missing(rows)) {  # subset rows
    r <- eval(substitute(rows), envir=data, enclos=parent.frame())
    r <- r & !is.na(r)  # set missing for a row to FALSE
    data <- data[r,,drop=FALSE]
  }
  n.obs <- nrow(data)

  predictors <- character(length=n.pred)
  for (i in 2:n.vars) predictors[i-1] <- nm[i]

  # do variables nm exist in df.name?
  # for (i in 1:n.vars) .xcheck(nm[i], df.name, names(data)) 
  .xcheck(nm, df.name, names(data)) 

  # check that variables are not function calls
  v.str <- deparse(attr(terms.formula(my_formula), which="variables"))
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
  
  if ( !is.null(X1_new)  &&  (n.pred) > max_new ) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "No new data for prediction if more than", max_new,
          "predictor variables.\n\n")
  }
  
  if ( !is.null(X1_new) && !numeric.all ) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "All variables must be numeric to use new data for prediction.\n\n")
  }

  # check new.data option for consistency  
  new.data <- FALSE
  if ( n.pred > 0  &&  n.pred <= max_new ) { 
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
  if (n.pred == 1) { 
    o <- order(data[,nm[2]], decreasing=FALSE)
    data <- data[o,]
  }

  if (is.null(digits_d)) digits_d <- .getdigits(data[,nm[1]], 3)
  options(digits_d=digits_d) 


  # standardize option
  if (standardize) {
    stnd.flag <- TRUE
    for (i in 1:n.vars)
      data[,nm[i]] <- round(scale(data[,nm[i]]), digits_d)
  }
  else
    stnd.flag <- FALSE

  # keep track of generated graphic, see if manage graphics
    if (graphics) {
      plot.i <- 0L
      plot.title  <- character(length=0)
      manage.gr <- .graphman()
    }

  # --------------------------------------------------------
  # reg analysis
  #   all analysis done on data in model construct lm.out$model
  #   this model construct contains only model vars, with Y listed first
  #assign("lm.out", lm(my_formula, data=data), pos=.GlobalEnv)
  lm.out <- lm(my_formula, data=data)

  if (lm.out$rank < n.vars) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
       "The attempted solution is singular. Too much linear dependence.\n\n")
  }

  n.keep <- nrow(lm.out$model)  # lm.out$model is the data with deleted
 
  # replace a factor with indicator variables in data frame
  #mm <- model.matrix(my_formula, data=data)
  #mf.out <- data.frame(lm.out$model[,1], mm[,2:ncol(mm)])
  #names(mf.out)[1] <- nm[1]


  if (kfold == 0) {
    title_bck <- "  BACKGROUND"
    bck <- .reg1bckBasic(lm.out, df.name, digits_d, show_R, n.obs, n.keep,
                         stnd.flag)
    tx1bck <- bck$tx


    title_basic <- "  BASIC ANALYSIS"
    est <- .reg1modelBasic(lm.out, digits_d, show_R)
    tx1est <- est$tx
    sterrs <- est$sterrs

    anv <- .reg1anvBasic(lm.out, digits_d, show_R)
    tx1anv <- anv$tx 
    MSW <- anv$MSW

    fit <- .reg1fitBasic(lm.out, anv$tot["ss"], digits_d, show_R)
    tx1fit <- fit$tx
  }


  txkfl <- ""
  title_kfold <- "  K-FOLD CROSS-VALIDATION"
  m_se <- NA;  m_MSE <- NA;  m_Rsq <- NA

  if (kfold > 0) {
    Kfld <- .regKfold(data, my_formula, kfold, nm, predictors, n.vars,
                      n.keep, digits_d, show_R)
    txkfl <- Kfld$tx
    m_se <- Kfld$m_se; m_MSE <- Kfld$m_MSE; m_Rsq <- Kfld$m_Rsq
  }


  title_rel <- "  RELATIONS AMONG THE VARIABLES"
  tx2rel <- ""; tx2cor <- ""; tx2cln <- ""; tx2all <- ""
  if (relate  &&  n.pred > 0) {
    max.sublns <- 50
    if (subsets > 1) {
      max.sublns <- subsets
      subsets <- TRUE
    }
    rel <- .reg2Relations(lm.out, df.name, n.keep, show_R,
         cor, collinear, subsets, max.sublns, numeric.all, in.data.frame,
         sterrs, MSW)
    tx2cor <- rel$txcor
    tx2cln <- rel$txcln
    tx2all <- rel$txall
    if (is.matrix(rel$crs)) crs <- round(rel$crs,3) else crs <- NA
    if (is.vector(rel$tol)) tol <- round(rel$tol,3) else tol <- NA
    if (is.vector(rel$vif)) vif <- round(rel$vif,3) else vif <- NA
  }
  else { # not relate and n.pred > 0
    crs <- NA_real_; tol <- NA; vif <- NA
  }
  

  title_res <- "  RESIDUALS AND INFLUENCE"
  if (is.null(res_rows)) res_rows <- ifelse (n.keep < 20, n.keep, 20) 
  if (res_rows == "all") res_rows <- n.keep  # turn off resids with res_rows=0


  tx3res <- ""
  resid.max <- NA
  cook <- NA
  if (res_rows > 0) {

    cook <- round(cooks.distance(lm.out), 5)
    res <- .reg3txtResidual(lm.out, cook, digits_d, res_sort, res_rows, show_R)
    tx3res <- res$tx
    if (!is.na(res$resid.max[1])) resid.max <- round(res$resid.max,3)

    if (graphics  &&  n.pred > 0) {
      if (!pdf && manage.gr) {  # set up graphics system
        if (numeric.all || n.pred==1)
          .graphwin(3, width, height) 
        else
          .graphwin(2, width, height)  # no sp matrix if not all numeric
      }

      if (manage.gr && !pdf) dev.set(which=3)
      plt <- .reg3dnResidual(lm.out, pdf, width, height, manage.gr, ...)
      for (i in (plot.i+1):(plot.i+plt$i)) plot.title[i] <- plt$ttl[i-plot.i]
      plot.i <- plot.i + plt$i 

      
      if (manage.gr && !pdf) dev.set(which=4)
      fr <- .reg3resfitResidual(lm.out, cook, cooks_cut,
                 pdf, width, height, manage.gr)
      for (i in (plot.i+1):(plot.i+fr$i)) plot.title[i] <- fr$ttl[i-plot.i]
      crfitres <- fr$crfitres
      plot.i <- plot.i + fr$i
    } # graphics

  }  # res_rows > 0

 
  title_pred <- "  FORECASTING ERROR"
  # scatter plot(s)
  if (is.null(pred_rows)) pred_rows <- ifelse (n.keep < 25, n.keep, 10) 
  if (pred_rows == "all") pred_rows <- n.keep  # turn off preds with pred_rows=0

  tx3prd <- ""
  predmm <- NA
  if (pred_rows > 0  ||  !is.null(X1_new)) {  # if requested, do X1_new, etc.
    prd <- .reg4Pred(lm.out,
         n.keep, digits_d, show_R,
         new.data, pred_sort, pred_rows, scatter_coef,
         in.data.frame, X1_new, X2_new, X3_new, X4_new, X5_new, X6_new)
    tx3prd <- prd$tx
    predmm <- prd$predmm
  }

  if (graphics) {
    if (manage.gr && !pdf) {
      if (res_rows > 0  &&  n.pred > 0)  # already did two plots 
        dev.set(which=5) 
      else {
        .graphwin(1, width, height)  #  only plot is a scatterplot
        dev.set(which=3)
      }
    }
 
    if ((numeric.all || n.pred==1) && in.data.frame) {
      splt <- .reg5Plot(lm.out, res_rows, pred_rows, scatter_coef, 
         X1_new, numeric.all, in.data.frame, prd$cint, prd$pint,
         pdf, width, height, manage.gr, scatter_3D, ...)

      for (i in (plot.i+1):(plot.i+splt$i)) plot.title[i] <- splt$ttl[i-plot.i]
      plot.i <- plot.i + splt$i
    } 
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
    txknt <- .reg.Rmd(nm, df.name, fun_call, res_rows, pred_rows,
        res_sort, digits_d, results, explain, interpret, document, code,
        est$pvalues, tol, resid.max, numeric.all, X1_new, new.val, Rmd_data)
    if (!grepl(".Rmd", Rmd)) Rmd <- paste(Rmd, ".Rmd", sep="")
    cat(txknt, file=Rmd, sep="\n") 
    txRmd <- .showfile2(Rmd, "R Markdown file")

    if (!requireNamespace("rmarkdown", quietly=TRUE)) {
      stop("Package \"rmarkdown\" needed for this regression output\n",
           "Please install it:  install.packages(\"rmarkdown\")\n\n",
           call. = FALSE)
    }
    if (rmarkdown::pandoc_available()) {
      pandocYN <- TRUE
      Rmd_format <- tolower(Rmd_format)

      # render R markdown to current working dir for each specified doc type
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
    class(tx1bck) <- "out"
    class(title_kfold) <- "out"
    class(txkfl) <- "out"
    class(title_basic) <- "out"
    class(tx1est) <- "out"
    class(tx1fit) <- "out"
    class(tx1anv) <- "out"
    class(title_rel) <- "out"
    class(tx2cor) <- "out"
    class(tx2cln) <- "out"
    class(tx2all) <- "out"
    class(title_res) <- "out"
    class(tx3res) <- "out"
    class(title_pred) <- "out"
    class(tx3prd) <- "out"
    class(txplt) <- "out"
    class(txref) <- "out"
    class(txRmd) <- "out"
    class(txWrd) <- "out"
    class(txpdf) <- "out"
    class(txodt) <- "out"
    class(txrtf) <- "out"
    
    output <- list(
      out_suggest=txsug,
      
      call=fun_call, formula=my_formula,

      out_title_bck=title_bck, out_background=tx1bck,

      out_title_basic=title_basic, out_estimates=tx1est,
      out_fit=tx1fit, out_anova=tx1anv,

      out_title_kfold=title_kfold, out_kfold=txkfl,

      out_title_rel=title_rel, out_cor=tx2cor, out_collinear=tx2cln,
      out_subsets=tx2all,

      out_title_res=title_res, out_residuals=tx3res,
      out_title_pred=title_pred, out_predict=tx3prd,

      out_ref=txref, out_Rmd=txRmd, out_Word=txWrd, out_pdf=txpdf,
                     out_odt=txodt, out_rtf=txrtf, out_plots=txplt,
      n.vars=bck$n.vars, n.obs=bck$n.obs, n.keep=n.keep, 
      coefficients=est$estimates, sterrs=est$sterrs, tvalues=est$tvalues,
      pvalues=est$pvalues, cilb=est$cilb, ciub=est$ciub,
      anova_model=anv$mdl, anova_residual=anv$rsd, anova_total=anv$tot, 
      se=fit$se, resid_range=fit$range,
      Rsq=fit$Rsq, Rsqadj=fit$Rsqadj, PRESS=fit$PRESS, RsqPRESS=fit$RsqPRESS,
      m_se=m_se, m_MSE=m_MSE, m_Rsq=m_Rsq,
      cor=crs, tolerances=tol, vif=vif,
      resid.max=resid.max, pred_min_max=predmm, 
      residuals=lm.out$residuals, fitted=lm.out$fitted, 
      cooks.distance=cook, model=lm.out$model, terms=lm.out$terms
    )
  }

  # kfold > 0
  else {
    class(txkfl) <- "out"
    output <- list(out_title_kfold=title_kfold, out_kfold=txkfl)
  }

  class(output) <- "out_all"

  in.knitr <- ifelse (is.null(options()$knitr.in.progress), FALSE, TRUE)
  if (in.knitr)
    return(output)    
  else
    if (!quiet) return(output)

  
  cat("\n")
  
}
