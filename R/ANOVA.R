ANOVA <-
function(my_formula, data=d, rows=NULL,
         brief=getOption("brief"), digits_d=NULL, 
         Rmd=NULL, jitter_x=0.4,
         res_rows=NULL, res_sort=c("zresid", "fitted", "off"),
         graphics=TRUE, pdf=FALSE, width=5, height=5,
         fun_call=NULL, ...) {  


  # a dot in a parameter name to an underscore
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (grepl(".", names(dots)[i], fixed=TRUE)) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }

  if (is.null(fun_call)) fun_call <- match.call()

  res_sort <- match.arg(res_sort)

  if (missing(my_formula)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Specify a model by listing it first or set according to:  my_formula\n\n")
  }

  dots <- list(...)  # check for deprecated parameters
  if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (names(dots)[i] == "knitr.file") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "knitr.file  no longer used\n",
          "Instead use  Rmd  for R Markdown file\n\n")
      }
    }
  }


  # let deprecated mydata work as default
  dfs <- .getdfs() 
  mydata.ok <- FALSE
  if ("mydata" %in% dfs  &&  !("d" %in% dfs)) {
    d <- mydata
    df.name <- "mydata"
    mydata.ok <- TRUE
    options(dname = df.name)
  }

  if (!mydata.ok) {
    df.name <- deparse(substitute(data))  # get name of data table
    options(dname = df.name)
  }
 
  # if a tibble convert to data frame
  if (!is.null(dfs)) {
    if (df.name %in% dfs) {  # tibble to df
      if (any(grepl("tbl", class(data), fixed=TRUE))) {
        data <- data.frame(data)
      }
    }
  }

 
  op <- options()  # save current options to reset at end

  if (!exists(df.name)) {
    txtC <- "Function ANOVA requires the data exist in a data frame\n"
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
  
  if (!missing(rows)) {  # subset rows
    r <- eval(substitute(rows), envir=data, enclos=parent.frame())
    r <- r & !is.na(r)  # set missing for a row to FALSE
    data <- data[r,,drop=FALSE]
  }
  n.obs <- nrow(data)

  in.data.frame <- TRUE
  for (i in 1:n.vars) {
    if (!(nm[i] %in% names(data))) {
      cat("\n\n\n>>> Note: ", nm[i], "is not in the data frame.\n")
      in.data.frame <- FALSE
    }
  }

  for (i in 2:n.vars) {  # all IVs must be factors
      nms <- which(names(data) == nm[i])
      if (in.data.frame && !is.factor(data[ , nms])) {
        data[ ,nms] <- as.factor(data[ ,nms])
      }
    }  

  # ANOVA
  #   all analysis done on data in model construct av.out$model
  #   this model construct contains only model vars, with Y listed first
  #assign("av.out", aov(my_formula, data=data), pos=.GlobalEnv)
  if (!mydata.ok) .nodf(df.name)
  av.out <- aov(my_formula, data=data)

  n.keep <- nrow(av.out$model)

  if (is.null(digits_d)) digits_d <- .getdigits(data[,nm[1]], 2)
    

# ----------
# Background
# ----------

  title_bck <- "  BACKGROUND"

  tx <- character(length=0)

  if (sys.nframe() == 1) {  # only accurate if not called from model
    tx[length(tx)+1] <- paste("Data Frame: ", df.name)
    tx[length(tx)+1] <- ""
  }
  
  for (i in 1:n.vars) {
    ind <- i
    tx2 <- .varlist2(n.pred, ind, nm[i], "Factor", n.obs,
                     n.keep, levels(data[,nm[i]]))
    for (j in 1:length(tx2)) tx[length(tx)+1] <- tx2[j]
  }

  if (n.pred == 2) {
    if (!is.list(replications(my_formula, data=data))) {
      tx[length(tx)+1] <- ""
      tx[length(tx)+1] <- "The design is balanced"
    }
    else {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "The design is not balanced. The results would be invalid.\n",
        "Consider function  lmer  in the  lme4  package.\n\n")
    }
  }

  txbck <- tx


# --------
# Analysis
# --------

  # keep track of generated graphics
  if (graphics) {
    plot.i <- 0L
    plot.title  <- character(length=0)
    manage.gr <- .graphman()
  }

  if (n.pred == 1)  {

    plt1 <- .ANOVAz1(av.out, av.out$model[,nm[1]], av.out$model[,nm[2]],
        nm, n.obs, jitter_x, digits_d, brief, graphics, pdf, width, height)
    title_des <- plt1$title_des
    txdes <- plt1$txdes
    title_basic <- plt1$title_basic
    txanv <- plt1$txanv
    txeft <- plt1$txeft
    title_tukey <- plt1$title_tukey
    txhsd <- plt1$txhsd
    if (graphics) {
      for (i in (plot.i+1):(plot.i+plt1$i)) plot.title[i] <- plt1$ttl[i-plot.i]
      plot.i <- plot.i + plt1$i
    }
  }

  if (n.pred == 2) {
    plt2 <- .ANOVAz2(av.out, av.out$model[,nm[1]], av.out$model[,nm[2]],
        av.out$model[,nm[3]], nm, digits_d, brief, as.character(my_formula)[3],
        graphics, pdf, width, height)
    txbck2 <- plt2$txbck2
    for (i in 1:length(txbck2)) tx[length(txbck)+1] <- txbck2[i]
    title_des <- plt2$title_des
    txb2 <- plt2$txbck2
    for (i in 1:length(txb2)) txbck[length(txbck)+1] <- txb2[i]
    txcn <- plt2$txcn
    txcm <- plt2$txcm
    txmm <- plt2$txmm
    txgm <- plt2$txgm
    txcs <- plt2$txcs
    title_basic <- plt2$title_basic
    txanv <- plt2$txanv
    txeft <- plt2$txeft
    title_tukey <- plt2$title_tukey
    txhsd <- plt2$txhsd
    if (graphics) {
      for (i in (plot.i+1):(plot.i+plt2$i)) plot.title[i] <- plt2$ttl[i-plot.i]
      plot.i <- plot.i + plt2$i
    }
  }

  # residuals
  txres <- ""
  title_res <- "  RESIDUALS" 
  res <- ""
  fit <- ""
  if (!brief) {
    tx <- character(length=0)

    n.keep <- nrow(av.out$model)
    if (is.null(res_rows)) if (n.keep < 20) res_rows <- n.keep else res_rows <- 20 
    if (res_rows == "all") res_rows <- n.keep  # turn off resids with res_rows=0

    tx[length(tx)+1] <- "Fitted Values, Residuals, Standardized Residuals"
    if (res_sort == "zresid")
      tx[length(tx)+1] <- "   [sorted by Standardized Residuals, ignoring + or - sign]"
    if (res_sort == "fitted")  
      tx[length(tx)+1] <- "   [sorted by Fitted Value, ignoring + or - sign]"
    if (res_rows < n.keep)
      txt <- "cases (rows) of data, or res_rows=\"all\"]"
    else
      txt="]"
    tx[length(tx)+1] <- paste("   [res_rows = ", res_rows, ", out of ", n.keep, " ", txt, sep="")

    fit <- fitted(av.out)
    res <- residuals(av.out)
    sres <- rstandard(av.out)
    out <- cbind(av.out$model[c(nm[seq(2,n.vars)],nm[1])], fit, res, sres)
    out <- data.frame(out, stringsAsFactors=TRUE)
    names(out)[n.vars+1] <- "fitted"
    names(out)[n.vars+2] <- "residual"
    names(out)[n.vars+3] <- "zresid"

    if (res_sort != "off") {
      if (res_sort == "zresid") o <- order(abs(out$zresid), decreasing=TRUE)
      if (res_sort == "fitted") o <- order(abs(out$fitted), decreasing=TRUE)
      out <- out[o,]
    }
    names(out)[n.vars+3] <- "z-resid"
    for (i in 1:(n.vars+3))
      if (is.numeric(out[,i])) if (!is.integer(out[,i])) {
        if (digits_d > 2) dec.digits <- digits_d-1 else dec.digits <- digits_d
        out[,i] <- .fmt(out[,i],dec.digits)
      }
    tx2 <- .prntbl(out[1:res_rows,], digits_d)
    for (i in 1:length(tx2)) tx[length(tx)+1] <- tx2[i]

    txres <- tx
  }  # end !brief

# pairwise.t.test(d$Steady, d$TrtA)
# power.anova.test(groups=4, n=8, between.var=16.33, within.var=2.179)

  options(op)  # restore options going into reg

  # display list of plots if more than 1
  txplt <- ""
  if (graphics) {
    if (plot.i > 1) txplt <- .plotList2(plot.i, plot.title)
    dev.set(which=2)  # reset graphics window for standard R functions
  }

  # R Markdown
  txkfl <- ""
  if (!is.null(Rmd)) {
    txt <- ifelse (grepl(".Rmd", Rmd), "", ".Rmd")
    Rmd <- paste(Rmd, txt, sep="") 
    txknt <- .av.Rmd(nm, df.name, fun_call, digits_d)
    cat(txknt, file=Rmd, sep="\n")
    txkfl <- .showfile2(Rmd, "R Markdown instructions")
  }

  class(title_bck) <- "out"
  class(txbck) <- "out"
  class(title_des) <- "out"
  if (n.pred == 1)
    class(txdes) <- "out"
  if (n.pred == 2) {
    class(txcn) <- "out"
    class(txcm) <- "out"
    class(txmm) <- "out"
    class(txcs) <- "out"
  }
  class(title_basic) <- "out"
  class(txanv) <- "out"
  class(txeft) <- "out"
  class(title_tukey) <- "out"
  class(txhsd) <- "out"
  class(title_res) <- "out"
  class(txres) <- "out"
  class(txplt) <- "out"
  

  if (n.pred == 1)  {
    output <- list(
      call=fun_call, formula=my_formula,

      out_title_bck=title_bck, out_background=txbck,

      out_title_des=title_des,
      out_descriptive=txdes,

      out_title_basic=title_basic,
      out_anova=txanv, out_effects=txeft,

      out_title_tukey=title_tukey,
      out_hsd=txhsd, 

      out_title_res=title_res, out_res=txres,

      out_plots=txplt,

      n.vars=n.vars, n.obs=n.obs, n.keep=n.keep,
      residuals=res, fitted=fit
    )
  }

  if (n.pred == 2)  {
    output <- list(
      call=fun_call, formula=my_formula,

      out_title_bck=title_bck, out_background=txbck,

      out_title_des=title_des,
      out_cell.n=txcn, out_cell.means=txcm, out_marginals=txmm,
      out_gm=txgm, out_cell.sd=txcs,

      out_title_basic=title_basic,
      out_anova=txanv, out_effects=txeft,

      out_title_tukey=title_tukey,
      out_hsd=txhsd, 

      out_title_res=title_res, out_res=txres,

      out_plots=txplt,

      n.vars=n.vars, n.obs=n.obs, n.keep=n.keep,
      residuals=res, fitted=fit
    )
  }

  class(output) <- "out_all"

  return(output)

}
