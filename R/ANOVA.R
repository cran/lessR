ANOVA <-
function(my.formula, data=mydata, brief=getOption("brief"), digits.d=NULL, 
         rb.points=TRUE, res.rows=NULL, res.sort=c("zresid", "fitted", "off"),
         pdf=FALSE, pdf.width=5, pdf.height=5, ...) {  

  res.sort <- match.arg(res.sort)

  dname <- deparse(substitute(data))
  options(dname = dname)
 
  op <- options()  # save current options to reset at end
  options(show.signif.stars=FALSE, scipen=30)

  if (!exists(dname)) {
    txtC <- "Function ANOVA requires the data exist in a data frame\n"
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

  in.data.frame <- TRUE
  for (i in 1:n.vars) {
    if (!(nm[i] %in% names(data))) {
      cat("\n\n\n>>> Note: ", nm[i], "is not in the data frame.\n")
      in.data.frame <- FALSE
    }
  }

  for (i in 2:n.vars) {
      nms <-  which(names(data) == nm[i])
      if (in.data.frame && !is.factor(data[ , nms])) {
        cat("\n>>> Note: Converting", nm[i], "to a factor for this analysis only.\n")
        data[ ,nms] <- as.factor(data[ ,nms])
      }
    }  

  # ANOVA
  #   all analysis done on data in model construct av.out$model
  #   this model construct contains only model vars, with Y listed first
  #assign("av.out", aov(my.formula, data=data), pos=.GlobalEnv)
  av.out <- aov(my.formula, data=data)

  n.keep <- nrow(av.out$model)

  if (is.null(digits.d)) digits.d <- .getdigits(data[,nm[1]], 2)
    

# ----------
# Background
# ----------

  cat( "\n\n", "Background", "\n", sep="")
  .dash(10)

  cat("\n")
  if (sys.nframe() == 1) {  # only accurate if not called from model
    cat("Data Frame: ", dname, "\n\n")
  }
  
  for (i in 1:n.vars) {
    ind <- i
    .varlist(n.pred, ind, nm[i], "Factor", n.obs, n.keep, levels(data[,nm[i]]))
  }


# --------
# Analysis
# --------

  if (n.pred == 1) 
    .ANOVAz1(av.out, av.out$model[,nm[1]], av.out$model[,nm[2]],
        nm, n.obs, digits.d, brief, pdf, pdf.width, pdf.height)

  if (n.pred == 2) {
    if (!is.list(replications(my.formula, data=data)))
      cat("\nThe design is balanced\n")
    else {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "The design is not balanced. The results would be invalid.\n",
        "Consider function  lmer  in the  lme4  package.\n\n")
    }
    .ANOVAz2(av.out, av.out$model[,nm[1]], av.out$model[,nm[2]],
        av.out$model[,nm[3]], nm, digits.d, brief, as.character(my.formula)[3],
        rb.points, pdf, pdf.width, pdf.height)
  }


  # residuals
  if (!brief) {
    n.keep <- nrow(av.out$model)
    if (is.null(res.rows)) if (n.keep < 20) res.rows <- n.keep else res.rows <- 20 
    if (res.rows == "all") res.rows <- n.keep  # turn off resids with res.rows=0

    cat("\n\n\n")
    cat("Fitted Values, Residuals, Standardized Residuals\n")
    if (res.sort == "zresid")
      cat("   [sorted by Standardized Residuals, ignoring + or - sign]\n")
    if (res.sort == "fitted")  
      cat("   [sorted by Fitted Value, ignoring + or - sign]\n")
    if (res.rows < n.keep)
      txt <- "cases (rows) of data, or res.rows=\"all\"]"
    else
      txt="]"
    cat("   [res.rows = ", res.rows, " out of ", n.keep, " ", txt, sep="", "\n")
    .dash(68)
    fit <- fitted(av.out)
    res <- residuals(av.out)
    sres <- rstandard(av.out)
    out <- cbind(av.out$model[c(nm[seq(2,n.vars)],nm[1])], fit, res, sres)
    out <- data.frame(out)
    names(out)[n.vars+1] <- "fitted"
    names(out)[n.vars+2] <- "residual"
    names(out)[n.vars+3] <- "zresid"

    if (res.sort != "off") {
      if (res.sort == "zresid") o <- order(abs(out$zresid), decreasing=TRUE)
      if (res.sort == "fitted") o <- order(abs(out$fitted), decreasing=TRUE)
      out <- out[o,]
    }
    names(out)[n.vars+3] <- "z-resid"
    for (i in 1:(n.vars+3))
      if (is.numeric(out[,i])) if (!is.integer(out[,i])) {
        if (digits.d > 2) dec.digits <- digits.d-1 else dec.digits <- digits.d
        out[,i] <- .fmt(out[,i],dec.digits)
      }
    print(out[1:res.rows,])
    .dash(68)
    rm(out)
  }

# pairwise.t.test(mydata$Steady, mydata$TrtA)
# power.anova.test(groups=4, n=8, between.var=16.33, within.var=2.179)

  options(op)  # restore options going into reg

  cat("\n")


  invisible(av.out)

}
