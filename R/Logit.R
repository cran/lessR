Logit <-
function(my.formula, data=mydata, digits.d=4, text.width=120, 

         res.rows=NULL, res.sort=c("cooks","rstudent","dffits","off"), 
         pred=TRUE, pred.all=FALSE, pred.sort=TRUE, cooks.cut=1, 

         X1.new=NULL, X2.new=NULL, X3.new=NULL, X4.new=NULL, 
         X5.new=NULL,

         pdf=FALSE, pdf.width=5, pdf.height=5, ...) {
 
  
  dname <- deparse(substitute(data))
  options(dname = dname)
 
  # produce actual argument, such as from an abbreviation, and flag if not exist
  res.sort <- match.arg(res.sort)

  brief <- FALSE
  show.R <- FALSE
  explain <- FALSE

  old.opt <- options()
  on.exit(options(old.opt))

  options(width=text.width)

  # output
  cor <- TRUE

  if (brief) {
    if (is.null(res.rows)) res.rows <- 0
    pred <- FALSE
    relate <- FALSE
    show.R <- FALSE
   }
   else relate <- TRUE
      
  pre <- "> "
  line <- "--------------------------------------------------------------------\n"
  
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
  
  if(n.pred > 1) {
    collinear <- TRUE
  }
  else {
    collinear <- FALSE
  }

  is.bin <- TRUE
  for (i in 1:n.obs) if (data[i,nm[1]]!=0 && data[i,nm[1]]!=1) is.bin <- FALSE
  if (!is.bin) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "The response variable ", nm[1], " can only have values of 0 or 1.\n\n")
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

  in.data.frame <- TRUE
  for (i in 1:n.vars) {
    if (!(nm[i] %in% names(data))) {
      cat("\n\n\n>>> Note: ", nm[i], "is not in the data frame.\n")
      in.data.frame <- FALSE
    }
  }

  # logit analysis
  #   all analysis done on data in model construct lm.out$model
  #   this model construct contains only model vars, with Y listed first
  #assign("lm.out", glm(my.formula, data=data, family="binomial"),
         #pos=.GlobalEnv)
  lm.out <- glm(my.formula, data=data, family="binomial")

  n.keep <- nrow(lm.out$model)
    
  if (is.null(res.rows)) if (n.keep < 20) res.rows <- n.keep else res.rows <- 20 
  if (res.rows == "all") res.rows <- n.keep  # turn off resids with res.rows=0 call

  
  cat("\n")
  if (sys.nframe() == 1) {  # only accurate if not called from model
    cat("Data Frame: ", dname, "\n\n")
  }

  for (i in 1:n.vars) {
    ind <- i
    .varlist(n.pred, ind, nm[i], "Predictor", n.obs, n.keep)
  }
  
  
  cat( "\n\n\n", "  BASIC ANALYSIS", "\n\n")

  sm <- summary(lm.out)
  cat("The Model, Hypothesis Tests\n")
  print(sm)

  # CIs using profiled log-likelihood
  ci <- suppressMessages(confint(lm.out))
  cat("\n\nConfidence intervals from profiled log-likelihoods\n\n")
  print(ci)

  # CIs using standard errors
  ci.se <- confint.default(lm.out)
  cat("\n\nConfidence intervals from standard errors\n\n")
  print(ci.se)

  # ANOVA
  av <- anova(lm.out)
  cat("\n\n")
  print(av)

  # odds ratios and 95% CI
  orci <- exp(cbind(OR = coef(lm.out), ci))
  cat("\n\nOdds ratios and confidence intervals\n\n")
  print(orci)
  
  # check for all numeric vars  in.data.frame <- TRUE
  numeric.all <- TRUE
  for (i in 1:n.vars) {
    if (in.data.frame && !is.numeric(data[1,which(names(data) == nm[i])])) {
      cat("\n\n\n>>> Note: ", nm[i], "is not a numeric variable.\n")
      numeric.all <- FALSE
    }
  }
  

  if (res.rows > 0) {  # two plots here

    if (!pdf) .graphwin(3)  # set up graphics system

    .logit3Residual(lm.out, nm, dname,
         n.vars, n.pred, n.obs, n.keep, digits.d, pre, line,
         res.sort, res.rows, cooks.cut, 
         pdf, pdf.width, pdf.height)
  }
 
  if (pred) {

    if (res.rows == 0) .graphwin(1)

    .logit4Pred(lm.out, nm, dname, my.formula, brief, res.rows,
         n.vars, n.pred, n.obs, n.keep, digits.d, pre, line,
         new.data, pred.sort, pred, pred.all, 
         numeric.all, in.data.frame, X1.new, 
         X2.new, X3.new, X4.new, X5.new,
         pdf, pdf.width, pdf.height)
  }

  if (pdf) dev.off()

}
