Regression <-
function(my.formula, data=mydata, digits.d=NULL, standardize=FALSE,
         text.width=120, brief=FALSE, explain=FALSE, show.R=FALSE,

         res.rows=NULL, res.sort=c("cooks","rstudent","dffits","off"), 
         pred.rows=NULL, pred.sort=c("predint", "off"),
         subsets=TRUE, cooks.cut=1, 

         scatter.coef=FALSE, scatter.3D=FALSE,

         X1.new=NULL, X2.new=NULL, X3.new=NULL, X4.new=NULL, 
         X5.new=NULL,

         pdf=FALSE, pdf.width=5, pdf.height=5, refs=FALSE, ...) {


  dname <- deparse(substitute(data))  # get data frame name for cor before sort
  options(dname = dname)

  # produce actual argument, such as from an abbreviation, and flag if not exist
  res.sort <- match.arg(res.sort)
  pred.sort <- match.arg(pred.sort)

  old.opt <- options()
  on.exit(options(old.opt))

  options(width=text.width)

  # output
  cor <- TRUE

  if (brief) {
    if (is.null(res.rows)) res.rows <- 0
    if (is.null(pred.rows)) pred.rows <- 0
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
    subsets <- TRUE
  }
  else {
    collinear <- FALSE
    subsets <- FALSE
  }

  if ( scatter.3D && (n.pred)!=2 ) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
       "Can have a 3d scatterplot only with exactly two predictor variables.\n\n")
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
  if (standardize)
    for (i in 1:n.vars)
      data[,nm[i]] <- round(scale(data[,nm[i]]), digits.d)

  in.data.frame <- TRUE
  for (i in 1:n.vars) {
    if (!(nm[i] %in% names(data))) {
      cat("\n\n\n>>> Note: ", nm[i], "is not in the data frame.\n")
      in.data.frame <- FALSE
    }
  }

  # reg analysis
  #   all analysis done on data in model construct lm.out$model
  #   this model construct contains only model vars, with Y listed first
  #assign("lm.out", lm(my.formula, data=data), pos=.GlobalEnv)
  lm.out <- lm(my.formula, data=data)

  if (lm.out$rank < n.vars) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
       "The attempted solution is singular. Too much linear dependence.\n\n")
  }
 
  n.keep <- nrow(lm.out$model)

  .reg1Basic(lm.out, nm, dname,
         n.vars, n.pred, n.obs, n.keep, digits.d, explain, show.R, pre, line)
  
  # check for all numeric vars  in.data.frame <- TRUE
  numeric.all <- TRUE
  for (i in 1:n.vars) {
      if (in.data.frame && !is.numeric(data[1,which(names(data) == nm[i])])) {
        cat("\n\n\n>>> Note: ", nm[i], "is not a numeric variable.\n")
        numeric.all <- FALSE
      }
    }

  if (relate)
    .reg2Relations(lm.out, nm, dname,
         n.vars, n.pred, n.obs, n.keep, digits.d, explain, show.R, pre, line,
         cor, collinear, subsets, numeric.all, in.data.frame)
  
    
  if (is.null(res.rows)) if (n.keep < 20) res.rows <- n.keep else res.rows <- 20 
  if (res.rows == "all") res.rows <- n.keep  # turn off resids with res.rows=0

  if (res.rows > 0) {  # two plots here

    if (!pdf) .graphwin(3)  # set up graphics system

    .reg3Residual(lm.out, nm,
         n.vars, n.pred, n.obs, n.keep, digits.d, explain, show.R, pre, line,
         res.sort, res.rows, cooks.cut,
         pdf, pdf.width, pdf.height, ...)
   }

    
  if (is.null(pred.rows)) if (n.keep < 25) pred.rows <- n.keep else pred.rows <- 4 
  if (pred.rows == "all") pred.rows <- n.keep  # turn off preds with pred.rows=0

  if (pred.rows > 0)
    prd <- .reg4Pred(lm.out, nm, my.formula, brief,
         n.vars, n.pred, n.obs, n.keep, digits.d, explain, show.R, pre, line,
         new.data, pred.sort, pred.rows, scatter.3D, scatter.coef,
         numeric.all, in.data.frame, X1.new, 
         X2.new, X3.new, X4.new, X5.new)
   
  .reg5Plot(lm.out, nm, my.formula, brief, res.rows,
         n.vars, n.pred, n.obs, n.keep, digits.d, explain, show.R, pre, line,
         new.data, pred.rows, scatter.3D, scatter.coef,
         numeric.all, in.data.frame, X1.new, 
         X2.new, X3.new, X4.new, X5.new, prd$cint, prd$pint,
         pdf, pdf.width, pdf.height, ...)


# ----------
# References
# ----------

  if (refs) {
    cat( "\n\n", "  REFERENCES", "\n")

    cat("\n",
        "Function Regression is from David Gerbing's lessR package.\n",
        "  To obtain the reference: Enter citation(\"lessR\")\n")
    cat("\n",
        "Collinearity analysis is from the vif function in\n",
        "John Fox's car package.\n",
        "  To obtain the reference: Enter citation(\"car\")\n")
    cat("\n",
        "Best model subset analysis is from Thomas Lumley's leaps function\n",
        "in his package leaps.\n",
        "  To obtain the reference: Enter citation(\"leaps\")\n")
    cat("\n",
        "All analyses based on R.\n",
        "  To obtain the reference: Enter citation()\n")
    cat("\n")
  }
  
}
