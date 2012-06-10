Regression <-
function(my.formula, dframe=mydata,
         digits.d=4, text.width=120, graphics.save=FALSE, 
         brief=FALSE, explain=FALSE, show.R=FALSE,
         colors=c("blue", "gray", "rose", "green", "gold", "red"), 

         res.rows=NULL, res.sort=c("cooks","rstudent","dffits","off"), 
         pred=TRUE, pred.all=FALSE, pred.sort=c("predint", "off"),
         subsets=TRUE, cooks.cut=1, 

         scatter.coef=FALSE, scatter.3d=FALSE,

         X1.new=NULL, X2.new=NULL, X3.new=NULL, X4.new=NULL, 
         X5.new=NULL, ...) {

  if (missing(colors)) col.default <- TRUE else col.default <- FALSE
  colors <- match.arg(colors)
  if (col.default && !is.null(getOption("colors"))) colors <- getOption("colors")
  
  mydframe <- deparse(substitute(dframe))  # get data frame name for cor before sort
 
  # produce actual argument, such as from an abbreviation, and flag if not exist
  res.sort <- match.arg(res.sort)
  pred.sort <- match.arg(pred.sort)

  op <- options()  # save current options to reset at end of reg
  options(show.signif.stars=FALSE, scipen=30, width=text.width)

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
  
  if (!exists(mydframe)) {
    txtC <- "Function reg requires the data exist in a data frame\n"
    if (mydframe == "mydata") 
      txtA <- ", the default data frame name, " else txtA <- " "
    txtB1 <- "Either create the data frame, such as with data.frame function, or\n"
    txtB2 <- "  specify the actual data frame with the parameter: dframe\n"
    txtB <- paste(txtB1, txtB2, sep="")
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        txtC, "Data frame ", mydframe, txtA, "does not exist\n\n", txtB, "\n")
  }

  nm <- all.vars(my.formula)  # names of vars in the model
  n.vars <- length(nm)
  n.pred <- n.vars - 1
  n.obs <- nrow(dframe)
  
  if(n.pred > 1) {
    collinear <- TRUE
    subsets <- TRUE
  }
  else {
    collinear <- FALSE
    subsets <- FALSE
  }

  if ( scatter.3d && (n.pred)!=2 ) {
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
    o <- order(dframe[,nm[2]], decreasing=FALSE)
    dframe <- dframe[o,]
  }

  in.data.frame <- TRUE
  for (i in 1:n.vars) {
    if (!(nm[i] %in% names(dframe))) {
      cat("\n\n\n>>> Note: ", nm[i], "is not in the data frame.\n")
      in.data.frame <- FALSE
    }
  }

  # reg analysis
  #   all analysis done on data in model construct lm.out$model
  #   this model construct contains only model vars, with Y listed first
  lm.out <<- lm(my.formula, data=dframe)

  n.keep <- nrow(lm.out$model)
    
  if (is.null(res.rows)) if (n.keep < 20) res.rows <- n.keep else res.rows <- 20 
  if (res.rows == "all") res.rows <- n.keep  # turn off resids with res.rows=0 call


  .reg1Basic(nm, mydframe,
         n.vars, n.pred, n.obs, n.keep, digits.d, explain, show.R, pre, line)
  
  # check for all numeric vars  in.data.frame <- TRUE
  numeric.all <- TRUE
  for (i in 1:n.vars) {
      if (in.data.frame && !is.numeric(dframe[1,which(names(dframe) == nm[i])])) {
        cat("\n\n\n>>> Note: ", nm[i], "is not a numeric variable.\n")
        numeric.all <- FALSE
      }
    }

  if (relate) .reg2Relations(nm, mydframe,
         n.vars, n.pred, n.obs, n.keep, digits.d, explain, show.R, pre, line,
         cor, collinear, subsets, numeric.all, in.data.frame)
  
  if(graphics.save) pdf("regOut.pdf") 
  else 
    if (res.rows > 0) .graphwin(3) else .graphwin()
  if (res.rows > 0) .reg3Residual(nm, mydframe,
         n.vars, n.pred, n.obs, n.keep, digits.d, explain, show.R, pre, line,
         res.sort, res.rows, cooks.cut, colors, graphics.save)
   
  # separate pred from graphics inside of .reg4 
  .reg4PredGraph(nm, mydframe, my.formula, brief, res.rows,
         n.vars, n.pred, n.obs, n.keep, digits.d, explain, show.R, pre, line,
         new.data, pred.sort, pred, pred.all, scatter.3d, scatter.coef,
         numeric.all, in.data.frame, colors, graphics.save, X1.new, 
         X2.new, X3.new, X4.new, X5.new)


# ----------
# References
# ----------

  if (!brief) {
    cat( "\n\n", "  REFERENCES", "\n")

    cat("\n",
        "Function Regression is from David Gerbing's lessR package.\n",
        "  To obtain the reference: Enter citation(\"lessR\")\n")
    cat("\n",
        "Collinearity analysis and 3d scatterplot are from the vif and scatter3d\n",
        "functions in John Fox's car package.\n",
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
  

# ----------

  if (graphics.save) {
    cat("\n\n")
    .dash(68)
    .showfile("regOut.pf", "regression graphics")
    dev.off()
  }

  options(op)  # restore options going into reg

}
