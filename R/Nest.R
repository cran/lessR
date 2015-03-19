Nest <-
function(y, nested.model, full.model, method=c("ls", "logit"), data=mydata) {

  method <- match.arg(method)

  my.vars <- as.list(seq_along(data))
  names(my.vars) <- names(data)

  # get response variable
  rv <- eval(substitute(y), envir=my.vars, enclos=parent.frame())
  y.name <-  names(my.vars)[rv]

  if (method == "logit") {
    is.bin <- TRUE
    if (is.factor(data[,y.name])) { 
       if (nlevels(data[,y.name]) != 2) is.bin  <- FALSE
    }
    else {
      for (i in 1:nrow(data))
        if (!is.na(data[i,y.name]))
          if (data[i,y.name]!=0 && data[i,y.name]!=1) is.bin <- FALSE
     }
    if (!is.bin) { 
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Response variable: ", y.name, "\n",
        "If numeric, can only have values of 0 or 1.\n",
        "If a factor, can only have two levels.\n\n")
    }
  }

  # get full model and analyze
  full.vars <- eval(substitute(full.model), envir=my.vars, enclos=parent.frame())
  n.pred <- length(full.vars)

  x.name <- character(length=n.pred)

  for (ivar in 1:n.pred) x.name[ivar] <- names(my.vars)[full.vars[ivar]]

  x.preds <- x.name[1]
  if (n.pred > 1)
    for(ivar in 2:n.pred) x.preds <- paste(x.preds, "+", x.name[ivar])

  my.formula <- as.formula(paste(y.name, "~", x.preds))
  if (method == "ls") 
    lm.full <- lm(my.formula, data=data)
  else
    lm.full <- suppressWarnings(glm(my.formula, data=data, family="binomial"))
    

  # get nested model and analyze with data from full model
  nest.vars <- eval(substitute(nested.model), envir=my.vars, enclos=parent.frame())
  n.pred <- length(nest.vars)
  x.name <- character(length=n.pred)
  for (ivar in 1:n.pred) {
    x.name[ivar] <- names(my.vars)[nest.vars[ivar]]
  }
  x.preds <- x.name[1]
  if (n.pred > 1)
    for(ivar in 2:n.pred) x.preds <- paste(x.preds, "+", x.name[ivar])

  my.formula <- as.formula(paste(y.name, "~", x.preds))
  if (method == "ls")
    lm.nest <- lm(my.formula, data=lm.full$model)
  else
    lm.nest <- suppressWarnings(glm(my.formula, data=lm.full$model, family="binomial"))


  # do the model comparison
  if (method == "ls") 
    print(anova(lm.nest, lm.full))
  else
    print(anova(lm.nest, lm.full, test="Chisq"))

}
