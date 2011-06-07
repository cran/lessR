smd.t.test.formula <-
function (formula, data, ...) {

  if ((length(formula) != 3) || (length(attr(terms(formula[-2]),"term.labels")) !=1)) 
      stop("'Formula' missing or incorrect.")

  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) 
      m$data <- as.data.frame(data)
  m[[1L]] <- as.name("model.frame")
  m$... <- NULL
  mf <- eval(m, parent.frame())
  Ynm <- names(mf)[1]
  Xnm <- names(mf)[2]
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  
  response <- attr(attr(mf, "terms"), "response")
  if (!is.numeric(mf[[response]])) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "You specified ", Ynm, " as the response variable, the 1st variable listed.\n",
    "The response variable must have only numeric values.\n",
    "The first value of ", Ynm, " is ", mf[[response]][1], ".\n",
    "Perhaps you have the order of the variables reversed.\n\n")
  }

  g <- factor(mf[[-response]])      
  gu <- unique(g)
  if (length(gu) != 2) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "Values of the grouping variable: ", levels(g), "\n",
    "Number of unique values: ", length(gu), "\n",
    "The grouping variable for a t-test must have exactly two unique values.\n\n")
  }

  DATA <- split(mf[[response]], g)
  names(DATA) <- c("Y1", "Y2")
  attach(DATA, warn.conflicts=FALSE)
  
  smd.t.test(Y1, Y2, Ynm=Ynm, Xnm=Xnm, X1nm=levels(gu)[1], X2nm=levels(gu)[2], ...)

}
