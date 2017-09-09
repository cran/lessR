.plt.bad <-
function (x.miss, y.miss, values, breaks, bin.start, n.row, n.col,
          MD.cut, out.cut, fit.se, ...) {

  # check for inconsistent parameters
  
  if (x.miss) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Must specify at least one variable to analyze\n\n")
  }
  
  if (MD.cut > 0  &&  out.cut > 0) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Choose MD.cut or out.cut to specify outliers, but not both\n\n")
  }
  
  if (fit.se[1] > 0.999) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "fit.se now specified as the confidence level, between 0 and 1\n\n")
  }
  
  if (values %in% c("mean", "sd", "min", "max") && y.miss) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Must specify a numeric y-variable from which to compute the\n ",
      " ", values, " for each level of ", deparse(substitute(x)), "\n\n")
  }

  #if (values != "data"  &&  object == "sunflower") {
    #cat("\n"); stop(call.=FALSE, "\n","------\n",
      #"Sunflowers are only plotted for data\n\n")
  #}

  #if (method %in% c("spearman", "kendall")) {
    #cat("\n"); stop(call.=FALSE, "\n","------\n",
      #"The  method  parameter has another meaning for Plot\n\n",
      #"Compute a Spearman or Kendall correlation\n",
      #"  with the Correlation function\n\n")
  #}

  if (is.numeric(breaks) && !is.null(bin.start)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Choose only one option to specify a start value.\n",
      "Either choose the option  breaks  or the option  bin.start.\n\n")
  }

  if (!is.null(n.row)  &&  !is.null(n.col)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Only specify n.row or n.col, but not both\n\n",
      "From one of the values, the other value is calculated\n\n")
  }

}
