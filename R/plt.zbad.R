.plt.bad <-
function (x.miss, y.miss, stat, breaks, bin_start, n_row, n_col,
          MD_cut, out_cut, fit_se, ...) {

  # check for inconsistent parameters
  
  if (x.miss) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Must specify at least one variable to analyze\n\n")
  }
  
  if (MD_cut > 0  &&  out_cut > 0) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Choose MD_cut or out_cut to specify outliers, but not both\n\n")
  }
  
  if (fit_se[1] > 0.999) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "fit_se now specified as the confidence level, between 0 and 1\n\n")
  }
  
  if (stat %in% c("mean", "sd", "dev", "min", "median", "max") && y.miss) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Must specify a numeric y-variable from which to compute the\n ",
      " ", stat, " for each level of ", deparse(substitute(x)), "\n\n")
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

  if (is.numeric(breaks) && !is.null(bin_start)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Choose only one option to specify a start value.\n",
      "Either choose the option  breaks  or the option  bin_start.\n\n")
  }

  if (!is.null(n_row)  &&  !is.null(n_col)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Only specify n_row or n_col, but not both\n\n",
      "From one of the values, the other value is calculated\n\n")
  }

}
