.plt.bad <-
function (x.miss, y.miss, values, method, breaks, bin.start, ...) {

  # check for dated parameters no longer used
  dots <- list(...)
  
  if (!is.null(dots)) if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (grepl("color.", names(dots)[i], fixed=TRUE)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "color options dropped the  color. prefix\n",
          "eg., fill, instead of color.fill\n\n")
      }
      if (grepl("col.", names(dots)[i], fixed=TRUE)) 
        if (names(dots)[i] != "col.main"  &&
            names(dots)[i] != "col.lab"  &&
            names(dots)[i] != "col.sub") {
          cat("\n"); stop(call.=FALSE, "\n","------\n",
            "color options dropped the  col. prefix\n",
            "eg., fill, instead of col.fill\n\n")
      }
      if (grepl("fit.line", names(dots)[i], fixed=TRUE)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "fit.line options dropped the  .line suffix\n",
          "eg., fit, instead of fit.line\n\n")
      }
      if (names(dots)[i] %in% c("x.start","x.end","y.start","y.end")) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "x.start, x.end, y.start, and y.end no longer used\n\n",
          "Instead use the standard R xlim and ylim parameters,\n",
          "such as xlim=c(0,40) to specify from 0 to 40. Same for ylim\n\n")
      }
      if (names(dots)[i] == "line.chart") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  line.chart  is renamed  run\n\n")
      }
      if (names(dots)[i] == "line.width") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  line.width  is renamed  lwd\n\n")
      }
      if (names(dots)[i] == "bubble.size") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  bubble.size  is renamed  radius\n\n")
      }
      if (names(dots)[i] == "bubble.scale") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  bubble.scale  is renamed  power\n\n")
      }
      if (names(dots)[i] == "bubble.text") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  bubble.text  is renamed  labels\n\n")
      }
      if (names(dots)[i] == "topic") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  topic  is renamed  values\n\n")
      }
      if (names(dots)[i] == "topic") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  topic  is renamed  values\n\n")
      }
      if (names(dots)[i] == "kind") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  kind  is no longer active\n\n")
      }
      if (names(dots)[i] == "object") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  object  is no longer active\n\n",
          "use line.chart=TRUE to get a line chart\n",
          "set size=0 to remove points from the plot\n\n")
      }
      if (names(dots)[i] == "type") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  type  is renamed  object\n\n")
      }
      if (names(dots)[i] == "knitr.file") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "knitr.file  no longer used\n",
          "Instead use  Rmd  for R Markdown file\n\n")
      }
      if (names(dots)[i] == "diag") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "diag  option no longer available\n\n")
      }
      if (names(dots)[i] == "low.color") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  low.color  is renamed  low.fill\n\n")
      }
      if (names(dots)[i] == "hi.color") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  hi.color  is renamed  hi.fill\n\n")
      }
    }
  }
  
  
  # check for inconsistent parameters
  
  if (x.miss) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Must specify at least one variable to analyze\n\n")
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

  if (method == "stack") {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Option  stack  does not work\n\n")
  }

  if (method %in% c("spearman", "kendall")) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "The  method  parameter has another meaning for Plot\n\n",
      "Compute a Spearman or Kendall correlation\n",
      "  with the Correlation function\n\n")
  }

  if (is.numeric(breaks) && !is.null(bin.start)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Choose only one option to specify a start value.\n",
      "Either choose the option  breaks  or the option  bin.start.\n\n")
  }

}
