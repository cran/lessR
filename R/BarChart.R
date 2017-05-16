BarChart <- function(x=NULL, y=NULL, data=mydata, n.cat=getOption("n.cat"), 

         by=NULL, by1=NULL,
         n.row=NULL, n.col=NULL, aspect="fill",

         fill=getOption("bar.fill"),
         stroke=getOption("bar.stroke"),
         bg.fill=getOption("bg.fill"),
         bg.stroke=getOption("bg.stroke"),
         trans=NULL,

         colors=c("rainbow", "terrain", "heat"),

         horiz=FALSE, addtop=0.05,
         gap=NULL, proportion=FALSE,
         
         xlab=NULL, ylab=NULL, main=NULL, cex.names=0.70,
         cex.lab=0.84, cex.axis=getOption("cex.axis"),
         value.labels=NULL, label.max=20,
         rotate.x=getOption("rotate.x"),
         rotate.y=getOption("rotate.y"),
         offset=getOption("offset"),

         beside=FALSE, low.fill=NULL, hi.fill=NULL, 

         legend.title=NULL, legend.loc="right.margin", legend.labels=NULL,
         legend.horiz=FALSE, 

         quiet=getOption("quiet"),
         width=5, height=4.5, pdf.file=NULL, ...)  {


  if (missing(colors)) 
    colors <- getOption("theme")
  else
    colors <- match.arg(colors)

  Trellis <- ifelse(!missing(by1), TRUE, FALSE)
  do.plot <- TRUE

  if (!is.null(fill)) {
    for (i in 1:length(fill))
      if (fill[i] == "off") fill[i] <- "transparent"
  }
  for (i in 1:length(stroke))
    if (stroke[i] == "off") stroke[i] <- "transparent"
  if (bg.fill == "off") bg.fill <- "transparent"
  if (bg.stroke == "off") bg.stroke <- "transparent"

  if (missing(stroke))  # default black border unless dark bg
    if (sum(col2rgb(bg.fill))/3 > 80) stroke <- "black"

  dots <- list(...)  # check for deprecated/changed parameters
  if (length(dots) > 0) {
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
      if (names(dots)[i] == "addtop") 
        cat("\naddtop  is now a multiplicative factor instead of additive\n\n")
      if (names(dots)[i] == "count.levels") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Now use  count.labels  instead of count.levels\n\n")
      }
      if (names(dots)[i] == "count.labels") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "count.labels  not available, but now specify a y-variable for\n",
          "the data values to obtain the same effect, which can be\n",
          " continuous, and a categorical variable for the labels\n\n")
      }
      if (grepl("color.", names(dots)[i], fixed=TRUE)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "color options dropped the  color. prefix\n",
          "eg., fill, instead of color.fill.\n\n")
      }
      if (names(dots)[i] == "over.grid") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "over.grid  option removed\n\n")
      }
      if (names(dots)[i] == "pdf") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "pdf  changed to  pdf.file\n\n")
      }
      if (names(dots)[i] == "box") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  box  is renamed  bg.stroke\n\n")
      }
      if (names(dots)[i] == "bg") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  bg  is renamed  bg.fill\n\n")
      }
      if (names(dots)[i] == "axes") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  axes  is renamed  values.stroke\n\n")
      }
    }
  }

  x.name <- deparse(substitute(x))
  options(xname = x.name)

  df.name <- deparse(substitute(data))
  options(dname = df.name)


# -----------------------------------------------------------
# establish if a data frame, if not then identify variable(s)

  x.call <- NULL

  if (!missing(x)) {
    if (!exists(x.name, where=.GlobalEnv)) {  # x not in global env, in df
      .nodf(df.name)  # check to see if data frame container exists 
      .xcheck(x.name, df.name, data)  # see if var in df, vars lists not checked
      vars.list <- as.list(seq_along(data))
      names(vars.list) <- names(data)
      x.col <- eval(substitute(x), envir=vars.list)  # col num of each var
      if (length(x.col) > 1) data <- data[, x.col]  # x is a vars list
      if (length(x.col) == 1) x.call <- eval(substitute(data$x))  # x is 1 var
    }
    else {  # x is in the global environment (vector, matrix or data frame)
      if (is.data.frame(x))  # x a data frame
        data <- x
      else {  # x a vector or matrix in global
        if (exists(x.name, where=.GlobalEnv)) if (is.matrix(x)) { 
          x.name <- xlab
          xlab <- NULL
          by.name <- legend.title
          options(xname = x.name)
          options(byname = by.name)
        }
        x.call <- x
        if (is.function(x.call)) x.call <- eval(substitute(data$x))
      }
    }
  }


  if (!is.null(x.call)) {  # x is a single var, not a data frame

    # evaluate by
    if (!missing(by)) {

      # get actual variable name before potential call of data$x
      by.name <- deparse(substitute(by)) 
      options(byname = by.name)

      # see if y exists from a function call
      # indicate a function call with sys.nframe returns larger than 1 
      #if (exists(y.name, where=parent.frame(n=1)) && sys.nframe() > 1) 
        #in.call <- TRUE else in.call <- FALSE

      # get conditions and check for data existing
      #if (!in.call) {
        xs <- .xstatus(by.name, df.name, quiet)
        in.global <- xs$ig 
      #}
      #else in.global <- FALSE
      # if y is in global, sys.nframe() returns two, in.call is TRUE,
      #   which leads to in.global FALSE
      #if (exists(by.name, where=.GlobalEnv)) in.global <- TRUE

      # see if var exists in data frame, if x not in global Env or function call 
      if (!in.global) .xcheck(by.name, df.name, data)
      #if (!in.global && !in.call) .xcheck(by.name, df.name, data)
      if (!in.global)
        by.call <- eval(substitute(data$by))
      else {  # vars that are function names get assigned to global
        by.call <- by
        if (is.function(by.call)) by.call <- eval(substitute(data$by))
      }

    }
    else
      by.call <- NULL


    # evaluate y
    #-------------
    if (!missing(y)) {

      # get actual variable name before potential call of data$x
      y.name <- deparse(substitute(y)) 
      options(yname = y.name)

      # get conditions and check for data existing
      xs <- .xstatus(y.name, df.name, quiet)
      in.global <- xs$ig 

      # see if var exists in data frame, if x not in global Env or function call 
      if (!in.global) .xcheck(y.name, df.name, data)
      if (!in.global)
        y.call <- eval(substitute(data$y))
      else {  # vars that are function names get assigned to global
        y.call <- y
        if (is.function(y.call)) y.call <- eval(substitute(data$y))
      }

    }
    else
      y.call <- NULL

    
    # evaluate by1
    #-------------
    if (!missing(by1)) {

      # get actual variable name before potential call of data$x
      by1.name <- deparse(substitute(by1))
      options(by1name = by1.name)

      # get conditions and check for data existing
      xs <- .xstatus(by1.name, df.name, quiet)
      in.global <- xs$ig

      # see if var exists in data frame, if x not in global Env or function call
      if (!missing(x) && !in.global)
        .xcheck(by1.name, df.name, data)

      if (!in.global)
        by1.call <- eval(substitute(data$by1))
      else {  # vars that are function names get assigned to global
        by1.call <- by1
        if (is.function(by1.call)) by1.call <- eval(substitute(data$by1))
      }

      if (!is.factor(by1.call)) by1.call <- factor(by1.call)
    }

    else
     by1.call <- NULL



    if (length(unique(na.omit(x.call))) == 1) { 
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "There is only one unique value for the values of ", x.name,
        ": ", na.omit(x.call)[1], "\n",
        "The bar chart is only computed if there is more than one",
        " unique value\n\n")
    }


  # -----------  x, y, by and size variables established ------------
  # -----------------------------------------------------------------

    # do the analysis

    if (Trellis && do.plot) {
      .bar.lattice(x.call, by1.call, by2=NULL, n.row, n.col, aspect, prop=FALSE,
                   fill, stroke, bg.fill, bg.stroke, trans, 
                   size.pt=NULL, xlab, ylab, main, cex.lab, cex.axis,
                   rotate.x, rotate.y, width, height, pdf.file,
                   segments.x=NULL, breaks=NULL, c.type="bar")
    }

    else {

      if (!is.null(pdf.file))
        pdf.fnm <- paste("BarChart_", x.name, ".pdf", sep="") 
      else
        pdf.fnm <- NULL
      .opendev(pdf.fnm, width, height)

      bc <- .bc.main(x.call, y.call, by.call,
           fill, stroke, bg.fill,
           bg.stroke, trans, colors,
           horiz, addtop, gap, proportion, xlab, ylab, main, cex.lab,
           value.labels, label.max,
           cex.axis, cex.names, rotate.x, rotate.y, offset,
           beside, low.fill, hi.fill,
           legend.title, legend.loc, legend.labels, legend.horiz, quiet, ...)

      if (!is.null(pdf.file)) {
        dev.off()
        if (!quiet) .showfile(pdf.fnm, "barchart")
      }

      invisible(bc)
    }  # end !Trellis
  }
  

  else {

    bc.data.frame(data, n.cat,
      fill, stroke, bg.fill, bg.stroke,
      trans, colors,
      horiz, addtop, gap, proportion, xlab, ylab, main, cex.lab,
      value.labels, label.max,
      cex.axis, cex.names, rotate.x, rotate.y, offset,
      beside, low.fill, hi.fill,
      legend.title, legend.loc, legend.labels, legend.horiz, quiet,
      width, height, pdf.file, ...)
  }

}

