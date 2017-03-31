BarChart <-
function(x=NULL, y=NULL, by=NULL, data=mydata, n.cat=getOption("n.cat"), 

         fill=getOption("fill.bar"),
         stroke=getOption("stroke.bar"),
         bg=getOption("bg"),
         grid=getOption("grid"),
         box=getOption("box"),
         trans=NULL,

         colors=c("rainbow", "terrain", "heat"),

         horiz=FALSE, over.grid=FALSE, addtop=0.05,
         gap=NULL, proportion=FALSE,
         
         xlab=NULL, ylab=NULL, main=NULL,
         cex.axis=0.75, axes="gray30",
         value.labels=NULL, label.max=20, rotate.x=0, rotate.y=0, offset=0.5,

         beside=FALSE, low.fill=NULL, hi.fill=NULL, 

         legend.title=NULL, legend.loc="right.margin", legend.labels=NULL,
         legend.horiz=FALSE, 

         quiet=getOption("quiet"),
         width=4.5, height=4.5, pdf=FALSE, ...)  {


  if (missing(colors)) 
    colors <- getOption("colors")
  else
    colors <- match.arg(colors)

  if (!is.null(fill)) {
    for (i in 1:length(fill))
      if (fill[i] == "off") fill[i] <- "transparent"
  }
  for (i in 1:length(stroke))
    if (stroke[i] == "off") stroke[i] <- "transparent"
  if (bg == "off") bg <- "transparent"
  if (grid == "off" ) grid <- "transparent"
  if (box == "off") box <- "transparent"

  if (missing(stroke))  # default black border unless dark bg
    if (sum(col2rgb(bg))/3 > 80) stroke <- "black"

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
      if (names(dots)[i] == "pdf.file") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "pdf.file  changed to  pdf, either TRUE or FALSE\n\n")
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

      # see if var exists in data frame, if x not in Global Env or function call 
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
    if (!missing(y)) {

      # get actual variable name before potential call of data$x
      y.name <- deparse(substitute(y)) 
      options(yname = y.name)

      # get conditions and check for data existing
        xs <- .xstatus(y.name, df.name, quiet)
        in.global <- xs$ig 

      # see if var exists in data frame, if x not in Global Env or function call 
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


    if (length(unique(na.omit(x.call))) == 1) { 
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "There is only one unique value for the values of ", x.name,
        ": ", na.omit(x.call)[1], "\n",
        "The bar chart is only computed if there is more than one",
        " unique value\n\n")
    }

    # do the analysis

    if (pdf)
      pdf.fnm <- paste("BarChart_", x.name, ".pdf", sep="") 
    else
      pdf.fnm <- NULL
    .opendev(pdf.fnm, width, height)

    orig.params <- par(no.readonly=TRUE)
    on.exit(par(orig.params))

    bc <- .bc.main(x.call, y.call, by.call,
         fill, stroke, bg, grid, box, trans, colors,
         horiz, over.grid, addtop, gap, proportion, xlab, ylab, main,
         value.labels, label.max,
         cex.axis, axes, rotate.x, rotate.y, offset, beside, low.fill, hi.fill,
         legend.title, legend.loc, legend.labels, legend.horiz, quiet, ...)

    if (is.null(pdf)) {
      dev.off()
      if (!quiet) .showfile(pdf.fnm, "barchart")
    }

    invisible(bc)
  }
  

  else
    bc.data.frame(data, n.cat,
      fill, stroke, bg, grid, box, trans, colors,
      horiz, over.grid, addtop, gap, proportion, xlab, ylab, main,
      value.labels, label.max,
      cex.axis, axes, rotate.x, rotate.y, offset, beside, low.fill, hi.fill,
      legend.title, legend.loc, legend.labels, legend.horiz, quiet,
      width, height, pdf, ...)

}

