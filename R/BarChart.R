BarChart <-
function(x=NULL, by=NULL, data=mydata, n.cat=getOption("n.cat"), 

         col.fill=NULL, col.stroke=getOption("col.stroke.bar"),
         col.bg=getOption("col.bg"),
         col.grid=getOption("col.grid"),
         random.col=FALSE,
         colors=c("rainbow", "terrain", "heat"),

         horiz=FALSE, over.grid=FALSE, addtop=1,
         gap=NULL, prop=FALSE,
         
         xlab=NULL, ylab=NULL, main=NULL,
         cex.axis=0.75, col.axis="gray30",
         value.labels=NULL, rotate.values=0, offset=0.5,

         beside=FALSE, col.low=NULL, col.hi=NULL, count.levels=NULL,

         legend.title=NULL, legend.loc="right.margin", legend.labels=NULL,
         legend.horiz=FALSE, 

         quiet=getOption("quiet"),
         pdf.file=NULL, pdf.width=5, pdf.height=5, ...)  {


  if (missing(colors)) 
    colors <- getOption("colors")
  else
    colors <- match.arg(colors)

  if (missing(col.stroke))  # default black border unless dark bg
    if (sum(col2rgb(col.bg))/3 > 80) col.stroke <- "black"

  if (!is.null(pdf.file))
    if (!grepl(".pdf", pdf.file)) pdf.file <- paste(pdf.file, ".pdf", sep="")

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
          y.name <- legend.title
          options(xname = x.name)
          options(yname = y.name)
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
      y.name <- deparse(substitute(by)) 
      options(yname = y.name)

      # see if y exists from a function call
      # indicate a function call with sys.nframe returns larger than 1 
      #if (exists(y.name, where=parent.frame(n=1)) && sys.nframe() > 1) 
        #in.call <- TRUE else in.call <- FALSE


      # get conditions and check for data existing
      #if (!in.call) {
        xs <- .xstatus(y.name, df.name, quiet)
        in.global <- xs$ig 
      #}
      #else in.global <- FALSE
      # if y is in global, sys.nframe() returns two, in.call is TRUE,
      #   which leads to in.global FALSE
      #if (exists(y.name, where=.GlobalEnv)) in.global <- TRUE

      # see if var exists in data frame, if x not in Global Env or function call 
      if (!in.global) .xcheck(y.name, df.name, data)
      #if (!in.global && !in.call) .xcheck(y.name, df.name, data)
      if (!in.global)
        y.call <- eval(substitute(data$by))
      else {  # vars that are function names get assigned to global
        y.call <- by
        if (is.function(y.call)) y.call <- eval(substitute(data$by))
      }

    }
    else
      y.call <- NULL


    # evaluate count.levels
    #---------------------
    if (!missing(count.levels)) {

      # get actual variable name before potential call of data$x
      x.name <- deparse(substitute(count.levels)) 
      options(xname = x.name)

      # get conditions and check for data existing
      xs <- .xstatus(x.name, df.name, quiet)
      in.global <- xs$ig 

      # see if var exists in data frame, if x not in Global Env or function call 
      if (!missing(x) && !in.global)
        .xcheck(x.name, df.name, data)

      if (!in.global) count.levels.call <- eval(substitute(data$count.levels))
      else {  # vars that are function names get assigned to global
        count.levels.call <- count.levels
        if (is.function(count.levels.call)) 
          count.levels.call <- eval(substitute(data$count.levels))
      }
    }
    else
      count.levels.call <- NULL


    if (length(unique(na.omit(x.call))) == 1) { 
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "There is only one unique value for the values of ", x.name,
        ": ", na.omit(x.call)[1], "\n",
        "The bar chart is only computed if there is more than one",
        " unique value\n\n")
    }

# ---------------
# do the analysis

    .opendev(pdf.file, pdf.width, pdf.height)

    orig.params <- par(no.readonly=TRUE)
    on.exit(par(orig.params))

    bc <- .bc.main(x.call, y.call,
         col.fill, col.stroke, col.bg, col.grid, random.col, colors,
         horiz, over.grid, addtop, gap, prop, xlab, ylab, main, value.labels,
         cex.axis, col.axis, rotate.values, offset, beside, col.low, col.hi,
         count.levels.call,
         legend.title, legend.loc, legend.labels, legend.horiz, quiet, ...)

    if (!is.null(pdf.file)) {
      dev.off()
      if (!quiet) .showfile(pdf.file, "barchart")
    }

    invisible(bc)
  }
  

  else
    bc.data.frame(data, n.cat,
      col.fill, col.stroke, col.bg, col.grid, random.col, colors,
      horiz, over.grid, addtop, gap, prop, xlab, ylab, main, value.labels,
      cex.axis, col.axis, rotate.values, offset, beside, col.low, col.hi,
      count.levels,
      legend.title, legend.loc, legend.labels, legend.horiz, quiet,
      pdf.width, pdf.height, ...)

}

