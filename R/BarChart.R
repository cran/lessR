BarChart <-
function(x=NULL, by=NULL, data=mydata, n.cat=getOption("n.cat"), 

         col.fill=NULL, col.stroke="black",
         col.bg=getOption("col.bg"),
         col.grid=getOption("col.grid"),
         random.col=FALSE,
         colors=c("rainbow", "terrain", "heat"),

         horiz=FALSE, over.grid=FALSE, addtop=1,
         gap=NULL, prop=FALSE,
         
         xlab=NULL, ylab=NULL, main=NULL,
         cex.axis=.85, col.axis="gray30", col.ticks="gray30",

         beside=FALSE, col.low=NULL, col.hi=NULL, count.levels=NULL,

         legend.title=NULL, legend.loc="right.margin", legend.labels=NULL,
         legend.horiz=FALSE, 

         quiet=getOption("quiet"),
         pdf.file=NULL, pdf.width=5, pdf.height=5, ...)  {


  if (missing(colors)) 
    colors <- getOption("colors")
  else
    colors <- match.arg(colors)

  is.df <- FALSE  # is data frame

  if (missing(x)) {
    x.name <- ""  # in case x is missing, i.e., data frame mydata
    is.df <- TRUE
    data <- eval(substitute(mydata))
  }
  else {
    # get actual variable name before potential call of data$x
    x.name <- deparse(substitute(x)) 
    options(xname = x.name)
    if (exists(x.name, where=1)) if (is.data.frame(x)) {
       is.df <- TRUE
       data <- x
    }
  }

  if (!is.df) {

    dname <- deparse(substitute(data))
    options(dname = dname)

    # get conditions and check for data existing
    xs <- .xstatus(x.name, dname, quiet)
    is.frml <- xs$ifr
    in.global <- xs$ig 

    # warn user that old formula mode no longer works
    if (is.frml) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Instead, of 'Y ~ X', now use the by option, 'Y, by=X' \n\n")
    }

    # see if the variable exists in data frame, if x not in Global Env 
    if (!in.global) .xcheck(x.name, dname, data)


    if (!in.global) x.call <- eval(substitute(data$x))
    else {  # vars that are function names get assigned to global
      x.call <- x
      if (is.function(x.call)) x.call <- eval(substitute(data$x))
    }

    # evaluate by
    if (!missing(by)) {

      # get actual variable name before potential call of data$x
      y.name <- deparse(substitute(by)) 
      options(yname = y.name)

      # see if y exists from a function call
      # indicate a function call with sys.frame returns larger than 1 
      if (exists(y.name, where=parent.frame(n=1)) && sys.nframe() > 1) 
        in.call <- TRUE else in.call <- FALSE

      # get conditions and check for data existing
      if (!in.call) {
        xs <- .xstatus(y.name, dname, quiet)
        in.global <- xs$ig 
      }
      else in.global <- FALSE

      # see if var exists in data frame, if x not in Global Env or function call 
      if (!in.global && !in.call) .xcheck(y.name, dname, data)

      if (!in.global) y.call <- eval(substitute(data$by))
      else {  # vars that are function names get assigned to global
        y.call <- by
        if (is.function(y.call)) y.call <- eval(substitute(data$by))
      }

    }
    else y.call <- NULL


  # evaluate count.levels
  #---------------------
  if (!missing(count.levels)) {

    # get actual variable name before potential call of data$x
    count.levels.name <- deparse(substitute(count.levels)) 
    options(count.levelsname = count.levels.name)

    # get conditions and check for data existing
    xs <- .xstatus(count.levels.name, dname, quiet)
    in.global <- xs$ig 

    # see if var exists in data frame, if x not in Global Env or function call 
    if (!missing(x) && !in.global)
      .xcheck(count.levels.name, dname, data)

    if (!in.global) count.levels.call <- eval(substitute(data$count.levels))
    else {  # vars that are function names get assigned to global
      count.levels.call <- count.levels
      if (is.function(count.levels.call)) 
        count.levels.call <- eval(substitute(data$count.levels))
    }
  }
  else
   count.levels.call <- NULL

  }  # x not data frame

  if (is.df) bc.data.frame(data, n.cat,
         col.fill, col.stroke, col.bg, col.grid, random.col, colors,
         horiz, over.grid, addtop, gap, prop, xlab, ylab, main,
         cex.axis, col.axis, col.ticks, beside, col.low, col.hi,
         count.levels,
         legend.title, legend.loc, legend.labels, legend.horiz, quiet,
         pdf.width, pdf.height, ...)

  else {
    .opendev(pdf.file, pdf.width, pdf.height)

    orig.params <- par(no.readonly=TRUE)
    on.exit(par(orig.params))

    .bc.main(x.call, y.call,
         col.fill, col.stroke, col.bg, col.grid, random.col, colors,
         horiz, over.grid, addtop, gap, prop, xlab, ylab, main,
         cex.axis, col.axis, col.ticks, beside, col.low, col.hi,
         count.levels.call,
         legend.title, legend.loc, legend.labels, legend.horiz, quiet, ...)

    if (!is.null(pdf.file)) {
      dev.off()
      .showfile(pdf.file, "barchart")
    }
  }

}

