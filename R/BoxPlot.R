BoxPlot <-
function(x=NULL, data=mydata, n.cat=getOption("n.cat"),

        col.fill=getOption("col.fill.bar"),
        col.stroke=getOption("col.stroke.bar"), 
        col.bg=getOption("col.bg"),
        col.grid=getOption("col.grid"),

        cex.axis=.85, col.axis="gray30", col.ticks="gray30",
        xlab=NULL, main=NULL, digits.d=NULL,

        horiz=TRUE, add.points=FALSE,

        quiet=getOption("quiet"),
        pdf.file=NULL, pdf.width=5, pdf.height=5, ...)  {


  if (getOption("colors") == "gray") col.stroke <- "black"
  if (getOption("colors") == "gray.black") col.stroke <- getOption("col.stroke.pt")

  x.name <- deparse(substitute(x))
  options(xname = x.name)

  is.df <- FALSE  # is data frame

  if (missing(x)) {
    x.name <- ""  # in case x is missing, i.e., data frame mydata
    is.df <- TRUE
    data <- eval(substitute(mydata))
  }

  else if ( (!grepl(":", x.name) && !grepl(",", x.name)) ) {  # not a var list
    if (exists(x.name, where=1)) if (is.data.frame(x)) {
        data <- x
        is.df <- TRUE
    }
  }

  # proceed here only if x.name is a var list
  else if (grepl(":", x.name) || grepl(",", x.name) ) {
    all.vars <- as.list(seq_along(data))
    names(all.vars) <- names(data)
    x.col <- eval(substitute(x), envir=all.vars, enclos=parent.frame())
    data <- data[, x.col]  # create subset data frame
    is.df <- TRUE
  }


  if (!is.df) {

    dname <- deparse(substitute(data))
    options(dname = dname)

    # get conditions and check for data existing
    xs <- .xstatus(x.name, dname, quiet)
    in.global <- xs$ig 

    # see if the variable exists in data frame, if x not in Global Env 
    if (!in.global) .xcheck(x.name, dname, data)

    if (!in.global) x.call <- eval(substitute(data$x))
    else {  # vars that are function names get assigned to global
      x.call <- x
      if (is.function(x.call)) x.call <- eval(substitute(data$x))
    }

  }  # x not data frame


  if (is.df) bx.data.frame(data, n.cat,
         col.fill, col.stroke, col.bg, col.grid,
         cex.axis, col.axis, col.ticks,
         horiz, add.points, xlab, main, digits.d, quiet,
         pdf.width, pdf.height, ...) 

  else {
    .opendev(pdf.file, pdf.width, pdf.height)

    b <- .bx.main(x.call, col.fill, col.stroke, col.bg, col.grid,
         cex.axis, col.axis, col.ticks,
         horiz, add.points, xlab, main, digits.d, quiet, ...)

    if (!is.null(pdf.file)) {
      dev.off()
      .showfile(pdf.file, "boxplot")
    }
 
  invisible(b)
  }

}


