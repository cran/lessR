Histogram <-
function(x=NULL, data=mydata, n.cat=getOption("n.cat"),

    col.fill=getOption("col.fill.bar"), 
    col.stroke=getOption("col.stroke.bar"),
    col.bg=getOption("col.bg"),
    col.grid=getOption("col.grid"),

    col.reg="snow2", over.grid=FALSE,
    cex.axis=.85, col.axis="gray30", col.ticks="gray30",

    breaks="Sturges", bin.start=NULL, bin.width=NULL,

    prop=FALSE, cumul=c("off", "on", "both"), 
    digits.d=NULL, xlab=NULL, ylab=NULL, main=NULL,

    quiet=getOption("quiet"),
    pdf.file=NULL, pdf.width=5, pdf.height=5, ...)  {

  # produce actual argument, such as mydata, from an abbreviation
  cumul <- match.arg(cumul)

  is.df <- FALSE  # is data frame

  if (missing(x)) {
    x.name <- ""  # in case x is missing, i.e., data frame mydata
      is.df <- TRUE
      data <- eval(substitute(mydata))
  }
  else {  # get actual variable name before potential call of data$x
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
    is.frml <- xs$ifrmydata
    in.global <- xs$ig 

    # see if the variable exists in data frame, if x not in Global Env 
    if (!in.global) .xcheck(x.name, dname, data)

    if (!in.global)
      x.call <- eval(substitute(data$x))
    else {  # vars that are function names get assigned to global
      x.call <- x
      if (is.function(x.call)) x.call <- eval(substitute(data$x))
    }

  }  # x not data frame

  if (is.df) hst.data.frame(data, n.cat,
         col.fill, col.stroke, col.bg, col.grid, col.reg,
         over.grid, cex.axis, col.axis, col.ticks, breaks, bin.start, bin.width,
         prop, cumul, digits.d, xlab, ylab, main, quiet,
         pdf.width, pdf.height, ...) 

  else {
    .opendev(pdf.file, pdf.width, pdf.height)

    h <- .hst.main(x.call, col.fill, col.stroke, col.bg, col.grid, col.reg,
         over.grid, cex.axis, col.axis, col.ticks, breaks, bin.start, bin.width,
         prop, cumul, digits.d, xlab, ylab, main, quiet, ...) 

    if (!is.null(pdf.file)) {
      dev.off()
      .showfile(pdf.file, "histogram")
    }

    invisible(h)
  }   

}
