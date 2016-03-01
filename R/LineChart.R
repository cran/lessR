LineChart <-
function(x, data=mydata, n.cat=getOption("n.cat"), type=NULL, 

         col.fill=getOption("col.fill.bar"), 
         col.stroke=getOption("col.stroke.pt"),
         col.bg=getOption("col.bg"),
         col.grid=getOption("col.grid"),
         col.line=getOption("col.stroke.pt"),

         col.area=NULL, col.box="black",

         shape.pts=21, cex.axis=0.75, col.axis="gray30",

         rotate.values=0, offset=.5,

         xy.ticks=TRUE, line.width=1.1,
         xlab=NULL, ylab=NULL, main=NULL, sub=NULL, cex=NULL,

         time.start=NULL, time.by=NULL, time.reverse=FALSE,

         center.line=c("default", "mean", "median", "zero", "off"),

         show.runs=FALSE, quiet=getOption("quiet"),
         pdf.file=NULL, pdf.width=5, pdf.height=5, ...) {


  center.line <- match.arg(center.line)

  if (!is.null(pdf.file))
    if (!grepl(".pdf", pdf.file)) pdf.file <- paste(pdf.file, ".pdf", sep="")

  # get actual variable name before potential call of data$x
  x.name <- deparse(substitute(x))
  options(xname = x.name)
  options(yname = x.name)  # for .lc.main, which uses y as the var

  df.name <- deparse(substitute(data))
  options(dname = df.name)

  pdf.nm <- FALSE
  if (!missing(pdf.file)) pdf.nm <- TRUE

  if (!quiet && !show.runs)
    cat("[To view the individual runs: show.runs=TRUE]\n")

# -----------------------------------------------------------
# establish if a data frame, if not then identify variable(s)

  if (!missing(x)) {
    if (!exists(x.name, where=.GlobalEnv)) {  # x not in global env, in df
      .nodf(df.name)  # check to see if data frame container exists 
      .xcheck(x.name, df.name, data)  # see if var in df, vars lists not checked
      vars.list <- as.list(seq_along(data))
      names(vars.list) <- names(data)
      x.col <- eval(substitute(x), envir=vars.list)  # col num of each var
      if (class(data) != "list") {
        data <- data[, x.col]
        if (length(x.col) == 1) {
          data <- data.frame(data)  # x is 1 var
          names(data) <- x.name
         }
      }
      else {
        data <- data.frame(data[[x.col]])
        names(data) <- x.name
      }
    }
    else { # x is in the global environment (vector or data frame)
      if (is.data.frame(x))  # x a data frame
        data <- x
      else {  # x a vector in global
        data <- data.frame(x)  # x is 1 var
        names(data) <- x.name
      }
    }
  }


# ---------------
# do the analysis

  go.pdf <- FALSE
  if (pdf.nm || ncol(data) > 1) go.pdf <- TRUE

  for (i in 1:ncol(data)) {
    cat("\n")

    nu <- length(unique(na.omit(data[,i])))

    x.name <- names(data)[i]
    options(xname = x.name)

    if (is.numeric(data[,i])) {
      if (nu > n.cat) {

      pdf.fnm <- .pdfname("LC", x.name, go.pdf, pdf.nm, pdf.file)
     .opendev(pdf.fnm, pdf.width, pdf.height)

      .lc.main(data[,i], type,
         col.line, col.area, col.box, col.stroke, col.fill, shape.pts,
         col.grid, col.bg, cex.axis, col.axis, rotate.values, offset, xy.ticks,
         line.width, xlab, ylab, main, sub, cex,
         time.start, time.by, time.reverse, 
         center.line, show.runs, quiet, ...)

      if (go.pdf) {
        dev.off()
        if (!quiet) .showfile(pdf.fnm, "Line Chart")
      }

    }  # nu > n.cat
    else
      .ncat("Line Chart", x.name, nu, n.cat)

    }  # is.numeric(data[,i])
  }  # for

}
