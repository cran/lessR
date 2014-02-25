Histogram <-
function(x=NULL, data=mydata, n.cat=getOption("n.cat"),

    col.fill=getOption("col.fill.bar"), 
    col.stroke=getOption("col.stroke.bar"),
    col.bg=getOption("col.bg"),
    col.grid=getOption("col.grid"),

    col.reg="snow2", over.grid=FALSE,
    cex.axis=.85, col.axis="gray30", col.ticks="gray30",

    breaks="Sturges", bin.start=NULL, bin.width=NULL, bin.end=NULL,

    prop=FALSE, cumul=c("off", "on", "both"), 
    digits.d=NULL, xlab=NULL, ylab=NULL, main=NULL,

    quiet=getOption("quiet"),
    pdf.file=NULL, pdf.width=5, pdf.height=5, ...)  {


  # limit actual argument to alternatives, perhaps abbreviated
  cumul <- match.arg(cumul)

  
  # get actual variable name before potential call of data$x
  x.name <- deparse(substitute(x))
  options(xname = x.name)

  df.name <- deparse(substitute(data))
  options(dname = df.name)

  pdf.nm <- FALSE
  if (!missing(pdf.file)) pdf.nm <- TRUE

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

      pdf.fnm <- .pdfname("Hist", x.name, go.pdf, pdf.nm, pdf.file)
     .opendev(pdf.fnm, pdf.width, pdf.height)

      h <- .hst.main(data[,i], col.fill, col.stroke, col.bg, col.grid, col.reg,
          over.grid, cex.axis, col.axis, col.ticks, breaks, bin.start, bin.width,
          bin.end, prop, cumul, digits.d, xlab, ylab, main, quiet, ...)

      if (go.pdf) {
        dev.off()
        if (!quiet) .showfile(pdf.fnm, "Histogram")
      }

    }  # nu > n.cat
    else
      .ncat("Histogram", x.name, nu, n.cat)

    }  # is.numeric(data[,i])
  }  # for

  if (ncol(data)==1  && nu>n.cat) invisible(h)

}
