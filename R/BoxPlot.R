BoxPlot <-
function(x=NULL, data=mydata, n.cat=getOption("n.cat"),
    knitr.file=NULL,

    col.fill=getOption("col.fill.bar"),
    col.stroke=getOption("col.stroke.bar"), 
    col.bg=getOption("col.bg"),
    col.grid=getOption("col.grid"),

    cex.axis=.85, col.axis="gray30",
    xlab=NULL, main=NULL, digits.d=NULL,

    horiz=TRUE, add.points=FALSE,

    quiet=getOption("quiet"),
    pdf.file=NULL, pdf.width=5, pdf.height=5, 
    fun.call=NULL, ...) {


  if (is.null(fun.call)) fun.call <- match.call()


  if (getOption("colors") == "gray") col.stroke <- "black"
  if (getOption("colors") == "gray.black") col.stroke <- getOption("col.stroke.pt")

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

    nu <- length(unique(na.omit(data[,i])))

    x.name <- names(data)[i]
    options(xname = x.name)

    if (is.numeric(data[,i])) {
      if (nu > n.cat) {

      pdf.fnm <- .pdfname("BoxPlot", x.name, go.pdf, pdf.nm, pdf.file)
     .opendev(pdf.fnm, pdf.width, pdf.height)

      stuff <- .bx.main(data[,i], col.fill, col.stroke, col.bg, col.grid,
         cex.axis, col.axis, 
         horiz, add.points, xlab, main, digits.d, quiet, ...)
      txsts <- stuff$tx
      if (length(txsts)==0) txsts <- ""

      txotl <- ""
      if (!quiet) {
        txotl <- .outliers2(data[,i])
        if (length(txotl)==0) txotl <- "No outliers"
      }

      if (ncol(data) > 1) {  # for a variable range, just the text output
        class(txsts) <- "out_piece"
        class(txotl) <- "out_piece"
        output <- list(out_stats=txsts, out_outliers=txotl)
        class(output) <- "out_all"
        print(output)
      }

      if (go.pdf) {
        dev.off()
        if (!quiet) .showfile(pdf.fnm, "Box Plot")
      }

    }  # nu > n.cat
    else
      .ncat("Box Plot", x.name, nu, n.cat)

    }  # is.numeric(data[,i])
  }  # for


  dev.set(which=2)  # reset graphics window for standard R functions

  if (ncol(data)==1  &&  nu>n.cat) {

    # knitr
    txkfl <- ""
    if (!is.null(knitr.file)) {
      txt <- ifelse (grepl(".Rmd", knitr.file), "", ".Rmd")
      knitr.file <- paste(knitr.file, txt, sep="") 
      txknt <- .dist.knitr(x.name, df.name, fun.call, digits.d)
      cat(txknt, file=knitr.file, sep="\n")
      txkfl <- .showfile2(knitr.file, "knitr instructions")
    }
 
    class(txsts) <- "out_piece"
    class(txotl) <- "out_piece"
    class(txkfl) <- "out_piece"

    output <- list(type="BoxPlot",
      call=fun.call,
      out_stats=txsts, out_outliers=txotl, out_file=txkfl,
      n=stuff$n, n.miss=stuff$n.miss, min=stuff$mn, lower_whisker=stuff$lw,
      lower_hinge=stuff$lh, median=stuff$md, upper_hinge=stuff$uh,
      upper_whisker=stuff$uw, max=stuff$mx, IQR=stuff$IQR)

    class(output) <- "out_all"

    return(output)

  }

}


