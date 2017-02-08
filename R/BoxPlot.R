BoxPlot <-
function(x=NULL, data=mydata, n.cat=getOption("n.cat"),
    Rmd=NULL,

    fill=getOption("fill.bar"),
    stroke=getOption("stroke.bar"), 
    bg=getOption("bg"),
    grid=getOption("grid"),
    box=getOption("box"),

    cex.axis=0.75, axes="gray30",
    xlab=NULL, main=NULL, sub=NULL, digits.d=NULL,

    rotate.values=0, offset=0.5,

    horiz=TRUE, add.points=FALSE,

    quiet=getOption("quiet"),
    width=4.5, height=4.5, pdf=FALSE,
    fun.call=NULL, ...) {


  if (is.null(fun.call)) fun.call <- match.call()

  for (i in 1:length(fill))
    if (fill[i] == "off") fill[i] <- "transparent"
  for (i in 1:length(stroke))
    if (stroke[i] == "off") stroke[i] <- "transparent"
  if (bg == "off") bg <- "transparent"
  if (grid == "off" ) grid <- "transparent"
  if (box == "off") box <- "transparent"

  if (getOption("colors") == "gray") stroke <- "black"
  if (getOption("colors") == "gray.black") stroke <- getOption("stroke.pt")

  dots <- list(...)  # check for deprecated parameters
  if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (names(dots)[i] == "knitr.file") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "knitr.file  no longer used\n",
          "Instead use  Rmd  for R Markdown file\n\n")
      }
      if (grepl("color.", names(dots)[i], fixed=TRUE)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "color options dropped the  color. prefix\n",
          "eg., fill, instead of color.fill.\n\n")
      }
    }
    if (substr(names(dots)[i], 1, 4) == "col.") {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "options that began with the abbreviation  col  now begin with  ",
        "color \n\n")
    }
    if (names(dots)[i] == "pdf.file") {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "pdf.file  changed to  pdf, either TRUE or FALSE\n\n")
    }
  }

  # get actual variable name before potential call of data$x
  x.name <- deparse(substitute(x))
  options(xname = x.name)

  df.name <- deparse(substitute(data))
  options(dname = df.name)


# -----------------------------------------------------------
# establish if a data frame, if not then identify variable(s)

  if (!missing(x)) {

    if (!exists(x.name, where=.GlobalEnv)) {  # x not in global env, in df
      .nodf(df.name)  # check to see if data frame container exists 
      .xcheck(x.name, df.name, data)  # var in df?, vars lists not checked
      all.vars <- as.list(seq_along(data))  # even if only a single var
      names(all.vars) <- names(data)  # all data in data frame
      x.col <- eval(substitute(x), envir=all.vars)  # col num selected vars
      if (!("list" %in% class(data))) {
        data <- data[, x.col]
        if (length(x.col) == 1) {  # x is 1 var
          data <- data.frame(data)
          names(data) <- x.name
         }
      }
      else {  # class of data is "list"
        data <- data.frame(data[[x.col]])
        names(data) <- x.name
      }
    }

    else { # x is in the global environment (vector or data frame)
      if (is.data.frame(x))  # x a data frame
        data <- x
      else {  # x a vector in global
        if (!is.function(x))
          data <- data.frame(x)  # x is 1 var
        else
          data <- data.frame(eval(substitute(data$x)))  # x is 1 var
        names(data) <- x.name
      }
    }

  }


# ---------------
# do the analysis

    plot.i <- 0  # keep track of generated graphics
    plot.title  <- character(length=0)
    manage.gr <- .graphman()  # see if graphics are to be managed
    if (manage.gr  &&  !pdf) {
      i.win <- 0
      for (i in 1:ncol(data)) {
        if (is.numeric(data[,i])  &&  !.is.num.cat(data[,i], n.cat)) 
          i.win <- i.win + 1
      }
      .graphwin(i.win, width, height)
    }
    open.win <- 2


  for (i in 1:ncol(data)) {

    nu <- length(unique(na.omit(data[,i])))

    x.name <- names(data)[i]
    options(xname = x.name)

    if (is.numeric(data[,i])) {
      # let 1 variable go through, even if num.cat
      if (ncol(data) == 1  ||  !.is.num.cat(data[,i], n.cat)) {

      if (pdf) {
        pdf.fnm <- paste("BoxPlot", "_", x.name, ".pdf", sep="") 
        .opendev(pdf.fnm, width, height)
      }
      else {
        pdf.fnm <- NULL
        plot.i <- plot.i + 1
        plot.title[plot.i] <- paste("BoxPlot of ", x.name, sep="")
        if (manage.gr) {
          open.win <- open.win + 1
          dev.set(which = open.win)
        }
      }

      stuff <- .bx.main(data[,i], fill, stroke, bg, grid,
         box, cex.axis, axes, rotate.values, offset, 
         horiz, add.points, xlab, main, sub, digits.d, quiet, fun.call, ...)
      txsts <- stuff$tx
      if (length(txsts)==0) txsts <- ""

      txotl <- ""
      if (!quiet) {
        txotl <- .outliers(data[,i])
        if (length(txotl)==0) txotl <- "No outliers"
      }

      if (ncol(data) > 1) {  # for a variable range, just the text output
        class(txsts) <- "out_piece"
        class(txotl) <- "out_piece"
        output <- list(out_stats=txsts, out_outliers=txotl)
        class(output) <- "out_all"
        print(output)
      }

      if (pdf) {
        dev.off()
        if (!quiet) .showfile(pdf.fnm, "Box Plot")
      }

    }  # nu > n.cat
    else
      if (!quiet) .ncat("Box Plot", x.name, nu, n.cat)

    }  # is.numeric(data[,i])
  }  # for

  if (ncol(data) > 1) {
    if (!pdf) if (is.null(options()$knitr.in.progress))
      .plotList(plot.i, plot.title)
  }

  dev.set(which=2)  # reset graphics window for standard R functions


  if (ncol(data)==1) {

    # R Markdown
    txkfl <- ""
    if (!is.null(Rmd)) {
      if (!grepl(".Rmd", Rmd)) Rmd <- paste(Rmd, ".Rmd", sep="")
      txknt <- .dist.Rmd(x.name, df.name, fun.call, digits.d)
      cat(txknt, file=Rmd, sep="\n")
      txkfl <- .showfile2(Rmd, "R Markdown instructions")
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


