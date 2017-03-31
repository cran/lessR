Histogram <-
function(x=NULL, data=mydata, n.cat=getOption("n.cat"), Rmd=NULL,

    by=NULL, by2=NULL,
    n.row=NULL, n.col=NULL, aspect="fill",

    fill=getOption("fill.bar"), 
    stroke=getOption("stroke.bar"),
    bg=getOption("bg"),
    grid=getOption("grid"),
    box=getOption("box"),
    trans=getOption("trans.fill.bar"),
    reg="snow2",

    over.grid=FALSE, cex.axis=0.75, axes="gray30",
    rotate.x=0, rotate.y=0, offset=0.5,

    bin.start=NULL, bin.width=NULL, bin.end=NULL, breaks="Sturges",

    prop=FALSE, cumul=c("off", "on", "both"), hist.counts=FALSE,
    digits.d=NULL, xlab=NULL, ylab=NULL, main=NULL, sub=NULL,

    quiet=getOption("quiet"), do.plot=TRUE,
    width=4.5, height=4.5, pdf=FALSE, 
    fun.call=NULL, ...) {


  if (is.null(fun.call)) fun.call <- match.call()

  # limit actual argument to alternatives, perhaps abbreviated
  cumul <- match.arg(cumul)

  df.name <- deparse(substitute(data))   # get name of data table
  options(dname = df.name)

  Trellis <- ifelse(!missing(by), TRUE, FALSE)

  if (!missing(trans)) fill <- .maketrans(fill, (1-trans)*256) 

  for (i in 1:length(fill))
    if (fill[i] == "off") fill[i] <- "transparent"
  for (i in 1:length(stroke))
    if (stroke[i] == "off") stroke[i] <- "transparent"
  if (bg == "off") bg <- "transparent"
  if (grid == "off" ) grid <- "transparent"
  if (box == "off") box <- "transparent"

  kf <- FALSE
  lbls <- FALSE
  dots <- list(...)  # check for deprecated parameters
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
      if (names(dots)[i] == "pdf.file") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "pdf.file  changed to  pdf, either TRUE or FALSE\n\n")
      }
      if (names(dots)[i] == "knitr.file") kf <- TRUE 
      if (names(dots)[i] == "labels") lbls <- TRUE 
    }
  }

  if (is.numeric(breaks) && !is.null(bin.start)) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Choose only one option to specify a start value.\n",
      "Either choose the option  breaks  or the option  bin.start.\n\n")
  }

  if (lbls) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "labels  has multiple definitions in R\n",
      "Instead use  hist.counts  to get the bar labels displayed\n\n")
  }

  if (kf) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "knitr.file  no longer used\n",
      "Instead use  Rmd  for R Markdown file\n\n")
  }
        
  if (is.numeric(breaks) && !is.null(bin.start)) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Choose only one option to specify a start value.\n",
      "Either choose the option  breaks  or the option  bin.start.\n\n")
  }

  # get actual variable name before potential call of data$x
  x.name <- deparse(substitute(x))  # could be a list of var names
  options(xname = x.name)


# -----------------------------------------------------------
# establish if a data frame, if not then identify variable(s)
# x can be missing entirely, with a data frame passed instead


  if (!missing(x)) {

    if (!exists(x.name, where=.GlobalEnv)) {  # x not in global env, in df
      .nodf(df.name)  # check to see if data frame container exists 
      .xcheck(x.name, df.name, data)  # var in df?, vars lists not checked
      all.vars <- as.list(seq_along(data))  # even if only a single var
      names(all.vars) <- names(data)  # all data in data frame
      x.col <- eval(substitute(x), envir=all.vars)  # col num selected vars
      if (!("list" %in% class(data))) {
        data.x <- data[, x.col]
        if (length(x.col) == 1) {  # x is 1 var
          if (!is.numeric(data.x)) { 
            cat("\n"); stop(call.=FALSE, "\n","------\n",
              "A histogram is only computed from a numeric variable\n",
              "For the frequencies of a categorical variable:\n\n",
              "  Plot(", x.name, ", topic=\"count\")\n",
              "or\n",
              "  BarChart(", x.name, ")\n\n", sep="")
          }
          data.x <- data.frame(data.x)
          names(data.x) <- x.name
        }
      }
      else {  # class of data is "list"
        data.x <- data.frame(data[[x.col]])
        names(data.x) <- x.name
      }
    }

    else { # x is in the global environment (vector or data frame)

      if (is.data.frame(x))  # x a data frame
        data.x <- x
      else {  # x a vector in global
        if (!is.function(x))
          data.x <- data.frame(x)  # x is 1 var
        else
          data.x <- data.frame(eval(substitute(data$x)))  # x is 1 var
        names(data.x) <- x.name
      }
    }

  }
  
  # evaluate by
  #------------
  if (!missing(by)) {

    # get actual variable name before potential call of data$x
    by.name <- deparse(substitute(by))
    options(byname = by.name)

    # get conditions and check for data existing
    xs <- .xstatus(by.name, df.name, quiet)
    in.global <- xs$ig

    # see if var exists in df, if x not in Global Env or function call
    if (!missing(x) && !in.global)
      .xcheck(by.name, df.name, data)

    if (!in.global)
      by.call <- eval(substitute(data$by))
    else {  # vars that are function names get assigned to global
      by.call <- by
      if (is.function(by.call)) by.call <- eval(substitute(data$by))
    }

    if (!is.factor(by.call)) by.call <- factor(by.call)
  }

  else
   by.call <- NULL


  # evaluate by2
  #-------------
  if (!missing(by2)) {

    # get actual variable name before potential call of data$x
    by2.name <- deparse(substitute(by2))
    options(by2name = by2.name)

    # get conditions and check for data existing
    xs <- .xstatus(by2.name, df.name, quiet)
    in.global <- xs$ig

    # var in data frame? if x not in Global Env or function call
    if (!missing(x) && !in.global)
      .xcheck(by2.name, df.name, data)

    if (!in.global)
      by2.call <- eval(substitute(data$by2))
    else {  # vars that are function names get assigned to global
      by2.call <- by2
      if (is.function(by2.call)) by2.call <- eval(substitute(data$by2))
    }

    if (!is.factor(by2.call)) by2.call <- factor(by2.call)
  }

  else
   by2.call <- NULL


# ---------------
# do the analysis

  if (Trellis && do.plot) {
    .hst.lattice(data.x[,1], by.call, by2.call, n.row, n.col, aspect,
                 fill, stroke, bg, grid, trans,
                 xlab, ylab, main, breaks, cex.axis, width, height, pdf)
  }

  else {

    if (!missing(x)) data <- data.x

    # set up graphics
    manage.gr <- .graphman()  # manage graphics?
    if (manage.gr) {
      i.win <- 0
      for (i in 1:ncol(data)) {
        if (is.numeric(data[,i])  &&  !.is.num.cat(data[,i], n.cat)) 
          i.win <- i.win + 1
      }
      .graphwin(i.win, d.w=width, d.h=height)
      open.win <- 2
    }

    plot.i <- 0  # keep track of generated graphics
    plot.title  <- character(length=0)

    # no suggestions if multiple variables
    if (ncol(data) > 1) {
      sug <- getOption("suggest")
      options(suggest = FALSE)
    }

    for (i in 1:ncol(data)) {

      nu <- length(unique(na.omit(data[,i])))

      x.name <- names(data)[i]
      options(xname = x.name)

      if (is.numeric(data[,i])) {
        # let 1 variable go through, even if num.cat
        if (ncol(data) == 1  ||  !.is.num.cat(data[,i], n.cat)) {

        if (pdf) {
          pdf.fnm <- paste("Hist", "_", x.name, ".pdf", sep="") 
          .opendev(pdf.fnm, width, height)
        }
        else {
          pdf.fnm <- NULL
          if (ncol(data) > 1) {
            plot.i <- plot.i + 1
            plot.title[plot.i] <- paste("Histogram of ", x.name, sep="")
            if (manage.gr) {
              open.win <- open.win + 1
              dev.set(which = open.win)
            }
          }
        }

        txss <- ""
        if (!quiet) {
          ssstuff <- .ss.numeric(data[,i], digits.d=digits.d, brief=TRUE)
          txss <- ssstuff$tx
        }

        # nothing returned if quiet=TRUE
        stuff <- .hst.main(data[,i], fill, stroke, bg,
            grid, box, reg,
            over.grid, cex.axis, axes, rotate.x, rotate.y, offset,
            breaks, bin.start, bin.width,
            bin.end, prop, hist.counts, cumul, xlab, ylab, main, sub,
            quiet, do.plot, fun.call=fun.call, ...)
        txsug <- stuff$txsug
        if (is.null(txsug)) txsug <- ""
        txdst <- stuff$ttx
        if (is.null(txdst)) txdst <- ""

        txotl <- ""
        if (!quiet) {
          txotl <- .outliers(data[,i])
          if (txotl[1] == "") txotl <- "No (Box plot) outliers"
        }

        if (ncol(data) > 1  &&  !quiet) {  # for var range, print text output
          class(txss) <- "out_piece"
          class(txdst) <- "out_piece"
          class(txotl) <- "out_piece"
          output <- list(out_ss=txss, out_freq=txdst, out_outliers=txotl)
          class(output) <- "out_all"
          print(output)
        }

        if (pdf) {
          dev.off()
          if (!quiet) .showfile(pdf.fnm, "Histogram")
        }

      }  # nu > n.cat
      else
        if (!quiet) .ncat("Histogram", x.name, nu, n.cat)

      }  # is.numeric(data[,i])
    }  # end for

    if (ncol(data) > 1) {
      options(suggest = sug)
      if (!pdf  &&  plot.i > 0) if (is.null(options()$knitr.in.progress))
        .plotList(plot.i, plot.title)
    }

    dev.set(which=2)  # reset graphics window for standard R functions


    if (ncol(data) == 1) {

      # R Markdown
      txkfl <- ""
      if (!is.null(Rmd)) {
        if (!grepl(".Rmd", Rmd)) Rmd <- paste(Rmd, ".Rmd", sep="")
        txknt <- .dist.Rmd(x.name, df.name, fun.call, digits.d)
        cat(txknt, file=Rmd, sep="\n")
        txkfl <- .showfile2(Rmd, "R Markdown instructions")
      }
   
      class(txsug) <- "out_piece"
      class(txss) <- "out_piece"
      class(txdst) <- "out_piece"
      class(txotl) <- "out_piece"
      class(txkfl) <- "out_piece"

      output <- list(type="Histogram",
        call=fun.call, 
        out_suggest=txsug, out_ss=txss, out_outliers=txotl, out_freq=txdst,
        out_file=txkfl,
        bin_width=stuff$bin.width, n_bins=stuff$n.bins,
        breaks=stuff$breaks,
        mids=stuff$mids, counts=stuff$counts, prop=stuff$prop,
        counts_cumul=stuff$counts_cum, prop_cumul=stuff$prop_cum)

      class(output) <- "out_all"

      return(output)

    }  # end ncol(data) == 1

  }  # else not Trellis

}
