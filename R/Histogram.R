Histogram <-
function(x=NULL, data=mydata, rows=NULL,
         n.cat=getOption("n.cat"), Rmd=NULL,

    by1=NULL, by2=NULL,
    n.row=NULL, n.col=NULL, aspect="fill",

    fill=getOption("bar.fill.ordered"),
    color=getOption("bar.color.ordered"),
    trans=getOption("trans.bar.fill"),

    bin.start=NULL, bin.width=NULL, bin.end=NULL, breaks="Sturges",

    prop=FALSE, values=FALSE,
    reg="snow2", cumul=c("off", "on", "both"),

    xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
    lab.adj=c(0,0), margin.adj=c(0,0,0,0),


    rotate.x=getOption("rotate.x"), rotate.y=getOption("rotate.y"),
    offset=getOption("offset"),
    scale.x=NULL, scale.y=NULL,

    add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,

    eval.df=NULL, digits.d=NULL, quiet=getOption("quiet"), do.plot=TRUE,
    width=6, height=6, pdf=FALSE, 
    fun.call=NULL, ...) {


  if (is.null(fun.call)) fun.call <- match.call()

  # limit actual argument to alternatives, perhaps abbreviated
  cumul <- match.arg(cumul)

  if (missing(fill))
    fill <- ifelse (is.null(getOption("bar.fill.ordered")), 
      getOption("bar.fill"), getOption("bar.fill.ordered"))

  if (!is.null(scale.x)) if (length(scale.x) != 3)  {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Starting value, ending value, and number of intervals\n",
      "  must all be specified as a vector, e.g., scale.x=c(0, 9 , 5)\n\n")
  }

  if (!is.null(scale.y)) if (length(scale.y) != 3)  {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Starting value, ending value, and number of intervals\n",
      "  must all be specified as a vector, e.g., scale.y=c(0, 9 , 5)\n\n")
  }

  panel.fill <- getOption("panel.fill")
  panel.color <- getOption("panel.color")
  grid.color <- getOption("grid.color")
  lab.color <- getOption("lab.color")
  lab.cex <- getOption("lab.cex")
  axis.cex <- getOption("axis.cex") 

  fill[which(fill == "off")] <- "transparent"
  color[which(color == "off")] <- "transparent"

  Trellis <- ifelse(!missing(by1), TRUE, FALSE)
  
  xlab.adj <- lab.adj[1];   ylab.adj <- lab.adj[2]
  tm.adj <- margin.adj[1];  rm.adj <- margin.adj[2]
  bm.adj <- margin.adj[3];  lm.adj <- margin.adj[4]

  .param.old(...)

  shiny <- ifelse (isNamespaceLoaded("shiny"), TRUE, FALSE) 
  if (is.null(eval.df))  # default values
    eval.df <- ifelse (shiny, FALSE, TRUE)

  # get actual variable name before potential call of data$x
  if (!missing(x))  # can't do is.null or anything else with x until evaluated
    x.name <- deparse(substitute(x))  # could be a list of var names
  else
    x.name <- NULL  # otherwise is actually set to "NULL" if NULL
    options(xname = x.name)

  if ((missing(data) && shiny))  # force eval (not lazy) if data not specified
    data <- eval(substitute(data), envir=parent.frame())
  df.name <- deparse(substitute(data))  # get name of data table
  options(dname = df.name)

  if (exists(df.name, where=.GlobalEnv))  # tibble to df
    if (class(data)[1] == "tbl_df")
      data <- as.data.frame(data, stringsAsFactors=FALSE)

  if (!is.null(x.name))
    x.in.global <- .in.global(x.name)  # see if in global, includes vars list
  else
    x.in.global <- FALSE
    
# -----------------------------------------------------------
# establish if a data frame, if not then identify variable(s)
# x can be missing entirely, with a data frame passed instead
# if x a vector, then x.name not in data, but also not in global

  if (!missing(x)) {

    # x not in global env, in df, specify data= forces to data frame
    if (!x.in.global) {
      if (eval.df) {
        .nodf(df.name)  # check to see if data frame container exists 
        .xcheck(x.name, df.name, names(data))  # x-vars in df?
      }
      data.vars <- as.list(seq_along(data))
      names(data.vars) <- names(data)
      ind <- eval(substitute(x), envir=data.vars)  # col num of each var     
      if (!missing(rows)) {  # subset rows
        r <- eval(substitute(rows), envir=data, enclos=parent.frame())
        r <- r & !is.na(r)  # set missing for a row to FALSE
        data <- data[r,,drop=FALSE]
      }
      if (!("list" %in% class(data))) {
        data.x <- data[, ind]
        if (length(ind) == 1) {  # x is 1 var
          if (!is.numeric(data.x)) { 
            cat("\n"); stop(call.=FALSE, "\n","------\n",
              "A histogram is only computed from a numeric variable\n",
              "To tabulate the values of a categorical variable:\n\n",
              "  Plot(", x.name, ", topic=\"count\")\n",
              "or\n",
              "  BarChart(", x.name, ")\n\n", sep="")
          }
          data.x <- data.frame(data.x)
          names(data.x) <- x.name
        }
      }
      else {  # class of data is "list"
        data.x <- data.frame(data[[ind]])
        names(data.x) <- x.name
      }
    }  # x not in global

    else { # x is in the global environment (vector or data frame)
      if (is.data.frame(x))  # x a data frame
        data.x <- x
      else {  # x a vector in global
        .xstatus(x.name, df.name, quiet)
        if (!is.function(x))
          data.x <- data.frame(x)  # x is 1 var
        else
          data.x <- data.frame(eval(substitute(data$x)))  # x is 1 var
        names(data.x) <- x.name
      }
    }  # x is in global
  }
  
  # evaluate by1
  #-------------
  if (!missing(by1)) {

    # get actual variable name before potential call of data$x
    by1.name <- deparse(substitute(by1))
    options(by1name = by1.name)

    # get conditions and check for data existing
    xs <- .xstatus(by1.name, df.name, quiet)
    in.global <- xs$ig

    # see if var exists in df, if x not in global Env or function call
    if (!missing(x) && !in.global)
      .xcheck(by1.name, df.name, names(data))

    if (!in.global)
      by1.call <- eval(substitute(data$by1))
    else {  # vars that are function names get assigned to global
      by1.call <- by1
      if (is.function(by1.call)) by1.call <- eval(substitute(data$by1))
    }

    if (!is.factor(by1.call)) by1.call <- factor(by1.call)
  }

  else
    by1.call <- NULL


  # evaluate by2
  #-------------
  if (!missing(by2)) {

    # get actual variable name before potential call of data$x
    by2.name <- deparse(substitute(by2))
    options(by2name = by2.name)

    # get conditions and check for data existing
    xs <- .xstatus(by2.name, df.name, quiet)
    in.global <- xs$ig

    # var in data frame? if x not in global Env or function call
    if (!missing(x) && !in.global)
      .xcheck(by2.name, df.name, names(data))

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

    .bar.lattice(data.x[,1], by1.call, by2.call, n.row, n.col, aspect, prop,
                 fill, color, trans, size.pt=NULL,
                 xlab, ylab, main,
                 rotate.x, offset,
                 width, height, pdf, segments.x=NULL, breaks, c.type="hist")
  }

  else {

    if (!missing(x)) data <- data.x

    # set up graphics
    manage.gr <- .graphman()  # manage graphics?
    if (manage.gr && !shiny) {
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
          ssstuff <- .ss.numeric(data[,i], digits.d=digits.d,
            brief=TRUE)
          txss <- ssstuff$tx
        }

        # nothing returned if quiet=TRUE

        stuff <- .hst.main(data[,i], fill, color, trans, reg,
            rotate.x, rotate.y, offset,
            breaks, bin.start, bin.width,
            bin.end, prop, values, cumul, xlab, ylab, main, sub, 
            xlab.adj, ylab.adj, bm.adj, lm.adj, tm.adj, rm.adj,
            add, x1, x2, y1, y2,
            scale.x, scale.y,
            quiet, do.plot, fun.call=fun.call, ...)

        txsug <- stuff$txsug
        if (is.null(txsug)) txsug <- ""
        txdst <- stuff$ttx
        if (is.null(txdst)) txdst <- ""

        txotl <- ""
        if (!quiet) {
          txotl <- .bx.stats(data[,i])$txotl
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

    if (!shiny)
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

