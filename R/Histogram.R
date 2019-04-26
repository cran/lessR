Histogram <-
function(x=NULL, data=d, rows=NULL,
         stat_x=c("count", "proportion"),
         n_cat=getOption("n_cat"), Rmd=NULL,

    by1=NULL, by2=NULL,
    n_row=NULL, n_col=NULL, aspect="fill",

    bin_start=NULL, bin_width=NULL, bin_end=NULL, breaks="Sturges",

    theme=getOption("theme"),
    fill=getOption("bar_fill_ordered"),
    color=getOption("bar_color_ordered"),
    trans=getOption("trans_bar_fill"),

    values=FALSE,
    reg="snow2", cumulate=c("off", "on", "both"),

    xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
    lab_adj=c(0,0), margin_adj=c(0,0,0,0),

    rotate_x=getOption("rotate_x"), rotate_y=getOption("rotate_y"),
    offset=getOption("offset"),
    scale_x=NULL, scale_y=NULL,

    add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,

    eval_df=NULL, digits_d=NULL, quiet=getOption("quiet"), do_plot=TRUE,
    width=6, height=6, pdf=FALSE, 
    fun_call=NULL, ...) {


  # a dot in a parameter name to an underscore
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    change <- c("stat.x", "stat.y", "n.cat", "n.row", "n.col",
                "bin.start", "bin.width", "bin.end", "lab.adj", "margin.adj",
                "rotate.x", "rotate.y", "scale.x", "scale.y",  
                "digits.d", "fun.call", "do.plot")
    for (i in 1:length(dots)) {
      if (names(dots)[i] %in% change) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }

  # a dot in a parameter name to an underscore
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (length(grep(".", names(dots)[i], fixed=TRUE)) > 0) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }

  if (is.null(fun_call)) fun_call <- match.call()

  # limit actual argument to alternatives, perhaps abbreviated
  cumulate <- match.arg(cumulate)

  stat_x <- match.arg(stat_x)
  proportion <- ifelse (stat_x == "proportion", TRUE, FALSE)   # old signal

  # let deprecated mydata work as default
  dfs <- .getdfs() 
  mydata.ok <- FALSE
  if ("mydata" %in% dfs  &&  !("d" %in% dfs)) {
    d <- mydata 
    mydata.ok <- TRUE
  }

  if (theme != getOption("theme")) {
    sty <- style(theme, reset=FALSE)
    fill <- sty$bar$bar_fill_ordered
    color <- sty$bar$color_ordered
    trans <- sty$bar$trans_fill
  }

  if (missing(fill))
    fill <- ifelse (is.null(getOption("bar_fill_ordered")), 
      getOption("bar_fill"), getOption("bar_fill_ordered"))

  if (!is.null(scale_x)) if (length(scale_x) != 3)  {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Starting value, ending value, and number of intervals\n",
      "  must all be specified as a vector, e.g., scale_x=c(0, 9 , 5)\n\n")
  }

  if (!is.null(scale_y)) if (length(scale_y) != 3)  {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Starting value, ending value, and number of intervals\n",
      "  must all be specified as a vector, e.g., scale_y=c(0, 9 , 5)\n\n")
  }

  panel_fill <- getOption("panel_fill")
  panel_color <- getOption("panel_color")
  grid_color <- getOption("grid_color")
  lab_color <- getOption("lab_color")
  lab_cex <- getOption("lab_cex")
  axis_cex <- getOption("axis_cex") 

  fill[which(fill == "off")] <- "transparent"
  color[which(color == "off")] <- "transparent"

  Trellis <- ifelse(!missing(by1), TRUE, FALSE)
  
  xlab_adj <- lab_adj[1];   ylab_adj <- lab_adj[2]
  tm.adj <- margin_adj[1];  rm.adj <- margin_adj[2]
  bm.adj <- margin_adj[3];  lm.adj <- margin_adj[4]

  .param.old(...)

  shiny <- ifelse (isNamespaceLoaded("shiny"), TRUE, FALSE) 
  if (is.null(eval_df))  # default values
    eval_df <- ifelse (shiny, FALSE, TRUE)

  # get actual variable name before potential call of data$x
  if (!missing(x))  # can't do is.null or anything else with x until evaluated
    x.name <- deparse(substitute(x))  # could be a list of var names
  else
    x.name <- NULL  # otherwise is actually set to "NULL" if NULL
    options(xname = x.name)

  # let deprecated mydata work as default
  dfs <- .getdfs() 
  mydata.ok <- FALSE
  if ("mydata" %in% dfs  &&  !("d" %in% dfs)) {
    d <- mydata
    df.name <- "mydata"
    mydata.ok <- TRUE
    options(dname = df.name)
  }

  if (!mydata.ok) {
    df.name <- deparse(substitute(data))  # get name of data table
    options(dname = df.name)
  }
 
  # if a tibble convert to data frame
  if (df.name %in% ls(name=.GlobalEnv)) {  # tibble to df
   if (any(grepl("tbl", class(data), fixed=TRUE))) {
      data <- data.frame(data, stringsAsFactors=FALSE)
   }
  }

  if ((missing(data) && shiny))  # force evaluation (not lazy) if data not specified
    data <- eval(substitute(data), envir=parent.frame())

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
      if (eval_df) {
        if(!mydata.ok) .nodf(df.name)  # check to see if data frame container exists 
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
              "  Plot(", x.name, ", stat=\"count\")\n",
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

  if (Trellis && do_plot) {

    .bar.lattice(data.x[,1], by1.call, by2.call, n_row, n_col, aspect, 
                 proportion, fill, color, trans, size.pt=NULL,
                 xlab, ylab, main, rotate_x, offset,
                 width, height, pdf, segments_x=NULL, breaks, c.type="hist",
                 quiet)
  }

  else {

    if (!missing(x)) data <- data.x

    # set up graphics
    manage.gr <- .graphman()  # manage graphics?
    if (manage.gr && !shiny) {
      i.win <- 0
      for (i in 1:ncol(data)) {
        if (is.numeric(data[,i])  &&  !.is.num.cat(data[,i], n_cat)) 
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
        if (ncol(data) == 1  ||  !.is.num.cat(data[,i], n_cat)) {

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
        ssstuff <- .ss.numeric(data[,i], digits_d=digits_d,
          brief=TRUE)
        txss <- ssstuff$tx

        # nothing returned if quiet=TRUE

        stuff <- .hst.main(data[,i], fill, color, trans, reg,
            rotate_x, rotate_y, offset,
            breaks, bin_start, bin_width,
            bin_end, proportion, values, cumulate, xlab, ylab, main, sub, 
            xlab_adj, ylab_adj, bm.adj, lm.adj, tm.adj, rm.adj,
            add, x1, x2, y1, y2,
            scale_x, scale_y,
            quiet, do_plot, fun_call=fun_call, ...)

        txsug <- stuff$txsug
        if (is.null(txsug)) txsug <- ""
        txdst <- stuff$ttx
        if (is.null(txdst)) txdst <- ""

        txotl <- ""
          txotl <- .bx.stats(data[,i])$txotl
          if (txotl[1] == "") txotl <- "No (Box plot) outliers"

        if (ncol(data) > 1  &&  !quiet) {  # for var range, print text output
          class(txss) <- "out"
          class(txdst) <- "out"
          class(txotl) <- "out"
          output <- list(out_ss=txss, out_freq=txdst, out_outliers=txotl)
          class(output) <- "out_all"
          if (!quiet) print(output)
        }

        if (pdf) {
          dev.off()
          if (!quiet) .showfile(pdf.fnm, "Histogram")
        }

      }  # nu > n_cat
      else
        if (!quiet) .ncat("Histogram", x.name, nu, n_cat)

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
        txknt <- .dist.Rmd(x.name, df.name, fun_call, digits_d)
        cat(txknt, file=Rmd, sep="\n")
        txkfl <- .showfile2(Rmd, "R Markdown instructions")
      }
   
      class(txsug) <- "out"
      class(txss) <- "out"
      class(txdst) <- "out"
      class(txotl) <- "out"
      class(txkfl) <- "out"

      output <- list(type="Histogram",
        call=fun_call, 
        out_suggest=txsug, out_ss=txss, out_outliers=txotl, out_freq=txdst,
        out_file=txkfl,
        bin_width=stuff$bin_width, n.bins=stuff$n.bins,
        breaks=stuff$breaks,
        mids=stuff$mids, counts=stuff$counts, prop=stuff$prop,
        cumulate=stuff$counts_cum, cprop=stuff$prop_cum)

      class(output) <- "out_all"
      if (!quiet) print(output)

      # names and order of components per documentation in BarChart.Rd
      stuff$out_outliers <- txotl  # after to class out for line breaks
      stuff$out_summary <- txss
      stuff$out_freq <- txdst
      names(stuff) <- c("out_suggest", "out_freq", "bin_width", "n.bins",
              "breaks", "mids", "counts", "prop", "cumulate", "cprop",
              "out_summary", "out_outliers")
      stuff <- c(stuff[1], stuff[11], stuff[2], stuff[12], stuff[3], stuff[4],
                 stuff[5], stuff[6], stuff[7], stuff[8], stuff[9], stuff[10])
      invisible(stuff)


    }  # end ncol(data) == 1

  }  # else not Trellis

}

