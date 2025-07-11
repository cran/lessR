Histogram <-
function(x=NULL, data=d, filter=NULL,
    stat=c("count", "proportion", "density"),

    facet1=NULL, facet2=NULL,
    n_row=NULL, n_col=NULL, aspect="fill",

    theme=getOption("theme"),
    fill=getOption("bar_fill_cont"),
    color=getOption("bar_color_cont"),
    transparency=getOption("trans_bar_fill"),

    values=FALSE,

    bin_start=NULL, bin_width=NULL, bin_end=NULL, breaks="Sturges",

    cumulate=c("off", "on", "both"), reg="snow2",

    show_histogram=TRUE,
    bandwidth=NULL, type=c("general", "normal", "both"),
    fill_general=NULL, fill_normal=NULL, fill_hist=getOption("se_fill"),
    color_general="gray20", color_normal="gray20", line_width=NULL,
    x_pt=NULL, y_axis=FALSE,
    rug=FALSE, color_rug="black", size_rug=0.5,

    xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
    lab_adjust=c(0,0), margin_adjust=c(0,0,0,0),

    rotate_x=getOption("rotate_x"), rotate_y=getOption("rotate_y"),
    offset=getOption("offset"),
    scale_x=NULL,
    axis_fmt=c("K", ",", ".", ""), axis_x_pre="", axis_y_pre="",

    add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,

    quiet=getOption("quiet"), do_plot=TRUE,
    pdf_file=NULL, width=6.5, height=6,
    digits_d=NULL,
    Rmd=NULL,

    n_cat=getOption("n_cat"),
    rows=NULL, by1=NULL, by2=NULL,

    eval_df=NULL, fun_call=NULL, ...) {


  # limit actual argument to alternatives, perhaps abbreviated
  cumulate <- match.arg(cumulate)
  type <- match.arg(type)
  stat <- match.arg(stat)
  if (is.null(fun_call)) fun_call <- match.call()
  if (nzchar(axis_fmt[1])) axis_fmt <- match.arg(axis_fmt)

  proportion <- ifelse (stat == "proportion", TRUE, FALSE)   # old signal
  density <- ifelse (stat == "density", TRUE, FALSE)
  histogram <- ifelse (density, FALSE, TRUE)
  trans <- transparency

  if (rug) density <- TRUE

  if (theme != getOption("theme")) {
    sty <- style(theme, reset=FALSE)
    fill <- sty$bar$bar_fill_cont
    color <- sty$bar$color_ordered
    trans <- sty$bar$trans_fill
  }

  if (missing(fill))
    fill <- ifelse (is.null(getOption("bar_fill_cont")),
      getOption("bar_fill"), getOption("bar_fill_cont"))
  breaks.miss <- ifelse (missing(breaks), TRUE, FALSE)
  bw.miss <- ifelse (missing(bandwidth), TRUE, FALSE)


  # ------------ Old Stuff ----------------------------------

  # a dot in a parameter name to an underscore and more
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (names(dots)[i] == "dn.hist") show_histogram <- dots[[i]]
      if (names(dots)[i] == "fill_gen") fill_general <- dots[[i]]
      if (names(dots)[i] == "fill_nrm") fill_normal <- dots[[i]]
      if (names(dots)[i] == "color_gen") color_general <- dots[[i]]
      if (names(dots)[i] == "color_nrm") color_normal <- dots[[i]]
      if (names(dots)[i] == "bw") bandwidth <- dots[[i]]
      if (names(dots)[i] == "density") {
        cat("\n"); stop(call.=FALSE, "\n------\n",
          "Now enter:  stat=\"density\"\n\n")
      } 
      if (length(grep(".", names(dots)[i], fixed=TRUE)) > 0) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }

  facet1 <- .newparam(missing(by1), substitute(by1), "by1",
                      missing(facet1), substitute(facet1), "facet1")
  if (!is.null(facet1)) data[[as.character(facet1)]]  # extract facet1 from data

  facet2 <- .newparam(missing(by2), substitute(by2), "by2",
                      missing(facet2), substitute(facet2), "facet2")
  if (!is.null(facet2)) data[[as.character(facet2)]]  # extract facet2 from data

  if (!missing(rows))
    message(">>> Parameter  rows  renamed to:  filter.\n",
            "    Change to  filter,  rows  will stop working in the future.\n")

  if (!missing(n_cat)) {
    message(">>> Parameter  n_cat  will no longer work in the future.\n",
             "    Better to convert a categorical integer variable to ",
             "a factor.\n")
  }
  # ---------------------------------------------------------

  facet1.miss <- ifelse (is.null(facet1), TRUE, FALSE)
  facet2.miss <- ifelse (is.null(facet2), TRUE, FALSE)
  Trellis <- ifelse(!facet1.miss, TRUE, FALSE)

  if (!breaks.miss && density)  {
    cat("\n"); stop(call.=FALSE, "\n------\n",
      "When plotting density, parameter  breaks  is ignored.\n",
      "Bins must be equal width, but can use bin_start and bin_width.\n\n")
  }

  if (density &&  !is.null(facet1)) {
    cat("\n"); stop(call.=FALSE, "\n------\n",
      "Facets not yet working with density visualizations.\n\n")
  }

  fill[which(fill == "off")] <- "transparent"
  color[which(color == "off")] <- "transparent"

  xlab_adj <- lab_adjust[1];   ylab_adj <- lab_adjust[2]
  tm.adj <- margin_adjust[1];  rm.adj <- margin_adjust[2]
  bm.adj <- margin_adjust[3];  lm.adj <- margin_adjust[4]

  .param.old(...)


  # --------- data frame stuff

  data.miss <- ifelse (missing(data), TRUE, FALSE)

  # let deprecated mydata work as default
  dfs <- .getdfs()
  mydata.ok <- FALSE
  if (!is.null(dfs)) {
    if ("mydata" %in% dfs  &&  !("d" %in% dfs)) {
      d <- mydata
      rm(mydata)
      df.name <- "mydata"
      mydata.ok <- TRUE
      options(dname = df.name)
    }
  }

  # get name of data table
  if (!mydata.ok) {
    df.name <- deparse(substitute(data))
    options(dname = df.name)
  }

  shiny <- FALSE
  if (!is.null(sys.call(-1)))  # is NULL when called directly from R console
    if (sys.call(-1) == "renderPlot()") {  # from shiny, user or interact()
      shiny <- TRUE
      data <- eval(substitute(data), envir=parent.frame())
    }

  # if a tibble, convert to data frame
  if (!shiny) {
    if (exists(df.name, envir=.GlobalEnv)) {
      if (any(grepl("tbl", class(data), fixed=TRUE)))
        data <- data.frame(data)
    }
  }
  else  # no check for existence of df.name
    if (any(grepl("tbl", class(data), fixed=TRUE)))
      data <- data.frame(data)

  x.name <- deparse(substitute(x), width.cutoff = 120L)
  options(xname = x.name)

  if (!is.null(x.name))
    x.in.global <- .in.global(x.name, quiet)  # in global?, includes vars list
  else
    x.in.global <- FALSE

  if (!x.in.global)  {
    if (df.name != "NULL") {  # if NULL, force global (shiny, from interact() )
      # force evaluation (not lazy) if data not specified, relies on default d
      if (data.miss) {
        if (!mydata.ok) .nodf(df.name)  # check to see if df exists
#       data <- eval(substitute(data), envir=parent.frame())
        # the 1.201 comes from Shiny, need to reset
        # l.cex and l.axc are set in interact() before shiny run
        if (getOption("lab_cex") == 1.201) {
         if (getOption("l.cex") != 1.201) {
            style(lab_cex=getOption("l.cex"))
            style(axis_cex=getOption("l.axc"))
          }
          else
            style()
        }
      }
    }
    else # df.name is NULL
      x.in.global <- TRUE
  }

  eval_df <- !x.in.global


# subset filter (with deprecated rows parameter) --------------------------

  if (!missing(filter) || !missing(rows)) {

    if (x.in.global) {
      cat("\n"); stop(call.=FALSE, "\n------\n",
        "Parameter  filter  not applicable if no data frame\n\n")
    }

    txt <- .filter(deparse(substitute(filter)))

    # get r, label each row as TRUE or FALSE
    intYN <- try(eval(parse(text = txt)), silent = TRUE)
    if (is.numeric(intYN)) {
      r <- rep(FALSE, nrow(data))
      r[intYN] <- TRUE
    }
    else {
      if (!missing(filter))  # subset filter
        r <- eval(str2expression(txt), envir=data, enclos=parent.frame())
      if (!missing(rows))  # tag each row as TRUE or FALSE
        R <- eval(substitute(rows), envir=data, enclos=parent.frame())
      r <- r & !is.na(r)  # set missing for a row to FALSE
    }

    nr.before <- nrow(data)
    if (any(r))
      data <- data[r,,drop=FALSE]
    if (!quiet) {
      if (!missing(filter))  # filter parameter present
        cat("\nfilter: ",  txt, "\n-----\n")
      cat("Rows of data before filtering: ", nr.before, "\n")
      cat("Rows of data after filtering:  ", nrow(data), "\n\n")
    }
  }  # end filter


  # -----------------------------------------------------------
  # establish if a data frame, if not then identify variable(s)
  # x can be missing entirely, with a data frame passed instead
  # if x a vector, then x.name not in data, but also not in global

  if (!missing(x)) {

    # x not in global env, in df, specify data= forces to data frame
    if (!x.in.global) {
      if (eval_df) {
        if (!mydata.ok) .nodf(df.name)  # does data frame container exist?
        .xcheck(x.name, df.name, names(data))  # x-vars in df?
      }
      data.vars <- as.list(seq_along(data))
      names(data.vars) <- names(data)

      ind <- eval(substitute(x), envir=data.vars)  # col num of each var
      if (!("list" %in% class(data))) {
        data.x <- data[, ind]
        if (length(ind) == 1) {  # x is 1 var
          if (!is.numeric(data.x)) {
            cat("\n"); stop(call.=FALSE, "\n------\n",
              "A histogram is only computed from a numeric variable\n",
              "To tabulate the values of a categorical variable:\n\n",
              "  Plot(", x.name, ", stat=\"count\")\n",
              "or\n",
              "  BarChart(", x.name, ")\n\n", sep="")
          }
          data.x <- data.frame(data.x, stringsAsFactors=TRUE)
          names(data.x) <- x.name
        }
      }
      else {  # class of data is "list"
        data.x <- data.frame(data[[ind]], stringsAsFactors=TRUE)
        names(data.x) <- x.name
      }
    }  # end x not in global

    # x is in the global environment (vector or data frame)
    else {
      if (is.data.frame(x))  # x a data frame
        data.x <- x
      else {  # x a vector in global
        .in.global(x.name, quiet)  # x.name is expression?
        if (!is.function(x))
          data.x <- data.frame(x, stringsAsFactors=TRUE)  # x is 1 var
        else
          data.x <- data.frame(eval(substitute(data$x)), stringsAsFactors=TRUE)
        names(data.x) <- x.name
      }
    }  # x is in global
  }

  # evaluate facet1
  #-------------
  if (!facet1.miss) {

    # get actual variable name before potential call of data$x
    facet1.name <- deparse(substitute(facet1))
    options(facet1name = facet1.name)

    # get conditions and check for data existing
    in.global <- .in.global(facet1.name, quiet)

    # see if var exists in df, if x not in global Env or function call
    if (!missing(x) && !in.global)
      .xcheck(facet1.name, df.name, names(data))

    if (!in.global)
      facet1.call <- eval(substitute(data$facet1))
    else {  # vars that are function names get assigned to global
      facet1.call <- facet1
      if (is.function(facet1.call)) facet1.call <- eval(substitute(data$facet1))
    }

    if (!is.factor(facet1.call)) facet1.call <- factor(facet1.call)
  }

  else
    facet1.call <- NULL

  # evaluate facet2
  #-------------
  if (!facet2.miss) {

    # get actual variable name before potential call of data$x
    facet2.name <- deparse(substitute(facet2))
    options(facet2name = facet2.name)

    # get conditions and check for data existing
    in.global <- .in.global(facet2.name, quiet)

    # var in data frame? if x not in global Env or function call
    if (!missing(x) && !in.global)
      .xcheck(facet2.name, df.name, names(data))

    if (!in.global)
      facet2.call <- eval(substitute(data$facet2))
    else {  # vars that are function names get assigned to global
      facet2.call <- facet2
      if (is.function(facet2.call)) facet2.call <- eval(substitute(data$facet2))
    }

    if (!is.factor(facet2.call)) facet2.call <- factor(facet2.call)
  }

  else
   facet2.call <- NULL


  # ---------------
  # do the analysis

  if (Trellis && do_plot) {

    .bar.lattice(data.x[,1], facet1.call, facet2.call, n_row, n_col, aspect,
           proportion, fill, color, trans, size.pt=NULL,
           xlab, ylab, main, rotate_x, offset,
           axis_fmt, axis_x_pre, axis_y_pre,
           width, height, pdf_file,
           segments_x=NULL, breaks, T.type="hist", quiet)
  }

  else {  # not Trellis

    if (!missing(x)) data <- data.x

    # set up graphics
    manage.gr <- .graphman()  # manage graphics?
    if (manage.gr) {
      i.win <- 0
      for (i in 1:ncol(data)) {
        if (is.numeric(data[,i])  &&  !.is.num.cat(data[,i], 0))
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

    for (i in 1:ncol(data)) {  # data only contains data to be analyzed
      nu <- length(unique(na.omit(data[,i])))

      x.name <- names(data)[i]
      options(xname = x.name)

      if (is.numeric(data[,i])) {
        # let 1 variable go through, even if num.cat
        if (ncol(data) == 1  ||  !.is.num.cat(data[,i], 0)) {

        if (!is.null(pdf_file)) {
          if (!grepl(".pdf", pdf_file))
            pdf_file <- paste(pdf_file, ".pdf", sep="")
          pdf(file=pdf_file, width=width, height=height, onefile=FALSE)
        }
        else {
          if (df.name != "NULL")  # not dev.new for shiny
              .opendev(pdf_file, width, height)
        }

        txss <- ""
        ssstuff <- .ss.numeric(data[,i], digits_d=digits_d, brief=TRUE)
        txss <- ssstuff$tx

        if (histogram) {

          # nothing returned if quiet=TRUE
          stuff <- .hst.main(data[,i], fill, color, trans, reg,
              rotate_x, rotate_y, offset,
              breaks, bin_start, bin_width,
              bin_end, proportion, values, cumulate, xlab, ylab, main, sub,
              xlab_adj, ylab_adj, bm.adj, lm.adj, tm.adj, rm.adj,
              add, x1, x2, y1, y2,
              scale_x, axis_fmt, axis_x_pre, axis_y_pre,
              quiet, do_plot, fun_call=fun_call, ...)

          txsug <- stuff$txsug
          if (is.null(txsug)) txsug <- ""
          txdst <- stuff$ttx
          if (is.null(txdst)) txdst <- ""

          txotl <- ""
            txotl <- .bx.stats(data[,i])$txotl
            if (txotl[1] == "") txotl <- "No (Box plot) outliers"

          if (ncol(data) > 1  &&  !quiet) { # for var range, print text output
            class(txss) <- "out"
            class(txdst) <- "out"
            class(txotl) <- "out"
            output <- list(out_ss=txss, out_freq=txdst, out_outliers=txotl)
            class(output) <- "out_all"
            if (!quiet) print(output)
          }
        } # end histogram

        else {  # density
          if (bw.miss) bandwidth <- .band.width(data[,i], ...)  # band width

          clr <- getOption("theme")  # color theme not used except monochrome

          if (!missing(color_rug)  ||  !missing(size_rug)) rug <- TRUE

          if (missing(fill_general)) {
              fill_general <- rgb(80,150,200, alpha=80, maxColorValue=255)
            if (clr == "gray" ||
               (getOption("theme") == "gray"  &&
                getOption("sub_theme") == "black")) {
              fill_general <- rgb(.75,.75,.75, .5)
            }
          }
          else {  # add some transparency to a named color
            if (fill_general %in% colors()) {
              fg.rgb <- col2rgb(fill_general)
              fill_general <- rgb(fg.rgb[1], fg.rgb[2], fg.rgb[3],
                                  alpha=80, maxColorValue=255)
            }
          }

          if (missing(fill_normal)) {
              fill_normal <- rgb(250,210,230, alpha=80, maxColorValue=255)
            if (clr == "gray" ||
               (getOption("theme") == "gray"  &&
                getOption("sub_theme") == "black")) {
              fill_normal <- ifelse (type=="normal",
                                     rgb(.75,.75,.75, .5), "transparent")
            }
          }
          else {  # add some transparency to a named color
            if (fill_normal %in% colors()) {
              fg.rgb <- col2rgb(fill_normal)
              fill_normal <- rgb(fg.rgb[1], fg.rgb[2], fg.rgb[3],
                                  alpha=80, maxColorValue=255)
            }
          }

          x.min <- NULL
          x.max <- NULL
          if (!is.null(scale_x)) {
            x.min <- scale_x[1]
            x.max <- scale_x[2]
          }

          stuff <- .dn.main(data[,i], bandwidth, type, show_histogram,
                bin_start, bin_width,
                fill_hist, color_normal, color_general,
                fill_normal, fill_general, line_width,
                rotate_x, rotate_y, offset,
                axis_fmt, axis_x_pre, axis_y_pre,
                x_pt, xlab, main, sub, y_axis, x.min, x.max,
                rug, color_rug, size_rug, quiet, fncl=fun_call, ...)


          txdst <- ""  # should be named txbw
          txotl <- ""
          txsug <- ""
            txdst <- stuff$tx

            txotl <- .bx.stats(data[,i])$txotl
            if (txotl[1] == "") txotl <- "No (Box plot) outliers"

            txsug <- stuff$txsug

            class(txdst) <- "out"
            class(txotl) <- "out"
            class(txsug) <- "out"
          gl <- .getlabels()
          x.name <- gl$xn; x.lbl <- gl$xl;
          y.name <- gl$yn; y.lbl <- gl$yl
          if (!quiet  &&  ncol(data) > 1) {
            ttlns <- .title2(x.name, y.name, x.lbl, y.lbl, TRUE)
            ttlns <- paste(" ", "\n", ttlns, sep="")
          }
          else
            ttlns <- ""

        }  # end density

        if (!is.null(pdf_file)) {
          dev.off()
          if (!quiet) .showfile(pdf_file, "Histogram")
        }

      }  # end ncol(data) == 1 ...

      else {
        if (ncol(data) > 1) {
          plot.i <- plot.i + 1
          plot.title[plot.i] <- paste("Histogram of ", x.name, sep="")
          if (manage.gr) {
            open.win <- open.win + 1
            dev.set(which = open.win)
          }
        }
        if (!quiet) .ncat("Histogram", x.name, nu, 0)
      }

      }  # is.numeric(data[,i])
    }  # end for i from 1 to ncol

    if (ncol(data) > 1) {
      options(suggest = sug)
      if (is.null(pdf_file)  &&  plot.i > 0)
        if (is.null(options()$knitr.in.progress))
          .plotList(plot.i, plot.title)
    }

    if (df.name != "NULL")  # not shiny
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

      if (histogram) {

        output <- list(type="Histogram",
          call=fun_call,
          out_suggest=txsug, out_ss=txss, out_outliers=txotl, out_freq=txdst,
          out_file=txkfl,
          bin_width=stuff$bin_width, n_bins=stuff$n.bins,
          breaks=stuff$breaks,
          mids=stuff$mids, counts=stuff$counts, prop=stuff$prop,
          cumulate=stuff$counts_cum, cprop=stuff$prop_cum)

        class(output) <- "out_all"
        if (!quiet) print(output)

        # names and order of components per documentation in Histogram.Rd
        stuff$out_outliers <- txotl  # after to class out for line breaks
        stuff$out_summary <- txss
        stuff$out_freq <- txdst
        names(stuff) <- c("out_suggest", "out_freq", "bin_width", "n_bins",
                "breaks", "mids", "counts", "prop", "cumulate", "cprop",
                "out_outliers", "out_summary"
                )
        stuff <- c(stuff[1], stuff[12], stuff[2], stuff[11], stuff[3], stuff[4],
                   stuff[5], stuff[6], stuff[7], stuff[8], stuff[9], stuff[10])
        return(invisible(stuff))
      }  # end histogram

      else {  # density
          output <- list(type="Density",
#           out_suggest=txsug, out_title=ttlns, out_stats=txdst,
            out_suggest=txsug, out_stats=txdst,
            out_ss=txss, out_outliers=txotl,
            out_file=txkfl,
            bw=stuff$bw, n=stuff$n, n_miss=stuff$n.miss)

          class(output) <- "out_all"

        if (!quiet) print(output)
        return(invisible(output))
      }  # end density

    }  # end ncol(data) == 1

  }  # else not Trellis

}
