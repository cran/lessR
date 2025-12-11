X <-
function(x=NULL, by=NULL, facet=NULL, data=d, filter=NULL,

    type = c("histogram", "freq_poly", "density", "scatter",
             "violin", "box", "strip", "bs", "vbs"), # violin, box, strip
    stat=c("count", "proportion", "density"),

    n_row=NULL, n_col=NULL, aspect="fill",  # for facets

    theme=getOption("theme"),
    fill=getOption("bar_fill_cont"),
    color=getOption("bar_color_cont"),
    transparency=getOption("trans_bar_fill"),

    counts=FALSE,
    bin_start=NULL, bin_width=NULL, bin_end=NULL, breaks="Sturges",

    cumulate=c("off", "on", "both"), reg="snow2",

    show_histogram=TRUE,
    bandwidth=NULL, kind=c("general", "normal", "both"),
    fill_normal=NULL, fill_hist=getOption("se_fill"),
    color_normal="gray20", line_width=NULL,
    x_pt=NULL, y_axis=FALSE,
    rug=FALSE, color_rug="black", size_rug=0.5,

    vbs_plot="vbs", vbs_ratio=0.9, bw=NULL, bw_iter=10,
    violin_fill=getOption("violin_fill"),
    box_fill=getOption("box_fill"),
    pt_size=NULL,
    vbs_pt_fill="black",
    vbs_mean=FALSE, fences=FALSE, n_min_pivot=1,
    k=1.5, box_adj=FALSE, a=-4, b=3,

    ID="row.name", ID_size=0.60,
    MD_cut=0, out_cut=0, out_shape="circle", out_size=1,

    xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
    lab_adjust=c(0,0), margin_adjust=c(0,0,0,0),

    rotate_x=getOption("rotate_x"), rotate_y=getOption("rotate_y"),
    offset=getOption("offset"),
    scale_x=NULL,
    axis_fmt=c("K", ",", ".", ""), axis_x_pre="", axis_y_pre="",

    add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,

    quiet=getOption("quiet"), do_plot=TRUE,
    use_plotly=getOption("lessR.use_plotly"),
    pdf_file=NULL, width=6.5, height=6, digits_d=NULL, Rmd=NULL,

    n_cat=getOption("n_cat"),
    rows=NULL, facet1=NULL, facet2=NULL,

    eval_df=NULL, fun_call=NULL, ...) {


  # limit actual argument to alternatives, perhaps abbreviated
  cumulate <- match.arg(cumulate)
  type <- match.arg(type)
  stat <- match.arg(stat)
  kind <- match.arg(kind)
  if (is.null(fun_call)) fun_call <- match.call(expand.dots=TRUE)
  if (nzchar(axis_fmt[1])) axis_fmt <- match.arg(axis_fmt)

  darkred <- "\033[38;5;88m"   # 256-color "dark red"
  reset   <- "\033[0m"

  if (stat == "density") {
    message("The new and future way to request a density plot\n",
            "  is with the type parameter: ",
            darkred, "type == \"density\"", reset, ".\n",
            "This setting is done for you now but in the future\n",
            "  the stat alternative for this setting will be dropped.\n\n")
    type <- "density"
  }

  proportion <- ifelse (stat == "proportion", TRUE, FALSE)   # old signal
  density <- ifelse (stat == "density", TRUE, FALSE)
  histogram <- ifelse (density, FALSE, TRUE)
  trans <- transparency

  fill.miss <- ifelse (missing(fill), TRUE, FALSE)
  box_fill.miss <- ifelse (missing(box_fill), TRUE, FALSE)
  violin_fill.miss <- ifelse (missing(violin_fill), TRUE, FALSE)
  trans.miss <- ifelse (missing(transparency), TRUE, FALSE)
  color.miss <- ifelse (missing(color), TRUE, FALSE)
  out_size.miss <- ifelse (missing(out_size), TRUE, FALSE)
  n_col.miss <- ifelse (missing(n_col), TRUE, FALSE)
  n_row.miss <- ifelse (missing(n_row), TRUE, FALSE)
  out_shape.miss <- ifelse (missing(out_shape), TRUE, FALSE)

  if (rug) density <- TRUE

  if (theme != getOption("theme")) {
    sty <- style(theme, reset=FALSE)
    fill <- sty$bar$bar_fill_cont
    color <- sty$bar$color_ordered
    trans <- sty$bar$trans_fill
    violin_fill <- sty$VBS$violin_fill
    box_fill <- sty$VBS$box_fill
    se_fill <- sty$se_fill
  }

  if (use_plotly && (type %in% c("histogram", "density"))) {
    txt <- "[Interactive plot from the Plotly R package (Sievert, 2020)]"
    cat(txt, "\n\n")
  }

  breaks.miss <- ifelse (missing(breaks), TRUE, FALSE)
  bw.miss <- ifelse (missing(bandwidth), TRUE, FALSE)


  # ------------ Old Stuff ----------------------------------

  # hard stop on deprecated args BEFORE they can partially match 'facet'
  if (!missing(facet1) || !missing(facet2)) {
    stop(call. = FALSE, "\n------\n",
         "The arguments 'facet1' and 'facet2' are replaced.\n",
         "Please use 'facet' instead.\n",
         "For two facets, specify:  facet = c(Var1, Var2)\n\n")
  }

  # a dot in a parameter name to an underscore and more
  dots <- list(...)

  if (!is.null(dots)) if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (names(dots)[i] == "dn.hist") show_histogram <- dots[[i]]
      if (names(dots)[i] == "fill_gen") fill <- dots[[i]]
      if (names(dots)[i] == "fill_nrm") fill_normal <- dots[[i]]
      if (names(dots)[i] == "color_nrm") color_normal <- dots[[i]]
      if (names(dots)[i] == "bw") bandwidth <- dots[[i]]
      if (names(dots)[i] == "density") {
        cat("\n"); stop(call.=FALSE, "\n------\n",
          "Now enter:  type=\"density\"\n\n")
      } 
      if (length(grep(".", names(dots)[i], fixed=TRUE)) > 0) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }

  # ---------------------------------------------------------


  if (!breaks.miss && density)  {
    cat("\n"); stop(call.=FALSE, "\n------\n",
      "When plotting density, parameter  breaks  is ignored.\n",
      "Bins must be equal width, but can use bin_start and bin_width.\n\n")
  }

  if (density &&  !is.null(facet)) {
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
  else  { # no check for existence of df.name
    if (any(grepl("tbl", class(data), fixed=TRUE)))
      data <- data.frame(data)
  }

  x.name <- deparse(substitute(x), width.cutoff = 120L)
  x.name <- paste(x.name, collapse = "")
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
              "  Chart(", x.name, ", stat=\"count\")\n\n", sep="")
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

    # evaluate by
    #------------
    # cannot directly evaluate is.null(by) if by is present as a variable
    # so instead process as below to either get by.call or it is NULL
    # can get by.name
    if (!missing(by)) {

      # get variable name before potential call of data$x
      by.name <- deparse(substitute(by))
      options(byname = by.name)
      # get conditions and check for data existing
      by.in.global <- ifelse (df.name!="NULL",
                              .in.global(by.name, quiet), TRUE)

      if (!by.in.global) {
        if (eval_df)
          .xcheck(by.name, df.name, names(data))
        by.call <- eval(substitute(data$by))
      }
      else {  # vars that are function names get assigned to global
        by.call <- by
        if (is.function(by.call)) by.call <- eval(substitute(data$by))
      }
      by.call <- factor(by.call)

      n.by <- length(unique(by.call))
    }  # end having by

    else {
      n.by <- 1  # i.e., just one group
      by.call <- NULL
      by.name <- NULL
    }


# --- resolve facet --------------------------------------------------
  facet.miss <- ifelse (missing(facet), TRUE, FALSE)

  # Initialize legacy internal vars
  facet1.call <- facet2.call <- NULL
  facet1.name <- facet2.name <- NULL
  facet1.miss <- facet2.miss <- TRUE

  if (!missing(facet)) {
    facet_expr <- substitute(facet)
    facet_vars <- all.vars(facet_expr)

    get_col <- function(nm) {
      in_global <- if (df.name != "NULL") .in.global(nm, quiet) else TRUE
      if (!in_global) {
        .xcheck(nm, df.name, names(data))
        data[[nm]]
      } else {
        get(nm, envir = parent.frame())
      }
    }

    # Resolve symbols to factors
    fac_list <- lapply(facet_vars, function(nm) {
      v <- get_col(nm)
      if (!is.factor(v)) v <- factor(v)
      droplevels(v)
    })

    # Map into legacy internal vars
    if (length(fac_list) >= 1L) {
      facet1.call <- fac_list[[1L]]
      facet1.name <- facet_vars[1L]
      facet1.miss <- FALSE
    }
    if (length(fac_list) >= 2L) {
      facet2.call <- fac_list[[2L]]
      facet2.name <- facet_vars[2L]
      facet2.miss <- FALSE
    }
    if (length(fac_list) > 2L) {
      message("`facet` has ", length(fac_list),
              " variables; using the first two: ",
              paste(facet_vars[1:2], collapse = ", "), ".")
    }
  }


  # evaluate ID  (for VBS)
  #------------
  get.ID <- FALSE
  if (!is.null(add)) if (add[1] == "labels") get.ID <- TRUE
  if (MD_cut>0 || out_cut>0) get.ID <- TRUE

  if (get.ID) {
    # ID.name is the actual var name if specified directly,
    #  or the name of the var that contains the var name
    ID.name <- deparse(substitute(ID))
    ID.name <- gsub("\"", "", ID.name)  # remove extra quotes if "row.name"
    if (ID.name == "row.name") ID.name <- "row.names"
    if (!x.in.global) {
  # if x is in a data frame, then in the function call it is a name
  # if x is in global, then in the function its name is a variable direct
  # ID.col is the actual var column if specified directly,
  #  or the var name if a variable that contains the name was entered
      if (ID.name != "row.names") {
        ID.col <- eval(substitute(ID), envir=data.vars, parent.frame())
        if (!is.numeric(ID.col)) {
          ID.col <- which(names(data) == ID.col)
          ID.name <- names(data)[ID.col]
        }
      .xcheck(ID.name, df.name, names(data))  # var exists in data frame?
      ID.call <- data[, ID.col]
      }  # end not row.names
      else  # ID is row.names
        ID.call <- row.names(data)
    } # end x.in.global

    else {  # in global
      ID.call <- eval(substitute(ID), parent.frame())
    }
  }  # end get.ID

  else  # no ID to get
    ID.call <- NULL


  # -----------------------------------------------------------
  # -----------  x, y, by, and facet variables established ----
  # -----------------------------------------------------------

  if (is.null(by.call) && type=="scatter")
    type <- "strip"  # scatter and strip only different if a by var

  Trellis <- ifelse(!facet.miss, TRUE, FALSE)

  if (Trellis && do_plot && type=="histogram") {
    .bar.lattice(data.x[,1], facet1.call, facet2.call, n_row, n_col, aspect,
           proportion, fill, color, trans, size.pt=NULL,
           xlab, ylab, main, rotate_x, offset,
           axis_fmt, axis_x_pre, axis_y_pre,
           width, height, pdf_file,
           segments_x=NULL, breaks, T.type="hist", quiet)
    
    return(invisible(NULL))
  }


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


# evaluate specified fill (NULL, numeric constant, or a variable) ---------
    if (!color.miss && !is.null(by.call)) {
      if (!.is.palette(color[1]))
        color  <- .align_vector(color, by.call)  # convert named color to legacy
    }
    if (!fill.miss) {
      fill.name <- deparse(substitute(fill))
      if (length(fill.name) == 1) {
        if (exists(df.name, where=.GlobalEnv))
          in.df <- ifelse (exists(fill.name, where=data), TRUE, FALSE)
        else
          in.df <- FALSE
      }
        else in.df <- FALSE
      # only works for y given, not tabulated
      if (in.df) {  # fill is a variable
      # need to aggregate cat var x and set fill.val to those limited values
      # currently, fill.val consists of all data values of variable fill
        fill.val <- eval(substitute(data$fill))  # fill is a variable in data
        fill <- .getColC(fill.val, fill_name=fill.name)
        if (sort != "0") {
          srt.dwn <- ifelse (sort == "-", TRUE, FALSE)
          fill <- fill[order(fill.val, decreasing=srt.dwn)]
        }
      }

      n.fill <- ifelse (missing(by), 12, n.by)  # 12 is an arbitrary hack
      if (.is.palette(fill[1])) {
        fill <- .color_range(fill, n.fill)
      }

      # evaluate getColors at the time of the function call
      # re-evaluate here by setting fill with the specified value of n
      lx.u <- length(unique(data[,i]))  # includes NA values
      if (substr(fill.name[1], 1, 9) == "getColors")
        fill <- .do_getColors(fill.name, lx.u)
    }  # end !fill.miss

    else {
      n.x <- length(unique(data[[x.name]]))
      if (is.null(by.call)) {
        if (theme == "colors")
          fill <- rgb(150,170,195, maxColorValue=255)
        else
          fill <- .color_range(.get_fill(theme), 1)
      }   
      else
        fill <- .color_range(.get_fill(theme), n.by)
    }

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

      txkfl <- ""
      if (!is.null(Rmd)) {
        if (!grepl(".Rmd", Rmd)) Rmd <- paste(Rmd, ".Rmd", sep="")
        txknt <- .dist.Rmd(x.name, df.name, fun_call, digits_d)
        cat(txknt, file=Rmd, sep="\n")
        txkfl <- .showfile2(Rmd, "R Markdown instructions")
      }  # end R Markdown

      txss <- ""
      ssstuff <- .ss.numeric(data[,i], digits_d=digits_d, brief=TRUE)
      txss <- ssstuff$tx


    # drop all rows with any missing data across x, y, by, and size -----------

    # 1) Build a temporary data frame that contains all relevant columns
      df <- data

      if (!is.null(by.call) && length(by.call) > 0L)
        df$..by <- by.call

      if (!is.null(facet1.call) && length(facet1.call) > 0L)
        df$..facet1 <- facet1.call

      if (!is.null(facet2.call) && length(facet2.call) > 0L)
        df$..facet2 <- facet2.call  # fixed name

      # 2) Compute complete cases, then delete rows with an NA
      cc <- stats::complete.cases(df)

      # 3) Delete rows with missing data
      if (!all(cc)) {
        data <- data[cc, , drop = FALSE]

        if (!is.null(by.call) && length(by.call) > 0L)
          by.call <- by.call[cc]

        if (!is.null(facet1.call) && length(facet1.call) > 0L)
          facet1.call <- facet1.call[cc]

        if (!is.null(facet2.call) && length(facet2.call) > 0L)
          facet2.call <- facet2.call[cc]
      }

      rm(df)


# descriptive statistics by group -----------------------------------------

    desc.stats <- ss.pivot(
      x = data[, i],
      by = by.call,
      data       = data,
      x.name     = x.name,
      by.name    = by.name,
      print_result = FALSE
    )

     
# histogram ---------------------------------------------------------------

    if (type == "histogram") {

      digits_d <- .max.dd(data[,i])
      # process plotly and by=1 Plots
      stuff <- .hst.main(data[,i], by.call, by.name, n.by,
          fill, color, trans, reg,
          rotate_x, rotate_y, offset,
          breaks, bin_start, bin_width,
          bin_end, proportion, counts, cumulate, xlab, ylab, main, sub,
          xlab_adj, ylab_adj, bm.adj, lm.adj, tm.adj, rm.adj,
          add, x1, x2, y1, y2,
          scale_x, axis_fmt, axis_x_pre, axis_y_pre, use_plotly,
          digits_d, quiet, do_plot, fun_call=fun_call, ...)

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

    if (n.by == 1) {  # stat output

      if (ncol(data) == 1) {
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

          if (.allow.interactive())
            .viewer_notice_once(plot_name = type, window_target = "Both")

          if (!is.null(pdf_file)) {
            dev.off()
            if (!quiet && df.name!="NULL") .showfile(pdf_file, "Histogram")
          }

          return(invisible(stuff))
        }  # end end 1 col data

     }  # n.by is 1
    } # end histogram


# density -----------------------------------------------------------------

    else if (type == "density") {
      if (bw.miss) bandwidth <- .band.width(data[,i], ...)  # band width

      clr <- getOption("theme")  # color theme not used except monochrome

      if (!missing(color_rug)  ||  !missing(size_rug)) rug <- TRUE

      if (missing(fill)) {
          fill <- rgb(80,150,200, alpha=80, maxColorValue=255)
        if (clr == "gray" ||
           (getOption("theme") == "gray"  &&
            getOption("sub_theme") == "black")) {
          fill <- rgb(.75,.75,.75, .5)
        }
      }
      else {  # add some transparency to a named color
        for (i.clr in 1:length(fill)) {
          if (fill[i] %in% colors()) {
            fg.rgb <- col2rgb(fill[i])
            fill[i] <- rgb(fg.rgb[1], fg.rgb[2], fg.rgb[3],
                                alpha=80, maxColorValue=255)
          }
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

      gl <- .getlabels()
      x.name <- gl$xn; x.lbl <- gl$xl;
      y.name <- gl$yn; y.lbl <- gl$yl

      # need this for dn.plotly even if not for Plots window
      stuff <- .dn.main(data[,i], by.call, by.name, n.by,
            bandwidth, kind, show_histogram, theme,
            bin_start, bin_width, fill.miss,
            fill_hist, color_normal, color,
            fill_normal, fill, line_width,
            rotate_x, rotate_y, offset,
            axis_fmt, axis_x_pre, axis_y_pre,
            x_pt, xlab, main, sub, y_axis, x.min, x.max,
            rug, color_rug, size_rug,
            use_plotly, quiet, fncl=fun_call, ...)

      if (n.by == 1) {  # stat output
        txdst <- stuff$tx  # should be named txbw

        txotl <- .bx.stats(data[,i])$txotl
        if (txotl[1] == "") txotl <- "No (Box plot) outliers"

        txsug <- stuff$txsug

        class(txdst) <- "out"
        class(txotl) <- "out"
        class(txsug) <- "out"

        if (!quiet  &&  ncol(data) > 1) {
          ttlns <- .title2(x.name, y.name, x.lbl, y.lbl, TRUE)
          ttlns <- paste(" ", "\n", ttlns, sep="")
        }
        else
          ttlns <- ""

          output <- list(type="Density",
            out_suggest=txsug, out_stats=txdst,
            out_ss=txss, out_outliers=txotl,
            out_file=txkfl,
            bw=stuff$bw, n=stuff$n, n_miss=stuff$n.miss)

          class(output) <- "out_all"
        if (!quiet) print(output)

        if (.allow.interactive())
          .viewer_notice_once(plot_name="density plot", window_target="Both")

        if (!is.null(pdf_file)) {
          dev.off()
          if (!quiet && df.name!="NULL") .showfile(pdf_file, "Density")
        }

        return(invisible(output))
      }  # end n.by is 1

    }  # end density


    if (type %in% c("histogram", "density")  &&  n.by > 1) { 

      if (n.by == 2) {
        if (.allow.interactive())
          .viewer_notice_once(plot_name = "density plot", window_target = "Both")

        cat(x.name, "by", by.name, "\n\n")
        print(desc.stats, row.names = FALSE)

        cat("\nFor inferential analysis of the mean difference:\n")
        message("> ttest(", x.name, " ~ ", by.name, ")\n")

        ## --- Build compact data for the t-test ---
        x_nm  <- x.name      # e.g. "Salary"
        by_nm <- by.name     # e.g. "Gender"

        x_vec  <- data[, i]  # numeric: Salary
        by_vec <- by.call    # grouping: Gender (already aligned)

        df_small <- data.frame(
          x  = x_vec,
          by = by_vec
        )
        names(df_small) <- c(x_nm, by_nm)

        # legacy check so do not run ttest if Histogram() alias with n.by=2
        calls <- sys.calls()
        top <- calls[[1L]]  # Outermost user call is the first one
        fun <- top[[1L]]
        if (identical(fun, quote(X)) || identical(fun, as.name("X"))) {

          ## Make it visible to ttest() as a named data table
          tmp_name <- ".lessR_ttest_df"
          assign(tmp_name, df_small, envir = parent.frame())

          ## Formula: Salary ~ Gender
          frm <- reformulate(by_nm, response = x_nm)

          tt_call <- bquote(
            lessR::ttest(
              .(frm),
              data       = .(as.name(tmp_name)),
              quiet      = TRUE,   # suppress text from ttest()
              graph      = TRUE,   # still draw the plot
              line_chart = FALSE
            )
          )

          # Evaluate in the caller’s env so .xcheck() sees the data table
          res <- eval(tt_call, envir = parent.frame())

          rm(list = tmp_name, envir = parent.frame())
        }
        else
          res <- NULL
      }  # end n.by == 2

      else {  # n.by is 3 or more
        if (.allow.interactive())
          .viewer_notice_once(plot_name="histogram", window_target="Viewer")
        cat(x.name, "by", by.name, "\n\n")
        print(desc.stats, row.names = FALSE)
        cat("\n")
      }

      if (!is.null(pdf_file)) {
        dev.off()
        show <- ifelse (type == "histogram", "Histogram", "Density")
        if (!quiet && df.name!="NULL") .showfile(pdf_file, show)
      }

      return(invisible(NULL))
    }  # end histogram or density and n.by > 1

    ## --------------------------------------------------
    ## X(..., by= , type="scatter") -> delegate to XY()
    ## Numeric x, single categorical by: profile / dotplot
    ## --------------------------------------------------
    else if (type == "scatter") {

      # Only run this when user actually called X(), not an internal helper
      calls <- sys.calls()
      top   <- calls[[1L]]
      fun   <- top[[1L]]
      if (identical(fun, quote(X)) || identical(fun, as.name("X"))) {

        # Reconstruct original user call:
        #    X(Salary, by = Gender, type="scatter", ...)
        fun_call <- match.call(expand.dots = TRUE)

        # Original x expression is the second argument
        x_expr <- fun_call[[2L]]

        # Get the by expression: prefer named 'by'
        # fall back to second positional arg
        by_expr <- fun_call$by
        if (is.null(by_expr) && length(fun_call) >= 3L) {
          # X(Salary, Gender, type="scatter") style
          by_expr <- fun_call[[3L]]
        }

        if (is.null(by_expr)) {
          stop("For X(..., type=\"scatter\"),\n",
               "Supply a grouping variable via the second argument or by=.")
        }

        # We want XY(x = by, y = x, ...):
        #   conceptual numeric-by-category profile
        fun_call[[1L]] <- as.name("XY")
        fun_call$x     <- by_expr
        fun_call$y     <- x_expr
        fun_call$by    <- NULL  # XY() doesn't need by= here; grouping is now x=

        # Let XY() do all the usual myData/data= lookup, plotting, etc.
        return(eval.parent(fun_call))
      }
    }


# VBS ---------------------------------------------------------------------

    else if (type %in% c("vbs", "bs", "violin", "box", "strip")) {

        lx <- length(data[,1])
        iter.details <- ifelse (missing(bw_iter), FALSE, TRUE)
        n.ux <- length(unique(data[,1]))
        by.miss <- ifelse (missing(by), TRUE, FALSE)  # interact sets
      txt <- ifelse (!is.null(facet1.call),
                     "[Trellis (facet)", "[Violin/Box/Scatterplot")
      if (!quiet)
        cat(paste(txt, "graphics from Deepayan Sarkar's lattice package]\n\n"))

      # total number of facets
      n.facet1 <- length(levels(facet1.call))
      n.facet2 <- ifelse (facet2.miss, 1, length(levels(facet2.call)))
      n.lvl <- n.facet1 * n.facet2
      ord.by.call <- is.ordered(facet1.call)

      # if facets, vary box fill, less intense for 1 panel
      if (box_fill.miss  &&  vbs_plot == "b"  &&  n.lvl == 0)
          box_fill <- getOption("box_fill")
      else if (grepl("b", vbs_plot))
          box_fill <- .plt.fill(box_fill, box_fill.miss, ord.by.call,
                                n.facet1, n.lvl, theme)

      if (violin_fill.miss  &&  vbs_plot == "v") {
        if (n.lvl == 0) {
          if (theme != "gray")
            violin_fill <- getOption("box_fill")
          else
            violin_fill <- "gray75"
        }
        else {
          if (theme != "gray")
            violin_fill <- .plt.fill(box_fill, box_fill.miss, ord.by.call,
                                  n.facet1, n.lvl, theme)
          else
            for (i in seq_len(n.lvl)) violin_fill[i] <- "gray75"
        }
      }
      else {
        if (!violin_fill.miss)  # otherwise specified value
          violin_fill <- .plt.fill(violin_fill, violin_fill.miss, ord.by.call,
                                   n.facet1, n.lvl, theme)
        else
          for (i in seq_len(n.lvl)) violin_fill[i] <- violin_fill[1]
      }

        if (n.by > 1) {
          if (color.miss) {
            if (getOption("theme") %in% c("gray", "white"))
              pt.color <- getColors("grays", n=n.by)
            else
              pt.color <- getColors("hues", n=n.by, l=40)
          }
          else
            pt.color <- color
          for (i in 1:n.by) {
            if (fill.miss) {
              if (getOption("theme") %in% c("gray", "white"))
                pt.fill <- getColors("grays", n=n.by)
              else
                pt.fill <- getColors("hues", n=n.by, l=40)
            }
            else
              pt.fill <- fill
            pt.fill[i] <- .maketrans(pt.fill[i], (1-trans)*256)
            if (box_fill.miss) box_fill[i] <- .maketrans(box_fill[i], 0.6*256)
          }
        }  # n.by > 1
        else {  # n.by is 0 or 1
          if ("black" %in% vbs_pt_fill) {
            if (fill.miss) pt.fill <- "black"
            if (trans.miss) pt.trans <- 0.1
            if (color.miss) pt.color <- "black"
          }
          if ("default" %in% vbs_pt_fill) {
            if (fill.miss) pt.fill <- "black"
            if (trans.miss) pt.trans <- getOption("trans_pt_fill")
            if (color.miss) pt.color <- "black"
          }
          else {
            if (fill.miss) pt.fill <- vbs_pt_fill
            if (trans.miss) pt.trans <- getOption("trans_pt_fill")
            if (color.miss) pt.color <- getOption("pt_color")
          }
        }  # end n.by is 0 or 1

        # grayscale
        if (theme %in% c("gray", "white"))
          if (any(pt.size > 0.4)) if (out_shape.miss) out_shape <- 23
        if (getOption("sub_theme") == "black") {
          if (fill.miss) pt.fill <- "black"
          if (color.miss) pt.color <- "black"
        }

        # shiny kludge, where jitter_y is set at 0.01 to allow for set to 0
        # the issue with shiny is that parameter values are never missing
        # j.y.miss NOT USED
        # if (df.name == "NULL"  &&  jitter_y == 0.01) j.y.miss <- TRUE

        vbs_plot <- tolower(vbs_plot)
        violin <- ifelse (type %in% c("violin", "vbs"), TRUE, FALSE)
        box <- ifelse (type %in% c("box", "bs", "vbs"), TRUE, FALSE)

        if (df.name == "NULL"  &&  out_size == 1) out_size.miss <- TRUE
        k.iqr <- k   # k is a function name, so do not use internally

        # vbs_plot should be replaced in .param.VBS with violin, box, and strip
        if (type %in% c("bs", "vbs"))
          vbs_plot <- type
        else if (type == "violin") {
          violin <- TRUE
          box <- FALSE
          vbs_plot <- "v"
        }
        else if (type == "box") {
          box <- TRUE
          violin <- FALSE
          vbs_plot <- "b"
        }
        else if (type == "strip") {
          strip <- TRUE  # currently not used, pt.size set to 0 instead
          vbs_plot <- "s"
        }

        # get some VBS parameters, including: pt.size, jitter, bw
        # ss.numeric called there
        output <- NULL
        VBS <- .param.VBS(data[,1], ID.call, facet1.call, facet1.miss,
                by.call, by.miss, bw, bw.miss, bw_iter, iter.details,
                lx, n.ux, k.iqr, box_adj, a, b,
                x.name, facet1.name, by.name, vbs_plot,
                n_col.miss, n_row.miss,
                size = .9, out_size = out_size, out_size.miss = out_size.miss,
                jitter_x=NULL, jitter_y=NULL,
                bin = FALSE, breaks = "Sturges", bin_start = NULL,
                bin_width = NULL, bin_end = NULL,
                proportion, digits_d, quiet, fun_call, ...)
        if (is.null(pt_size))
           pt.size <- VBS$pt.size
        else
           pt.size <- pt_size  # user supplied value
        pt.out_size <- VBS$out_size
        jitter_y <- VBS$jitter_y
        jitter_x <- VBS$jitter_x
        bw <- VBS$bw
        adj.bx.ht <- VBS$adj.bx.ht

        output$out_stats <- VBS$output  # text output
        cat("\n")
        print(output$out_stats)

      if (!is.null(jitter_x)) if (jitter_x > 0)  # not available in stripplot
        data[,1] <- jitter(data[,1], factor=jitter_x)

      if (!missing(vbs_plot))
        if (!grepl("s", vbs_plot)) pt.size <- 0  # gets rescaled if earlier

      # n_col is null for at Plot(x), Plot(x, by=), Plot(x, facet1=)
      if (n_col.miss && n_row.miss && !is.null(facet1.call))
        n_col <- 1  # default n_col for Trellis


# pivot tables ------------------------------------------------------------

      x_vec <- data[[ x.name ]]   # e.g., data[["Salary"]]
      group.vars <- character(0)

      cat("\n\n---------- Summary Statistics for", x.name)

      if (!by.miss) {
        group.vars <- c(group.vars, by.name)
        out_by <- .vbs_summary_table(
          x_vec    = x_vec,
          grp_vec  = by.call,
          grp_label = by.name,
          digits_d  = .max.dd(x_vec)
        )
        if (!quiet) {
          cat("\n\n")
          print(out_by)
        }
      }

      if (!facet1.miss) {
        group.vars <- c(group.vars, facet1.name)
        out_facet1 <- .vbs_summary_table(
          x_vec    = x_vec,
          grp_vec  = facet1.call,
          grp_label = facet1.name,
          digits_d  = .max.dd(x_vec)
        )
        if (!quiet) {
          cat("\n\n")
          print(out_facet1)
        }
      }

      if (!facet2.miss) {
        group.vars <- c(group.vars, facet2.name)
        out_facet2 <- .vbs_summary_table(
          x_vec    = x_vec,
          grp_vec  = facet2.call,
          grp_label = facet2.name,
          digits_d  = .max.dd(x_vec)
        )
        if (!quiet) {
          cat("\n\n")
          print(out_facet2)
        }
      }

      if (length(group.vars) == 0) {
        out_all <- .vbs_summary_table(
          x_vec  = x_vec,
          grp_vec = NULL,
          grp_label = x.name,
          digits_d  = .max.dd(x_vec)
        )
        if (!quiet) {
          cat("\n\n")
          print(out_all)
        }
      }

# do the plot -------------------------------------------------------------

      options(facet1name=facet1.name)
      .plt.lattice(
        x = data[,1], y = NULL,
        facet1 = facet1.call, facet2 = facet2.call, by = by.call,
        adj.bx.ht = adj.bx.ht,
        object = "point",
        n_row = n_row, n_col = n_col, asp = aspect,

        fill = pt.fill, area_fill = "transparent", color = pt.color,
        panel_fill = getOption("panel_fill"),
        panel_color = getOption("panel_color"),

        trans = pt.trans, size.pt = pt.size, size.ln = line_width,

        xlab = xlab, ylab = ylab, main = main,
        shape = "circle",
        lab_cex = getOption("lab_cex"), axis_cex = getOption("axis_cex"),

        lvl = 0, ellipse_color = NULL, ellipse_lwd = NULL,

        fit.ln = "off", fit_power = 1,
        fit_color = NULL, fit_lwd = NULL, fit_se = NULL,

        plot_errors = FALSE,
        origin = NULL,
        jitter = jitter_y,

        violin = violin, violin_fill = violin_fill,
        box = box, box_fill = box_fill,

        bw = bw, vbs_ratio = vbs_ratio, box_adj = box_adj,
        a = a, b = b, k.iqr = k.iqr,
        fences = fences, vbs_mean = vbs_mean,

        out_shape = out_shape, out_size = pt.out_size,
        out_fill = getOption("out_fill"),
        out_color = getOption("out_color"),
        out2_fill = getOption("out2_fill"),
        out2_color = getOption("out2_color"),

        ID = ID.call, out_cut = out_cut, ID_color = "black", ID_size = ID_size,

        axis_fmt = c("K", ",", ".", ""), axis_x_pre = "", axis_y_pre = "",

        rotate_x = rotate_x, rotate_y = rotate_y,

        x_n_axis_skip = 0, y_n_axis_skip = 0,

        width = width, height = height, pdf_file = pdf_file,
        T.type = "cont",
        quiet = quiet, ...
      ) 


      # reset for next analysis
      options(xname=NULL)
      options(yname=NULL)
      options(facet1name=NULL)
      options(facet2name=NULL)
      options(byname=NULL)

    }  # end vbs


# frequency polygon -------------------------------------------------------

      else if (type == "freq_poly") {

        if (n.by > 1) {
          cat("\n"); stop(call.=FALSE, "\n------\n",
            "Frequency polygons are only available for 1 group.\n", 
            "No  by  variable.\n\n")
        }

        send1_to_XY <- function(fun_call, target = "XY") {
          cl <- fun_call
          cl[[1L]] <- as.name(target)   # X(...) -> XY(...)
          cl$type  <- NULL              # drop visualization selector

          # inject stat_x = "count" if user has not already specified
          nm <- names(cl)
          if (is.null(nm)) nm <- character(length(cl))
          if (!("stat_x" %in% nm)) {
            cl[["stat_x"]] <- "count"
          }
          cl
        }

        cl_XY <- send1_to_XY(fun_call, target = "XY")
        return(invisible(eval.parent(cl_XY)))  # as if user typed XY(...)
      }

    } # end ncol(data) ---------

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

# if (df.name != "NULL")  # not shiny
#   dev.set(which=2)  # reset graphics window for standard R functions

}
