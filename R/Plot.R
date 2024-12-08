Plot <-
function(x, y=NULL, data=d, filter=NULL,

    by=NULL, facet1=NULL, facet2=NULL,
    n_row=NULL, n_col=NULL, aspect="fill",

    theme=getOption("theme"),
    fill=NULL, color=NULL,
    transparency=getOption("trans_pt_fill"),

    enhance=FALSE,
    size=NULL, size_cut=NULL, shape="circle", lwd=1.5, means=TRUE,
    segments=FALSE, segments_y=FALSE, segments_x=FALSE,

    sort_yx=c("0", "-", "+"),
    jitter_x=NULL, jitter_y=NULL,

    ID="row.name", ID_size=0.60,
    MD_cut=0, out_cut=0, out_shape="circle", out_size=1,

    fit=c("off","loess", "lm", "ls", "null", "exp", "quad",
          "power", "log"),
    fit_power=1, fit_se=0.95,
    fit_color=getOption("fit_color"),
    plot_errors=FALSE, ellipse=0,

    time_unit=NULL, time_agg=c("sum","mean"), time_ahead=0, time_format=NULL,
    time_fit=FALSE, es_level=NULL, es_trend=NULL, es_seasons=NULL,
    es_type=c("additive", "multiplicative"), es_PIlevel=0.95,
    stack=FALSE, area_fill="transparent", area_split=0, n_date_tics=NULL,
    show_runs=FALSE, center_line=c("off", "mean", "median", "zero"),

    stat=c("mean", "sum", "sd", "deviation", "min", "median", "max"),
    stat_x=c("count", "proportion", "%"),

    vbs_plot="vbs", vbs_size=0.9, bw=NULL, bw_iter=10,
    violin_fill=getOption("violin_fill"),
    box_fill=getOption("box_fill"),
    vbs_pt_fill="black",
    vbs_mean=FALSE, fences=FALSE,
    k=1.5, box_adj=FALSE, a=-4, b=3,

    radius=NULL, power=0.5, low_fill=NULL, hi_fill=NULL,

    smooth=FALSE, smooth_points=100, smooth_size=1,
    smooth_exp=0.25, smooth_bins=128, n_bins=1,

    bin=FALSE, bin_start=NULL, bin_width=NULL, bin_end=NULL,
    breaks="Sturges", cumulate=FALSE,

    xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
    lab_adjust=c(0,0), margin_adjust=c(0,0,0,0),

    rotate_x=getOption("rotate_x"), rotate_y=getOption("rotate_y"),
    offset=getOption("offset"),

    xy_ticks=TRUE, origin_x=NULL,
    scale_x=NULL, scale_y=NULL,
    pad_x=c(0,0), pad_y=c(0,0),
    legend_title=NULL,

    add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,

    quiet=getOption("quiet"), do_plot=TRUE,
    pdf_file=NULL, width=6.5, height=6,
    digits_d=NULL,

    n_cat=getOption("n_cat"), value_labels=NULL,
    rows=NULL, by1=NULL, by2=NULL,

    eval_df=NULL, fun_call=NULL, ...) {


  if (is.null(fun_call)) fun_call <- match.call()

  time_agg <-  match.arg(time_agg)

  stat.miss <- ifelse (missing(stat), TRUE, FALSE)
  if (stat[1] != "data") stat <- match.arg(stat)  # if condition for shiny
  stat_x.miss <- ifelse (missing(stat_x), TRUE, FALSE)
  stat_x <- match.arg(stat_x)

  output <- NULL
  out_fitted <- NULL
  out_y.frcst <- NULL

  if (!stat.miss && !stat_x.miss) {
    cat("\n"); stop(call.=FALSE, "\n------\n",
      "Cannot specify both parameters:  stat  and  stat_x\n\n")
  }
  y.miss <- ifelse (missing(y), TRUE, FALSE)
  if (!stat_x.miss && !y.miss) {
    cat("\n"); stop(call.=FALSE, "\n------\n",
      "Use parameter  stat to aggregate numerical variable y.\n",
      "Parameter  stat_x  is only for the categorical variable x.\n\n")
  }
  if (stat.miss) stat <- "data"  # no aggregation
  if (stat.miss  &&  !stat_x.miss) stat <- stat_x

  if (is.logical(fit[1])) {
    cat("\n"); stop(call.=FALSE, "\n------\n",
      "Now specify a value for  fit  such as \"loess\" or \"lm\".\n\n")
  }

  if (!is.null(time_unit)  &&  !is.null(substitute(facet1))) {
    cat("\n"); stop(call.=FALSE, "\n------\n",
      "Parameter  time_unit  does not yet apply to facet plots.\n\n")
  }

  if (fit[1] %in% c("xlog", "xylog")) {
    cat("\n"); stop(call.=FALSE, "\n------\n",
      "Parameters  xlog  and  xylog  not yet implemented.\n\n")
  }

  fit.ln <- ifelse (!missing(fit), match.arg(fit), "off")

  # a dot in a parameter name to an underscore and more
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    for (i in seq_along(dots)) {
      if (names(dots)[i] == "stat_yx") stat <- dots[[i]]
      if (names(dots)[i] == "area_origin") area_split <- dots[[i]]
      if (names(dots)[i] == "run") {
      cat("\n"); stop(call.=FALSE, "\n------\n",
        "Parameter  run  dropped. Now, specify the first variable, the\n",
        "  x-variable, as .Index to automatically create the index variable.\n",
        "  e.g., Plot(.Index, Y), where Y is the variable to plot\n\n")
      }
      if (grepl(".", names(dots)[i], fixed=TRUE)) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }

  # ------------ Old Stuff ----------------------------------
  facet1 <- .newparam(missing(by1), substitute(by1), "by1",
                      missing(facet1), substitute(facet1), "facet1")
  if (!is.null(facet1)) data[[as.character(facet1)]]  # extract facet1 from data

  facet2 <- .newparam(missing(by2), substitute(by2), "by2",
                      missing(facet2), substitute(facet2), "facet2")
  if (!is.null(facet2)) data[[as.character(facet2)]]  # extract facet2 from data

  if (!missing(rows))
    message(">>> Parameter  rows  renamed to:  filter.\n",
            "    Change to  filter,  rows  will stop working in the future.\n")

  if (!missing(n_cat) || !missing(value_labels)) {
    message(">>> Parameters  n_cat  and  value_labels  will no longer ",
            "work in the future.\n",
             "    Better to convert a categorical integer variable to ",
             "an R factor.\n\n",
             "Eg: d$Item1 <- factor(d$Item1, levels=1:3,",
                 "labels=c(\"Disagree\", \"Neutral\", \"Agree\", \n")
  }

  if (!is.null(x1)) if ("mean.x" %in% x1) x1 <- "mean_x"
  if (!is.null(y1)) if ("mean.y" %in% y1) y1 <- "mean_y"
  if (!is.null(add)) if ("h.line" %in% add[1])  add[1] <- "h_line"
  if (!is.null(add)) if ("v.line" %in% add[1])  add[1] <- "v_line"

  trans <- transparency

  # Note: stat is both object (dot plot) and statistic

  # limit actual argument to alternatives, perhaps abbreviated
  sort_yx.miss <- ifelse (missing(sort_yx), TRUE, FALSE)
  sort_yx <- match.arg(sort_yx)

  cl.miss <- ifelse (missing(center_line), TRUE, FALSE)
  center_line <- match.arg(center_line)

  data.do <- ifelse (stat == "data", TRUE, FALSE)

  proportion <- FALSE  # old signal, adjusted below if needed
  if (stat == "proportion") proportion <- TRUE

  if (plot_errors)
    if (fit.ln == "off") fit <- "lm"

  if (length(pad_x) == 1) {  # only the first element of pad_x specified
    temp <- pad_x
    pad_x <- double(length=2)
    pad_x[1] <- temp
    pad_x[2] <- temp
  }
  if (length(pad_y) == 1) {
    temp <- pad_y
    pad_y <- double(length=2)
    pad_y[1] <- temp
    pad_y[2] <- temp
  }

  vbs_plot <- tolower(vbs_plot)
  violin <- ifelse (grepl("v", vbs_plot), TRUE, FALSE)
  box <- ifelse (grepl("b", vbs_plot), TRUE, FALSE)

  iter.details <- ifelse (missing(bw_iter), FALSE, TRUE)

  k.iqr <- k   # k is a function name, so do not use internally

  cat.x <- FALSE;  cat.y <- FALSE

  pt.fill <- fill
  pt.color <- color
  pt.trans <- trans
  bubble_text <- getOption("bubble_text_color")
  segment_color <- getOption("segment_color")
  ID_color <- getOption("ID_color")

  ellipse_fill <- getOption("ellipse_fill")
  ellipse_color <- getOption("ellipse_color")
  ellipse_lwd <- getOption("ellipse_lwd")

  fit_lwd <- getOption("fit_lwd")
  se_fill <- getOption("se_fill")

  out_fill <- getOption("out_fill")
  out_color <- getOption("out_color")
  out2_fill <- getOption("out2_fill")
  out2_color <- getOption("out2_color")

  panel_fill <- getOption("panel_fill")
  panel_color <- getOption("panel_color")

  lab_cex <- getOption("lab_cex")  # NOT passed to .plt.main
  axis_cex <- getOption("axis_cex")
  main_cex <- getOption("main_cex")

  add_cex <- getOption("add_cex")
  add_lwd <- getOption("add_lwd")
  add_lty <- getOption("add_lty")
  add_color <- getOption("add_color")
  add_fill <- getOption("add_fill")
  add_trans <- getOption("add_trans")

  if (theme != getOption("theme")) {  # not the default theme
    sty <- style(theme, reset=FALSE)
    trans <- sty$pt$trans_fill
    fill <- sty$pt$fill
    pt.color <- sty$pt$fill  # just solid color points
    ellipse_fill <- sty$ellipse$fill
    violin_fill <- sty$VBS$violin_fill
    box_fill <- sty$VBS$box_fill
    se_fill <- sty$se_fill
  }

  # missing function only reliable if arg not modified, so capture
  x.miss <- ifelse (missing(x), TRUE, FALSE)
  by.miss <- ifelse (missing(by), TRUE, FALSE)  # interact sets
  facet1.miss <- ifelse (is.null(facet1), TRUE, FALSE)
  facet2.miss <- ifelse (is.null(facet2), TRUE, FALSE)
  size.miss <- ifelse (missing(size), TRUE, FALSE)
  radius.miss <- ifelse (missing(radius), TRUE, FALSE)
  fill.miss <- ifelse (missing(fill), TRUE, FALSE)
  if (is.null(fill)) fill.miss <- TRUE  # shiny sets fill at NULL if by var
  color.miss <- ifelse (missing(color), TRUE, FALSE)
  trans.miss <- ifelse (missing(trans), TRUE, FALSE)
  box_fill.miss <- ifelse (missing(box_fill), TRUE, FALSE)
  violin_fill.miss <- ifelse (missing(violin_fill), TRUE, FALSE)
  area_fill.miss <- ifelse (missing(area_fill), TRUE, FALSE)
  seg.miss <- ifelse (missing(segments), TRUE, FALSE)
  seg.y.miss <- ifelse (missing(segments_y), TRUE, FALSE)  # for Cleveland plot
  seg.x.miss <- ifelse (missing(segments_x), TRUE, FALSE)
  ellipse.miss <- ifelse (missing(ellipse), TRUE, FALSE)
  fit.miss <- ifelse (missing(fit), TRUE, FALSE)
  fit_se.miss <- ifelse (missing(fit_se), TRUE, FALSE)
  MD.miss <- ifelse (missing(MD_cut), TRUE, FALSE)
  out_size.miss <- ifelse (missing(out_size), TRUE, FALSE)
  out_shape.miss <- ifelse (missing(out_shape), TRUE, FALSE)
  bw.miss <- ifelse (missing(bw), TRUE, FALSE)
  n_col.miss <- ifelse (missing(n_col), TRUE, FALSE)
  n_row.miss <- ifelse (missing(n_row), TRUE, FALSE)
  add_miss <- ifelse (missing(add), TRUE, FALSE)
  ylab.miss <- ifelse (missing(ylab), TRUE, FALSE)

  if (!missing(a) || !missing(b)) box_adj <- TRUE

  if (missing(vbs_size)) if (!violin)  # wider box if no violin
    vbs_size <- ifelse (y.miss || facet1.miss, vbs_size*3.75, vbs_size*5)

  # "off" substitutes for official value of "transparent"
  fill[which(fill == "off")] <- "transparent"
  color[which(color == "off")] <- "transparent"
  ellipse_color[which(ellipse_color == "off")] <- "transparent"
  ellipse_fill[which(ellipse_fill == "off")] <- "transparent"
  add_fill[which(add_fill == "off")] <- "transparent"
  add_color[which(add_color == "off")] <- "transparent"

  if (enhance) {
    if (ellipse.miss) ellipse <- 0.95
    if (MD.miss) MD_cut <- 6
    if (add_miss) add <- "means"
    if (fit.miss) fit.ln <- "lm"
  }

  # run chart settings
  run <- ifelse (substitute(x) == ".Index", TRUE, FALSE)
  if (show_runs) run <- TRUE
  if (run) {
    object <- "both"
    n.y_var <- 1
    if (cl.miss) center_line <- "default"
    if (seg.miss) segments <- TRUE
    if (fill.miss) {
      fill <- "gray20"
      color <- "gray60"
   }
  }  # end run

  # default point size for a stacked time series
  if (stack) {
    if (size.miss) size <- 0
    size.miss <- FALSE
  }

  # presume text output to the console, could turn off at end of function
  outp <- TRUE

  txdif <- ""  # used to transfer output for diffs of two vars Cleveland sp

  xlab_adj <- lab_adjust[1];   ylab_adj <- lab_adjust[2]
  tm.adj <- margin_adjust[1];  rm.adj <- margin_adjust[2]
  bm.adj <- margin_adjust[3];  lm.adj <- margin_adjust[4]

  date.var <- FALSE  # default is not a date variable for x
  freq.poly <- FALSE  # default is not a frequency polygon

  if (!fit_se.miss) if (missing(fit))  fit.ln <- "loess"
  if (fit_se.miss && plot_errors) fit_se <- 0  # default for plot_errors
  if (!by.miss && fit_se.miss && fit.ln!="off") fit_se <- 0  # default for by

  # any bin parameter activates bins for VBS plot, or freq.poly=TRUE
  if (!missing(breaks)  ||  !missing(bin_start)  ||  !missing(bin_width)  ||
      !missing(bin_end))
    bin <- TRUE

  if (!data.do) if (is.null(pt.trans)) pt.trans <- 0  # trans, so dot plot

  # set object
  if (!missing(radius) || !missing(power)) {
    object <- "bubble"  # any bubble parameter activates a bubble plot
  }
  else
    object <- "default"

  # ------
  # see if dated or inconsistent parameter values
  .param.old(...)
  .plt.bad(x.miss, y.miss, stat, breaks, bin_start, n_row, n_col,
           MD_cut, out_cut, fit_se, ...)

  # if x is in a data frame, then in the function call it is a name
  # if x is in global, then in the function its name is a variable direct

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
        # the 1.201 comes from Shiny, need to reset
        # l.cex and l.axc are set in interact() before shiny analysis
        if (getOption("lab_cex") == 1.201) {
         if (getOption("l.cex") != 1.201) {
            style(lab_cex=getOption("l.cex"))
            style(axis_cex=getOption("l.axc"))
          }
          else
            style(quiet=TRUE)
        }
      }
    }
    else # df.name is NULL (only for shiny)
      x.in.global <- TRUE
  }

  eval_df <- !x.in.global

  # ---------------

  #  get data to be analyzed into data.x data frame

    # subset filter (with deprecated rows parameter also)
    if (!missing(filter) || !missing(rows)) {
      txt <- .filter(deparse(substitute(filter)))
      if (!missing(filter))  # subset filter
        r <- eval(str2expression(txt), envir=data, enclos=parent.frame())
      if (!missing(rows))
        r <- eval(substitute(rows), envir=data, enclos=parent.frame())
      r <- r & !is.na(r)  # set missing for a row to FALSE
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

  # process row.names if specified
  if (x.name %in% c("row_names", "row.names")) {
    # retain order of row names, otherwise will be alphabetical
    data.x <- data.frame(factor(row.names(data), levels=row.names(data)))
    if (is.null(xlab)) xlab <- ""  # unless specified, drop the axis label
    cat.x <- TRUE
  }

  # x not in global env, in df, specify data= forces to data frame
  else if (!x.in.global) {
    if (eval_df) {
      if (!mydata.ok) .nodf(df.name)  # check to see if df exists
      if (x.name != ".Index")
        .xcheck(x.name, df.name, names(data))  # stop if x an expression
    }

    if (x.name != ".Index") {  # not a run chart
      data.vars <- as.list(seq_along(data))
      names(data.vars) <- names(data)
      x.col <- eval(substitute(x), envir=data.vars)  # col num of each var

      if (!("list" %in% class(data))) {
        data.x <- data[, x.col]  # need stringsAsFactors
        data.x <- data.frame(data.x)
      }
      else {  # class of data is "list"
         data.x <- data.frame(data[[x.col]])
      }
      if (is.numeric(x.col))
        names(data.x) <- names(data.vars)[x.col]
      else
        names(data.x) <- x.col  # if x a vector, x.col can return names
    } 
    else {  # run chart, create x as sequence of integers from 1 
      data.x <- data.frame(seq_len(nrow(data)))
      names(data.x) <- "Index"
      options(xname="Index")
    }
    
  }  # end x in df

  # x is in the global environment (vector or data frame)
  # can only access x directly if it is not in a data frame
  else if (is.data.frame(x)) { # x a data frame
      cat("\n"); stop(call.=FALSE, "\n------\n",
        "Need to specify variables for analysis,\n",
        "not a full data frame\n\n")
  }

  # time series in global env
  # deconstruct to a data frame of dates for x var and then a df for y
  else if (is.ts(x)) {
    y.name <- deparse(substitute(x))  # get the name of the time series
    options(yname = y.name)
    date.var <- TRUE
    if (is.null(xlab)) xlab <- ""  # unless specified, drop the axis label
    tsPull <- .tsExtract(x, x.name)
    data.x <- tsPull$x.dates
    y.call <- tsPull$y
    n.y_var <- 1
  }  # end is.ts()

  else {  # x is a not ts vector in global
    if (!is.function(x))
      data.x <- data.frame(x)  # x is 1 var
    else  # x is 1 var
      data.x <- data.frame(eval(substitute(data$x)))
    names(data.x) <- x.name
  }

  n.x_var <- ncol(data.x)  # number of x-variables
  x.call <- data.x  # x.call is the primary x-variable

  # get data.x data to be analyzed into x.call, except for BPFM
  BPFM <- FALSE
  spmat <- FALSE

  # just one x variable for now, a vector of cat or num values
  # see if convert to variable of -- type Date --
  if (n.x_var == 1) {

    if (grepl("POSIX",  class(x.call[,1]), fixed=TRUE)[1])
      x.call[,1] <- as.Date(x.call[,1])
    date.var <- ifelse (.is.date(x.call[,1]), TRUE, FALSE)

    # if sequence of integer years, then convert to type Date
    if (.is.integer(x.call[,1])  &&  !run) {
      diff1 <- all(diff(x.call[,1]) == 1)  # see if consecutive years
      yr.range <- all(x.call[,1] >= 10 & x.call[,1] <= 9999)  # range for yrs
      if (diff1 && yr.range) {
        x.call[,1] <- as.Date(paste0(x.call[,1], "-01-01"))  # to Date
        date.var <- TRUE
        if (!quiet)
          message("\n>>> Note: Variable ", x.name, "  assumed to be years\n",
                  "Converted to a type Date variable\n",
                  "If forecasting there will be no seasonality.\n\n")
      }
    }

    # if x.call[,1] has char string values, see if to convert to type Date
    x11 <- x.call[1,1]
    if (is.character(x11)) {
      if (!is.null(time_format))  # specify the date format
        x.call[,1] <- as.Date(x.call[,1], format=time_format)

      else {  # guess the date format
        n.ch <- nchar(x11)
        if (n.ch %in% 6:10) {
          isQ <- grepl("Q1|Q2|Q3|Q4", x11)
          isM <- grepl("Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec", x11)
          if (isM) {
            x.call[,1] <- gsub(" ", "", x.call[,1])  # remove all spaces
            year <- substr(x.call[,1], 1, 4)
            monthNm <- substr(x.call[,1], 5, 7)
            x.call[,1] <- as.Date(paste(year, monthNm, "01", sep="-"),
                                   format="%Y-%b-%d")
          }
          else if (isQ) {  # convert dates entered as 2024 Q3 to R Date
            parts <- strsplit(gsub("\\s+", "", x.call[,1]), "Q")  # remove Q
            # Extract quarter, `[` is R extraction operator
            year <- as.numeric(sapply(parts, `[`, 1))  # 1st list element (year)
            quarter <- as.numeric(sapply(parts, `[`, 2))  # 2nd list element 
            month <- 1 + (quarter - 1) * 3  # get month Q1=1, Q2=4, Q3=7, Q4=10
            x.call[,1] <- as.Date(paste(year, month, "01", sep="-"))  #to Date
          }  # end quarter
          else {  # see if numeric numeric date format
            punct <- " "  # see if there are two punctuation delimiters
            if (length(gregexpr("/", x11, fixed=TRUE)[[1]]) == 2) punct <- "/"
            if (length(gregexpr("-", x11, fixed=TRUE)[[1]]) == 2) punct <- "-"
            if (length(gregexpr(".", x11, fixed=TRUE)[[1]]) == 2) punct <- "."
            if (punct %in% c("/", "-", "."))   # only evaluate probable dates
              x.call[,1] <- .charToDate(x.call[,1], punct)
          }  # numeric date format
        }  # end n.ch is 6, 7, 8
      }  # end do best guess
    }  # is.char x.call[1,1]

    cat.x <- ifelse (is.character(x.call[,1]) || is.factor(x.call[,1]),
                     TRUE, FALSE)
    if (cat.x  &&  object == "both") {
      cat("\n"); stop(call.=FALSE, "\n------\n",
        "A run chart applies only to continuous variables\n",
        x.name, " is a categorical variable\n\n")
    }

    if (.is.date(x.call[,1])) date.var <- TRUE
  }  # end n.x_var == 1

  # more than one x-variable
  else {
    # no y, see if eligible for BPFM where all selected vars are cat
    # multiple lines of a continuous x-variable also can occur
    if (y.miss  &&  object != "both") {
      is.cat <- logical(length=length(x.col))
      is.nmb <- logical(length=length(x.col))
      for (i in seq_along(x.col)) {  # see if variables are all categorical
        is.string <- is.factor(data.x[,i]) || is.character(data.x[,i])
        is.cat[i] <- ifelse (is.string, TRUE, FALSE)
        is.nmb[i] <- ifelse (is.numeric(data.x[,i]), TRUE, FALSE)
      }  # end for

      if (all(is.cat)) {
        BPFM <- TRUE
        cat.x <- TRUE
        spmat <- FALSE
      }
      else if (all(is.nmb)) {
        BPFM <- FALSE
        cat.x <- FALSE
        spmat <- TRUE
      }

      else {
        cat("\n"); stop(call.=FALSE, "\n------\n",
          "Multiple x-variables must all be numeric or all R factors\n\n")
      }
    }  # y is missing and not a line graph
    else
      cat.x <- FALSE
  }  # end more than 1 x-variable

   if (!is.factor(x.call[,1])) if (cat.x) x.call[,1] <- factor(x.call[,1])

  if (!BPFM)
    nrows <- ifelse(is.matrix(x.call[,1]), nrow(x.call[,1]), length(x.call[,1]))
  else
    nrows <- nrow(data.x)

  if (date.var || run) object <- "both"


  #-----------
  # evaluate y
  if (!y.miss) {
    # get actual variable name before potential call of data$y
    y.name <- deparse(substitute(y))
    options(yname = y.name)

    if (df.name != "NULL")
      in.global <- .in.global(y.name, quiet)
    else
      in.global <- TRUE

    # row.names deprecated in favor of row_names
    if (deparse(substitute(y)) %in% c("row_names", "row.names")) {
      # retain order of row names, otherwise will be alphabetical
      y.call <- factor(row.names(data), levels=row.names(data))
      if (is.null(ylab)) ylab <- ""  # unless specified, drop the axis label
      cat.y <- TRUE
      data.y <- data.frame(y.call)
    }

    # y not in global env, in df, specify data= forces to data frame
    else if (!in.global) {
      if (eval_df) .xcheck(y.name, df.name, names(data))  # var in df?
      data.vars <- as.list(seq_along(data))  # even if only a single var
      names(data.vars) <- names(data)  # all data in data frame
      y.col <- eval(substitute(y), envir=data.vars)  # col num selected vars

      if (!("list" %in% class(data))) {
        data.y <- data[, y.col]
        data.y <- data.frame(data.y)
      }
      else  # class of data is "list"
        data.y <- data.frame(data[[y.col]])

      if (is.numeric(y.col))
        names(data.y) <- names(data.vars)[y.col]
      else
        names(data.y) <- y.col
    }  # end not global y

    # y is a data frame in the global env (vector or data frame)
    else if (is.data.frame(y)) { # y a data frame
        data.y <- y
    }

    else {  # y a vector in global
      if (!is.function(y))
        data.y <- data.frame(y, stringsAsFactors=TRUE)  # y is 1 var
      else  # y is 1 var
        data.y <- data.frame(eval(substitute(data$y)), stringsAsFactors=TRUE)
      names(data.y) <- y.name
    }

    n.y_var <- ncol(data.y)  # number of y-variables
    y.call <- data.y

    if (ncol(y.call) == 1) {  # y is one variable
      if (is.character(y.call[,1]) || is.factor(y.call[,1])) cat.y <- TRUE
      if (!is.factor(y.call[,1])) if (cat.y) y.call[,1] <- factor(y.call[,1])
    }
    else {  # 2 or more y vars
      cat.y <- FALSE  #  multiple y-vars must be numerical
      if (ylab.miss) ylab <- ""  # use legend instead
      y.call <- data.frame(data.y)
      # y.call <- data.matrix(data.y, rownames.force=FALSE)
    }
  }  # end y not missing

  else { # missing y
    if (!date.var) y.call <- NULL
  }  # end missing y

  if (time_ahead > 0 && n.y_var > 1) {
    cat("\n"); stop(call.=FALSE, "\n------\n",
      "Can only do a forecast for a single y-variable\n\n")
  }

  if (!is.numeric(y.call[,1]) && run) {
    cat("\n"); stop(call.=FALSE, "\n------\n",
      "Run chart only applies to a numerical variable\n\n")
  }


  # ---------------------------------
  # ellipse, fit line stop conditions
  if (ellipse[1] > 0) {
    many.y <- FALSE
    if (!y.miss) if (is.matrix(y.call)) many.y <- TRUE
    if ((ncol(data.x)>1 || many.y)  ||  cat.x  ||  cat.y) {
      cat("\n"); stop(call.=FALSE, "\n------\n",
        "An ellipse only applies to a scatterplot of two, ",
        "continuous variable\n\n")
    }
    if (y.miss) {
      cat("\n"); stop(call.=FALSE, "\n------\n",
        "Need a y-variable to compute an ellipse\n\n")
    }
  }

  if (y.miss && (fit.ln != "off") && n.x_var == 1 && (!date.var && !run)) {
    cat("\n"); stop(call.=FALSE, "\n------\n",
      "Fit line only applicable if only one x variable, if y is present\n\n")
  }


  # ---------------------------------------
  # ---------------------------------------
  # master control funnel for analysis type
  # ---------------------------------------
  lx <- length(x.call[,1])
  n.ux <- length(unique(x.call[,1]))
    ly <- length(y.call[,1])
    n.uy <- length(unique(y.call[,1]))
  x.unique <- ifelse (n.ux == lx, TRUE, FALSE)
  y.unique <- ifelse (n.uy == ly, TRUE, FALSE)

  # get Trellis and T.type
  fnl <- .plt.funnel(data.do, n.x_var, y.miss, cat.x, cat.y, run, date.var,
                     facet1.miss, y.unique)
  Trellis <- fnl$Trellis
  T.type <- fnl$T.type  # needed only for Trellis

  if (!data.do  &&  n.x_var == 1)
    if (!cat.x) violin <- FALSE

  # end master control funnel
  # -------------------------


  if (is.logical(ellipse))
    ellipse <- ifelse (ellipse, 0.95, 0.00)
  if (ellipse[1] > 0  &&  !Trellis  &&  !quiet  &&  df.name != "NULL") {
    txt <- "[Ellipse with Murdoch and Chow's function ellipse"
    cat(txt, "from their ellipse package]\n")
  }


  # evaluate by
  #------------
  # cannot directly evaluate is.null(by) if by is present as a variable
  # so instead process as below to either get by.call or it is NULL
  # can get by.name

  do.by <- TRUE
  if (by.miss)
    do.by <- FALSE
  else {
    by.name <- deparse(substitute(by))
    if (by.name == "NULL")
      do.by <- FALSE  # specified by=NULL in call, including shiny
    by.in.global <- ifelse (df.name!="NULL", .in.global(by.name, quiet), TRUE)
    if (by.in.global) if (is.null(by))
      do.by <- FALSE
  }

  if (do.by)  {  # a by var
    options(byname = by.name)

    # see if var exists in data frame, if x not in global Env or function call
    if (!missing(x) && !by.in.global)
      .xcheck(by.name, df.name, names(data))

    if (!by.in.global) {  # get the variable's values from data frame col(s)
      data.vars <- as.list(seq_along(data))  # even if only a single var
      names(data.vars) <- names(data)  # all var names in data frame
      by.col <- eval(substitute(by), envir=data.vars)  # col num selected vars
      by.call <- data[, by.col]
    }
    else {  # vars that are function names get assigned to global
      by.call <- by
      if (is.function(by.call)) by.call <- eval(substitute(data$by))
    }

   # need by to be a factor
   # .plt.by.legend, plt.main #818, needs levels(by)
    if (!is.factor(by.call)) by.call <- factor(by.call)
    by.unq <- length(unique(by.call))
    shp <- .plt.shapes(shape, out_shape, n.by=by.unq)
    shape <- shp$shape
  }

  else {   # process shapes (without a by var)
    if (shape[1] == "sunflower")
      object <- "sunflower"
    else {  # get numeric code, check for bad shapes
      shp <- .plt.shapes(shape, out_shape)
      shape <- shp$shape
      out_shape <- shp$out_shape
    }
    by.call <- NULL
    by.miss <- TRUE  # just need is.null(by.call) from here forward
  }

  n.by <- ifelse (is.null(by.call), 0, nlevels(by.call))


  # evaluate facet1
  #-------------
  if (!facet1.miss) {

    # get actual variable name before potential call of data$x
    facet1.name <- deparse(substitute(facet1))

    options(facet1name = facet1.name)

    in.global <- ifelse (df.name!="NULL", .in.global(facet1.name, quiet), TRUE)

    # see if var exists in data frame, if x not in global Env or function call
    if (!missing(x) && !in.global)
      .xcheck(facet1.name, df.name, names(data))

    if (!in.global)
      facet1.call <- eval(substitute(data$facet1))
    else {  # vars that are function names get assigned to global
      facet1.call <- facet1
      if (length(facet1.call) == 0) facet1.call <- NULL
      if (is.function(facet1.call)) facet1.call <- eval(substitute(data$facet1))
    }

    if (!is.null(facet1.call)) {
      if (!is.factor(facet1.call)) facet1.call <- factor(facet1.call)
    }
    else
      facet1.miss <- TRUE
  }

  else
   facet1.call <- NULL

  if (MD_cut > 0 && (!is.null(by.call) || !facet1.miss)) {
    cat("\n"); stop(call.=FALSE, "\n------\n",
      "Outlier analysis works only with no  by  or  facet1  groups\n\n")
  }


  # evaluate facet2
  #-------------
  if (!facet2.miss) {

    # get actual variable name before potential call of data$x
    facet2.name <- deparse(substitute(facet2))
    options(facet2name = facet2.name)

    in.global <- .in.global(facet2.name, quiet)  # in global?

    # see if var exists in data frame, if x not in global Env or function call
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

  else {
   facet2.call <- NULL
   facet2.miss <- TRUE
  }


  # evaluate size (NULL, numeric constant, or a variable)
  #--------------

  if (!size.miss) {
    size.name <- deparse(substitute(size))
    if (size.name == "NULL") size.miss <-TRUE
  }

  if (!size.miss) {
    suppressWarnings(size.num <- as.numeric(size.name))  # number or variable?

  if (is.na(size.num)) {  # size a variable (did not resolve to a number)
      in.global <- ifelse (df.name!="NULL", .in.global(size.name, quiet), TRUE)
    if (!in.global) {
      data.vars <- as.list(seq_along(data))
      names(data.vars) <- names(data)
      size.col <- eval(substitute(size), envir=data.vars)  # col num of each var
      size <- data[,size.col]
      if (!is.numeric(size)) {
        cat("\n"); stop(call.=FALSE, "\n------\n",
          "Variable ", size.name, " must be numeric\n\n",
          "Perhaps use: by=", size.name, "\n\n")
      }
      options(sizename = size.name) # for later access
    }  # end not global
  }  # end size is a variable
    if (is.null(size[1])) size.miss <- TRUE

    if (length(size) > 1) {
      object <- "bubble"
      options(sizename = size.name) # for later access
    }
  }  # end not size.miss

  if (is.null(size_cut)) {
    if (length(size) > 1)
      size_cut <- 2
    else
      size_cut <- ifelse (cat.x, 1, 2)
  }

  if (!grepl("s", vbs_plot)) size <- 0


  # evaluate ID
  #------------
  get.ID <- FALSE
  if (!is.null(add)) if (add[1] == "labels") get.ID <- TRUE
  if (!y.miss) if (MD_cut>0 || out_cut>0)
    get.ID <- TRUE

  if (get.ID) {
    # ID.name is the actual var name if specified directly,
    #  or the name of the var that contains the var name
    ID.name <- deparse(substitute(ID))
    ID.name <- gsub("\"", "", ID.name)  # remove extra quotes if "row.name"
    if (ID.name == "row.names") ID.name <- "row.name"
    if (!x.in.global) {
  # if x is in a data frame, then in the function call it is a name
  # if x is in global, then in the function its name is a variable direct
  # ID.col is the actual var column if specified directly,
  #  or the var name if a variable that contains the name was entered
      if (ID.name != "row.name") {
        ID.col <- eval(substitute(ID), envir=data.vars, parent.frame())
        if (!is.numeric(ID.col)) {
          ID.col <- which(names(data) == ID.col)
          ID.name <- names(data)[ID.col]
        }
      .xcheck(ID.name, df.name, names(data))  # var exists in data frame?
      ID.call <- data[, ID.col]
      }  # end not row.name
      else
        ID.call <- seq_len(nrow(data))
    } # end x.in.global

    else {
      ID.call <- eval(substitute(ID), parent.frame())
    }
  }  # end get.ID

  else  # no ID to get
    ID.call <- NULL


  # -----------  x, y, by, size, and ID variables established -------
  # -----------------------------------------------------------------

  if (!run)
    n.xcol <- ifelse (y.miss, 1, ncol(x.call))  # sp matrix means only 1 xcol
  else
    n.xcol <- ncol(x.call)
  n.ycol <- ifelse (y.miss, 0, ncol(y.call))
  nn_col <- max(n.xcol, n.ycol)  # n_col goes into lattice, do not change

  if (is.null(height)) {
    if (is.null(y.call)  &&  !BPFM
        &&  data.do  &&  object != "both"  &&  !.is.date(x.call))
      height <- ifelse (is.null(main), 4, 4.6)  # narrow for 1-D dot plot
    else
      height <- 6

    if (BPFM)  # more than 7 variables, make plot extra long
      height <- height + ((ncol(data.x) - 7) * 0.5)
  }

  if (is.null(width)) width <- 6
  if (!is.null(by.call)) width <- width + .85  # wider plot


  # --------
    # adjust by, manage regular-R or PDF graphics
  if (!Trellis) {
    if (!is.null(pdf_file)) {
      if (!grepl(".pdf", pdf_file))
        pdf_file <- paste(pdf_file, ".pdf", sep="")
      pdf(file=pdf_file, width=width, height=height, onefile=FALSE)
    }
    else {
      if (df.name != "NULL")  # not dev.new for shiny
          .opendev(pdf_file, width, height)
    }
  }


  # ------------------------------------------------
  # set object and values where needed

  # prep 1-variable bubble plot to call regular scatter plot function
  # y.call to 0
  if (is.null(y.call)  &&  cat.x  &&  n.x_var == 1  &&  data.do) {
    y.call <- data.frame(rep(0, nrow(x.call)), stringsAsFactors=TRUE)
    cat.y <- FALSE
    if (object == "default") object <- "bubble"
  }

  # if numeric x is sorted with equal intervals, set as line chart
  #   unless doing a fit line as only want one line
  # x.call does not exist for BPFM
  if (!cat.x && fit.ln=="off") if (is.numeric(x.call[,1])  &&  nrows > 2) {
    if (object == "default") {
      eq.int <- ifelse (any(is.na(x.call[,1])), FALSE, TRUE)
      if (eq.int) {
        d.x <- diff(x.call[,1])  # only look at first x-variable
        for (i in 2:(length(d.x)))
          if ((abs(d.x[i-1] - d.x[i]) > 0.0000000001)) eq.int <- FALSE
        rm(d.x)
      }  # also no y missing

      if (!is.unsorted(x.call) && eq.int && sum(is.na(y.call))==0) {
        object <- "both"
        if (!seg.miss && !segments) object <- "point"
        if (is.null(size)) size <- 0  # by default, just plot line w/o points
      }
    }
  }

  # if numeric y is sorted with equal intervals, line chart
  if (fit.ln == "off") if (is.numeric(y.call)  &&  nrows > 2) {
    if (object == "default") {
      if (sum(is.na(y.call)) > 0)
        eq.int <- FALSE  # missing data in x present
      else {
        eq.int <- TRUE
        if (is.matrix(y.call))
          d.y <- diff(y.call[,1])  # only look at first x-variable
        else
          d.y <- diff(y.call)
        for (i in 2:(length(d.y)))
          if ((abs(d.y[i-1] - d.y[i]) > 0.0000000001)) eq.int <- FALSE
        rm(d.y)
      }  # also no y missing

      if (!is.unsorted(y.call) && eq.int && sum(is.na(x))==0)
        object <- "both"
    }
  }

  # set point or bubble plot
  if (object == "default") {  # set default
    if (!y.miss) {
      object <- "point"
      if (data.do) if (cat.x && cat.y) object <- "bubble"
    }
    else if (stat %in% c("count", "proportion", "%")) {
      object <- "point"
      n.y_var <- 1
    }
    else {
      if (data.do) object <- ifelse (cat.x, "bubble", "point")
      if (BPFM) object <- "bubble"  # BPFM
    }
  }

  if (y.miss  &&  !date.var  &&  object != "bubble") {
    if (!cat.x) {

      if (stat %in% c("count", "proportion", "%")) {  # frequency polygon

        ssstuff <- .ss.numeric(x.call[,1], digits_d=digits_d, brief=TRUE)

        values <- NULL
        hist.cumul <- ifelse(cumulate, "on", "off")
        reg <- "snow2"  # applies to cumulative histogram
        h <- .hst.main(x.call[,1], pt.fill, pt.color, pt.trans, reg,
           rotate_x, rotate_y, offset,
           breaks, bin_start, bin_width, bin_end,
           proportion, values, hist.cumul,
           xlab, ylab, main, sub, quiet=quiet, do_plot=FALSE,
           fun_call=NULL, ...)

        n_cat <- 0  # not many midpoints, do not want to trigger num.cat
        x.call <- h$mids
        y.call <- h$counts
        if (stat == "count")
          ylab <- paste("Count of", x.name)
        else {
          y.call <- y.call / sum(y.call)
          ylab <- paste("Proportion of", x.name)
        }

        # last assignment of object, now determined
        object <- "both"  # do freq poly as a line chart
        freq.poly <- TRUE  # need to indicate fill possibility
        center_line <- "off"  # not meaningful here

        cat.x <- FALSE
        cat.y <- FALSE

        txsug <- ""
        if (getOption("suggest")) {
          txsug <- ">>> Suggestions  or  enter: style(suggest=FALSE)"

          fncl <- .fun_call.deparse(fun_call)
          fncl <- gsub(")$", "", fncl)  # get function call less closing )
          fncl <- gsub(" = ", "=", fncl)

          fc <- ""
          if (!grepl("size", fncl))
            fc <- paste(fc, ", size=0", sep="")
          if (nzchar(fc)) {
            fc <- gsub(" = ", "=", fc)
            fc <- paste(fncl, fc, ")   # just line segments, no points", sep="")
            txsug <- paste(txsug, "\n", fc, sep="")
          }

          bw_new <- pretty((x.call[2] - x.call[1]) / 1.5)  # arbitrary new bw
          fc <- ""
          if (!grepl("bin_width", fncl))
            fc <- paste(fc, ", bin_width=", as.character(bw_new[1]), sep="")
          if (nzchar(fc)) {
            fc <- paste(fncl, fc, ") ", sep="")
            txsug <- paste(txsug, fc, sep="")
          }

          txsug <- .rm.arg.2(" x=", txsug)
          txsug <- .rm.arg.2("(x=", txsug)
        }

        if (!quiet) {

          txss <- ssstuff$tx  # stats output before reduce data

          txdst <- h$ttx
          if (is.null(txdst)) txdst <- ""

          bx <- .bx.stats(x.call, box_adj, k.iqr, a, b)
          txotl <- bx$txotl

          class(txsug) <- "out"
          class(txss) <- "out"
          class(txdst) <- "out"
          class(txotl) <- "out"
          output <- list(out_suggest=txsug, out_ss=txss, out_freq=txdst,
                         out_outliers=txotl)
          if (!is.null(output)) class(output) <- "out_all"
          print(output)  # OUTPUT, move to end
        }

        x.call <- data.frame(x.call, stringsAsFactors=TRUE)
        y.call <- data.frame(y.call, stringsAsFactors=TRUE)

      }  # end freq polygon
    }  # end !cat.x

    else {  # cat.x
      # just x variable, so set y.call to plot points for count and prop
      if (stat %in% c("count", "proportion", "%")) {

        if (Trellis) {  # follow dot plot format and do horizontal plot
          cat.y <- FALSE
          if (seg.x.miss) segments_x <- TRUE
          if (ylab.miss) {
            ylab <- ifelse (stat == "count", "Count of", "Proportion of")
            ylab <- paste(ylab, x.name)
          }
          x.call <- data.frame(x.call, stringsAsFactors=TRUE)
        }  # end if Trellis

        else {  # not Trellis, so manually flip to match dot plot style
          cat.x <- FALSE
          if (seg.y.miss) segments_y <- TRUE
          if (stat == "count")
            xlab <- "Count of"
          else if (stat == "proportion")
            xlab <- "Proportion of"
          else
            xlab <- "Percentage of"
          xlab <- paste(xlab, x.name)
          ylab <- NULL
          frq <- table(x.call)
          if (stat == "proportion") frq <- frq / sum(frq)
          if (stat == "%") frq <- (frq / sum(frq)) * 100
          if (is.factor(x.call))  # preserve ordering, will lose order attribute
            y.call <- factor(names(frq), levels=levels(x.call))
          else
            y.call <- factor(names(frq))
          cat.y <- TRUE
          options(yname=x.name)
          y.call <- data.frame(y.call, stringsAsFactors=TRUE)
          x.call <- data.frame(as.vector(frq), stringsAsFactors=TRUE)
        }
      }  # end values in
    }  # end cat.x

  }  # end is null y.call


  # ----------------------------------------------------------------
  # object now determined: "both", "point", "bubble", or "sunflower"

  # flag unsorted data for time series
  if (date.var) {

    b.name <- NULL
    sort_flag <- NULL
    if (!is.null(by.call)) if (!is.null(by.name)) {
      b.name <- by.name
      sort_flag <- ifelse (is.unsorted(by.call), FALSE, TRUE)
    }
    if (!is.null(facet1.call)) if (!is.null(facet1.name)) {
      b.name <- facet1.name
      sort_flag <- ifelse (is.unsorted(facet1.call), FALSE, TRUE)
    }

    # is.unsorted() flags sorted but in descending order
    if (df.name != "NULL") {  # by var not detected at first with shiny
      if (is.unsorted(x.call[,1]) && is.null(by.call) && is.null(facet1.call)) {
        if (is.null(b.name)) {  # not evaluated for by var
          message(">>> Warning\n",
            "The  Date  variable is not sorted in Increasing Order.\n\n",
            "For a data frame named d, enter: \n    ",
             paste("d <- sort_by(d, ", x.name, ")", sep=""), "\n",
            "Maybe you have a  by  variable with repeating Date values?\n",
          "Enter  ?sort_by  for more information and examples.\n\n")
        }
        else {  # a by variable
          if (!sort_flag) {
            message(">>> Warning\n",
              "The  by  variable is not sorted in Increasing Order.\n\n",
              "For a data frame named d, enter: \n    ",
              paste("d <- sort_by(d, by=c(", b.name, ", ", x.name, "))",
                  sep=""), "\n",
                  "Enter  ?sort_by  for more information and examples.\n\n")
          }
        }  # end else
      }  # end is.unsorted
    } # end df.name not "NULL"
  }  # end date.var

  # bubble size
  if (radius.miss)
    radius <- ifelse (length(size) > 1, .12, .22)

  # size of lines for line chart
    size.ln <- lwd  # size of lines

  # size of fit line
  # windows line too thin at 1, but no increments allowed, and 2 is too thick
  if (fit.ln != "off") if (is.null(fit_lwd)) fit_lwd <- getOption("fit_lwd")
  #   fit_lwd <- ifelse(.Platform$OS.type == "windows", 2, 1.5)

  # size of points
  if (size.miss)  # pt.size not set yet
    pt.size <- ifelse (.Platform$OS.type == "windows", 1.00, 0.80)
  else {  # size had been set
    if (length(size) == 1)
      scale.pt <- ifelse (.Platform$OS.type == "windows", 1.00, 0.80)
    else {  # size var
      scale.pt <- 1  # forget Win/Mac scaling for size var, ruins size for Mac
    }
    pt.size <- size * scale.pt
  }
  if (size.miss) {  # pt.size is a scaler
    if (object == "both") {  # pts and lines specified
      pt.size <- 0.85 * pt.size  # default pt size when lines specified
      if (!("transparent" %in% area_fill))
        pt.size[1] <- 0  # default no points if area shown
      else if (nrows > 50) {
        pt.size <- .75 - 0.002*nrows
        if (pt.size < 0.10) pt.size <- 0
      }
    }  # end both
  }

  ord.by.call <- is.ordered(by.call)
  fc <- .plt.colors(object, nn_col, n.by, theme, fill, fill.miss,
          color, color.miss, area_fill, area_fill.miss, trans, stack,
          n.ycol, n.y_var, ord.by.call, run, pt.size)

  pt.fill <- fc$pt_fill
  pt.color <- fc$pt_col
  area_fill <- fc$area_fill


  # ------------------------------------------------
  # analysis
  # ------------------------------------------------

  txstats <- NULL  # remains NULL if stat analysis (e.g., mean) does not occur

  # Trellis plot
  # ------------

  if (Trellis && do_plot) {

    if (T.type %in% c("cont", "cont_cont", "cont_cat")) {

      # VBS plot
      txt <- ifelse (!is.null(facet1.call),
                     "[Trellis (facet)", "[Violin/Box/Scatterplot")
      if (!quiet)
        cat(paste(txt, "graphics from Deepayan Sarkar's lattice package]\n\n"))

      # box_fill and violin_fill
      n.facet1 <- length(levels(facet1.call))
      n.facet2 <- ifelse (facet2.miss, 1, length(levels(facet2.call)))
      n.lvl <- n.facet1 * n.facet2
      ord.by.call <- is.ordered(facet1.call)

      if (!y.miss) {
        if (xor(cat.x, cat.y)  &&  n.facet1 > 1) {
          cat("\n"); stop(call.=FALSE, "\n------\n",
            "The way to submit this analysis is to have both categorical\n",
            "  variables be  facet1  and  facet2  variables, respectively.\n\n")
        }
      }

      if (df.name == "NULL"  &&  box_fill == "#419BD2")   # proxy for in shiny
        box_fill.miss <- TRUE

      vfc <- rgb(150,170,195, maxColorValue=255)  # default histogram bar fill

      # if facets, vary box fill, less intense for 1 panel
      if (box_fill.miss  &&  vbs_plot == "b"  &&  n.lvl == 0)
          box_fill <- vfc
      else if (grepl("b", vbs_plot))
          box_fill <- .plt.fill(box_fill, box_fill.miss, ord.by.call,
                                n.facet1, n.lvl, theme)

      if (violin_fill.miss  &&  vbs_plot == "v") {
        if (n.lvl == 0)
          violin_fill <- vfc
        else
          for (i in seq_len(n.lvl)) violin_fill[i] <- vfc
      }
      else if (!violin_fill.miss)  # otherwise default
        violin_fill <- .plt.fill(violin_fill, violin_fill.miss, ord.by.call,
                                 n.facet1, n.lvl, theme)
      else
        for (i in seq_len(n.lvl)) violin_fill[i] <- violin_fill[1]

      # VBS plot, cont_cont is a 2-var scatterplot, not a VBS plot
      if (y.miss  &&  !run  &&  T.type != "cont_cont") {

        if (n.by > 1) {
          if (color.miss) {
            if (getOption("theme") %in% c("gray", "white"))
              pt.color <- getColors("grays", n=n.by)
            else
              pt.color <- getColors("hues", n=n.by, l=40)
          }
          for (i in 1:n.by) {
            if (fill.miss) {
              pt.fill[i] <- pt.color[i]
              pt.fill[i] <- .maketrans(pt.fill[i], (1-trans)*256)
            }
            if (box_fill.miss) box_fill[i] <- .maketrans(box_fill[i], 0.6*256)
          }
        }
        else {  # n.by is 0 or 1
          if ("black" %in% vbs_pt_fill) {
            if (fill.miss) pt.fill <- "black"
            if (trans.miss) pt.trans <- 0
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

        # get some VBS parameters
        # ss.numeric called there
        if (df.name == "NULL"  &&  out_size == 1) out_size.miss <- TRUE
        VBS <- .param.VBS(x.call[,1], ID.call, facet1.call, facet1.miss,
                by.call, by.miss, bw, bw.miss, bw_iter, iter.details,
                lx, n.ux, k.iqr, box_adj, a, b,
                x.name, facet1.name, by.name, vbs_plot,
                n_col.miss, n_row.miss,
                size, out_size, out_size.miss,
                jitter_x, jitter_y,
                bin, breaks, bin_start, bin_width, bin_end, proportion,
                digits_d, quiet, fun_call, ...)
        pt.size <- VBS$pt.size
        pt.out_size <- VBS$out_size
        jitter_y <- VBS$jitter_y
        jitter_x <- VBS$jitter_x
        bw <- VBS$bw
        adj.bx.ht <- VBS$adj.bx.ht
        output <- VBS$output  # text output
      }  # end VBS plot

      else {  # Trellis but not a VBS plot, such as when there is a y-variable

        # e.g., y.miss=FALSE,  Plot(Years, Salary, facet1=Gender)
        adj.bx.ht <- nrows  # just to adjust box height
        if (size.miss) pt.size <- pt.size * .8
      }

      if (!is.null(jitter_x)) if (jitter_x > 0)  # not available in stripplot
        x.call[,1] <- jitter(x.call[,1], factor=jitter_x)

      if (!missing(vbs_plot))
        if (!grepl("s", vbs_plot)) pt.size <- 0  # gets rescaled if earlier

      # n_col is null for at Plot(x), Plot(x, by=), Plot(x, facet1=)
      if (n_col.miss && n_row.miss && !is.null(facet1.call))
        n_col <- 1  # default n_col for Trellis

      .plt.lattice(x.call[,1], y.call[,1], facet1.call, facet2.call, by.call,
                   adj.bx.ht, object, n_row, n_col, aspect,
                   pt.fill, area_fill, pt.color, panel_fill, panel_color,
                   pt.trans, pt.size, size.ln,
                   xlab, ylab, main, shape, lab_cex, axis_cex,
                   max(ellipse), ellipse_color, ellipse_lwd,
                   fit.ln, fit_power, fit_color, fit_lwd, fit_se,
                   plot_errors, area_split, jitter_y,
                   violin, violin_fill, box, box_fill,
                   bw, vbs_size, box_adj, a, b, k.iqr, fences, vbs_mean,
                   out_shape, pt.out_size,
                   out_fill, out_color, out2_fill, out2_color,
                   ID.call, out_cut, ID_color, ID_size,
                   rotate_x, rotate_y, width, height, pdf_file,
                   T.type, quiet, ...)

    }  # end T.type != "dot"

    else {  # bar plot

      if (seg.x.miss) segments_x <- TRUE
      .bar.lattice(x.call[,1], facet1.call, facet2.call, n_row, n_col, aspect,
                   proportion, pt.fill, pt.color,
                   pt.trans, pt.size, xlab, ylab, main,
                   rotate_x, offset,
                   width, height, pdf_file,
                   segments_x, breaks=NULL, T.type, quiet, ...)
    }

  }  # end Trellis && do_plot


  # -----------------------------------
  # bubble plot frequency matrix (BPFM)

  else if (ncol(data.x) > 1  &&  y.miss  &&  object == "bubble") {
    # get labels just for subset data matrix
    l <- attr(data, which="variable.labels")
    nm <- names(data.x)
    mylabs <- character(length=length(nm))
    for (i in seq_along(nm)) {
      if (!(nm[i] %in% names(l)))
        mylabs[i] <- "not available"
      else
        mylabs[i] <- l[which(names(l) == nm[i])]
    }
    if (all(mylabs == "not available")) mylabs <- NULL

    l.name <- "l"
    if (l.name %in% ls(name=.GlobalEnv))
      mylabs <- get(l.name, pos=.GlobalEnv)

    if (is.null(xlab)) xlab <- ""  # suppress x-axis label if not specified

    .dpmat.main(data[,x.col], mylabs, sort_yx,
      getOption("bar_fill_cont"), pt.color, panel_fill,
      pt.trans, shape, panel_color,
      low_fill, hi_fill,
      xy_ticks, xlab, ylab, main, sub, size,
      radius, size_cut, bubble_text, power,
      bm.adj, lm.adj, tm.adj, rm.adj,
      value_labels, rotate_x, rotate_y, offset, quiet, do_plot, fun_call, ...)
  }


  # ------------------
  # scatterplot matrix

  else if (spmat && do_plot) {
    bckg <- ifelse(panel_fill=="transparent",
                   getOption("window_fill"), panel_fill)
    .plt.mat(data.x, fit=fit.ln, col_fill=pt.fill, col_color=pt.color,
                     col.bg=bckg, col_trans=pt.trans,
                     pt.size=pt.size, size.miss=size.miss)
  }

  else if (n_bins > 1) {  # bin the x-axis, compute mean or median of y by bin

    if (stat.miss) stat <- "mean"
    if (seg.miss) segments <- TRUE
    if (size.miss) pt.size <- 1.1
    if (is.null(digits_d)) digits_d <- 3
    nm.x <- x.name
    nm.y <- y.name
    # size is NULL if not specified
    .plt.bins(x.call[,1], y.call[,1], nm.x, nm.y, stat, n_bins,
              segments=segments, size=size, digits_d, scale_x, scale_y,
              fill=pt.fill, color=pt.color, trans=pt.trans,
              quiet=quiet)
  }

  # all the other analyses
  # ----------------------

  else {
    if (stat %in% c("sum", "mean", "sd", "deviation", "min", "median", "max")) {

      n_cat <- 0
      means <- FALSE
      if (seg.x.miss) segments_x <- TRUE

      # do stats console output before reducing data
      if (!quiet) {
        if (!missing(y)) {
          if (cat.y) {
            cat("\n"); stop(call.=FALSE, "\n------\n",
            y.name, " is not numerical, so cannot compute its mean\n\n")
          }
          options(yname = x.name)  # reverse order of x and y for .ss.numeric
          options(xname = y.name)
          stats <- .ss.numeric(y.call[,1], by=x.call[,1],
                               digits_d=digits_d, brief=TRUE, y.name=x.name)
          txstats <- stats$tx
          options(xname = x.name)  # reverse back
          options(yname = y.name)
        }
        else  {
          stats <- .ss.factor(x.call, digits_d=digits_d, x.name=x.name,
                              brief=TRUE)
          txstats <- stats$counts
        }

        class(txstats) <- "out"
      }

    # set up new x.call and y.call for stats
    # only does overall stat, so not applicable if a by parameter
      if (stat == "sum") {
        ylab <- paste("Sum of", y.name)
        out <- tapply(y.call[,1], x.call[,1], sum, na.rm=TRUE)
      }
      if (stat == "mean") {
        ylab <- paste("Mean of", y.name)
        out <- tapply(y.call[,1], x.call[,1], mean, na.rm=TRUE)
      }
      if (stat == "sd") {
        ylab <- paste("Standard Deviation of", y.name)
        out <- tapply(y.call[,1], x.call[,1], sd, na.rm=TRUE)
      }
      if (stat == "deviation") {
        ylab <- paste("Mean Deviations of", y.name)
        out <- tapply(y.call[,1], x.call[,1], mean, na.rm=TRUE)
        out <- out - mean(out, na.rm=TRUE)
      }
      if (stat == "min") {
        ylab <- paste("Minimum of", y.name)
        out <- tapply(y.call[,1], x.call[,1], min, na.rm=TRUE)
      }
      if (stat == "median") {
        ylab <- paste("Median of", y.name)
        out <- tapply(y.call[,1], x.call[,1], median, na.rm=TRUE)
      }
      if (stat == "max") {
        ylab <- paste("Maximum of", y.name)
        out <- tapply(y.call[,1], x.call[,1], max, na.rm=TRUE)
      }

      x.call <- factor(names(out))
      y.call <- as.vector(out)

      x.call <- data.frame(x.call, stringsAsFactors=TRUE)
      y.call <- data.frame(y.call, stringsAsFactors=TRUE)
    }  # sum, mean, sd, min, median, max


    # ----------------------------------------------------
    # ----------------------------------------------------
    # 2-variable scatter plot
    # bubble plot for 1-variable (y.call=0) and 2-variable
    # line chart

    if ((!is.null(y.call) || date.var) &&
        object %in% c("point", "bubble", "both", "sunflower")) {

      if (object == "point"  &&  data.do) {  # for Cleveland dot plot
        if (!cat.x && cat.y && y.unique) {  # no sort_xy option
          if (seg.y.miss) if (y.unique && cat.y) segments_y <- TRUE
          if (seg.x.miss) if (x.unique && cat.x) segments_x <- TRUE
          if (sort_yx.miss) if (n.x_var <= 2) sort_yx <- "+"
        }
      }

      # sort y by x option (intended for Cleveland dot plot)
      tx <- character(length=0)
      if (sort_yx != "0") {
        srt.dwn <- ifelse (sort_yx == "-", FALSE, TRUE)
        if (n.x_var == 1) {  # one x-variable
          ord <- order(x.call[,1], decreasing=srt.dwn)
        }
        else if (n.x_var == 2) {  # two x-vars, sort on diffs
          difs <- x.call[,2] - x.call[,1]
          ord <- order(difs, decreasing=srt.dwn)
        }
        else {
          cat("\n"); stop(call.=FALSE, "\n------\n",
          "Sorting not meaningful for more than two x-variables\n\n")
        }

        y.c <- y.call[,1]
        y.c <- factor(y.c, levels=y.c[ord])
        y.call[,1] <- y.c

      }  # end sort_yx
      else { # sort_yx==0
        if (n.x_var == 2  &&  ncol(x.call) == 2) {  # run==TRUE only 1-col
          ord <- seq_len(nrow(x.call))
          difs <- x.call[,2] - x.call[,1]
        }
      }

      # for Cleveland dot plot of two vars, print difference by level
      if (n.x_var == 2  &&  ncol(x.call) == 2  &&  !quiet) {
        dd <- .max.dd(c(x.call[1], x.call[2])) + 1
        if (dd > getOption("digits")) dd <- getOption("digits")
        ny <- nrow(y.call)
        mx.i <- nchar(as.character(ny))
        mx.d <- max(nchar(.fmt(difs, dd)))
        mx.f <- ifelse (is.factor(y.call[,1]),
           max(nchar(as.character(levels(y.call[,1])))), 5)  #  5 is dummy
        tx[length(tx)+1] <- paste(.fmtc("n",mx.i), " ",
            .fmtc(" diff", mx.d), "  Row", sep="")
        tx[length(tx)+1] <- .dash2(mx.i + mx.d + mx.f + 2, "-")
        if (ny <= 20)
          rng <- 1:ny
        else
          rng <- c(1:10, (ny-10):ny)
        for (i in 1:ny) {
          k <- nrow(y.call) - (i - 1)  # reverse order, + diffs first
          if (i %in% rng)
            tx[length(tx)+1] <- paste(.fmti(i, mx.i),
              .fmt(difs[ord[k]], dd, mx.d), levels(y.call[,1])[k])
        }
        txdif <- tx  # a little hack, only display in .plt.txt
      }  # end two var Cleveland dot plot

      # bigger point for scatterplot of stats (instead of data)
      if (!data.do  &&  object == "point")
        if (is.null(size)) pt.size <- 1.25

      # outlier analysis, need before .plt.main
      outlpts <- NULL
      out_outliers <- ""
      if (!y.miss && !Trellis)
        if (n.x_var==1 && n.y_var==1
             && is.numeric(x.call[,1]) && is.numeric(y.call[,1]))
        if (MD_cut > 0  ||  out_cut > 0) {
          otl <- .plt.MD(x.call[,1], y.call[,1], ID.call, MD_cut, out_cut)
          out_outliers <- otl$tx  # descriptive text
          outlpts <- otl$outlpts  # the outliers
      }

      if (!is.ts(y.call)) {
        if (nrow(x.call) != nrow(y.call))  {
          cat("\n"); stop(call.=FALSE, "\n-----\n",
            "number of elements in x: ", nrow(x.call), "\n",
            "number of elements in y: ", nrow(y.call), "\n\n",
            "The number of elements must be equal, probably\n",
            "  have variables from user workspace so maybe\n",
            "  use the  remove function, e.g., remove(x)\n\n")
        }
      }

        if (object == "both"  &&  nn_col > 1) {
           # change to multi later
           if ("on" %in% area_fill) fill <- getOption("violin_fill")
        }

        # by default display center_line only if runs about a mean
        if (run  &&  center_line == "default") {
          y.clean <- y.call[complete.cases(y.call), 1]  # converts df to vector
          m <- mean(y.clean)
          n.change <- 0
          for (i in 1:(length(y.clean)-1))
            if ((y.clean[i+1] > m) != (y.clean[i] > m)) n.change <- n.change+1
          if (n.change/(length(y.clean)-1) < .15)
            center_line <- "off"
          else
            center_line <- "median"
        }

        # if x is a Date variable
        #   if not specified, get the existing time_unit
        #   if specified, aggregate the values of y.call over x.call
        if (date.var) {
          tsdata <- .plt.time(x.call, y.call, by.call, x.name, n.by,
                              time_unit, time_agg)
          time_unit <- tsdata$time_unit
          x.call <- tsdata$x.call
          y.call <- tsdata$y.call
          by.call <- tsdata$by.call
          do.agg <- tsdata$do.agg
        }

        if (time_unit=="unknown"  &&  time_ahead>0) {
          cat("\n"); stop(call.=FALSE, "\n------\n",
            "time_unit: ", time_unit, "\n",
            "Cannot forecast without a known time unit.\n\n")
        }

        # forecast
        if (time_ahead > 0) {
          f.out <- .plt.forecast(x.call, y.call, by.call,
            time_unit, time_ahead, time_fit, n_date_tics,
            es_level, es_trend, es_seasons, es_type, es_PIlevel,
            digits_d)  
          y.fit <- f.out$y.fit;  y.hat <- f.out$y.hat
          x.fit <- f.out$x.fit;  x.hat <- f.out$x.hat 
          y.upr <- f.out$y.upr;  y.lwr <- f.out$y.lwr 
          mx.x <- f.out$mx.x;  mn.y <- f.out$mn.y;  mx.y <- f.out$mx.y
          out_y.frcst <- f.out$y.frcst
          out_frcst <- f.out$out_frcst
          out_fitted <- f.out$out_fitted         
          out_err <- f.out$out_err         
          out_coefs <- f.out$out_coefs
          out_smooth <- f.out$out_params
        }
        else {  # no forecast
          y.fit <- NULL;  y.hat <- NULL
          x.fit <- NULL;  x.hat <- NULL
          y.upr <- NULL;  y.lwr <- NULL
          mx.x <- NULL;  mn.y <- NULL;  mx.y <- NULL
          out_y.frcst <- NULL;  out_frcst <- NULL
          out_fitted <- NULL; out_err <- NULL
          out_coefs <- NULL; out_smooth <- NULL
        }

      j_y <- NULL;  j_x <- NULL
      if (do_plot) {
        # main out, mostly the visualization, sometimes some stats
        m.out <- .plt.main(x.call, y.call, by.call,
          cat.x, cat.y, object, stat,
          pt.fill, area_fill, pt.color, pt.trans, segment_color,
          xy_ticks, xlab, ylab, main, main_cex, sub,
          rotate_x, rotate_y, offset, proportion, origin_x,
          pt.size, size.ln, shape, means,
          segments, segments_y, segments_x,
          smooth, smooth_points, smooth_size, smooth_exp, smooth_bins,
          radius, power, size_cut, bubble_text, low_fill, hi_fill,
          ID.call, ID_color, ID_size, outlpts,
          out_fill, out_color, out_shape, out_shape.miss,
          fit.ln, fit_power, fit_color, fit_lwd,
          fit_se, se_fill, plot_errors,
          ellipse, ellipse_color, ellipse_fill, ellipse_lwd,
          run, center_line, stack,
          time_unit, time_agg, do.agg, time_ahead, time_fit, n_date_tics,
          y.fit, y.hat, x.fit, x.hat, y.upr, y.lwr,
          mx.x, mn.y, mx.y, 
          freq.poly, jitter_x, jitter_y,
          xlab_adj, ylab_adj, bm.adj, lm.adj, tm.adj, rm.adj,
          scale_x, scale_y, pad_x, pad_y, legend_title,
          add, x1, x2, y1, y2, add_cex, add_lwd, add_lty,
          add_color, add_fill, add_trans, quiet, ...)

        if (fit.ln != "off") {
          b0 <- m.out$b0;  b1 <- m.out$b1
          mse <- m.out$mse;  Rsq <- m.out$Rsq
          by.cat <- m.out$by.cat
        }
        else {
          mse <- NULL
          by.cat <- NULL
        }

        if (!is.null(jitter_y)) if (jitter_y != 0) j.y <- m.out$jitter_y
        if (!is.null(jitter_x)) if (jitter_x != 0) j.x <- m.out$jitter_x

    }  # end do_plot

        if (outp && !quiet) {  # text output

          txjit <- NULL
#         if (xor(cat.x, cat.y) || xor(num.c.x, num.c.y)) {  # show jitter
          if (!is.null(jitter_x) && !is.null(jitter_y)) {
            if (jitter_x != 0  ||  jitter_y != 0) {  # show jitter
              tx <- character(length = 0)
              tx[length(tx)+1] <- "Some Parameter values (can be manually set)"
              tx[length(tx)+1] <- .dash2(55)
              tx[length(tx)+1] <- paste("size:", .fmt(pt.size,2),
                  " size of plotted points")
              if (!is.null(jitter_y)) if (jitter_y != 0) 
                tx[length(tx)+1] <- paste("jitter_y:", .fmt(j_y,2),
                  " random vertical movement of points")
              if (!is.null(jitter_x)) if (jitter_x != 0) 
                tx[length(tx)+1] <- paste("jitter_x:", .fmt(j_x,2),
                  " random horizontal movement of points")
              txjit <- tx
            }
          }

          #  radius, power values in the analysis
          txbub <- NULL
          if (object == "bubble") {   # move to .plt.txt
            tx <- character(length = 0)
            tx[length(tx)+1] <- "Some Parameter values (can be manually set)"
            tx[length(tx)+1] <- .dash2(55)
            tx[length(tx)+1] <- paste("radius:", .fmt(radius,2),
                "   size of largest bubble")
            tx[length(tx)+1] <- paste("power:", .fmt(power,2),
                "    relative bubble sizes")
            txbub <- tx
          }

          if (n_bins == 1) {  # if binning, .plt.bins does its own output

            o <- .plt.txt(x.call, y.call, stat, object, cat.x, cat.y,
              date.var, xlab, ylab, fit.ln, n.by, mse, b0, b1, Rsq, by.cat,
              center_line, run, show_runs,
              proportion, size, radius, digits_d, fun_call, txdif)

           # cumulate existing output pieces from o into final output
            output <- NULL

            if (getOption("suggest"))
              output <- list(out_suggest=o$out_suggest)

            if (!is.null(outlpts)) {
              class(outlpts) <- "out"  # MD outliers
              output$outlpts <- outlpts
            }
            if (length(out_outliers) > 1)  # source is here
              output$out_outliers <- out_outliers
            if (length(o$out_outliers) > 1)  # source is .plt.txt
              output$out_outliers <- o$out_outliers

            if (!is.null(o$out_stats))
              output$out_stats <- o$out_stats

            if (!is.null(txstats))  # from stat option earlier in this file
              output$out_txt <- txstats

            if (length(o$out_reg) == 1) {
              if (nzchar(o$out_reg))
                output$out_reg <- o$out_reg
            }
            else if (length(o$out_reg) > 1)  # by activated
              output$out_reg <- o$out_reg

            if (!is.null(o$out_XV)) if (length(o$out_XV) > 1)
              output$out_XV <- o$out_XV


            if (!is.null(txjit))
              class(txjit) <- "out"  # jitter parameter
              output$out_jitter <- txjit

            if (!is.null(txbub))
              class(txbub) <- "out"  # bubble plot parameters
              output$out_bubble <- txbub

            if (!is.null(out_err)) {
              class(out_err) <- "out"
              output$out_err <- out_err
            }

            if (!is.null(out_coefs)) {
              class(out_coefs) <- "out"
              output$out_coefs <- out_coefs
            }

            if (!is.null(out_smooth)) {
              class(out_smooth) <- "out"
              output$out_smooth <- out_smooth
            }

            if (!is.null(output)) class(output) <- "out_all"
          }  # end n_bins==1
        }  #  end text output

      }  #  end do_plot

      else {
        output <- NULL
      }

  }  # end all other analyses
  # -------------------------


  # terminate pdf graphics system if used
  if (!is.null(pdf_file)) {
    dev.off()
    if (!quiet && df.name!="NULL") .showfile(pdf_file, "Plot")
  }

  # reset
  options(xname=NULL)
  options(yname=NULL)
  options(facet1name=NULL)
  options(facet2name=NULL)
  options(byname=NULL)

  # display text output from Plot() unless turned off
  # T.type is the type of Trellis plot, otherwise is NULL
  if (!quiet && n_bins==1) {
    if (Trellis) {  # only VBS plots have output processed here
      if (!(T.type %in% c("cont", "cont_cat"))) outp <- FALSE
    }
    if (n.x_var > 1  &&  y.miss) outp <- FALSE  # scatterplot matrix

    if (outp) {
      if (!is.null(output)) print(output)

      # out_y.frcst is a multi times series, cannot display with class out
      if (time_ahead > 0) {  # did a forecast
        if (!is.null(out_fitted)) {
          print(out_fitted)
          cat("\n")
        }
        if (!is.null(out_y.frcst)) print(out_y.frcst) 
      }  # end time_ahead > 0
    }  # end outp
  }

  if (!is.null(out_fitted)) output$out_fitted <- out_fitted
  if (!is.null(out_y.frcst)) {
    output$out_y.frcst <- out_y.frcst
    cat("\n")
  }

  if (!is.null(output)) return(invisible(output))
}
