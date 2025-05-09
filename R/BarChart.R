BarChart <-
function(x=NULL, y=NULL, by=NULL, data=d, filter=NULL,

        stat=c("mean", "sum", "sd", "deviation", "min", "median", "max"),
        stat_x=c("count", "proportion"),

        facet1=NULL, n_row=NULL, n_col=NULL, aspect="fill",

        horiz=FALSE, sort=c("0", "-", "+"),
        beside=FALSE, stack100=FALSE,
        gap=NULL, scale_y=NULL, one_plot=NULL,

        theme=getOption("theme"),
        fill=NULL,
        color=getOption("bar_color_discrete"),
        transparency=getOption("trans_bar_fill"),
        fill_split=NULL, fill_scaled=FALSE, fill_chroma=75,

        labels=c("%", "input", "prop", "off"),
        labels_position=c("in","out"),
        labels_color="white",
        labels_size=0.75,
        labels_decimals=NULL,
        labels_cut=NULL,

        xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
        lab_adjust=c(0,0), margin_adjust=c(0,0,0,0),
        pad_y_min=0, pad_y_max=0,

        rotate_x=getOption("rotate_x"), break_x=NULL,
        offset=getOption("offset"),
        axis_fmt=c("K", ",", ".", ""), axis_x_pre="", axis_y_pre="",
        label_max=100,

        legend_title=NULL, legend_position="right_margin",
        legend_labels=NULL, legend_horiz=FALSE,
        legend_size=NULL, legend_abbrev=10, legend_adjust=0,

        add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,

        quiet=getOption("quiet"), do_plot=TRUE,
        pdf_file=NULL, width=6.5, height=6,
        digits_d=NULL, out_size=80,

        n_cat=getOption("n_cat"), value_labels=NULL,
        rows=NULL, by1=NULL,

        eval_df=NULL, ...) {


  labels_position <- match.arg(labels_position)

  # Note: if fill contains getColors() call, fill already evaluated
  fill.name <- deparse(substitute(fill))

  options(xname = NULL)
  options(yname = NULL)
  options(byname = NULL)

  # get parameter names passed in function call, does not evaluate the arg
  nms <- names(as.list(match.call()))
  if (!is.null(nms)) {
    if ("facet2" %in% nms) {
      cat("\n"); stop(call.=FALSE, "\n------\n",
        "parameter  facet2  not applicable to BarChart\n\n")
    }
  }


# old stuff ---------------------------------------------------------------

  if (!missing(rows))
    message(">>> Parameter  rows  renamed to:  filter.\n",
            "    Change to  filter,  rows  will stop working in the future.\n")

  if (!missing(n_cat) || !missing(value_labels)) {
    message(">>> Parameters  n_cat  and  value_labels  will no longer ",
            "work in the future.\n",
             "    Better to convert a categorical integer variable to ",
             "a factor.\n")
  }

  if (deparse(substitute(fill)) == "(count)") {
    message(">>> Now set to TRUE the more general parameter:  fill_scale.\n\n")
    fill <- NULL
  }


  dots <- list(...)
  n.values <- 0
  if (length(dots) > 0) {
    for (i in seq_along(dots)) {
      if (grepl("values", names(dots)[i], fixed=TRUE)) {
        n.values <- n.values + 1
        if (n.values == 1)
          message(">>> Parameters  values, values_color, etc. now ",
                  "renamed to:  labels, labels_color, etc.\n",
                  "    Old parameter names will stop working in the future.\n")
        if (names(dots)[i] == "values") labels <- dots[[i]]
        if (names(dots)[i] == "values_color") labels_color <- dots[[i]]
        if (names(dots)[i] == "values_size") labels_size <- dots[[i]]
        if (names(dots)[i] == "values_digits") labels_decimals <- dots[[i]]
        if (names(dots)[i] == "values_position") labels_position <- dots[[i]]
        if (names(dots)[i] == "values_cut") labels_cut <- dots[[i]]
      }
      if (names(dots)[i] == "addtop") pad_y_max <- dots[[i]]
      if (names(dots)[i] == "add_top") pad_y_max <- dots[[i]]
      if (names(dots)[i] == "stat_yx") stat <- dots[[i]]
      if (grepl(".", names(dots)[i], fixed=TRUE)) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)  # dot to _
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }

  if (!is.null(stat)) if (stat[1] == "proportion") {
    cat("\n"); stop(call.=FALSE, "\n------\n",
      "now use parameter  stat_x  for \"proportion\" \n",
      "  \"proportion\" only applies when there is no y numeric variable\n\n")
  }

  facet1 <- .newparam(missing(by1), substitute(by1), "by1",
                      missing(facet1), substitute(facet1), "facet1")
  if (!is.null(facet1)) data[[as.character(facet1)]]  # extract facet1 from data

  facet1.miss <- ifelse (is.null(facet1), TRUE, FALSE)

  Trellis <- ifelse(!facet1.miss, TRUE, FALSE)


# set variables -----------------------------------------------------------

  do.plot <- TRUE

  trans <- transparency

  if (fill_scaled  &&  is.null(fill_split)) fill_split <- 0

  if (nzchar(axis_fmt[1])) axis_fmt <- match.arg(axis_fmt)

  fill.miss <- ifelse (missing(fill), TRUE, FALSE)
  color.miss <- ifelse (missing(color), TRUE, FALSE)
  horiz.miss <- ifelse (missing(horiz), TRUE, FALSE)
  y.miss <- ifelse (missing(y), TRUE, FALSE)
  by.miss <- ifelse (missing(by), TRUE, FALSE)
  for (i in 1:length(color)) if (color[i] == "off") color[i] <- "transparent"

  sort.miss <- ifelse (missing(sort), TRUE, FALSE)
  sort <- match.arg(sort)
  labels.miss <- ifelse (missing(labels), TRUE, FALSE)
  labels <- match.arg(labels)

  stat.miss <- ifelse (missing(stat), TRUE, FALSE)
  if (stat.miss) stat <- NULL
  if (!is.null(stat[1])) stat <- match.arg(stat)  # if condition for shiny
  stat_x <- match.arg(stat_x)



# set some parameter values -----------------------------------------------

  proportion <- ifelse (stat_x[1] == "proportion", TRUE, FALSE)  # make stat_x

  if (horiz) {
    if (sort == "+") sort <- "x"
    if (sort == "-") sort <- "+"
    if (sort == "x") sort <- "-"
  }

  if (missing(break_x))
    break_x <- ifelse (!horiz && rotate_x==0, TRUE, FALSE)

  xlab.adj <- lab_adjust[1];   ylab.adj <- lab_adjust[2]
  tm.adj <- margin_adjust[1];  rm.adj <- margin_adjust[2]
  bm.adj <- margin_adjust[3];  lm.adj <- margin_adjust[4]

  lm.adj <- lm.adj + .1  # pull these margins back a bit for bc
  bm.adj <- bm.adj + .1

  if (missing(labels_color)) {
    labels_color <- "white"
    if (labels_position == "out") labels_color <- getOption("axis.text.color")
  }

  # ensure valid parameter values
  .bcParamValid(y.miss, by.miss, facet1.miss, Trellis, sort,
                fill_split, fill_scaled, fill_chroma, theme,
                fill.miss, labels_position, stat.miss)



# data frame stuff --------------------------------------------------------

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
  # df.name is NULL if from shiny interact(), not from user-written shiny code
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


# establish if a data frame, otherwise identify variable(s)----------------
  # x can be missing entirely, with a data frame passed instead
  # if x a vector, then x.name not in data, but also not in global


  x.call <- NULL

  if (is.null(x.name)) x.name <- ""
  if (x.name %in% c("row_names", "row.names")) {
    # retain order of row names, otherwise will be alphabetical
    x.call <- factor(row.names(data), levels=row.names(data))
    if (is.null(xlab)) xlab <- ""  # unless specified, drop the axis label
  }

  else if (!missing(x)) {

    if (!x.in.global) {
      if (eval_df) {
        if (!mydata.ok) if (!shiny) .nodf(df.name)  # check to see if df exists
        .xcheck(x.name, df.name, names(data))  # x-vars in df?
      }

      data.vars <- as.list(seq_along(data))
      names(data.vars) <- names(data)
      ind <- eval(substitute(x), envir=data.vars)  # col num of each var
      if (length(ind) > 1) data <- data[, ind]  # x a vars list, no by vars
      if (length(ind) == 1) x.call <- eval(substitute(data$x))  # x is 1 var
    }

    else {  # x is in the global environment (vector, matrix or data frame)
      if (is.data.frame(x))  # x a data frame
        data <- x
      else {  # x a vector or matrix in global
        if (exists(x.name, where=.GlobalEnv)) if (is.matrix(x)) {
          x.name <- xlab
          xlab <- NULL
          by.name <- legend_title
          options(xname = x.name)
          options(byname = by.name)
        }
        x.call <- x
        if (is.function(x.call)) x.call <- eval(substitute(data$x))
      }
    }

    # if read from console with text parameter can insert extra space at front
    if (is.factor(x.call))
      if (nchar(levels(x.call)[1]) > 5)
        levels(x.call) <- trimws(levels(x.call), which="left")
    else if (is.character(x.call))
      if (nchar(x.call[1]) > 5) x.call <- trimws(x.call, which="left")
  }  # !missing x


  # -------------------------------------------------
  # -------------------------------------------------
  # x is a single var, not a data frame or a var list
  if (!is.null(x.call)) {
    lx.u <- length(unique(x.call))  # includes NA values

    # evaluate by
    #------------
    # cannot directly evaluate is.null(by) if by is present as a variable
    # so instead process as below to either get by.call or it is NULL
    # can get by.name
    if (!by.miss) {

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

    }  # end not missing by

    else
      by.call <- NULL

    if (labels_position == "out"  &&  !is.null(by.call)  &&  !beside) {
      cat("\n"); stop(call.=FALSE, "\n------\n",
        "labels_position=\"out\" not meaningful for a  by  variable\n",
        "  without beside=TRUE\n\n")
    }


    # evaluate y
    y.name <- deparse(substitute(y), width.cutoff = 120L)
    options(yname = y.name)

    if (!is.null(y.name))
      y.in.global <- .in.global(y.name, quiet)  # in global?, also vars list
    else
      y.in.global <- FALSE

    if (!y.in.global)  {
      if (df.name == "NULL")  {  # from shiny
        y.in.global <- TRUE
      }
    }

    eval_df <- !y.in.global

    # if not missing, then must be aggregated data
    #-------------
    if (!missing(y)) {  # assign y.call from data or from global

      # see if var exists in data frame, if y not in global Env or fun call
      if (eval_df)
        if (!y.in.global) .xcheck(y.name, df.name, names(data))
      if (!y.in.global)
        y.call <- eval(substitute(data$y))
      else {  # vars that are function names get assigned to global
        y.call <- y
        if (is.function(y.call)) y.call <- eval(substitute(data$y))
      }

    }  # end !missing(y)
    else
      y.call <- NULL


    # evaluate facet1
    #-------------
    if (!missing(facet1)) {

      # get actual variable name before potential call of data$x
      facet1.name <- deparse(substitute(facet1))
      options(facet1name = facet1.name)

      # get conditions and check for data existing
      in.global <- .in.global(facet1.name, quiet)

      # see if var exists in data frame, if x not in global Env or fun call
      if (!missing(x) && !in.global)
          .xcheck(facet1.name, df.name, names(data))

      if (!in.global)
        facet1.call <- eval(substitute(data$facet1))
      else {  # vars that are function names get assigned to global
        facet1.call <- facet1
        if (is.function(facet1.call))
          facet1.call <- eval(substitute(data$facet1))
      }

      if (!is.factor(facet1.call)) facet1.call <- factor(facet1.call)
    }

    else
      facet1.call <- NULL


    # evaluate specified fill (NULL, numeric constant, or a variable)
    #--------------

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

      # evaluate getColors at the time of the function call
      # re-evaluate here by setting fill with the specified value of n
      if (substr(fill.name, 1, 9) == "getColors")
        fill <- .do_getColors(fill.name, lx.u)
    }  # end !fill.miss


    # ------------------------------------------------------------
    # -----------  x, y, and by variables established ------------
    # ------------------------------------------------------------

    # no missing data in the analysis, using na.omit
    n.x <- length(unique(na.omit(x.call)))
    n.by <- ifelse (!by.miss, length(unique(na.omit(by.call))), 0)
    n.levels <- ifelse (by.miss || is.null(by.call), n.x, n.by)

    # -----------------------
    # process stat parameter

    if (is.null(by.call))
      is.smry_tbl <- ifelse (lx.u < (length(x.call)), FALSE, TRUE)
    else {
      lby.u <- length(unique(by.call))  # includes NA values
      is.smry_tbl <- ifelse (lx.u*lby.u < length(by.call), FALSE, TRUE)
    }

    # set labels default for aggregated data 
    if (labels.miss) {
      if (!stat.miss || is.smry_tbl)
        labels <- "input"
    }

    if (!is.smry_tbl && !is.null(y.call) && stat.miss) {
      cat("\n"); stop(call.=FALSE, "\n------\n",
        "The data are not a summary table, and you have a ",
        "numerical variable,\n",
        "    y = ", y.name, "\n",
        "so need to specify a value of  stat  to define the aggregation of\n",
        y.name, ", such as stat=\"mean\".\n\n")
    }

    if (!is.null(y.call)) {  # a y variable present
      if (is.smry_tbl) {  # a summary table

        if (!stat.miss  &&  is.smry_tbl) { # y and a summary table, no stat
          cat("\n"); stop(call.=FALSE, "\n------\n",
            "The data are a summary table, so do not specify a value of\n",
            "  stat  as the data aggregation has already been done\n\n")
        }

        if (sum(is.na(x.call)) > 0 ||
              sum(is.na(by.call)) > 0 ||
              sum(is.na(y.call)) > 0)   {
#             ok <- is.finite(x.call) & is.finite(by.call) & is.finite(y.call)
            cat("\n"); stop(call.=FALSE, "\n------\n",
              "When reading a summary table, missing data not allowed.\n\n")
        }
      }  # end is summary table

    }  # a y variable



    # -----------------------
    if (Trellis && do.plot) {

      # facet2 not currently available
      .bar.lattice(x.call, facet1.call, facet2=NULL, n_row, n_col, aspect,
                   proportion,
                   fill, color, trans, size.pt=NULL, xlab, ylab, main,
                   rotate_x, offset,
                   axis_fmt, axis_x_pre, axis_y_pre,
                   width, height, pdf_file,
                   segments_x=NULL, breaks=NULL, T.type="bar", quiet)
    }

    # -----------------------
    else {  # not Trellis

      # set up pdf_file if needed
      if (!is.null(pdf_file)) {
        if (!grepl(".pdf", pdf_file))
          pdf_file <- paste(pdf_file, ".pdf", sep="")
        pdf(file=pdf_file, width=width, height=height, onefile=FALSE)
      }
      else {
        if (df.name != "NULL")  # not dev.new for shiny
            .opendev(pdf_file, width, height)
      }

      # y is present with raw data and stat not null
      if (!is.null(stat) && !is.null(y.call) && !is.smry_tbl) {
        n_cat <- 0

        # do stats here to the console output before reducing data
        if (!quiet) {

            txout <- ""
            if (missing(by)) {  # no show stats for one var when a by var
              options(yname = x.name)  # reverse x and y names, .ss.numeric
              options(xname = y.name)

              stats <- .ss.numeric(y.call, by=x.call, digits_d=digits_d,
                                   brief=TRUE, y.name=x.name)
              txout <- stats$tx
              options(xname = x.name)  # reverse back
              options(yname = y.name)
            }

          class(txout) <- "out"

          output <- list(out_txt=txout)
          class(output) <- "out_all"
          print(output)
        }  # end !quiet

        # get summary table from the data according to the stats parameter
        # Logical vector: TRUE where neither x.call nor y.call is NA
        n.xbefore <- length(x.call)
        if (is.null(by.call))
          keep <- !is.na(x.call) & !is.na(y.call)
        else
          keep <- !is.na(x.call) & !is.na(y.call) & !is.na(by.call)
        x.call <- x.call[keep]
        y.call <- y.call[keep]
        if (!is.null(by.call)) by.call <- by.call[keep]
        n.xafter <- length(x.call)
        n.m <- n.xbefore - n.xafter
        if (n.m > 0) cat("\nRows of data removed due to missing data:",
                         n.m, "\n\n")

        stat_out <- .bc.stat(x.call, y.call, by.call, stat, y.name)
        out <- stat_out$out
        if (is.null(ylab)) ylab <- stat_out$ylab

        if (is.null(by.call)) {
          x.call <- factor(names(out))
          y.call <- as.vector(out)
        }
        else {
          x.call <- out[,1]
          by.call <- out[,2]
          y.call <- out[,3]
        }
      }  # end y is present for original data and stats not NULL


      bc <- .bc.main(x.call, y.call, by.call, stack100,
            fill, color, trans,
            fill_split, fill_scaled, fill_chroma, theme,
            horiz, gap, proportion, scale_y,
            xlab, ylab, main,
            value_labels, label_max, beside,
            rotate_x, offset, 
            axis_fmt, axis_x_pre, axis_y_pre,
            break_x, sort,
            labels, labels_color, labels_size, labels_decimals,
            labels_position, labels_cut,
            xlab.adj, ylab.adj, bm.adj, lm.adj, tm.adj, rm.adj,
            pad_y_min, pad_y_max,
            legend_title, legend_position, legend_labels,
            legend_horiz, legend_size, legend_abbrev, legend_adjust,
            add, x1, x2, y1, y2, out_size, digits_d, do_plot, quiet,
            shiny, ...)

      if (!is.null(pdf_file)) {
        dev.off()
        if (!quiet) .showfile(pdf_file, "BarChart")
      }

      return(invisible(bc))
    }  # not Trellis

  }  # end x is a single var
  # ------------------------
  # ------------------------


  # ---------------------------------------------------
  # x is a data frame or var list of multiple variables
  # ---------------------------------------------------
  else {
    if (!is.null(by) || !is.null(facet1)) {
      cat("\n"); stop(call.=FALSE, "\n------\n",
        "by and facet1 variables not available for multiple x variables\n\n")
    }

    # if labels not assigned, do default
    if (is.null(labels)) {
        labels <- getOption("labels")
        if (labels != "off") if (missing(y)) labels <- "input"
    }

    if (is.null(one_plot) || one_plot) {  # see if one_plot was specified
      one_plot <- TRUE

      mx.ln <- 0  # get variable with the most unique responses
      for (i in 1:ncol(data)) {
        ln <- length(na.omit(unique(data[,i])))
        if (ln > mx.ln) {
          ind <- i
          mx.ln <- ln
        }
      }
      uq <- na.omit(unique(data[,ind]))  # first largest set of responses
      uq.ln <- length(uq)  # length of largest set

      # all elements of smaller response set should be in full response set
      for (i in 1:ncol(data)) {
        if (length(setdiff(na.omit(unique(data[,i])), uq)) > 0) {
          one_plot <- FALSE
          break;
        }
      }
    }  # end determine one_plot

    # one_plot all x's stacked on a single plot, BPFM for bars
    if (one_plot) {
      y.call <- NULL
      by.call <- NULL
      if (is.null(legend_title)) legend_title <- "Responses"
      options(byname = "Responses")
      if (is.null(xlab)) xlab <- ""
      if (is.null(ylab)) ylab <- ""
      if (missing(labels_size)) labels_size <- 0.8 - (0.008 * ncol(data))
      if (sort.miss) sort <- "+"
      if (horiz.miss) horiz <- TRUE

      if (color.miss) color <- "transparent"

      # if fill not specified, define divergent palette and get colors
      if (fill.miss) {
        fill <- .get_fill(theme, diverge=TRUE)  # get divergent color names
        fill <-.color_range(fill, uq.ln)  # translate color names to colors
      }  # end fill.miss

      # evaluate getColors at the time of the function call
      # re-evaluate here by setting fill with the specified value of n
      else if (substr(fill.name, 1, 9) == "getColors")
        fill <- .do_getColors(fill.name, uq.ln)

      else  # not fill=getColors(...) but color names were specified
        fill <-.color_range(fill, uq.ln)  # translate color names to colors

      if (!is.null(pdf_file)) {
        if (!grepl(".pdf", pdf_file))
          pdf_file <- paste(pdf_file, ".pdf", sep="")
        pdf(file=pdf_file, width=width, height=height, onefile=FALSE)
      }
      else {
        if (df.name != "NULL")  # not dev.new for shiny
            .opendev(pdf_file, width, height)
      }

      bc <- .bc.main(data, y.call, by.call, stack100,
            fill, color, trans,
            fill_split, fill_scaled, fill_chroma, theme,
            horiz, gap, proportion, scale_y,
            xlab, ylab, main,
            value_labels, label_max, beside,
            rotate_x, offset,
            axis_fmt, axis_x_pre, axis_y_pre,
            break_x, sort,
            labels, labels_color, labels_size, labels_decimals,
            labels_position, labels_cut,
            xlab.adj, ylab.adj, bm.adj, lm.adj, tm.adj, rm.adj,
            pad_y_min, pad_y_max,
            legend_title, legend_position, legend_labels,
            legend_horiz, legend_size, legend_abbrev, legend_adjust,
            add, x1, x2, y1, y2, out_size, digits_d, do_plot,  quiet,
            shiny, ...)

      if (!is.null(pdf_file)) {
        dev.off()
        if (!quiet) .showfile(pdf_file, "BarChart")
      }
    }  # end one_plot


    # --------------------------------
    # analyze each x column separately

    else {
      bc.data.frame(data, stack100,
        fill, color, trans, fill_split,
        fill_scaled, fill_chroma, theme,
        horiz, gap, proportion, scale_y,
        xlab, ylab, main,
        value_labels, label_max, beside,
        rotate_x, offset, 
        axis_fmt, axis_x_pre, axis_y_pre,
        break_x, sort,
        labels, labels_color, labels_size, labels_decimals,
        labels_position, labels_cut,
        xlab.adj, ylab.adj, bm.adj, lm.adj, tm.adj, rm.adj,
        pad_y_min, pad_y_max,
        legend_title, legend_position, legend_labels,
        legend_horiz, legend_size, legend_abbrev, legend_adjust,
        out_size, do_plot, quiet, width, height, pdf_file, shiny, ...)
    }
  }

}
