Chart <-
function(x=NULL, by=NULL, y=NULL, data=d, filter=NULL,

        type=c("bar", "radar", "bubble", "dot", "pie", "icicle", "treemap"),
        hole=0.65,  # pie chart
        radius=0.35, power=0.5,  # bubble chart

        stat=c("mean", "sum", "sd", "deviation", "min", "median", "max"),
        stat_x=c("count", "proportion"),

        facet=NULL, n_row=NULL, n_col=NULL, aspect="fill",

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

        rotate_x=getOption("rotate_x"), rotate_y=getOption("rotate_y"),
        break_x=NULL, offset=getOption("offset"),
        axis_fmt=c("K", ",", ".", ""), axis_x_pre="", axis_y_pre="",
        label_max=100,

        legend_title=NULL, legend_position="right_margin",
        legend_labels=NULL, legend_horiz=FALSE,
        legend_size=NULL, legend_abbrev=10, legend_adjust=0,

        add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,

        quiet=getOption("quiet"), do_plot=TRUE, 
        use_plotly=getOption("lessR.use_plotly"),
        pdf_file=NULL, width=6.5, height=6,
        digits_d=NULL, out_size=80,

        n_cat=getOption("n_cat"), value_labels=NULL,
        rows=NULL, facet1=NULL,

        eval_df=NULL, fun_call=NULL, ...) {


  if (is.null(fun_call)) fun_call <- match.call()

  mc <- match.call()
  y.is.named <- "y" %in% names(mc)

  labels <- match.arg(labels)
  labels_position <- match.arg(labels_position)

  # Note: if fill contains getColors() call, fill already evaluated
  fill.name <- deparse(substitute(fill))

  if (length(type) == 1 && type == "sunburst")
    type <- "pie"
  else
    type <- match.arg(type)

  stat.miss <- ifelse (missing(stat), TRUE, FALSE)
  if (stat.miss) stat <- NULL
  if (!is.null(stat[1])) stat <- match.arg(stat)  # if condition for shiny
  stat_x <- match.arg(stat_x)

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

  if (!is.null(pdf_file) &&  type != "bar") {
    cat("\n"); stop(call.=FALSE, "\n------\n",
      "Saving PDF files only works for bar charts.\n\n")
  }

  if (type == "bubble"  &&  !is.null(facet)) {
      cat("\n"); stop(call.=FALSE, "\n------\n",
        "The  facet  option is not available for bubble plots.\n\n")
  }
  facet.miss <- ifelse (missing(facet), TRUE, FALSE)
  if (type != "dot") {  # for now, goes to XY(), where this is also listed
    if (!quiet && use_plotly && !(type == "bar" && !facet.miss)) {
      txt <- "[Interactive chart from the Plotly R package (Sievert, 2020)]"
      cat(txt, "\n\n")
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


  Trellis <- ifelse(!facet.miss && type == "bar", TRUE, FALSE)


# set variables -----------------------------------------------------------

  trans <- transparency

  if (fill_scaled  &&  is.null(fill_split)) fill_split <- 0

  if (nzchar(axis_fmt[1])) axis_fmt <- match.arg(axis_fmt)

  fill.miss <- ifelse (missing(fill), TRUE, FALSE)
  color.miss <- ifelse (missing(color), TRUE, FALSE)
  transparency.miss <- ifelse (missing(transparency), TRUE, FALSE)
  horiz.miss <- ifelse (missing(horiz), TRUE, FALSE)
  y.miss <- ifelse (missing(y), TRUE, FALSE)
  by.miss <- ifelse (missing(by), TRUE, FALSE)
  main.miss <- ifelse (missing(main), TRUE, FALSE)
  for (i in 1:length(color)) if (color[i] == "off") color[i] <- "transparent"

  sort.miss <- ifelse (missing(sort), TRUE, FALSE)
  sort <- match.arg(sort)
  labels.miss <- ifelse (missing(labels), TRUE, FALSE)
  labels <- match.arg(labels)


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
  .bcParamValid(y.miss, by.miss, facet.miss, Trellis, sort,
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

  if (type %in% c("pie", "icicle", "treemap")) x.raw <- x.call  # save data


  # -------------------------------------------------
  # -------------------------------------------------
  # x is a single var, not a data frame or a var list
  if (!is.null(x.call)) {


# --- resolve `by`, which can be a vector ---------------------------------

    by.miss <- ifelse(missing(by), TRUE, FALSE)

    if (!by.miss) {
      by_expr <- substitute(by)
      by_vars <- all.vars(by_expr)

      get_col <- function(nm) {
        # prefer data= if present; else fall back to global
        in_global <- if (df.name != "NULL") .in.global(nm, quiet) else TRUE
        if (!in_global) {
          .xcheck(nm, df.name, names(data))
          return(data[[nm]])
        } else {
          return(get(nm, envir = parent.frame()))
        }
      }

      if (length(by_vars) == 0L) {
        # already-evaluated object (rare); coerce to factor
        by.call <- try(eval(by_expr, envir = parent.frame()), silent = TRUE)
        if (inherits(by.call, "try-error")) by.call <- NULL
        if (!is.null(by.call) && !is.data.frame(by.call)) {
          if (!is.factor(by.call)) by.call <- factor(by.call)
          by.name <- deparse(by_expr)
        } else if (is.data.frame(by.call)) {
          for (nm in names(by.call))
            if (!is.factor(by.call[[nm]])) by.call[[nm]] <- factor(by.call[[nm]])
          by.name <- paste(names(by.call), collapse = " - ")
        }
      }
      else if (length(by_vars) == 1L) {
        by.call <- get_col(by_vars[1])
        if (!is.factor(by.call)) by.call <- factor(by.call)
        by.name <- by_vars[1]
      }
      else {
        # vector of by's -> data.frame of factors
        by_df <- lapply(by_vars, get_col)
        by_df <- lapply(by_df, function(v)
                        if (is.factor(v)) droplevels(v) else factor(v))
        by.call <- as.data.frame(by_df, check.names = FALSE)
        names(by.call) <- by_vars
        by.name <- paste(by_vars, collapse = " - ")
      }
      options(byname = by.name)

      # by  should be categorical or integer variable with <= 10 unique values
      v <- by.call[[1]]   # always a vector
      if (!is.character(v) && !is.factor(v) && !.is.num.cat(v, n_cat = 10)) {
        warning("\n\nThe 2nd argument is by= , which should be categorical. \n",
                by.name, " likely is not categorical. Maybe set this\n",
                "variable to  y= , which should be a numerical variable.\n\n")
      }

      if (labels_position == "out"  &&  !beside) {
        warning("\n"); stop(call.=FALSE, "\n------\n",
          "labels_position=\"out\" not meaningful for a  by  variable\n",
          "  without beside=TRUE\n\n")
      }
    }  # end not by.miss 

    else {
      by.call <- NULL
      by.name <- NULL
    }

    if (type %in% c("pie", "icicle", "treemap")) by.raw <- by.call  # save data


# --- resolve y ------------------------------------------------------

    y.name <- deparse(substitute(y), width.cutoff = 120L)  # can be NULL
    options(yname = y.name)

  # process row.names if specified, applicable to type="dot"
  if (y.name %in% c("row_names", "row.names")) {
    # retain order of row names, otherwise will be alphabetical
    y.call <- data.frame(factor(row.names(data), levels=row.names(data)))
    if (is.null(ylab)) ylab <- ""  # unless specified, drop the axis label
  }

  else {
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
    if (!missing(y)) {  # assign y.call from data or from global
      y.name <- deparse(substitute(y))

      # see if var exists in data frame, if y not in global Env or fun call
      if (eval_df)
        if (!y.in.global) .xcheck(y.name, df.name, names(data))
      if (!y.in.global)
        y.call <- eval(substitute(data$y))
      else {  # vars that are function names get assigned to global
        y.call <- y
        if (is.function(y.call)) y.call <- eval(substitute(data$y))
      }
      if (is.null(digits_d)) digits_d <- .max.dd(y.call)
    }  # end !missing(y)

    else {
      y.name <- "Count"
      y.call <- NULL
      if (is.null(digits_d)) digits_d <- 0  # y will be counts
    }
  }  # end !row_name

  if (type %in% c("pie", "icicle", "treemap")) y.raw <- y.call  # save data


# --- resolve facet --------------------------------------------------

  facet.call <- NULL
  facet.name <- NULL

  if (!missing(facet)) {
    facet_expr <- substitute(facet)
    facet_var  <- deparse(facet_expr)

    get_col <- function(nm) {
      in_global <- if (df.name != "NULL") .in.global(nm, quiet) else TRUE
      if (!in_global) { .xcheck(nm, df.name, names(data)); data[[nm]] }
      else            { get(nm, envir = parent.frame()) }
    }

    v <- get_col(facet_var)

    # Ensure factor with dropped unused levels (matches old behavior)
    facet.call <- if (is.factor(v)) droplevels(v) else factor(v)

    facet.name <- facet_var

    ## IMPORTANT: use the legacy option name expected by .bar.lattice()
    options(facet1name = facet.name)
  }
 
  if (type %in% c("pie", "icicle", "treemap")) facet.raw <- facet.call


# ------------------------------------------------------------
# -----------  x, y, by, and facet variables established -----
# ------------------------------------------------------------

# suggestions -------------------------------------------------------------

    txsug <- ""
    if (getOption("suggest")) {
      # function call, with last ) removed for suggestions
      fncl <- .fun_call.deparse(fun_call)  # class call to class character
      fncl <- gsub(")$", "", fncl)  # get function call less closing )
      fncl <- gsub(" = ", "=", fncl)
      txsug <- ">>> Suggestions  or  enter: style(suggest=FALSE)"

      if (!is.null(by.call))
        fc <- paste("Chart(", x.name, ", by=", by.name, ",", sep="")
      else
        fc <- paste("Chart(", x.name, sep="")

      if (!grepl("radar", fncl)) {
        txt <- " type=\"radar\""  # many options"
        cmt <- "  # Plotly radar chart"
        txsug <- paste(txsug, "\n", fc, txt, ")", cmt, "\n", sep="")
      }

      if (!grepl("treemap", fncl)) {
        txt <- " type=\"treemap\""  # many options"
        cmt <- "  # Plotly treemap chart"
        txsug <- paste(txsug, fc, txt, ")", cmt, "\n", sep="")
      }

      if (!grepl("pie", fncl)) {
        txt <- " type=\"pie\""  # many options"
        cmt <- "  # Plotly pie/sunburst chart"
        txsug <- paste(txsug, fc, txt, ")", cmt, "\n", sep="")
      }

      if (!grepl("icicle", fncl)) {
        txt <- " type=\"icicle\""  # many options"
        cmt <- "  # Plotly icicle chart"
        txsug <- paste(txsug, fc, txt, ")", cmt, "\n", sep="")
      }

      if (!grepl("bubble", fncl)) {
        txt <- " type=\"bubble\""  # many options"
        cmt <- "  # Plotly bubble chart"
        txsug <- paste(txsug, fc, txt, ")", cmt, "\n", sep="")
      }

      txsug <- paste(txsug, "\n")

       class(txsug) <- "out"
       print(txsug)
    }  # end suggest


  # is a numerical, continuous variable in the second or by position?
  # assumes there is only one by, and n.by is the number of levels
  n.by <- ifelse (!is.null(by.call), length(unique(by.call)), 0)


  if (is.null(by.call)) by.name <- NULL
  if (!is.null(by.call) && is.numeric(by.call) && !y.is.named) {
    if (!stat.miss && y.miss) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Parameter  stat  requires a numerical y-variable to transform.\n\n",
        "Perhaps you have a numerical variable that you wish to transform.\n",
        "If so, then you now need to provide the label:  y = ", by.name, "\n\n",
        "Or, if the variable is the 2nd categorical variable in the analysis\n",
        "  then label as:  by = ", by.name, "\n")
    }
  }


# not pre-aggregated data -------------------------------------------------

    lx.u <- length(unique(x.call))
    lx <- ifelse (is.data.frame(x.call), nrow(x.call), length(x.call))
    if (is.null(by.call))
      is.agg <- ifelse (lx.u < lx, FALSE, TRUE)
    else {
      lby.u <- length(unique(by.call))
      lby <- ifelse (is.data.frame(by.call), nrow(by.call), length(by.call))
      is.agg <- ifelse (lx.u*lby.u < lby, FALSE, TRUE)
    }

  # set labels default for aggregated data 
  if (labels.miss && (!stat.miss || is.agg)) labels <- "input"


## ---- Missing data removal: x, y, by (possibly multi-column), facet ----

    cc <- .drop_casewise_missing(  # build data frame with relevant vars
      x.call    = x.call,
      y.call    = y.call,
      by.call   = by.call,
      facet.call= facet.call
    )

    if (!is.null(cc) && !all(cc)) {

      # x.call: vector OR matrix/data.frame
      if (!is.null(x.call)) {
        if (is.matrix(x.call) || is.data.frame(x.call)) {
          x.call <- x.call[cc, , drop = FALSE]
        } else {
          x.call <- x.call[cc]
        }
      }

      # y.call: vector
      if (!is.null(y.call)) {
        y.call <- y.call[cc]
      }

      # by.call: vector OR matrix/data.frame
      if (!is.null(by.call)) {
        if (is.matrix(by.call) || is.data.frame(by.call)) {
          by.call <- by.call[cc, , drop = FALSE]
        } else {
          by.call <- by.call[cc]
        }
      }

      # facet.call: vector OR matrix/data.frame
      if (!is.null(facet.call)) {
        if (is.matrix(facet.call) || is.data.frame(facet.call)) {
          facet.call <- facet.call[cc, , drop = FALSE]
        } else {
          facet.call <- facet.call[cc]
        }
      }
    }


# check conditions when a y variable is present ---------------------------

    if (!is.null(y.call)) {  # a y variable present
      if (is.agg) {  # a summary table
        if (!stat.miss  &&  is.agg) { # y and a summary table, no stat
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
      }  # is a summary table
      else {
        if (!is.agg && stat.miss && type != "dot") {
          cat("\n"); stop(call.=FALSE, "\n------\n",
            "The data are not a summary (pivot) table, and you have a ",
            "numerical variable,\n",
            "    y = ", y.name, "\n",
            "so need to specify a value of  stat  to define the aggregation of\n",
            y.name, ", such as stat=\"mean\".\n\n")
        }
      }  # end is not a summary table
    }  # a y variable


# evaluate specified fill (NULL, numeric constant, or a variable) ---------

  if (!is.null(fill)) {
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
    if (substr(fill.name[1], 1, 9) == "getColors")
      fill <- .do_getColors(fill.name, lx.u)
  }  # end fill is present

  n.x <- length(unique(data[[x.name]]))
  if (is.null(fill))
    fill <- .color_range(.get_fill(theme), max(n.x, n.by))
  else
    fill <- .color_range(fill, max(n.x, n.by))


# Trellis plots for bar plots ---------------------------------------------

  if (Trellis && do_plot) {

    # facet2 not currently available
    .bar.lattice(x.call, facet.call, facet2=NULL, n_row, n_col, aspect,
                 proportion, fill, color, trans, size.pt=NULL,
                 xlab, ylab, main, rotate_x, offset,
                 axis_fmt, axis_x_pre, axis_y_pre,
                 width, height, pdf_file,
                 segments_x=NULL, breaks=NULL, T.type="bar", quiet)

    return(invisible(NULL))
  }


# set up pdf_file if needed -----------------------------------------------
    plotly_types <- c("pie", "radar", "bubble", "sunburst", "treemap", "icicle")
    is_plotly_type <- type %in% plotly_types
    if (!is.null(pdf_file) && is_plotly_type) {
    warning(
      "pdf_file is currently not supported for Plotly-based Chart() types (",
      paste(sort(unique(type)), collapse = ", "),
      "). The pdf_file argument will be ignored for this call."
    )
    pdf_file <- NULL
  }
  if (!is.null(pdf_file)) {
    if (!grepl(".pdf", pdf_file))
      pdf_file <- paste(pdf_file, ".pdf", sep="")
    pdf(file=pdf_file, width=width, height=height, onefile=FALSE)
  }
  else {
    if (df.name != "NULL")  # not dev.new for shiny
        .opendev(pdf_file, width, height)
  }


# stat aggregation: y is present with raw data and stat not NULL ----------

  if (!is.agg) {
    if (!is.null(stat) && !is.null(y.call)) {  # y with stat
      n_cat <- 0

      ## ----- console stats BEFORE reducing data -------------------------
      if (!quiet) {
        txout <- ""
        ## only show the one-way numeric summary when there is
        ## truly only one variable (no by, no facet)
        if (is.null(by.call) && is.null(facet.call)) {  # <- allow for facets
          options(yname = x.name)  # reverse x and y names, .ss.numeric
          options(xname = y.name)

          stats <- .ss.numeric(y.call, by = x.call, digits_d = digits_d,
                               brief = TRUE, y.name = x.name)
          txout <- stats$tx

          options(xname = x.name)  # reverse back
          options(yname = y.name)
        }

        class(txout) <- "out"
        output <- list(out_txt = txout)
        class(output) <- "out_all"
        print(output)
      }  # end !quiet


      ## ----- aggregate --------------------------------------------------
      is.agg <- TRUE
      stat_out <- .stats(x.call, y.call, by.call, facet.call, stat, y.name)
      out <- stat_out$out  # aggregated data

      if (is.null(ylab)) {
        ylab   <- stat_out$ylab
        y.name <- ylab
      }

    # table already printed above
    if (is.null(by.call) && is.null(facet.call)) {  # 'out' is a named vector
      x.call <- factor(names(out))
      y.call <- as.vector(out)
    }

    else {  # by.call or facet.call is present, 'out' is a data.frame
      # build: y ~ group_cols + x

      ## all grouping columns (by + facet) are everything except x,y
      grp_cols <- setdiff(names(out), c("x", "y"))

      cat("Summary Table for", ylab, "\n\n")

      if (!length(grp_cols)) {
        x.tbl <- xtabs(y ~ x, data = out)
      } else {
        form  <- reformulate(c(grp_cols, "x"), response = "y")
        x.tbl <- xtabs(form, data = out)
      }

      .print_table(
        x.tbl    = x.tbl,
        x.name   = x.name,
        x.lbl    = x.lbl,
        by.name  = by.name,  # l Beachabel string, not the columns
        y.name   = y.name,
        stat     = stat,
        digits_d = digits_d
      )

      ## ----- Update x.call / by.call / facet.call / y.call ----------------
      ## After aggregation, downstream plotting should see the *reduced* data.

      # save original data for .hier.plotly() hover before aggregation
      x.raw <- x.call
      y.raw <- y.call
      by.raw <- by.call
      facet.raw <- facet.call

      x.call <- out[["x"]]
      y.call <- out[["y"]]

      ## how many by / facet variables did we start with?
      n_by <- if (is.null(by.call))
        0L
      else if (is.data.frame(by.call))
        ncol(by.call)
      else
        1L

      n_facet <- if (is.null(facet.call))
        0L
      else if (is.data.frame(facet.call))
        ncol(facet.call)
      else
        1L

      by_cols <- if (n_by > 0L)
        grp_cols[seq_len(n_by)]
      else
        character(0L)

      facet_cols <- if (n_facet > 0L)
        grp_cols[seq(from = n_by + 1L, length.out = n_facet)]
      else
        character(0L)

      ## by.call: NULL, vector, or data.frame depending on how many by vars
      if (!length(by_cols))
        by.call <- NULL
      else if (length(by_cols) == 1L)
        by.call <- out[[by_cols[1L]]]
      else
        by.call <- out[by_cols]

      ## facet.call: NULL, vector, or data.frame depending on how many facets
      if (!length(facet_cols))
        facet.call <- NULL
      else if (length(facet_cols) == 1L)
        facet.call <- out[[facet_cols[1L]]]
      else
        facet.call <- out[facet_cols]
    }
  }  # y is present for original data and stats not NULL


# else do table stats of counts -------------------------------------------

  else if (type != "bar") {  # do table of counts, done in bc.main for bar
    gl <- .getlabels(main=main, lab_cex=getOption("lab_cex"))
    x.name <- gl$xn; x.lbl <- gl$xl

    # build the 1-D / 2-D table strictly for console output & hover
    x.tbl <- .build_xtab(
      x        = x.call,
      y        = if (exists("y.call")) y.call else NULL,
      by       = by.call,
      facet    = facet.call,
      stat     = stat,
      is.agg   = is.agg,
      digits_d = digits_d
    )

    if (length(dim(x.tbl)) < 3) { # by not a vector 
      .print_table(  # print 1-D or 2-D, in zzz_plotly
        x.tbl   = x.tbl,
        x.name  = x.name,
        x.lbl   = x.lbl,
        by.name = if (is.null(by.call)) NULL else by.name,
        y.name  = if (!is.null(stat) && !is.null(y.call)) y.name else "Count",
        stat    = stat,
        digits_d = digits_d
      )
    }
  }
}  # end need to aggregate


# begin processing designated chart type ----------------------------------

  if (type == "bar") {
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
          add, x1, x2, y1, y2, out_size, do_plot, use_plotly, quiet,
          shiny, digits_d, ...)

    if (!is.null(pdf_file)) {
      dev.off()
      if (!quiet) .showfile(pdf_file, "BarChart")
    }

    return(invisible(bc))
  }


# Hierarchical chart -----------------------------------------------------

  else if (type %in% c("sunburst", "treemap", "icicle") ||
          (type == "pie" && !is.null(by.call))) {
    if (type == "pie") type <- "sunburst"

    plt <- .hier.plotly(
      x.call = x.raw,
      by.call = by.raw,
      facet.call = facet.raw,
      y.call = y.raw,
      x.name = x.name, by.name = by.name, facet.name = facet.name, y.name = y.name,
      type  = type,
      stat  = stat,
      fill=fill,
      border=color,
      digits_d = digits_d,
      facet_gap_x = 0.04,
      facet_gap_y = 0.11,
      facet_size  = 1.00,
      facet_title_y_base    = -0.018,  # down a bit into panel
      facet_title_row_shift = -0.003,  # additional downshift per lower row
      main = if (!missing(main)) main else NULL
    )

    if (.allow.interactive()) print(plt)
    return(invisible(plt))

  } 

  else if (type == "pie") {  # plain pie, no by, maybe facet

    if (is.null(by.call) && !facet.miss) {  # by already shown to not exist
      by.name    <- facet.name
      by.call    <- facet.call    # use the facet as the by for aggregation
      facet.name <- NULL
      facet.call <- NULL
    }

    plt <- .do.plotly(x.call, x.name, y.call, y.name, ylab, by.call, by.name,
               type, stat, fill, color, opacity=1-trans,
               hole, power, radius, ncols=NULL,
               labels, labels_position, labels_color, labels_size,
               labels_decimals, main_cex=1, main, main.miss, digits_d,
               is.agg, quiet)

    return(invisible(plt))
  }


# Bubble chart -----------------------------------------------------------

  else if (type == "bubble") {

    plt <- .do.plotly(x.call, x.name, y.call, y.name, ylab, by.call, by.name,
               type, stat, fill, color, opacity=1-trans,
               hole, power, radius, ncols=NULL,
               labels, labels_position, labels_color, labels_size,
               labels_decimals, main_cex=1, main, main.miss, digits_d,
               is.agg, quiet)

    return(invisible(plt))
  }


# Radar chart -------------------------------------------------------------

    else if (type == "radar") {

      n.x <- length(unique(x.call))
      if (n.x < 3) {  # Axes must define a polygon
        stop("\n\n",
          "radar(): The first categorical variable (x) must have at least ",
          "3 levels to form a polygon. ",
          "Found ", n.x, " levels for ", x.name, "."
        )
      }
      n.by <- length(unique(by.call))

      if (n.by == 1) {  # Must have at least two groups to compare
        stop("\n\n",
          "radar(): The second categorical variable (y) must have at least ",
          "2 levels to define multiple polygons. ",
          "Found ", n.by,  " levels for ", by.name, "."
        )
      }

      # stop when any cell is empty:\
      # not working now because NA's cause by.call and x.call different lengths
      if (!is.null(by.call)) {

        # ensure x is a vector
        xv <- if (is.data.frame(x.call)) x.call[[1L]] else x.call
        ok <- complete.cases(by.call, xv)

        tbl <- table(by.call[ok], xv[ok], useNA = "no")

        if (any(tbl == 0L)) {
          zeros <- which(tbl == 0L, arr.ind = TRUE)
          max_show <- 8L  # show up to a few examples
          show_idx <- seq_len(min(nrow(zeros), max_show))
          print(tbl)

          stop("\n\n",
               "radar(): One or more cells are missing, with 0 entries.\n",
               "Radar polygons assume each group has a value at every axis.\n",
               "Here, ", nrow(zeros), "  cells were found with count=0.\n\n",
               "Use a larger sample, reduce the number of levels,\n",
               " or choose a different chart.\n")
        }
      }

      if (transparency.miss && !is.null(by.call))
        trans <- 0.4

      plt <- .radar.plotly(
        x.call       = x.call,
        by.call      = by.call,
        facet.call   = facet.call,
        y.call       = y.call,
        x.name       = x.name,
        by.name      = by.name,
        facet.name   = facet.name,
        y.name       = y.name,
        stat         = stat,
        fill         = fill,
        border       = color,
        opacity      = 1 - trans,
        digits_d     = digits_d,
        # facet layout controls (same names as hierarchical for consistency)
        facet_size  = 1.00,
        facet_gap_x = 0.04,
        facet_gap_y = 0.11,
        facet_title_y_base    = -0.018,  # down a bit into panel
        facet_title_row_shift = -0.003  # additional downshift per lower row
      )

      if (.allow.interactive()) print(plt)
      return(invisible(plt))
    }


# Chart(x=, y=, ...,  type="dot") -> delegate to XY() for now -------------

    else if (type == "dot") {

      calls <- sys.calls()  # the function call
      top   <- calls[[1L]]
      fun   <- top[[1L]]  # function name: Chart
      if (identical(fun, quote(Chart)) || identical(fun, as.name("Chart"))) {

        # Reconstruct original user call
        #   Chart(Gender, y=row_names, type="dot", ...)
        fun_call <- match.call(expand.dots = TRUE)

        # Original x expression is the second argument
        x_expr <- fun_call[[2L]]

        # Get the by expression: prefer named 'by'
        # fall back to second positional arg
        y_expr <- fun_call$y
        if (is.null(y_expr) && length(fun_call) >= 3L) {
          # X(Salary, Gender, type="scatter") style
          y_expr <- fun_call[[3L]]
        }

        if (is.null(y_expr)) {
          stop("For Chart(..., type=\"scatter\"),\n",
               "Supply a grouping variable via the second argument or by=.")
        }

        # transform to XY(x = y, y = x, ...)
        fun_call[[1L]] <- as.name("XY")
        fun_call$x     <- y_expr
        fun_call$y     <- x_expr
        fun_call$type  <- "scatter"
        fun_call$ylab  <- ""

        # XY() creates the dot plot
        return(eval.parent(fun_call))
      }
    }

  }  # end x is a single var
  # ------------------------
  # ------------------------


# x is a data frame or var list of multiple variables ---------------------
# -------------------------------------------------------------------------

  else {
    if (!is.null(by) || !is.null(facet)) {
      cat("\n"); stop(call.=FALSE, "\n------\n",
        "by and facet variables not available for multiple x variables\n\n")
    }

    # if labels not assigned, do default
    if (is.null(labels)) {
        labels <- getOption("labels")
        if (labels != "off") if (missing(y)) labels <- "input"
    }

    if (type == "bubble") {  # vector bubble, BPFM

      # get labels just for subset data matrix
      l <- attr(data, which="variable.labels")
      nm <- names(x.call)
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

    if (is.null(fill)) fill <- getOption("bar_fill_cont")
    if (transparency.miss) trans <- 0.4

    .dpmat.main(
      x=data[, , drop = FALSE], l=mylabs,
      sort_type=sort,

      fill=fill, color=getOption("pt_color"), col.bg=getOption("panel_fill"),
      trans=trans, shape_pts="bubble", col.box=getOption("panel_color"),
      col.low=NULL, col.hi=NULL,

      xy_ticks=TRUE,
      xlab=xlab, ylab=ylab, main=main, sub=sub, cex=1,

      radius=radius, power=power,
      size_cut=1,
      txt_color=getOption("bubble_text_color"),

      bm.adj=0, lm.adj=0, tm.adj=0, rm.adj=0,

      value_labels=value_labels,
      rotate_x=rotate_x, rotate_y=rotate_y, offset=offset, quiet=quiet,

      do_plot=do_plot, fun_call=fun_call,
      ...)
    }  # end vector bubble

    else if (type == "bar") {  # vector bar

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

    # one_plot all x's stacked on a single plot, like a BPFM for bars
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
            add, x1, x2, y1, y2, out_size, do_plot, use_plotly=FALSE,
            quiet, shiny, digits_d, ...)

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
        out_size, do_plot, use_plotly=FALSE, quiet,
        digits_d, shiny, pdf_file, width, height, ...)
    }
  }
  }

}
