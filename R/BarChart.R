BarChart <-
function(x=NULL, y=NULL, by=NULL, data=d,
        rows=NULL, top=NULL,
        stat=c("mean", "sum", "sd", "deviation", "min", "median", "max"),
        stat_x=c("count", "proportion"),

        by1=NULL, n_row=NULL, n_col=NULL, aspect="fill",

        theme=getOption("theme"),
        fill=NULL,
        color=getOption("bar_color_discrete"),
        transparency=getOption("trans_bar_fill"),
        fill_split=NULL,

        values=c("%", "input", "off"),
        values_color=getOption("values_color"),
        values_size=getOption("values_size"),
        values_digits=getOption("values_digits"),
        values_position=getOption("values_position"),
        values_cut=NULL,

        horiz=FALSE, sort=c("0", "-", "+"),
        beside=FALSE, stack100=FALSE,
        gap=NULL, scale_y=NULL, one_plot=NULL,

        xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
        lab_adjust=c(0,0), margin_adjust=c(0,0,0,0),
        pad_y_min=0, pad_y_max=0,

        rotate_x=getOption("rotate_x"), break_x=NULL,    
        offset=getOption("offset"),
        label_max=100,

        legend_title=NULL, legend_position="right_margin",
        legend_labels=NULL, legend_horiz=FALSE,
        legend_size=NULL, legend_abbrev=NULL, legend_adjust=0,

        add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,

        quiet=getOption("quiet"), do_plot=TRUE,
        pdf_file=NULL, width=6.5, height=6, 
        digits_d=NULL, out_size=80, 

        n_cat=getOption("n_cat"), value_labels=NULL, 

        eval_df=NULL, ...) {


  # Note: if fill contains getColors() call, fill already evaluated
  fill.name <- deparse(substitute(fill))

  options(xname = NULL)
  options(yname = NULL)
  options(byname = NULL)

  # get parameter names passed in function call, does not evaluate the arg
  nms <- names(as.list(match.call()))
  if (!is.null(nms)) {
    if ("by2" %in% nms) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "parameter  by2  not applicable to BarChart\n\n")
    }
  }

  # ------------ Old Stuff ----------------------------------
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    for (i in 1:length(dots)) {
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
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "now use parameter  stat_x  for \"proportion\" \n",
      "  \"proportion\" only applies when there is no y numeric variable\n\n")
  }
  # ---------------------------------------------------------

  trans <- transparency

  fill.miss <- ifelse (missing(fill), TRUE, FALSE)
  color.miss <- ifelse (missing(color), TRUE, FALSE)
  horiz.miss <- ifelse (missing(horiz), TRUE, FALSE)
  y.miss <- ifelse (missing(y), TRUE, FALSE)
  by.miss <- ifelse (missing(by), TRUE, FALSE)
  by1.miss <- ifelse (missing(by1), TRUE, FALSE)
  for (i in 1:length(color)) if (color[i] == "off") color[i] <- "transparent"

  sort.miss <- ifelse (missing(sort), TRUE, FALSE)
  sort <- match.arg(sort)
  values <- match.arg(values)

  stat.miss <- ifelse (missing(stat), TRUE, FALSE)
  if (stat.miss) stat <- NULL
  if (!is.null(stat[1])) stat <- match.arg(stat)  # if condition for shiny
  stat_x <- match.arg(stat_x)
  

  #------------- Set Some Parameter Values --------------------------

  proportion <- ifelse (stat_x[1] == "proportion", TRUE, FALSE)  # make stat_x

  if (horiz) {
    if (sort == "+") sort <- "x" 
    if (sort == "-") sort <- "+" 
    if (sort == "x") sort <- "-" 
  }

  if (theme != getOption("theme")) {  # given theme not the current theme
    sty <- style(theme, reset=FALSE)
    fill <- sty$bar$bar_fill_discrete
    color <- sty$bar$color
    trans <- sty$bar$trans_fill
    if (is.null(trans)) trans <- 0.1  # kludge, should not be NULL
  }

  if (missing(break_x)) 
    break_x <- ifelse (!horiz && rotate_x==0, TRUE, FALSE)

  xlab.adj <- lab_adjust[1];   ylab.adj <- lab_adjust[2]
  tm.adj <- margin_adjust[1];  rm.adj <- margin_adjust[2]
  bm.adj <- margin_adjust[3];  lm.adj <- margin_adjust[4]

  lm.adj <- lm.adj + .1  # pull these margins back a bit for bc
  bm.adj <- bm.adj + .1

  if (missing(values_color)) {
    values_color <- "white"
    if (values_position == "out") values_color <- getOption("axis.text.color")
  }

  if (is.null(values_digits)) {
    if (values == "%") values_digits <- 0
    if (values == "proportion") values_digits <- 2
  }

  Trellis <- ifelse (!missing(by1), TRUE, FALSE)
  do.plot <- TRUE


  #------------- Ensure Valid Parameter Values ---------------------

  if (!by.miss  &&  !by1.miss) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "by  and  by1  parameters not currently available at the same time.\n\n")
  }

  if (Trellis  &&  sort != "0") {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Sort not applicable to Trellis plots\n\n")
  }

  if (!is.null(fill_split)  &&  !fill.miss) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "fill_split assigns its own color based on the theme\n",
      "  either drop  fill_split  or drop  fill  parameter values\n\n")
  }

  if (!(values_position %in% c("in", "out"))) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "values_position  must be set to \"in\", \"out\"\n\n")
  }

  if (!is.null(stat)) if (!stat.miss && y.miss) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "parameter  stat  is meaningless if no y-variable to transform\n\n")
  }
  #-----------------------------------------------------------------

 
  # --------- data frame stuff -------------------------------------
  
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
    df.name <- deparse(substitute(data))  # is NULL if from shiny
    options(dname = df.name)
  }
  shiny <- ifelse (df.name == "NULL", TRUE, FALSE)  # call from shiny?
 
  # if a tibble, convert to data frame
  if (exists(df.name, envir=parent.frame())) {
    if (any(grepl("tbl", class(data), fixed=TRUE)))
      data <- data.frame(data)
  }

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
        data <- eval(substitute(data), envir=parent.frame())
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
 

  # -----------------------------------------------------------
  # establish if a data frame, if not then identify variable(s)
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
        if (!mydata.ok) .nodf(df.name)  # check to see if df exists
        .xcheck(x.name, df.name, names(data))  # x-vars in df?
      }
      data.vars <- as.list(seq_along(data))
      names(data.vars) <- names(data)
      ind <- eval(substitute(x), envir=data.vars)  # col num of each var
      if (!missing(rows)) {  # subset rows
        r <- eval(substitute(rows), envir=data, enclos=parent.frame())
        if (!any(r)) {
          cat("\n"); stop(call.=FALSE, "\n","------\n",
            "No rows of data with the specified value of\n",
            "rows = ", deparse(substitute(rows)), "\n\n")
        }
        r <- r & !is.na(r)  # set missing for a row to FALSE
        data <- data[r,,drop=FALSE]
      }
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

    # x is a single var, not a data frame or a var list
    if (!is.null(x.call)) {


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
      by.in.global <- ifelse (df.name!="NULL", .in.global(by.name, quiet), TRUE)

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

    if (values_position == "out"  &&  !is.null(by.call)  &&  !beside) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "values_position=\"out\" not meaningful for a  by  variable\n",
        "  without beside=TRUE\n\n")
    }


    # evaluate y
    y.name <- deparse(substitute(y), width.cutoff = 120L)
    options(yname = y.name)

    if (!is.null(y.name))
      y.in.global <- .in.global(y.name, quiet)  # in global?, includes vars list
    else
      y.in.global <- FALSE

    if (!y.in.global)  {
      if (df.name == "NULL")  {  # from shiny
        y.in.global <- TRUE
      }
    }
      
    eval_df <- !y.in.global 

    # if not missing, then must be aggregate data
    #-------------
    if (!missing(y)) {  # assign y.call from data or from global

      # see if var exists in data frame, if y not in global Env or function call
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


    # evaluate by1
    #-------------
    if (!missing(by1)) {

      # get actual variable name before potential call of data$x
      by1.name <- deparse(substitute(by1))
      options(by1name = by1.name)

      # get conditions and check for data existing
      in.global <- .in.global(by1.name, quiet)

      # see if var exists in data frame, if x not in global Env or function call
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


  # evaluate fill (NULL, numeric constant or a variable)
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
    if (in.df) {
      fill.val <- eval(substitute(data$fill))
      fill <- .getColC(fill.val)
      if (sort != "0") {
        srt.dwn <- ifelse (sort == "-", TRUE, FALSE)
        fill <- fill[order(fill.val, decreasing=srt.dwn)]
      }
    }
    else if (substr(fill.name, 1, 6) != "(count") {
      if (length(which(fill == "off")) > 0)
        fill[which(fill == "off")] <- "transparent"
      if (length(which(color == "off")) > 0)
        color[which(color == "off")] <- "transparent"
    }

    # or do a tabulation to get value of y for (count)
    if (substr(fill.name, 1, 6) == "(count") {
      xtb <- table(x.call)
      if (sort != "0") {
        srt.dwn <- ifelse (sort == "-", TRUE, FALSE)
        xtb <- xtb[order(xtb, decreasing=srt.dwn)]
      }
      fill <- .getColC(xtb, fill_name=fill.name)
    }  # end .count 

    # add the n= to a getColors call
    # evaluate getColors at the time of the function call
    # re-evaluate here by setting fill with the specified value of n
    if (substr(fill.name, 1, 9) == "getColors") {
      if (!grepl("output", fill.name, fixed=TRUE)) {  # "output" does not exist
        lx.u <- length(na.omit(unique(x.call)))  # do not include NA's
        gc.args <- substr(fill.name, 11, nchar(fill.name)-1)
        txt <- paste("fill <- getColors(", gc.args, ", n=", lx.u,
                     ", output=FALSE)", sep="")
        pp <- parse(text=txt)
        eval(pp)
      }
    }
  }  # end !fill.miss


  # ------------------------------------------------------------
  # -----------  x, y, and by variables established ------------
  # ------------------------------------------------------------

  n.x <- length(unique(na.omit(x.call)))
  n.by <- ifelse (!by.miss, length(unique(by.call)), 0)
  n.levels <- ifelse (by.miss || is.null(by.call), n.x, n.by)

  # -------------
  # assign colors
  # fill_split done in sub call

  is.ord <- ifelse (is.ordered(x.call) || is.ordered(by.call), TRUE, FALSE)

  # if theme changed, then fill already set
  if (fill.miss  &&  theme == getOption("theme")) {
    if (is.ord || !is.null(by.call))
      fill <- .color_range(.get_fill(theme, is.ord), n.levels)  # default range
    else {
      fill <- getOption("bar_fill_discrete")  # to begin, already "hues" colors
      if (fill[1] == "hues")  # if invoke style(), then colors are "hues"
        fill <- .color_range("hues", n.levels)  # convert to actual colors
    }
  }  # end missing fill
  else
    fill <- .color_range(fill, n.levels)  # get actual colors

  if (trans > 0)
   for (i in 1:length(fill)) fill[i] <- .maketrans(fill[i], (1-trans)*256)

  # by default, no color borders if a range
  if (identical(color, getOption("bar_color_discrete")))
    color <- "transparent"
  # see if apply a pre-defined color range to **color**
  col.clr <- .color_range(color, n.levels)  # see if range, NULL if not
  if (!is.null(col.clr)) color <- col.clr


  # -----------------------
  # process stat parameter

  lx.u <- length(unique(x.call))  # includes NA values
  is.smry_tbl <- ifelse (lx.u < length(x.call), FALSE, TRUE)

  if (!is.null(y.call)) {  # a y variable present

    if (is.smry_tbl) {  # a summary table

      if (!stat.miss  &&  is.smry_tbl) { # y and a summary table, then no stat
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "The data are a summary table, so do not specify a value of\n",
          "  stat  as the data transformation has already been done\n\n")
      }      

     if (sum(is.na(x.call)) > 0 ||
          sum(is.na(by.call)) > 0 ||
          sum(is.na(y.call)) > 0)   {
          ok <- is.finite(x.call) & is.finite(by.call) & is.finite(y.call)
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "When reading a summary table, missing data not allowed.\n\n")
      }
    }  # end is summary table
  }  # a y variable

  # -----------------------
  if (Trellis && do.plot) {

  if (!stat.miss) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Only the original data work with Trellis plots,",
      " no data transformations. Use  by  instead of  by1.\n\n")
  }

  # by2 not currently available
  .bar.lattice(x.call, by1.call, by2=NULL, n_row, n_col, aspect,
               proportion, 
               fill, color, trans, size.pt=NULL, xlab, ylab, main,
               rotate_x, offset,
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
        digits.d <- getOption("digits.d")

          txout <- ""
          if (missing(by)) {  # do not show stats for one var when a by var
            options(yname = x.name)  # reverse x and y names for .ss.numeric
            options(xname = y.name)

            stats <- .ss.numeric(y.call, by=x.call,
                               digits_d=digits_d, brief=TRUE, y.name=x.name)
            txout <- stats$tx
            options(xname = x.name)  # reverse back
            options(yname = y.name)
          }

        class(txout) <- "out"

        output <- list(out_txt=txout)
        class(output) <- "out_all"
        print(output)
      }

      # get summary table from the data according to the stats parameter
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

    is.range.nm <- ifelse (length(.color_range(fill, n.clr=5)) > 1, TRUE, FALSE)
    if (!is.range.nm && !by.miss && !fill.miss && !is.null(by.call)) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "For custom fill for a two-variable bar chart,\n",
        " must specify a color range such as \"colors\" or \"grays\", \n\n")
    }

    bc <- .bc.main(x.call, y.call, by.call, stack100,
          fill, color, trans, fill_split, theme,
          horiz, gap, proportion, scale_y, top,
          xlab, ylab, main,
          value_labels, label_max, beside,
          rotate_x, offset, break_x, sort,
          values, values_color, values_size, values_digits,
          values_position, values_cut,
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


  # x is a data frame or var list of multiple variables
  # ---------------------------------------------------
  else {
    if (!is.null(by) || !is.null(by1)) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "by and by1 variables not available for multiple x variables\n\n")
    }

    # if values not assigned, do default
    if (is.null(values)) { 
        values <- getOption("values")
        if (values != "off") if (missing(y)) values <- "input"
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

    if (one_plot) {  # one_plot all x's into a single plot, BPFM for bars
      y.call <- NULL
      by.call <- NULL
      legend_title <- "Title"
      options(byname = "Responses")
      if (is.null(xlab)) xlab <- ""
      if (is.null(ylab)) ylab <- ""
      if (missing(values_size)) values_size <- 0.8 - (0.008 * ncol(data))
      if (sort.miss) sort <- "+"
      if (horiz.miss) horiz <- TRUE

      if (color.miss) color <- "transparent"
      if (fill.miss) {  # define divergent palette
        if ((theme %in% c("gray", "white"))) {
          fill <- c("grays", "grays")
          color <- c("gray50")
        }
        else if ((theme %in% c("hues", "lightbronze", "dodgerblue", "blue",
                                "gold", "brown", "sienna", "orange")))
          fill <- c("browns", "blues")
        else if ((theme %in% c("darkred", "red", "rose", "slatered")))
          fill <- c("turquoises", "reds")
        else if ((theme %in% c("darkgreen", "green", "purple")))
          fill <- c("violets", "greens")
        else
          fill <- c("browns", "blues")
      }  # end fill.miss
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
            fill, color, trans, fill_split, theme,
            horiz, gap, proportion, scale_y, top,
            xlab, ylab, main,
            value_labels, label_max, beside,
            rotate_x, offset, break_x, sort,
            values, values_color, values_size, values_digits,
            values_position, values_cut,
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
      bc.data.frame(data, n_cat, stack100,
        fill, color, trans, fill_split, theme,
        horiz, gap, proportion, scale_y, top,
        xlab, ylab, main,
        value_labels, label_max, beside,
        rotate_x, offset, break_x, sort,
        values, values_color, values_size, values_digits,
        values_position, values_cut,
        xlab.adj, ylab.adj, bm.adj, lm.adj, tm.adj, rm.adj,
        pad_y_min, pad_y_max,
        legend_title, legend_position, legend_labels,
        legend_horiz, legend_size, legend_abbrev, legend_adjust,
        out_size, do_plot, quiet, width, height, pdf_file, shiny, ...)
    }
  }

}
