BarChart <-
function(x=NULL, y=NULL, by=NULL, data=d, rows=NULL,
        stat=NULL, n_cat=getOption("n_cat"), one_plot=NULL,

        by1=NULL, n_row=NULL, n_col=NULL, aspect="fill",

        horiz=FALSE, beside=FALSE, stack100=FALSE,
        gap=NULL, scale_y=NULL,

        theme=getOption("theme"),
        fill=NULL,
        color=getOption("bar_color_discrete"),
        trans=getOption("trans_bar_fill"),
        fill_split=NULL,

        legend_title=NULL, legend_position="right_margin",
        legend_labels=NULL, legend_horiz=FALSE,
        legend_size=NULL,

        value_labels=NULL,
        rotate_x=getOption("rotate_x"),
        offset=getOption("offset"),
        break_x=NULL, sort=c("0", "-", "+"),

        label_max=100, out_size=80,

        values=NULL,
        values_color=getOption("values_color"),
        values_size=getOption("values_size"),
        values_digits=getOption("values_digits"),
        values_position=getOption("values_position"),
        values_cut=NULL,

        xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
        lab_adj=c(0,0), margin_adj=c(0,0,0,0), 
        pad_y_min=0, pad_y_max=0,

        add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,

        eval_df=NULL, quiet=getOption("quiet"),
        width=6.5, height=6, pdf=FALSE, ...)  {


  # a dot in a parameter name to an underscore
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (names(dots)[i] == "addtop") pad_y_max <- dots[[i]] 
      if (names(dots)[i] == "add_top") pad_y_max <- dots[[i]] 
      if (names(dots)[i] == "stat_x") stat <- dots[[i]]
      if (names(dots)[i] == "stat_yx") stat <- dots[[i]]
      if (grepl(".", names(dots)[i], fixed=TRUE)) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }

  fill.miss <- ifelse (missing(fill), TRUE, FALSE)
  color.miss <- ifelse (missing(color), TRUE, FALSE)
  horiz.miss <- ifelse (missing(horiz), TRUE, FALSE)

  sort.miss <- ifelse (missing(sort), TRUE, FALSE)
  sort <- match.arg(sort)

  proportion <- FALSE

  if (theme != getOption("theme")) {  # not the default theme
    sty <- style(theme, reset=FALSE)
    #fill <- sty$bar$bar.fill.discrete
    #color <- sty$bar$color
    trans <- sty$bar$trans.fill
  }

  if (is.null(values)) values <- "eval.later"

  if (missing(break_x)) 
    break_x <- ifelse (!horiz  &&  rotate_x == 0, TRUE, FALSE)

  if (sort[1] %in% c("off", "up", "down")) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Sort now uses \"0\", \"-\", \"+",
      " instead of \"off\", \"down\", \"up\"\n\n")
  }

  options(xname = NULL)
  options(yname = NULL)
  options(byname = NULL)

  xlab.adj <- lab_adj[1];   ylab.adj <- lab_adj[2]
  tm.adj <- margin_adj[1];  rm.adj <- margin_adj[2]
  bm.adj <- margin_adj[3];  lm.adj <- margin_adj[4]

  lm.adj <- lm.adj + .1  # pull these margins back a bit for bc
  bm.adj <- bm.adj + .1

  Trellis <- ifelse (!missing(by1), TRUE, FALSE)
  do.plot <- TRUE

  if (Trellis  &&  sort != "0") {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Sort not applicable to Trellis plots\n\n")
  }

  if (values != "eval.later") {
    if (!(values %in% c("off", "%", "proportion", "input"))) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "set  values  to \"off\", \"%\", \"proportion\" or \"input\"\n\n")
    }
  }

  if (missing(values_color)) {
    values_color <- "white"
    if (values_position == "out") values_color <- getOption("axis.text.color")
  }

  if (values_position == "out"  &&  !missing(by)  &&  !beside) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "values_position=\"out\" not meaningful for a  by  variable\n",
      "  without beside=TRUE\n\n")
  }

  if (!(values_position %in% c("in", "out"))) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "values_position  must be set to \"in\", \"out\"\n\n")
  }

  #if (missing(color))  # default black border unless dark bg
    #if (sum(col2rgb(panel.fill))/3 > 80) color <- "black"

  # this gets parameter names passed with vars, does not evaluate the arg
  # more robust than list(...) which dies with by2=Gender
  nms <- names(as.list(match.call()))
  if (!is.null(nms)) {
    for (i in 1:length(nms)) {
      if (nms[i] == "by2") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option by2 not applicable to BarChart\n\n")
      }
      if (nms[i] %in% c("proportion", "prop")) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  proportion  now  stat=\"proportion\"\n\n")
      }
    }
  }


  shiny <- ifelse (isNamespaceLoaded("shiny"), TRUE, FALSE)
  if (is.null(eval_df)) eval_df <- ifelse (shiny, FALSE, TRUE)

  # get actual variable name before potential call of data$x
  if (!missing(x))  # no is.null or anything else with x until evaluated
    x.name <- deparse(substitute(x))  # could be a list of var names
  else
    x.name <- NULL  # otherwise is actually set to "NULL" if NULL
  options(xname = x.name)

  # let deprecated mydata work as default
  dfs <- .getdfs() 
  mydata.ok <- FALSE
  if (!is.null(dfs)) {
    if ("mydata" %in% dfs  &&  !("d" %in% dfs)) {
      d <- mydata
      df.name <- "mydata"
      mydata.ok <- TRUE
      options(dname = df.name)
    }
  }
  if (!mydata.ok) {
    df.name <- deparse(substitute(data))  # get name of data table
    options(dname = df.name)
  }
 
  # if a tibble convert to data frame
  if (!is.null(dfs)) {
    if (df.name %in% dfs) {  # tibble to df
      if (any(grepl("tbl", class(data), fixed=TRUE))) {
        data <- data.frame(data)
      }
    }
  }

  # force evaluation (not lazy) if data not specified but relies on default d
  if ((missing(data) && shiny))
    data <- eval(substitute(data), envir=parent.frame())


  if (!is.null(x.name))
    x.in.global <- .in.global(x.name)  # see if in global, includes vars list
  else
    x.in.global <- FALSE
    
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

  # x is a single var, not a data frame
  if (!is.null(x.call)) {

    # evaluate by
    if (!missing(by)) {

      # get actual variable name before potential call of data$x
      by.name <- deparse(substitute(by))
      options(byname = by.name)

      # see if y exists from a function call
      # indicate a function call with sys.nframe returns larger than 1
      # if (exists(y.name, where=parent.frame(n=1)) && sys.nframe() > 1)
      # in.call <- TRUE else in.call <- FALSE

      # get conditions and check for data existing
      #if (!in.call) {
      xs <- .xstatus(by.name, df.name, quiet)
      in.global <- xs$ig
      #}
      #else in.global <- FALSE
      # if y is in global, sys.nframe() returns two, in.call is TRUE,
      #   which leads to in.global FALSE
      #if (exists(by.name, where=.GlobalEnv)) in.global <- TRUE

      # see if var exists in data frame, if x not in global Env or function call
      if (eval_df) if (!in.global) .xcheck(by.name, df.name, names(data))
      if (!in.global)
        by.call <- eval(substitute(data$by))
      else {  # vars that are function names get assigned to global
        by.call <- by
        if (is.function(by.call)) by.call <- eval(substitute(data$by))
      }

    }
    else  # end not missing by
      by.call <- NULL


    # evaluate y
    # if not missing, then must be aggregate data
    #-------------
    if (!missing(y)) {

      # get actual variable name before potential call of data$x
      y.name <- deparse(substitute(y))
      options(yname = y.name)

      # get conditions and check for data existing
      xs <- .xstatus(y.name, df.name, quiet)
      in.global <- xs$ig

      # see if var exists in data frame, if x not in global Env or function call
      if (eval_df) if (!in.global) .xcheck(y.name, df.name, names(data))
      if (!in.global)
        y.call <- eval(substitute(data$y))
      else {  # vars that are function names get assigned to global
        y.call <- y
        if (is.function(y.call)) y.call <- eval(substitute(data$y))
      }

    }
    else
      y.call <- NULL


    # evaluate by1
    #-------------
    if (!missing(by1)) {

      # get actual variable name before potential call of data$x
      by1.name <- deparse(substitute(by1))
      options(by1name = by1.name)

      # get conditions and check for data existing
      xs <- .xstatus(by1.name, df.name, quiet)
      in.global <- xs$ig

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

    if (exists(df.name, where=.GlobalEnv))
      in.df <- ifelse (exists(fill.name, where=data), TRUE, FALSE)
    else
      in.df <- FALSE

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
      fill[which(fill == "off")] <- "transparent"
      color[which(color == "off")] <- "transparent"
    }

    # or do a tabulation to get value of y
    if (substr(fill.name, 1, 6) == "(count") {
      xtb <- table(x.call)
      if (sort != "0") {
        srt.dwn <- ifelse (sort == "-", TRUE, FALSE)
        xtb <- xtb[order(xtb, decreasing=srt.dwn)]
      }
      fill <- .getColC(xtb, fill_name=fill.name)
         
    }  # end .count 
  }  # end !fill.miss


  # -----------  x, y, and by variables established ------------
  # ------------------------------------------------------------

  # do the analysis
  # data means raw_data
  if (!is.null(stat)) {
    if (stat == "proportion") proportion <- TRUE
    if (stat %in% c("count", "proportion")) stat <- "data"
  }

  # if data table is raw data, then default stat is "data"
  if (is.null(stat)) {
    if (!is.null(y.call)) {

      if (sum(is.na(x.call))  > 0 ||
          sum(is.na(by.call)) > 0 ||
          sum(is.na(y.call))  > 0)   {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "When reading the value of y directly, from aggregate data,\n",
          "  missing aggregated data not allowed\n",
          "Use the  na.omit()  function before aggregating the data\n\n")
      }
      
      lx.u <- length(unique(x.call))
      lb.u <- ifelse(is.null(by.call), 1, length(unique(by.call)))
      if (nrow(data) > lx.u*lb.u)
        stat <- "mean"
      else
        stat <- "data"
    }
    else  {# no y variable
      stat <- "data"
    }
  }

  if (stat != "data"  &&  is.null(y.call)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "To do a transformation of y for each level of ", x.name, "\n",
      " need to provide a numerical y variable\n\n")
  }

  if (Trellis && do.plot) {

           if (stat == "sum")
        ylab <- paste("Sum of", y.name)
      else if (stat == "mean")
        ylab <- paste("Mean of", y.name)
      else if (stat == "sd")
        ylab <- paste("Standard Deviation of", y.name)
      else if (stat == "dev")
        ylab <- paste("Mean Deviations of", y.name)
      else if (stat == "min")
        ylab <- paste("Minimum of", y.name)
      else if (stat == "median")
        ylab <- paste("Median of", y.name)
      else if (stat == "max")
        ylab <- paste("Maximum of", y.name)


    .bar.lattice(x.call, by1.call, by2=NULL, n_row, n_col, aspect,
                 proportion, 
                 fill, color, trans, size.pt=NULL, xlab, ylab, main,
                 rotate_x, offset,
                 width, height, pdf,
                 segments_x=NULL, breaks=NULL, c.type="bar", quiet)
  }

  else {  # not Trellis


    if (is.null(by.call))
      f.name <- x.name
    else
      f.name <- paste(x.name, "x", by.name, sep="")
      
    if (pdf) {
      pdf.fnm <- paste("BarChart", "_", f.name, ".pdf", sep="") 
      .opendev(pdf.fnm, width, height)
    }
    else {
      if (!shiny) {  # not dev.new for shiny
        pdf.fnm <- NULL
        .opendev(pdf.fnm, width, height)
      }
    }


    unq.x <- ifelse (length(x.call) == length(unique(x.call)), TRUE, FALSE)
    stat.val <- c("mean", "sum", "sd", "dev", "min", "median", "max")

    if ((stat %in% stat.val)  &&  (!unq.x)) {

      n_cat <- 0
      means <- FALSE

      # do stats console output before reducing data
      if (!quiet) {
        digits.d <- getOption("digits.d")

        if (!missing(y)) {
          options(yname = x.name)  # reverse order of x and y for .ss.numeric
          options(xname = y.name)
          stats <- .ss.numeric(y.call, by=x.call,
                               digits.d=digits.d, brief=TRUE, y.name=x.name)
          txout <- stats$tx
          options(xname = x.name)  # reverse back
          options(yname = y.name)
        }
        else  {
          stats <- .ss.factor(x.call, digits.d=digits.d, x.name=x.name,
                              brief=TRUE)
          txout <- stats$counts
        }

        class(txout) <- "out"

        output <- list(out_txt=txout)
        class(output) <- "out_all"
        print(output)
      }

    # set up new x.call and y.call for stats
      if (stat == "sum") {
        ylab <- paste("Sum of", y.name)
        if (is.null(by.call))
          out <- tapply(y.call, x.call, sum, na.rm=TRUE)
        else 
          out <- aggregate(y.call ~ x.call +  by.call, FUN=sum)
      }
      if (stat == "mean") {
        ylab <- paste("Mean of", y.name)
        if (is.null(by.call))
          out <- tapply(y.call, x.call, mean, na.rm=TRUE)
        else 
          out <- aggregate(y.call ~ x.call +  by.call, FUN=mean)
      }
      if (stat == "sd") {
        ylab <- paste("Standard Deviation of", y.name)
        if (is.null(by.call))
          out <- tapply(y.call, x.call, sd, na.rm=TRUE)
        else 
          out <- aggregate(y.call ~ x.call +  by.call, FUN=sd)
      }
      if (stat == "dev") {
        ylab <- paste("Mean Deviations of", y.name)
        if (is.null(by.call)) {
          out <- tapply(y.call, x.call, mean, na.rm=TRUE)
          out <- out - mean(out, na.rm=TRUE)
        }
        else { 
          cat("\n"); stop(call.=FALSE, "\n","------\n",
          "dev option not meaningful with a by variable\n\n")
        }
      }
      if (stat == "min") {
        ylab <- paste("Minimum of", y.name)
        if (is.null(by.call))
          out <- tapply(y.call, x.call, min, na.rm=TRUE)
        else 
          out <- aggregate(y.call ~ x.call + by.call, FUN=min)
      }
      if (stat == "median") {
        ylab <- paste("Median of", y.name)
        if (is.null(by.call))
          out <- tapply(y.call, x.call, median, na.rm=TRUE)
        else 
          out <- aggregate(y.call ~ x.call + by.call, FUN=median)
      }
      if (stat == "max") {
        ylab <- paste("Maximum of", y.name)
        if (is.null(by.call))
          out <- tapply(y.call, x.call, max, na.rm=TRUE)
        else 
          out <- aggregate(y.call ~ x.call +  by.call, FUN=max)
      }

    #if (is.factor(x.call))  # preserve ordering, will lose order attribute
      #x.call <- factor(names(out), levels=levels(x.call))
    #else {
      #if (is.numeric(x.call)) {
        #m1 <- min(sort(unique(x.call[,1])))
        #m2 <- max(sort(unique(x.call[,1])))
        #x.call <- factor(names(out), levels=m1:m2)  # get entire numeric range
      #}
      #else
      if (is.null(by.call)) {
        x.call <- factor(names(out))
        y.call <- as.vector(out)
      }
      else {
        x.call <- out[,1]
        by.call <- out[,2]
    #}
        y.call <- out[,3]
#       beside <- TRUE
      }

#     x.call <- data.frame(x.call)
#     y.call <- data.frame(y.call)
    }  # sum, mean, sd, min, median, max

      bc <- .bc.main(x.call, y.call, by.call, stack100,
            fill, color, trans, fill_split, theme,
            horiz, gap, proportion, scale_y,
            xlab, ylab, main,
            value_labels, label_max, beside,
            rotate_x, offset, break_x, sort,
            values, values_color, values_size, values_digits,
            values_position, values_cut,
            xlab.adj, ylab.adj, bm.adj, lm.adj, tm.adj, rm.adj,
            pad_y_min, pad_y_max,
            legend_title, legend_position, legend_labels,
            legend_horiz, legend_size,
            add, x1, x2, y1, y2, out_size, quiet, ...)

        if (pdf) {
          dev.off()
          if (!quiet) .showfile(pdf.fnm, "BarChart")
        }
        
      invisible(bc)
    }  # not Trellis

  }  # end x is a single var


  # -----------------------------------------------
  else {  # x is a data frame of multiple variables

    if (!is.null(by) || !is.null(by1)) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "by and by1 variables not available for multiple x variables\n\n")
    }

    # if values not assigned, do default
#   if (is.null(values) || (!missing(values_color) || !missing(values_size)
#     || !missing(values_digits) || !missing(values_position))) 
    if (is.null(values)) 
        values <- ifelse (missing(y), getOption("values"), "input")

    if (is.null(values_digits)) {
      if (values == "%") values_digits <- 0
      if (values == "prop") values_digits <- 2
    }

    if (is.null(one_plot)) {  # see if one_plot
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
          fill <- c("grays","grays")
          color <- c("gray50")
        }
        else if ((theme %in% c("hues", "lightbronze", "dodgerblue", "blue",
                                "gold", "brown", "sienna", "orange")))
          fill <- c("browns", "blues")
        else if ((theme %in% c("darkred", "red", "rose")))
          fill <- c("turquoises", "reds")
        else if ((theme %in% c("darkgreen", "green", "purple")))
          fill <- c("violets", "greens")
        else
          fill <- c("browns", "blues")
      }  # end fill.miss
        
      if (pdf) {
        f.name <- sub(":", "_", x.name, fixed=TRUE)
        pdf.fnm <- paste("BarChart", "_", f.name, ".pdf", sep="") 
        .opendev(pdf.fnm, width, height)
      }
      else {
        if (!shiny) {  # not dev.new for shiny
          pdf.fnm <- NULL
          .opendev(pdf.fnm, width, height)
        }
      }
      
      bc <- .bc.main(data, y.call, by.call, stack100,
            fill, color, trans, fill_split, theme,
            horiz, gap, proportion, scale_y,
            xlab, ylab, main,
            value_labels, label_max, beside,
            rotate_x, offset, break_x, sort,
            values, values_color, values_size, values_digits,
            values_position, values_cut,
            xlab.adj, ylab.adj, bm.adj, lm.adj, tm.adj, rm.adj,
            pad_y_min, pad_y_max,
            legend_title, legend_position, legend_labels,
            legend_horiz, legend_size,
            add, x1, x2, y1, y2, out_size, quiet, ...)
      
      if (pdf) {
        dev.off()
        if (!quiet) .showfile(pdf.fnm, "BarChart")
      }
    }  # end one_plot

    else {  # analyze each x column separately
      bc.data.frame(data, n_cat, stack100,
        fill, color, trans, fill_split, theme,
        horiz, gap, proportion, scale_y,
        xlab, ylab, main,
        value_labels, label_max, beside,
        rotate_x, offset, break_x, sort,
        values, values_color, values_size, values_digits,
        values_position, values_cut,
        xlab.adj, ylab.adj, bm.adj, lm.adj, tm.adj, rm.adj,
        pad_y_min, pad_y_max,
        legend_title, legend_position, legend_labels,
        legend_horiz, legend_size,
        out_size, quiet, width, height, pdf, ...)
    }
  }

}
