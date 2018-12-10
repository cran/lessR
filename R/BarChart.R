BarChart <-
function(x=NULL, y=NULL, by=NULL, data=mydata, rows=NULL,
        theme=getOption("theme"), n.cat=getOption("n.cat"),
        one.plot=NULL,

        by1=NULL, n.row=NULL, n.col=NULL, aspect="fill",

        horiz=FALSE, beside=FALSE, gap=NULL,
        proportion=FALSE, scale.y=NULL,

        fill=NULL,
        color=getOption("bar.color.discrete"),
        trans=getOption("trans.bar.fill"),
        fill.split=NULL,

        legend.title=NULL, legend.loc="right.margin",
        legend.labels=NULL, legend.horiz=FALSE,

        value.labels=NULL,
        rotate.x=getOption("rotate.x"),
        offset=getOption("offset"),
        break.x=TRUE, sort=c("0", "-", "+"),

        label.max=100, out.size=80,

        values=NULL,
        values.color=getOption("values.color"),
        values.size=getOption("values.size"),
        values.digits=getOption("values.digits"),
        values.pos=getOption("values.pos"),
        values.cut=NULL,

        xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
        lab.adj=c(0,0), margin.adj=c(0,0,0,0), addtop=0.05,

        add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,

        eval.df=NULL, quiet=getOption("quiet"),
        width=6.5, height=6, pdf=FALSE, ...)  {


  if (theme != getOption("theme")) {  # not the default theme
    sty <- style(theme, reset=FALSE)
    #fill <- sty$bar$bar.fill.discrete
    #color <- sty$bar$color
    trans <- sty$bar$trans.fill
  }

  # evaluate in bc.main, under color, when n.levels is known
  if (is.null(values)) values <- "eval.later"

  fill.miss <- ifelse (missing(fill), TRUE, FALSE)
  color.miss <- ifelse (missing(color), TRUE, FALSE)
  horiz.miss <- ifelse (missing(horiz), TRUE, FALSE)
  sort.miss <- ifelse (missing(sort), TRUE, FALSE)

  if (sort[1] %in% c("off", "up", "down")) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Sort now uses \"0\", \"-\", \"+",
      " instead of \"off\", \"down\", \"up\"\n\n")
  }
  sort <- match.arg(sort)

  options(xname = NULL)
  options(yname = NULL)
  options(byname = NULL)

  xlab.adj <- lab.adj[1];   ylab.adj <- lab.adj[2]
  tm.adj <- margin.adj[1];  rm.adj <- margin.adj[2]
  bm.adj <- margin.adj[3];  lm.adj <- margin.adj[4]

  lm.adj <- lm.adj + .1  # pull these margins back a bit for bc
  bm.adj <- bm.adj + .1

  Trellis <- ifelse (!missing(by1), TRUE, FALSE)
  do.plot <- TRUE

  if (Trellis  &&  sort != "0") {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Sort not applicable to Trellis plots\n\n")
  }

    if (values != "eval.later") {
      if (!(values %in% c("off", "%", "prop", "input"))) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "values  must be set to \"off\", \"%\", \"prop\" or \"input\"\n\n")
      }
    }

    if (missing(values.color)) {
      values.color <- "white"
      if (values.pos == "out") values.color <- getOption("axis.text.color")
    }

  if (values.pos == "out"  &&  !missing(by)  &&  !beside) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "values.pos=\"out\" not meaningful for a  by  variable\n",
      "  without beside=TRUE\n\n")
  }

  if (!(values.pos %in% c("in", "out"))) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "values.pos  must be set to \"in\", \"out\"\n\n")
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
    }
  }

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

  df.name <- deparse(substitute(data))  # get name of data table
  options(dname = df.name)

  if (exists(df.name, where=.GlobalEnv)) {  # tibble to df
    if (class(data)[1] == "tbl_df")
      data <- as.data.frame(data, stringsAsFactors=FALSE)
    if ((missing(data) && shiny))  # force eval (not lazy) if data not specified
      data <- eval(substitute(data), envir=parent.frame())
  }

  if (!is.null(x.name))
    x.in.global <- .in.global(x.name)  # see if in global, includes vars list
  else
    x.in.global <- FALSE
    
  # -----------------------------------------------------------
  # establish if a data frame, if not then identify variable(s)
  # x can be missing entirely, with a data frame passed instead
  # if x a vector, then x.name not in data, but also not in global

  x.call <- NULL

  if (!missing(x)) {

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
          by.name <- legend.title
          options(xname = x.name)
          options(byname = by.name)
        }
        x.call <- x
        if (is.function(x.call)) x.call <- eval(substitute(data$x))
      }
    }

    # read from console with text parameter can insert extra space at front
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
      #if (exists(y.name, where=parent.frame(n=1)) && sys.nframe() > 1)
        #in.call <- TRUE else in.call <- FALSE

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
      if (eval.df) if (!in.global) .xcheck(by.name, df.name, names(data))
      if (!in.global)
        by.call <- eval(substitute(data$by))
      else {  # vars that are function names get assigned to global
        by.call <- by
        if (is.function(by.call)) by.call <- eval(substitute(data$by))
      }

    }
    else
      by.call <- NULL


    # evaluate y
    #-------------
    if (!missing(y)) {

      # get actual variable name before potential call of data$x
      y.name <- deparse(substitute(y))
      options(yname = y.name)

      # get conditions and check for data existing
      xs <- .xstatus(y.name, df.name, quiet)
      in.global <- xs$ig

      # see if var exists in data frame, if x not in global Env or function call
      if (eval.df) if (!in.global) .xcheck(y.name, df.name, names(data))
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
    else if (fill.name != "(count)") {
      fill[which(fill == "off")] <- "transparent"
      color[which(color == "off")] <- "transparent"
    }

    # or do a tabulation to get value of y
    if (fill.name == "(count)") {
      xtb <- table(x.call)
      if (sort != "0") {
        srt.dwn <- ifelse (sort == "-", TRUE, FALSE)
        xtb <- xtb[order(xtb, decreasing=srt.dwn)]
      }
      fill <- .getColC(xtb)
    }  # end .count 
  }  # end !fill.miss


    if (length(unique(na.omit(x.call))) == 1) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "There is only one unique value for the values of ", x.name,
        ": ", na.omit(x.call)[1], "\n",
        "The bar chart is only computed if there is more than one",
        " unique value\n\n")
    }


  # -----------  x, y, and by variables established ------------
  # ------------------------------------------------------------

    # do the analysis

    if (Trellis && do.plot) {

      .bar.lattice(x.call, by1.call, by2=NULL, n.row, n.col, aspect,
                   prop=FALSE,
                   fill, color, trans, size.pt=NULL, xlab, ylab, main,
                   rotate.x, offset,
                   width, height, pdf,
                   segments.x=NULL, breaks=NULL, c.type="bar", quiet)
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

      bc <- .bc.main(x.call, y.call, by.call,
            fill, color, trans, fill.split, theme,
            horiz, addtop, gap, proportion, scale.y,
            xlab, ylab, main,
            value.labels, label.max, beside,
            rotate.x, offset, break.x, sort,
            values, values.color, values.size, values.digits,
            values.pos, values.cut,
            xlab.adj, ylab.adj, bm.adj, lm.adj, tm.adj, rm.adj,
            legend.title, legend.loc, legend.labels, legend.horiz,
            add, x1, x2, y1, y2, out.size, quiet, ...)

        if (pdf) {
          dev.off()
          if (!quiet) .showfile(pdf.fnm, "BarChart")
        }
        
      invisible(bc)
    }  # not Trellis
  }  # end x is a single var


  else {  # x is a data frame of multiple variables

    if (!is.null(by) || !is.null(by1)) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "by and by1 variables not available for multiple x variables\n\n")
    }

    # if values not assigned, do default
    if (is.null(values) || (!missing(values.color) || !missing(values.size)
      || !missing(values.digits) || !missing(values.pos))) 
        values <- ifelse (missing(y), getOption("values"), "input")

    if (is.null(values.digits)) {
      if (values == "%") values.digits <- 0
      if (values == "prop") values.digits <- 2
    }

    if (is.null(one.plot)) {  # see if one.plot
      one.plot <- TRUE

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
          one.plot <- FALSE
          break;
        }
      }
    }  # end determine one.plot

    if (one.plot) {  # one.plot all x's into a single plot
      y.call <- NULL
      by.call <- NULL
      legend.title <- "Title"
      options(byname = "Responses")
      if (is.null(xlab)) xlab <- ""
      if (is.null(ylab)) ylab <- ""
      if (missing(values.size)) values.size <- 0.8 - (0.008 * ncol(data))
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
      
      bc <- .bc.main(data, y.call, by.call,
            fill, color, trans, fill.split, theme,
            horiz, addtop, gap, proportion, scale.y,
            xlab, ylab, main,
            value.labels, label.max, beside,
            rotate.x, offset, break.x, sort,
            values, values.color, values.size, values.digits,
            values.pos, values.cut,
            xlab.adj, ylab.adj, bm.adj, lm.adj, tm.adj, rm.adj,
            legend.title, legend.loc, legend.labels, legend.horiz,
            add, x1, x2, y1, y2, out.size, quiet, ...)
      
      if (pdf) {
        dev.off()
        if (!quiet) .showfile(pdf.fnm, "BarChart")
      }
    }  # end one.plot

    else {  # analyze each x column separately
      bc.data.frame(data, n.cat,
        fill, color, trans, fill.split, theme,
        horiz, addtop, gap, proportion, scale.y,
        xlab, ylab, main,
        value.labels, label.max, beside,
        rotate.x, offset, break.x, sort,
        values, values.color, values.size, values.digits,
        values.pos, values.cut,
        xlab.adj, ylab.adj, bm.adj, lm.adj, tm.adj, rm.adj,
        legend.title, legend.loc, legend.labels, legend.horiz,
        out.size, quiet, width, height, pdf, ...)
    }
  }

}
