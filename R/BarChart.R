BarChart <-
function(x=NULL, y=NULL, by=NULL, data=mydata,
        n.cat=getOption("n.cat"),

        by1=NULL, n.row=NULL, n.col=NULL, aspect="fill",

        horiz=FALSE, addtop=0.05, gap=NULL,
        proportion=FALSE, beside=FALSE,
        scale.y=NULL,

        fill=getOption("bar.fill.discrete"),
        color=getOption("bar.color"),
        trans=getOption("trans.bar.fill"),

        legend.title=NULL, legend.loc="right.margin",
        legend.labels=NULL, legend.horiz=FALSE, 

        value.labels=NULL,
        rotate.x=getOption("rotate.x"),
        offset=getOption("offset"),
        break.x=TRUE, sort.x=c("off", "down", "up"),

        label.max=100, out.size=80,

        values=getOption("values"),
        values.color=getOption("values.color"), 
 	      values.cex=getOption("values.cex"),
        values.digits=getOption("values.digits"),
        values.pos=getOption("values.pos"),
		
        xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
        xlab.adj=0, ylab.adj=0,
        bm.adj=0, lm.adj=0, tm.adj=0, rm.adj=0,

        add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,

        eval.df=NULL, quiet=getOption("quiet"),
        width=6.5, height=6, pdf=NULL, ...)  {

  sort.x <- match.arg(sort.x)
  values.miss <- ifelse (missing(values), TRUE, FALSE)

  theme <- getOption("theme")

  if (missing(fill))
    fill <- ifelse (is.null(getOption("bar.fill.discrete")), 
      getOption("bar.fill"), getOption("bar.fill.discrete"))

  if (values.miss && (!missing(values.color) || !missing(values.cex)
      || !missing(values.digits) || !missing(values.pos)))
    values <- "%"

  if (is.null(values.digits)) {
    if (values == "%") values.digits <- 0
    if (values == "prop") values.digits <- 2
  }

  if (missing(values.color)) {
    values.color <- "white" 
    if (values.pos == "out") values.color <- getOption("axis.text.color")
  }

  options(xname = NULL)
  options(yname = NULL)
  options(byname = NULL)

  Trellis <- ifelse(!missing(by1), TRUE, FALSE)
  do.plot <- TRUE
 
  if (Trellis  &&  sort.x != "off") {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Sort not applicable to Trellis plots\n\n")
  }
 
  if (!(values %in% c("off", "%", "prop", "input"))) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "values  must be set to \"off\", \"%\", \"prop\" or \"input\"\n\n")
  }
 
  if (!(values.pos %in% c("in", "out"))) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "values.pos  must be set to \"in\", \"out\"\n\n")
  }
 
  if (values != "off"  &&  beside) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "beside=TRUE  option not currently working with values\n\n")
  }

  fill[which(fill == "off")] <- "transparent"
  color[which(color == "off")] <- "transparent"

  #if (missing(color))  # default black border unless dark bg
    #if (sum(col2rgb(panel.fill))/3 > 80) color <- "black"

  shiny <- ifelse (isNamespaceLoaded("shiny"), TRUE, FALSE) 
  if (is.null(eval.df))  # default values
    eval.df <- ifelse (shiny, FALSE, TRUE) 

  .param.old(...)

  x.name <- deparse(substitute(x))
  options(xname = x.name)

  data.miss <- ifelse (missing(data), TRUE, FALSE) 
  if (data.miss && shiny)  # force evaluation (not lazy)
    data <- eval(substitute(data), envir=parent.frame())
  df.name <- deparse(substitute(data))
  options(dname = df.name)


# -----------------------------------------------------------
# establish if a data frame, if not then identify variable(s)

  x.call <- NULL

  if (!missing(x) && is.null(x.call)) {
    # x not in global env, in df, specify data= forces to data frame
    if (!exists(x.name, where=.GlobalEnv) || !data.miss) {
      if (eval.df) {
        .nodf(df.name)  # check to see if data frame container exists 
        .xcheck(x.name, df.name, data)  # var in df?, vars lists not checked
      }
      vars.list <- as.list(seq_along(data))
      names(vars.list) <- names(data)
      x.col <- eval(substitute(x), envir=vars.list)  # col num of each var
      if (length(x.col) > 1) data <- data[, x.col]  # x is a vars list
      if (length(x.col) == 1) x.call <- eval(substitute(data$x))  # x is 1 var
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
  }


  if (!is.null(x.call)) {  # x is a single var, not a data frame

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
      if (eval.df) if (!in.global) .xcheck(by.name, df.name, data)
      #if (!in.global && !in.call) .xcheck(by.name, df.name, data)
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
      if (eval.df) if (!in.global) .xcheck(y.name, df.name, data)
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
        .xcheck(by1.name, df.name, data)

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



    if (length(unique(na.omit(x.call))) == 1) { 
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "There is only one unique value for the values of ", x.name,
        ": ", na.omit(x.call)[1], "\n",
        "The bar chart is only computed if there is more than one",
        " unique value\n\n")
    }


  # -----------  x, y, by and size variables established ------------
  # -----------------------------------------------------------------

    # do the analysis

    if (Trellis && do.plot) {
      .bar.lattice(x.call, by1.call, by2=NULL, n.row, n.col, aspect, prop=FALSE,
                   fill, color, trans, size.pt=NULL, xlab, ylab, main,
                   rotate.x, offset,
                   width, height, pdf,
                   segments.x=NULL, breaks=NULL, c.type="bar")
    }

    else {
      if (is.null(by.call))
        f.name <- x.name
      else
        f.name <- paste(x.name, "x", by.name, sep="")

      if (!is.null(pdf))
        pdf.fnm <- paste("BarChart_", f.name, ".pdf", sep="") 
      else {
        if (!shiny) {  # not dev.new for shiny
          pdf.fnm <- NULL
          .opendev(pdf.fnm, width, height)
        }
      }

      bc <- .bc.main(x.call, y.call, by.call,
            fill, color, trans, theme,
            horiz, addtop, gap, proportion, scale.y,
            xlab, ylab, main,
            value.labels, label.max, beside, 
            rotate.x, offset, break.x, sort.x,
            values, values.color, values.cex, values.digits, values.pos,
            xlab.adj, ylab.adj, bm.adj, lm.adj, tm.adj, rm.adj,
            legend.title, legend.loc, legend.labels, legend.horiz,
            add, x1, x2, y1, y2, out.size, quiet, ...)

      if (!is.null(pdf)) {
        dev.off()
        if (!quiet) .showfile(pdf.fnm, "barchart")
      }

      invisible(bc)
    }  # end .bc.main
  }
  

  else {

    bc.data.frame(data, n.cat,
      fill, color, trans, theme,
      horiz, addtop, gap, proportion, scale.y,
      xlab, ylab, main,
      value.labels, label.max, beside,
      rotate.x, offset, break.x, sort.x,
      values, values.color, values.cex, values.digits, values.pos,
      xlab.adj, ylab.adj, bm.adj, lm.adj, tm.adj, rm.adj,
      legend.title, legend.loc, legend.labels, legend.horiz,
      out.size, quiet, width, height, pdf, ...)
  }

}

