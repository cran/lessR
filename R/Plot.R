Plot <-
function(x, y=NULL, by=NULL, data=mydata, n.cat=getOption("n.cat"),

         values=c("data", "count", "prop", "sum", "mean", "sd", "min",
                  "median", "max"),

         color.fill=getOption("color.fill.pt"),
         color.stroke=getOption("color.stroke.pt"),
         color.bg=getOption("color.bg"),
         color.grid=getOption("color.grid"),
         color.box=getOption("color.box"),

         color=NULL, color.trans=NULL,

         cex.axis=0.76, color.axis="gray30", xy.ticks=TRUE,
         xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
         value.labels=NULL, rotate.values=0, offset=0.5,
         proportion=FALSE,

         run.chart=FALSE, line.width=2, color.area=FALSE, 
         center.line=c("default", "mean", "median", "zero", "off"),
         show.runs=FALSE,

         size=NULL, shape="circle", means=TRUE,
         sort.yx=FALSE,
         segments.y=FALSE, segments.x=FALSE,

         smooth=FALSE, smooth.points=100, smooth.trans=0.25,
         smooth.bins=128,

         bubble.scale=0.25, bubble.power=0.6, bubble.text=NULL,
         color.low=NULL, color.hi=NULL,

         fit.line=NULL, color.fit.line="gray55",

         ellipse=FALSE, color.ellipse=getOption("color.stroke.pt"),
         color.fill.ellipse=getOption("color.fill.ellipse"),

         method="overplot", pt.reg="circle", pt.out="circle",
         color.out30="firebrick2", color.out15="firebrick4", new=TRUE,
         boxplot=FALSE,

         bar=FALSE,
         breaks="Sturges", bin.start=NULL, bin.width=NULL, bin.end=NULL,
         cumul=c("off", "on", "both"), hist.counts=FALSE,
         color.reg="snow2",

         beside=FALSE, horiz=FALSE,
         over.grid=FALSE, addtop=0.05, gap=NULL, count.labels=NULL,
         legend.title=NULL, legend.loc="right.margin", legend.labels=NULL,
         legend.horiz=FALSE,

         digits.d=NULL, quiet=getOption("quiet"),
         pdf.file=NULL, pdf.width=NULL, pdf.height=NULL,
         fun.call=NULL, ...) {

  # ------
  # set parameter values 
  
  if (is.null(fun.call)) fun.call <- match.call()
   
  values <- match.arg(values)
  center.line <- match.arg(center.line)
  cumul <- match.arg(cumul)

  # missing function only reliable if arg not modified, so capture 
  x.miss <- ifelse (missing(x), TRUE, FALSE)
  y.miss <- ifelse (missing(y), TRUE, FALSE)
  by.miss <- ifelse (missing(by), TRUE, FALSE)
  values.miss <- ifelse (missing(values), TRUE, FALSE)
  seg.y.miss <- ifelse (missing(segments.y), TRUE, FALSE)  # for Cleveland plot
  seg.x.miss <- ifelse (missing(segments.x), TRUE, FALSE)
  sort.yx.miss <- ifelse (missing(sort.yx), TRUE, FALSE)
  col.grid.miss <- ifelse (missing(color.grid), TRUE, FALSE)

  if (color.grid == "on") color.grid <- getOption("color.grid") # for Cleveland
  if (is.logical(color.area))
    color.area <- ifelse (color.area, getOption("color.fill.pt"), "off")
 
  # any run.chart parameter activates a run.chart
  if (show.runs)
    run.chart <- TRUE

  # any bin parameter activates bins
  if (!missing(breaks)  ||  !missing(bin.start)  ||  !missing(bin.width)  ||
      !missing(bin.end))
    values <- "count"

  object <- "default"
  if (bar) {
    object <- "bar"
    if (values == "data") values <- "count"

    if (values.miss) values <- "count"  # default topic for bars
    if (values == "data") {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Bars do not apply to data, only to count, mean, etc.\n\n")
    }
    if (missing(color.fill)) color.fill <- getOption("color.fill.bar")
  }
  if (run.chart) object <- "both"

  # any bubble parameter activates a bubble plot
  if (!missing(bubble.scale) || !missing(bubble.scale) || !missing(bubble.text)) {
    object <- "bubble"
  }

  # any ellipse parameter actives an ellipse
  if (missing(ellipse)) 
    if (!missing(color.ellipse) || !missing(color.fill.ellipse))
      ellipse <- TRUE

  if (is.null(fit.line)) fit.ln <- "off"
  if (is.logical(fit.line))
    fit.ln <- ifelse (fit.line, "loess", "off")
  if (is.character(fit.line)) {
    if (!(fit.line %in% c("loess", "ls", "off"))) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "fit.line applies only for  loess  or  ls  (least squares)\n\n")
    }
    fit.ln <- fit.line  # fit.ln passed to .plt.main
  }
  
  # "off" substitutes for official arg value of "transparent"
  for (i in 1:length(color.fill))
    if (color.fill[i] == "off") color.fill[i] <- "transparent"
  for (i in 1:length(color.stroke))
    if (color.stroke[i] == "off") color.stroke[i] <- "transparent"
  if (color.bg == "off") color.bg <- "transparent"
  if (color.grid == "off" ) color.grid <- "transparent"
  if (color.box == "off") color.box <- "transparent"
  if (color.fill.ellipse == "off") color.fill.ellipse <- "transparent"
  if (!is.null(color.area)) if (color.area == "off") color.area <- "transparent"

  # populate color parameters
  if (!is.null(color)) {
    color.stroke <- color
    color.fill <- color
  }

  if (is.logical(ellipse)) if (ellipse) ellipse <- 0.95
  if (as.logical(ellipse[1])) {
    txt <- "[Ellipse with Murdoch and Chow's function ellipse"
    cat(txt, "from the ellipse package]\n")
  }

  if (!is.null(pdf.file))
    if (!grepl(".pdf", pdf.file)) pdf.file <- paste(pdf.file, ".pdf", sep="")


  # ------
  # process shapes
  if (shape[1] == "sunflower")
    object <- "sunflower"

  else {
    bad.shape <- NULL
    shapes <- c("circle", "square", "diamond", "triup", "tridown")
    shapes.all <- c(shapes, c(21:25), letters, LETTERS, 0:9, "+", "*", "#",
                    "%", "!", "=", "-", "&", "$", "?", "|", ">", "<", "@")

    num.flag <- FALSE
    for (i in 1:length(shape)) {
      if (!(shape[i] %in% shapes.all)) {
        bad.shape <- shape[i]
      }
      else if (shape[i] %in% shapes) {
        shape[i] <- which(shape[i] == shapes) + 20
        num.flag <- TRUE
      }
    }
    if (num.flag) shape <- as.numeric(shape)

    if (pt.reg %in% shapes)  # regular point
      pt.reg <- which(pt.reg == shapes) + 20
    else
      if (!(pt.reg %in% c(21:25))) bad.shape <- pt.reg

    if (pt.out %in% shapes)  # outlier point
      pt.out <- which(pt.out == shapes) + 20
    else
      if (!(pt.out %in% c(21:25))) bad.shape <- pt.out

    if (!is.null(bad.shape)) {
        message("\nValid shapes")
        message("------------")
        for (j in 1:length(shapes)) message(shapes[j])
        message("all uppercase and lowercase letters")
        message("all digits")
        message("+ * # % ! = - & $ ? | < > @")
        cat("\n")
        stop(call.=FALSE, "\n","------\n",
        "Not a valid shape: ", bad.shape, "\n\n")
    }
  }
    

  # ------
  # see if dated or inconsistent parameter values
  .plt.zbad(x.miss, y.miss, values, method, breaks, bin.start, bar, ...)


  # ------
  # evaluate x
  
  # get actual variable name before potential call of data$x
  x.name <- deparse(substitute(x))
  options(xname = x.name)

  # get data frame name
  df.name <- deparse(substitute(data))
  options(dname = df.name)

  # get data to be analyzed into data.x data frame
  if (deparse(substitute(x)) == "row.names") {
    # retain order of row names, otherwise will be alphabetical
    data.x <- data.frame(factor(row.names(data), levels=row.names(data)))
    if (is.null(xlab)) xlab <- ""  # unless specified, drop the axis label
    cat.x <- TRUE
  }

  else {
    if (!exists(x.name, where=.GlobalEnv)) {  # x not in global env, in df
      .nodf(df.name)  # check to see if data frame container exists
      .xcheck(x.name, df.name, data)  # var in df?, vars lists not checked
      all.vars <- as.list(seq_along(data))  # even if only a single var
      names(all.vars) <- names(data)  # all data in data frame
      x.col <- eval(substitute(x), envir=all.vars)  # col num of selected vars
      if (!("list" %in% class(data))) {
        data.x <- data[, x.col]
        if (length(x.col) == 1) {  # x is 1 var
          data.x <- data.frame(data.x)
          names(data.x) <- x.name
        }
      }
      else {  # class of data is "list"
        data.x <- data.frame(data[[x.col]])
        names(data.x) <- x.name
      }
    }

    else {  # x is in the global environment (vector or data frame)
      if (is.data.frame(x)) {  # x a data frame
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Need to specify variables for analysis,\n",
          "not a full data frame\n\n")
    }
      else {  # x a vector in global
        if (!is.function(x))
          data.x <- data.frame(x)  # x is 1 var
        else
          data.x <- data.frame(eval(substitute(data$x)))  # x is 1 var
        names(data.x) <- x.name
      }
    }
  }

  n.x_var <- ncol(data.x)  # number of x-variables

  # get data.x data to be analyzed into x.call, except for BPFM
  BPFM <- FALSE

  # just one x variable for now, a vector of cat or num values
  if (n.x_var == 1) {
    x.call <- data.x[,1]

    if (!is.ts(x.call)) {
      nu <- length(unique(na.omit(x.call)))
      num.cat.x <- .is.num.cat(x.call, n.cat)
    }
    else {
      nu <- length(unique(x.call))
      num.cat.x <- FALSE
    }

    if (!num.cat.x && is.integer(x.call) && !is.ts(x.call) && nu<= n.cat) {
      cat("\n")
      cat(">>> ", x.name, " has only only ", nu, " unique ",
          "integer values, but not equally spaced,\n",
          "      so treat as numerical in this analysis\n",
          "    Maybe convert to an R factor to treat as categorical\n",
          sep="")
    }

    if (num.cat.x && !quiet) .ncat("Plot", x.name, nu, n.cat)
    cat.x <- ifelse (num.cat.x || is.factor(x.call), TRUE, FALSE)

    if (cat.x  &&  object == "both") {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "A run chart applies only to continuous variables\n",
        x.name, " is a categorical variable\n\n")
    }
  }

  else {  # more than one x-variable
    # no y, see if eligible for BPFM where all selected vars are cat
    # multiple lines of a continuous x-variable also can occur
    if (y.miss  &&  !(object == "both")) {
      for (i in 1:length(x.col)) {  # see if variables are all categorical
        nu <- length(unique(na.omit(data.x[,i])))
        num.cat <- .is.num.cat(data.x[,i], n.cat)
        cat.all <- ifelse (num.cat || is.factor(data.x[,i]), TRUE, FALSE)
        if (!cat.all && object %in% c("point", "bubble")) {
          cat("\n"); stop(call.=FALSE, "\n","------\n",
            "All variables for multiple x-variables with no y-variable\n",
            "  must be categorical for a Bubble Plot Frequency Matrix\n\n",
            "A categorical variable is either an R factor variable,\n",
            "  or is numeric with not more than  n.cat  unique values\n\n",
            "Can set  n.cat  locally when calling Plot,\n",
            "  or globally with the function:  theme\n\n")
        }
      }
      cat.x <- TRUE
      BPFM <- TRUE
    }
    # if here already numeric, cat can be one-var or many x for a BPFM
    else {  # all to numeric, NA if char, internal code if factor
      x.call <- data.matrix(data.x, rownames.force=FALSE)
      cat.x <- FALSE
    }
  }  # end more than 1 x-variable
 
  
  if (!BPFM)
    n.rows <- ifelse (is.matrix(x.call), nrow(x.call), length(x.call))
  else
    n.rows <- length(data.x)


  # smooth default
  if (n.rows > 2499) if (missing(smooth)) smooth <- TRUE
  if (smooth) if (missing(color.grid)) color.grid <- "transparent"


  # evaluate y
  #-----------
  if (!missing(y)) {
    # get actual variable name before potential call of data$x
    y.name <- deparse(substitute(y))
    options(yname = y.name)

    if (deparse(substitute(y)) == "row.names") {
      # retain order of row names, otherwise will be alphabetical
      y.call <- factor(row.names(data), levels=row.names(data))
      if (is.null(ylab)) ylab <- ""  # unless specified, drop the axis label
      cat.y <- TRUE
    }

    else {
      if (!exists(y.name, where=.GlobalEnv)) {  # y not in global env, in df
        .nodf(df.name)  # check to see if data frame container exists
        .xcheck(y.name, df.name, data)  # var in df?, vars lists not checked
        all.vars <- as.list(seq_along(data))  # even if only a single var
        names(all.vars) <- names(data)  # all data in data frame
        y.col <- eval(substitute(y), envir=all.vars)  # col num selected vars
        if (!("list" %in% class(data))) {
          data.y <- data[, y.col]
          if (length(y.col) == 1) {  # y is 1 var
            data.y <- data.frame(data.y)
            names(data.y) <- y.name
          }
        }
        else {  # class of data is "list"
          data.y <- data.frame(data[[y.col]])
          names(data.y) <- y.name
        }
      }

      else { # y is in the global environment (vector or data frame)
        if (is.data.frame(y))  # y a data frame
          data.y <- y
        else {  # y a vector in global
          if (!is.function(y))
            data.y <- data.frame(y)  # y is 1 var
          else
            data.y <- data.frame(eval(substitute(data$y)))  # y is 1 var
          names(data.y) <- y.name
        }
      }

      if (ncol(data.y) == 1) {
        y.call <- data.y[,1]

        nu <- length(unique(na.omit(y.call)))
        num.cat.y <- .is.num.cat(y.call, n.cat)

        if (!num.cat.y && is.integer(y.call) && nu <= n.cat) {
          cat("\n")
          cat(">>> ", y.name, " has only only ", nu, " unique ",
              "integer values, but not equally spaced,\n",
              "      so treat as numerical in this analysis\n",
              "    Maybe convert to an R factor to treat as categorical\n",
              sep="")
        }

        if (num.cat.y && !quiet) .ncat("Plot", y.name, nu, n.cat)
        cat.y <- ifelse (num.cat.y || is.factor(y.call), TRUE, FALSE)
      }
      else {  # 2 or more y vars
        cat.y <- FALSE  #  multiple y-vars must be numerical
        if (missing(ylab)) ylab <- ""  # use legend instead
        y.call <- data.matrix(data.y, rownames.force=FALSE)
      }
    }
  }
  else { # missing y
    y.call <- NULL
    if (bar && missing(color.fill))
      color.fill <- getOption("color.fill.bar")
  }

    if (cat.x  &&  run.chart) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Run chart only applies to a numerical variable\n\n")
    }

  # ellipse stop conditions
  if (as.logical(ellipse[1])) {
    many.y <- FALSE
    if (!y.miss) if (is.matrix(y.call)) many.y <- TRUE
    if ((ncol(data.x)>1 || many.y)  ||  cat.x  ||  cat.y) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "An ellipse only applies to analysis of a single x-variable \n",
        "  with a single y-variable, both numerical\n\n")
    }
    if (y.miss) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Need a y-variable to compute an ellipse\n\n")
    }
  }

  
  # evaluate by
  #-----------
  if (!missing(by)) {

    # get actual variable name before potential call of data$x
    by.name <- deparse(substitute(by))
    options(byname = by.name)

    # get conditions and check for data existing
    xs <- .xstatus(by.name, df.name, quiet)
    in.global <- xs$ig

    # see if var exists in data frame, if x not in Global Env or function call
    if (!missing(x) && !in.global)
      .xcheck(by.name, df.name, data)

    if (!in.global)
      by.call <- eval(substitute(data$by))
    else {  # vars that are function names get assigned to global
      by.call <- by
      if (is.function(by.call)) by.call <- eval(substitute(data$by))
    }

    if (!is.factor(by.call)) by.call <- factor(by.call)
  }

  else
   by.call <- NULL


  # evaluate size (NULL, numeric constant or a variable)
  #--------------
  if (!missing(size)) {
    size.name <- deparse(substitute(size))

    # get conditions and check for data existing
    xs <- .xstatus(size.name, df.name, quiet)
    in.global <- xs$ig

    # if size.name is not a number, make.num gets NA with a warning
    make.num <- suppressWarnings(as.numeric(size.name))
    is.num <- ifelse (!is.na(make.num), TRUE, FALSE)

    # see if var exists in data frame, if x not in Global Env or function call
    if (!is.num) {  # size.name is the name of a variable
      if (!in.global) {
        .xcheck(size.name, df.name, data)
        size <- eval(substitute(data$size))
      }
      if (!is.numeric(size)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Variable ", size.name, " must be numeric\n\n",
          "Perhaps use: by=", size.name, "\n\n")
      }
      options(sizename = size.name) # for later access
      object <- "bubble"
      if (is.null(bubble.text)) bubble.text <- ifelse (cat.x, 1, 2)
    }
    else  # size is a numerical constant
      bubble.text <- FALSE
  }
  else
    if (missing(bubble.text)) bubble.text <- TRUE

  # -----------  x, y, by and size variables established ------------

  
  # --------
  # manage regular-R or PDF graphics window size
  
  if (is.null(y.call)  &&  !BPFM  &&  method != "stack" &&  values == "data"
      && !(object %in% c("both", "bar"))  &&  !is.ts(x.call))
    plt.h <- ifelse (is.null(main), 2.5, 3.1)  # narrow for 1-D plot
  else
    plt.h <- 7

  # BPFM, more than 7 variables, make plot extra long
  if (BPFM)
    plt.h <- plt.h + ((ncol(data.x) - 7) * 0.5)

  if (is.null(pdf.file)) {
    regR <- TRUE
    if (class(getOption("device")) == "character")
      if (getOption("device") == "RStudioGD") regR <- FALSE
    if (!is.null(options()$knitr.in.progress)) regR <- FALSE
    if (regR) {  # regular R run
      if (by.miss)  # set up graphics system to manage
        .graphwin(1, d.h=plt.h)
      else  # there is a by variable
        .graphwin(d.w=pdf.width)  # add width to default of 4.5 for legend
    }
  }
  else  {  # pdf file
    if (is.null(pdf.width)) pdf.width <- 4.5
    if (!missing(by.call)) pdf.width <- pdf.width + 0.6
    if (is.null(pdf.height)) pdf.height <- plt.h
    # windows puts a blank first page without the onefile=FALSE
    pdf(file=pdf.file, width=pdf.width, height=pdf.height, onefile=FALSE)
  }


  # ------------------------------------------------
  # set object and values where needed

  # prep 1-variable bubble plot to call regular scatter plot function
  # y.call to 0
  if (is.null(y.call)  &&  cat.x  &&  n.x_var == 1  &&  values == "data") {
    y.call <- rep(0, length(x.call))
    cat.y <- FALSE
    if (object == "default") object <- "bubble"
  }


  # if numeric x is sorted with equal intervals, set as line chart
  # x.call does not exist for BPFM
  if (!cat.x) if (is.numeric(x.call)  &&  n.rows > 2) {
    if (object == "default") {
      if (sum(is.na(x.call)) > 0)
        eq.int <- FALSE  # missing data in x present
      else {
        eq.int <- TRUE
        if (is.matrix(x.call))
          d.x <- diff(x.call[,1])  # only look at first x-variable
        else
          d.x <- diff(x.call)  # only look at first x-variable
        for (i in 2:(length(d.x)))
          if ((abs(d.x[i-1] - d.x[i]) > 0.0000000001)) eq.int <- FALSE
        rm(d.x)
      }  # also no y missing

      if(!is.unsorted(x.call) && eq.int && sum(is.na(y))==0) {
        object <- "both"
        if (is.null(size)) size <- 0  # by default, just plot a line without the points
      }
    }
  }

  if (object != "both") if (!BPFM) if (is.ts(x.call)) object <- "both"

  # if numeric y is sorted with equal intervals, line chart
  if (is.numeric(y.call)  &&  n.rows > 2) {
    if (object == "default") {
      if (sum(is.na(y.call)) > 0)
        eq.int <- FALSE  # missing data in x present
      else {
        eq.int <- TRUE
        if (is.matrix(y.call))
          d.y <- diff(y.call[,1])  # only look at first x-variable
        else
          d.y <- diff(y.call)  # only look at first x-variable
        for (i in 2:(length(d.y)))
          if ((abs(d.y[i-1] - d.y[i]) > 0.0000000001)) eq.int <- FALSE
        rm(d.y)
      }  # also no y missing

      if (!is.unsorted(y.call) && eq.int && sum(is.na(x))==0) object <- "both"
    }
  }

  # set point or bubble plot
  if (object == "default") {  # set default
    if (!missing(y)) {
      object <- "point"
      if (values == "data") if (cat.x && cat.y) object <- "bubble"
    }
    else if (values %in% c("count", "prop")) {
      object <- "point"
    }
    else {
      if (values == "data") object <- ifelse (cat.x, "bubble", "point")
      if (BPFM) object <- "bubble"  # BPFM
    }
  }


  # ------------------------------------------------
  # analysis
  # ------------------------------------------------
  
  
  if (getOption("suggest")) {
    # function call for suggestions
    fncl <- .fun.call.deparse(fun.call) 
    fncl <- gsub(")$", "", fncl)  # get function call less closing ) 
    fncl <- gsub(" = ", "=", fncl)
  }
  
  # histogram or bar chart
  if (object == "bar"  &&  values %in% c("count", "prop")) {

    if (!cat.x) {  # histogram

      if (values == "prop") proportion <- TRUE

      h <- .hst.main(x.call, color.fill, color.stroke, color.bg, color.grid,
         color.box, color.reg,
         over.grid=FALSE, cex.axis, color.axis, rotate.values, offset,
         breaks, bin.start, bin.width, bin.end, proportion, hist.counts, cumul,
         xlab, ylab, main, sub, quiet, fun.call=NULL, ...)

      if (!quiet) { # histogram text output

        stats <- .hst.stats(h, length(x.call), fun.call)

        txsug <- stats$txsug
        txdst <- stats$tx

        bin.width <- stats$bin.width
        n.bins <- stats$n.bins
        prop <- stats$prop
        cum.c <- stats$counts_cum
        cum.p <- stats$prop_cum

        txotl <- .outliers(data[,i])
        if (length(txotl)==0) txotl <- "No outliers"

        if (ncol(data) > 1) {  # for a variable range, print the text output
          class(txsug) <- "out_piece"
          class(txdst) <- "out_piece"
          class(txotl) <- "out_piece"
          output <- list(out_suggest=txsug, out_freq=txdst, out_outliers=txotl)
          class(output) <- "out_all"
          print(output)
        }
      }
    }

    else {  # categorical variable, bar chart for values = "count"

      if (values == "prop") proportion <- TRUE
      .bc.main(x.call, by=y.call,
        color.fill, color.stroke, color.bg, color.grid, color.box,
        colors=getOption("colors"),
        horiz, over.grid, addtop, gap,
        proportion, xlab, ylab, main, value.labels,
        cex.axis, color.axis, rotate.values, offset, beside,
        color.low, color.hi, count.labels,
        legend.title, legend.loc, legend.labels, legend.horiz,
        quiet=quiet, ...)
    }
  }

  # bubble plot frequency matrix (BPFM)
  else if (ncol(data.x) > 1  &&  y.miss  &&  object == "bubble") {
    # get labels just for subset data matrix
    mylabels <- attr(data, which="variable.labels")
    nm <- names(data.x)
    mylabs <- character(length=length(nm))
    for (i in 1:length(nm)) {
      if (!(nm[i] %in% names(mylabels)))
        mylabs[i] <- "not available"
      else
        mylabs[i] <- mylabels[which(names(mylabels) == nm[i])]
    }
    if (all(mylabs == "not available")) mylabs <- NULL

    if (is.null(xlab)) xlab <- ""  # suppress x-axis label if not specified

    .dpmat.main(data[,x.col], mylabs, sort.yx,
      color.fill, color.stroke, color.bg, color.grid, color.trans,
      shape, color.area, color.box,
      cex.axis, color.axis, color.low, color.hi,
      xy.ticks, xlab, ylab, main, sub, size,
      bubble.scale, bubble.text, bubble.power,
      value.labels, rotate.values, offset, quiet, fun.call, ...)
  }


  else {  # all the other analyses

    # line chart prep of x.call and y.call
    if (y.miss) {  

      if (!cat.x) { 

        if (object == "both"  &&  values == "data") {
          y.call <- x.call
          cat.y <- cat.x
          options(yname = x.name)
          if (!is.ts(y.call)) {
            options(xname = "Index")
            if (!is.matrix(x.call)) # xlab <- "Index"
              x.call <- 1:length(x.call)
            else {
              x.call <- 1:nrow(x.call)
            }
          }
          else {  # time series
            x.call <- ts.dates(y.call)
          }
        }

        else if (values %in% c("count", "prop")) {  # frequency polygon

          ssstuff <- .ss.numeric(x.call, digits.d=digits.d, brief=TRUE)
          txss <- ssstuff$tx  # stats output before reduce data
         
          h <- .hst.main(x.call, color.fill, color.stroke, color.bg, color.grid,
             color.box, color.reg,
             over.grid=FALSE, cex.axis, color.axis, rotate.values, offset,
             breaks, bin.start, bin.width, bin.end, proportion, hist.counts, cumul,
             xlab, ylab, main, sub, quiet, fun.call=NULL, do.plot=FALSE, ...) 

          x.call <- h$mids
          y.call <- h$counts
          if (values == "count")
            ylab <- paste("Count of", x.name)
          else {
            y.call <- y.call / sum(y.call)
            ylab <- paste("Proportion of", x.name)
          }

          object <- "both"  # do freq poly as a line chart
          center.line <- "off"  # not meaningful here
    
          txsug <- ""
          if (getOption("suggest")) {
            txsug <- ">>> Suggestions"

            fc <- ""
            if (!grepl("color.area", fncl))
              fc <- paste(fc, ", color.area=TRUE", sep="")
            if (nzchar(fc)) {
              fc <- paste(fncl, fc, ") ", sep="")
              txsug <- paste(txsug, "\n", fc, sep="")
            }
              
            fc <- ""
            if (!grepl("size", fncl)  &&  !grepl("color.area", fncl))
              fc <- paste(fc, ", size=0", sep="")
            if (nzchar(fc)) {
              fc <- gsub(" = ", "=", fc)
              fc <- paste(fncl, fc, ")   # just line segments, no points", sep="")
              txsug <- paste(txsug, "\n", fc, sep="")
            }

            bw.new <- pretty((x.call[2] - x.call[1]) / 1.5)  # arbitrary value of new bw
            fc <- ""
            if (!grepl("bin.width", fncl))
              fc <- paste(fc, ", bin.width=", as.character(bw.new[1]), sep="")
            if (nzchar(fc)) {
              fc <- paste(fncl, fc, ") ", sep="")
              txsug <- paste(txsug, "\n", fc, sep="")
            }
              
            txsug <- .rm.arg.2(" x=", txsug) 
            txsug <- .rm.arg.2("(x=", txsug) 
          }
      
          class(txsug) <- "out_piece"
        
          txdst <- h$ttx
          if (is.null(txdst)) txdst <- ""

          txotl <- .outliers(x.call)
          if (txotl[1] == "") txotl <- "No (Box plot) outliers"
          class(txss) <- "out_piece"
          class(txdst) <- "out_piece"
          class(txotl) <- "out_piece"
          output <- list(out_suggest=txsug, out_ss=txss, out_freq=txdst,
                         out_outliers=txotl)
          class(output) <- "out_all"
          print(output)

        }
      }

      else {  # cat.x
        # just x variable, so set y.call to plot points for count and prop
        if (values %in% c("count", "prop")) {
          cat.y <- FALSE
          if (seg.x.miss) segments.x <- TRUE
          ylab <- ifelse (values=="count", "Count of", "Proportion of")
          ylab <- paste(ylab, x.name)
          frq <- table(x.call)
          if (values == "prop") frq <- frq / sum(frq)
          y.call <- as.vector(frq)
          if (is.factor(x.call))  # preserve ordering, will lose order attribute
            x.call <- factor(names(frq), levels=levels(x.call))
          else
            x.call <- factor(names(frq))
        }
      }

    }  # end is null y.call
    

    else if (values %in% c("sum", "mean", "sd", "min", "median", "max")) {

      n.cat <- 0
      means <- FALSE
      if (missing(segments.x)) segments.x <- TRUE
      if (is.null(color.trans)) color.trans <- 0

      # do stats console output before reducing data
      if (!quiet) {
        if (!missing(y)) {
          if (cat.y) {
            cat("\n"); stop(call.=FALSE, "\n","------\n",
            y.name, " is not numerical, so cannot compute its mean\n\n")
          }
          options(yname = x.name)  # reverse order of x and y for .ss.numeric
          options(xname = y.name)
          stats <- .ss.numeric(y.call, by=x.call, digits.d=digits.d, brief=TRUE)
          txout <- stats$tx
          options(xname = x.name)  # reverse back
          options(yname = y.name)
        }
        else  {
          stats <- .ss.factor(x.call, digits.d=digits.d, x.name=x.name,
                              brief=TRUE)
          txout <- stats$counts
        }

        class(txout) <- "out_piece"

        output <- list(out_txt=txout)
        class(output) <- "out_all"
        print(output)
      }

    # set up new x.call and y.call for stats
      if (values == "sum") {
        ylab <- paste("Sum of", y.name)
        out <- tapply(y.call, x.call, sum, na.rm=TRUE)
      }
      if (values == "mean") {
        ylab <- paste("Mean of", y.name)
        out <- tapply(y.call, x.call, mean, na.rm=TRUE)
      }
      if (values == "sd") {
        ylab <- paste("Standard Deviation of", y.name)
        out <- tapply(y.call, x.call, sd, na.rm=TRUE)
      }
      if (values == "min") {
        ylab <- paste("Minimum of", y.name)
        out <- tapply(y.call, x.call, min, na.rm=TRUE)
      }
      if (values == "median") {
        ylab <- paste("Median of", y.name)
        out <- tapply(y.call, x.call, median, na.rm=TRUE)
      }
      if (values == "max") {
        ylab <- paste("Maximum of", y.name)
        out <- tapply(y.call, x.call, max, na.rm=TRUE)
      }

      if (is.factor(x.call))  # preserve ordering, will lose order attribute
        x.call <- factor(names(out), levels=levels(x.call))
      else {
        if (is.numeric(x.call)) {
          m1 <- min(sort(unique(x.call)))
          m2 <- max(sort(unique(x.call)))
          x.call <- factor(names(out), levels=m1:m2)  # get entire numeric range
        }
        else
          x.call <- factor(names(out))
      }
      y.call <- as.vector(out)

      if (bar) {  # bar chart for stats
        names(y.call) <- x.call

        .bc.main(y.call, by=NULL,
          color.fill, color.stroke, color.bg, color.grid, color.box,
          colors=getOption("colors"),
          horiz, over.grid, addtop, gap,
          proportion, xlab, ylab, main, value.labels,
          cex.axis, color.axis, rotate.values, offset, beside,
          color.low, color.hi, count.labels,
          legend.title, legend.loc, legend.labels, legend.horiz,
          quiet=quiet, ...)
      }
    }  # sum, mean, sd, min, median, max


    # 2-variable scatter plot
    # bubble plot for 1-variable (y.call=0) and 2-variable
    # line chart
    if (!is.null(y.call)  &&
        object %in% c("point", "bubble", "both", "sunflower")) {

      unique.x <- ifelse (length(unique(x.call)) == length(x.call), TRUE, FALSE)
      unique.y <- ifelse (length(unique(y.call)) == length(y.call), TRUE, FALSE)

      if (object == "point"  &&  values == "data"){  # for Cleveland dot plot
        if (!cat.x && cat.y && unique.y) {  # no sort.xy option
          if (seg.y.miss) if (unique.y && cat.y) segments.y <- TRUE
          if (seg.x.miss) if (unique.x && cat.x) segments.x <- TRUE
          if (sort.yx.miss) if (n.x_var <= 2) sort.yx <- TRUE
          if (col.grid.miss) color.grid <- "transparent"
        }
      }

     # sort y by x option (intended for Cleveland dot plot)
      if (sort.yx) {
        if (n.x_var == 1)  # one x-variable
          ord <- order(x.call)
        else
          if (n.x_var == 2)  # two x-vars, sort on diffs
            ord <- order(x.call[,2] - x.call[,1])
          else {
            cat("\n"); stop(call.=FALSE, "\n","------\n",
            "Sorting not meaningful for more than two x-variables\n\n")
          }
        y.call <- factor(y.call, levels=y.call[ord])
      }

      # bigger point for scatterplot of stats
      if (values != "data"  &&  object == "point"  )
        if (is.null(size)) size <- 1.75

      .plt.main(x.call, y.call, by.call, n.cat,
         object, values,
         color.fill, color.stroke, color.bg, color.grid, color.box,
         color.trans, color.area,
         cex.axis, color.axis, xy.ticks,
         xlab, ylab, main, sub, value.labels, rotate.values, offset,
         proportion,
         size, shape, means, sort.yx, segments.y, segments.x, line.width,
         smooth, smooth.points, smooth.trans, smooth.bins,
         bubble.scale, bubble.power, bubble.text, color.low, color.hi,
         fit.ln, color.fit.line,
         ellipse, color.ellipse, color.fill.ellipse,
         center.line, show.runs,
         method, pt.reg, pt.out, color.out30, color.out15,
         quiet, fun.call, ...)
    }

    # 1-D traditional scatter plot (not bubble plot)
    else if (!cat.x  &&  is.null(y.call)  &&  values == "data") {

      .dp.main(x.call, by.call, size,
         color.fill, color.stroke, color.bg, color.grid, color.trans,
         shape, cex.axis, color.axis, xlab, main, sub,
         rotate.values, offset, method, pt.reg, pt.out,
         color.out30, color.out15, boxplot, quiet, new, 
         vertical=FALSE, fun.call, ...)

        # R Markdown
        #txkfl <- ""
        #if (!is.null(Rmd)) {
          #if (!grepl(".Rmd", Rmd)) Rmd <- paste(Rmd, ".Rmd", sep="")
          #txknt <- .dist.Rmd(x.name, df.name, fun.call, digits.d)
          #cat(txknt, file=Rmd, sep="\n")
          #txkfl <- .showfile2(Rmd, "R Markdown instructions")
        #}

        #class(txkfl) <- "out_piece"

        #output <- list(
          #call=fun.call,
          #type="1D_ScatterPlot", out_file=txkfl)

        #class(output) <- "out_all"

        #return(output)

    }  # end 1-D plot

  }


  # terminate pdf graphics system if used
  if (!is.null(pdf.file)) {
    dev.off()
    .showfile(pdf.file, "plot")
  }

}
