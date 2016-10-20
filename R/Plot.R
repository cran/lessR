Plot <-
function(x, y=NULL, by=NULL, data=mydata, n.cat=getOption("n.cat"),

         topic=c("data", "count", "prop", "sum", "mean", "sd", "min", "median",
                 "max"),
         object=c("point", "line", "both", "sunflower", "bar", "off"),

         color.fill=getOption("color.fill.pt"),
         color.stroke=getOption("color.stroke.pt"),
         color.bg=getOption("color.bg"),
         color.grid=getOption("color.grid"),
         color.box=getOption("color.box"),

         color=NULL, color.trans=NULL, color.area=NULL,

         cex.axis=0.76, color.axis="gray30", xy.ticks=TRUE,
         xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
         value.labels=NULL, rotate.values=0, offset=0.5,
         proportion=FALSE,

         size=NULL, shape="circle", means=TRUE,
         sort.yx=FALSE, segments.y=FALSE, segments.x=FALSE,

         smooth=FALSE, smooth.points=100, smooth.trans=0.25,
         smooth.bins=128,

         bubble.scale=0.25, bubble.power=0.6, bubble.text=NULL,
         color.low=NULL, color.hi=NULL,

         fit.line=NULL, color.fit.line="gray55",

         ellipse=FALSE, color.ellipse="lightslategray",
         color.fill.ellipse="off",

         method="overplot", pt.reg="circle", pt.out="circle",
         color.out30="firebrick2", color.out15="firebrick4", new=TRUE,

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


  if (is.null(fun.call)) fun.call <- match.call()

  # missing function only reliable if arg not modified, so capture here
  object.miss <- ifelse (missing(object), TRUE, FALSE)
  topic.miss <- ifelse (missing(topic), TRUE, FALSE)
  seg.y.miss <- ifelse (missing(segments.y), TRUE, FALSE)  # for Cleveland plot
  seg.x.miss <- ifelse (missing(segments.x), TRUE, FALSE)
  sort.yx.miss <- ifelse (missing(sort.yx), TRUE, FALSE)
  col.grid.miss <- ifelse (missing(color.grid), TRUE, FALSE)

  if (color.grid == "on") color.grid <- getOption("color.grid") # for Cleveland

  object <- match.arg(object)
  topic <- match.arg(topic)
  cumul <- match.arg(cumul)

  if (object.miss) object <- "default"

  # any bubble parameter activates a bubble plot
  if (!missing(bubble.scale) || !missing(bubble.scale) || !missing(bubble.text)) {
    object <- "bubble"
  }

  # any ellipse parameter actives an ellipse
  if (missing(ellipse)) if (!missing(color.ellipse) || !missing(color.fill.ellipse))
    ellipse <- TRUE

  # any bin parameter activates bins
  if (!missing(breaks)  ||  !missing(bin.start)  ||  !missing(bin.width)  ||
      !missing(bin.end))
    object <- "bar"

  if (object == "bar") {
    if (topic.miss) topic <- "count"  # default topic for bars
    if (topic == "data") {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Bars do not apply to data, only to count, means, etc.\n\n")
    }
    if (missing(color.fill)) color.fill <- getOption("color.fill.bar")
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

  # populate color parameters
  if (!is.null(color)) {
    color.stroke <- color
    color.fill <- color
  }

  if (is.null(fit.line)) fit.ln <- "none"
  if (is.logical(fit.line))
    fit.ln <- ifelse (fit.line, "loess", "none")
  if (is.character(fit.line)) {
    if (!(fit.line %in% c("loess", "ls", "none"))) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "fit.line applies only for  loess  or  ls  (least squares)\n\n")
    }
    fit.ln <- fit.line
  }

  if (is.logical(ellipse)) if (ellipse) ellipse <- 0.95
  if (as.logical(ellipse[1])) {
    txt <- "[Ellipse with Murdoch and Chow's function ellipse"
    cat(txt, "from the ellipse package]\n")
  }

  if (!is.null(pdf.file))
    if (!grepl(".pdf", pdf.file)) pdf.file <- paste(pdf.file, ".pdf", sep="")


  dots <- list(...)  # check for deprecated parameters
  if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      old.nm <- c("col.fill", "col.stroke", "col.bg", "col.grid", "col.box",
                  "col.reg", "col.axis", "col.trans", "col.low", "col.hi",
                  "col.ellipse", "col.fill.ellipse", "col.fit.line", "col.out30",
                  "col.out15")
      if (names(dots)[i] %in% old.nm) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "options that began with the abbreviation  col  now begin with  ",
          "color \n\n")
      }
      if (names(dots)[i] %in% c("x.start","x.end","y.start","y.end")) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "x.start, x.end, y.start, and y.end no longer used\n\n",
          "Instead use the standard R xlim and ylim parameters,\n",
          "such as xlim=c(0,40) to specify from 0 to 40. Same for ylim.\n\n")
      }
      if (names(dots)[i] == "kind") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  kind  is renamed  object\n\n")
      }
      if (names(dots)[i] == "knitr.file") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "knitr.file  no longer used\n",
          "Instead use  Rmd  for R Markdown file\n\n")
      }
      if (names(dots)[i] == "type") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "type  option replaced with  object\n\n")
      }
      if (names(dots)[i] == "diag") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "diag  option no longer available\n\n")
      }
    }
  }

  # inconsistent parameter values
  if (missing(x)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Must specify at least one variable to analyze\n\n")
  }

  if (topic %in% c("mean", "sd", "min", "max") && missing(y)) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Must specify a numeric y-variable from which to compute the\n ",
      " ", topic, " for each level of ", deparse(substitute(x)), "\n\n")
  }

  if (topic != "data"  &&  object == "sunflower") {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Sunflowers are only plotted for data\n\n")
  }

  if (method == "stack") {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Option  stack  does not work\n\n")
  }

  if (method %in% c("spearman", "kendall")) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "The  method  parameter has another meaning for Plot\n\n",
      "Compute a Spearman or Kendall correlation\n",
      "  with the Correlation function\n\n")
  }

  if (is.numeric(breaks) && !is.null(bin.start)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Choose only one option to specify a start value.\n",
      "Either choose the option  breaks  or the option  bin.start.\n\n")
  }


  # process shapes
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
        shape[i] <- which(shape[i]==shapes) + 20
        num.flag <- TRUE
    }
  }
  if (num.flag) shape <- as.numeric(shape)

  if (pt.reg %in% shapes)
    pt.reg <- which(pt.reg==shapes) + 20
  else
    if (!(pt.reg %in% c(21:25))) bad.shape <- pt.reg

  if (pt.out %in% shapes)
    pt.out <- which(pt.out==shapes) + 20
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


  # ------
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

    if (cat.x  &&  object %in% c("line", "both")) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "A line applies only to continuous variables\n",
        x.name, " is a categorical variable\n\n")
    }
  }

  else {  # more than one x-variable
    # no y, see if eligible for BPFM where all selected vars are cat
    # multiple lines of a continuous x-variable also can occur
    if (missing(y)  &&  !(object %in% c("line", "both"))) {
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
    n.rows <- ifelse(is.matrix(x.call), nrow(x.call), length(x.call))
  else
    n.rows <- length(data.x)

  # smooth default
  smooth.switch <- FALSE
  if (n.rows > 2499) {
    if (missing(smooth)) smooth <- TRUE
    smooth.switch <- TRUE
  }
  if (smooth) if (missing(color.grid)) color.grid="transparent"


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
    if (!cat.x && topic %in% c("count", "prop") && (object.miss)) {
      object <- "bar"
      if (missing(color.fill)) color.fill <- getOption("color.fill.bar")
    }
  }


  # ellipse stop conditions
  if (as.logical(ellipse[1])) {
    many.y <- FALSE
    if (!missing(y)) if (is.matrix(y.call)) many.y <- TRUE
    if ((ncol(data.x)>1 || many.y)) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "An ellipse only applies to an analysis of a single x-variable\n",
        "  with a single y-variable\n\n")
    }
    if (missing(y)) {
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
      if (is.null(bubble.text)) bubble.text <- ifelse(cat.x, 1, 2)
    }
    else  # size is a numerical constant
      bubble.text <- FALSE
  }
  else
    if (missing(bubble.text)) bubble.text <- TRUE


  # --------
  # graphics
  if (is.null(y.call)  &&  !BPFM  &&  method != "stack" &&  topic == "data"
      && !(object %in% c("line", "both", "bar"))  &&  !is.ts(x.call))
    plt.h <- ifelse(is.null(main), 2.5, 3.1)  # narrow for 1-D plot
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
      if (missing(by)) {  # set up graphics system to manage
      #  if (is.null(y.call)) {  # applies to 1-D scatter plots and BPFM
          .graphwin(1, d.h=plt.h)
        # }
        # else
          # .graphwin(1)
      }
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
  # set object and topic where needed

  # prep 1-variable bubble plot to call regular scatter plot function
  # y.call to 0
  if (is.null(y.call)  &&  cat.x  &&  n.x_var == 1  &&  topic == "data") {
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

      if(!is.unsorted(x.call) && eq.int && sum(is.na(y))==0) object <- "line"
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

      if(!is.unsorted(y.call) && eq.int && sum(is.na(x))==0) object <- "line"
    }
  }

  # set point or bubble plot
  if (object == "default") {  # set default
    if (!missing(y)) {
      object <- "point"
      if (topic == "data") if (cat.x && cat.y) object <- "bubble"
    }
    else if (topic %in% c("count", "prop")) {
      object <- "point"
    }
    else {
      if (topic == "data") object <- ifelse (cat.x, "bubble", "point")
      if (BPFM) object <- "bubble"  # BPFM
    }
  }

  if (!quiet) {
    cat("\n")
    cat(">>> values to plot:   topic = \"", topic, "\"\n", sep="")
    cat(">>> geometric object: object = \"", object, "\"\n", sep="")
    if (smooth.switch)
      cat("\n>>> 2500 or more rows of data, smoothing turned on \n",
          "    To plot the data as is: smooth=FALSE\n", sep="")
    cat("\n")
  }


  # ------------------------------------------------
  # analysis

  # histogram or bar chart
  if (object == "bar"  &&  topic %in% c("count", "prop")){

    if (!cat.x) {  # histogram

      if (topic == "prop") proportion <- TRUE

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

    else {  # categorical variable, bar chart for topic is count

      if (topic == "prop") proportion <- TRUE
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
  else if (ncol(data.x) > 1 && missing(y) && object == "bubble") {
    # get labels just for subset data matrix
    mylabels <- attr(data, which="variable.labels")
    nm <- names(data.x)
    if (!is.null(mylabels)) {
      mylabs <- character(length=length(nm))
      for (i in 1:length(nm))
        mylabs[i] <- mylabels[which(names(mylabels) == nm[i])]
    }
    else
      mylabs <- NULL
    if (length(mylabs) == 0) mylabs <- NULL  # when labels, but not relevant

    if (is.null(xlab)) xlab <- ""  # suppress x-axis label if not specified

    .dpmat.main(data[,x.col], mylabs, nm,
      color.fill, color.stroke, color.bg, color.grid, color.trans,
      shape, color.area, color.box,
      cex.axis, color.axis, color.low, color.hi,
      xy.ticks, xlab, ylab, main, sub, size,
      bubble.scale, bubble.text, bubble.power,
      value.labels, rotate.values, offset, quiet, ...)
  }

  else {  # all the other analyses

    # line chart prep of x.call and y.call
    if (is.null(y.call) &&  !cat.x  && object %in% c("line", "both")
        && topic == "data") {
      y.call <- x.call
      cat.y <- cat.x
      options(yname = x.name)
      if (!is.ts(y.call)) {
        options(xname = "Index")
        if (!is.matrix(x.call)) #xlab <- "Index"
          x.call <- 1:length(x.call)
        else {
          x.call <- 1:nrow(x.call)
        }
      }
      else {  # time series
        x.call <- ts.dates(y.call)
      }
    }

    # only 1 variable, so set y.call to plot points for count and prop
    if (topic %in% c("count", "prop")) {
      if (is.null(y.call) && cat.x) {
        if (seg.x.miss) segments.x <- TRUE
        ylab <- ifelse(topic=="count", "Count of", "Proportion of")
        ylab <- paste(ylab, x.name)
        cat.y <- FALSE
        frq <- table(x.call)
        if (topic == "prop") frq <- frq / sum(frq)
        y.call <- as.vector(frq)
        if (is.factor(x.call))  # preserve ordering, will lose order attribute
          x.call <- factor(names(frq), levels=levels(x.call))
        else
          x.call <- factor(names(frq))
      }
    }

    # do stats console output before reducing data
    if (topic %in% c("sum", "mean", "sd", "min", "median", "max")) {

      n.cat <- 0
      means <- FALSE
      if (missing(segments.x)) segments.x <- TRUE
      if (is.null(color.trans)) color.trans <- 0

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
      if (topic == "sum") {
        ylab <- paste("Sum of", y.name)
        out <- tapply(y.call, x.call, sum, na.rm=TRUE)
      }
      if (topic == "mean") {
        ylab <- paste("Mean of", y.name)
        out <- tapply(y.call, x.call, mean, na.rm=TRUE)
      }
      if (topic == "sd") {
        ylab <- paste("Standard Deviation of", y.name)
        out <- tapply(y.call, x.call, sd, na.rm=TRUE)
      }
      if (topic == "min") {
        ylab <- paste("Minimum of", y.name)
        out <- tapply(y.call, x.call, min, na.rm=TRUE)
      }
      if (topic == "median") {
        ylab <- paste("Median of", y.name)
        out <- tapply(y.call, x.call, median, na.rm=TRUE)
      }
      if (topic == "max") {
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

      if (object == "bar") {  # bar chart for stats
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
        object %in% c("point", "bubble", "line", "both", "sunflower")) {

      unique.x <- ifelse(length(unique(x.call)) == length(x.call), TRUE, FALSE)
      unique.y <- ifelse(length(unique(y.call)) == length(y.call), TRUE, FALSE)

      if (object == "point"  &&  topic == "data"){  # for Cleveland dot plot
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
      if (topic != "data"  &&  object == "point"  )
        if (is.null(size)) size <- 1.75

      .plt.main(x.call, y.call, by.call, n.cat,
         object, topic,
         color.fill, color.stroke, color.bg, color.grid, color.box,
         color.trans, color.area,
         cex.axis, color.axis, xy.ticks,
         xlab, ylab, main, sub, value.labels, rotate.values, offset,
         proportion,
         size, shape, means, sort.yx, segments.y, segments.x,
         smooth, smooth.points, smooth.trans, smooth.bins,
         bubble.scale, bubble.power, bubble.text, color.low, color.hi,
         fit.ln, color.fit.line,
         ellipse, color.ellipse, color.fill.ellipse,
         method, pt.reg, pt.out, color.out30, color.out15,
         quiet, fun.call, ...)
    }

    # 1-D traditional scatter plot (not bubble plot)
    else if (!cat.x  &&  is.null(y.call)) {

      .dp.main(x.call, by.call, size,
         color.fill, color.stroke, color.bg, color.grid, color.trans,
         shape, cex.axis, color.axis, xlab, main, sub,
         rotate.values, offset, method, pt.reg, pt.out,
         color.out30, color.out15, quiet, new, fun.call, ...)

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
