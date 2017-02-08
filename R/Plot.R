Plot <-
function(x, y=NULL, by=NULL, data=mydata, n.cat=getOption("n.cat"),

         values=c("data", "count", "prop", "sum", "mean", "sd", "min",
                  "median", "max"),

         fill=getOption("fill.pt"),
         stroke=getOption("stroke.pt"),
         bg=getOption("bg"),
         grid=getOption("grid"),
         box=getOption("box"),
         segment=getOption("fill.pt"),
         color=NULL, trans=NULL,

         cex.axis=0.76, axes="gray30", xy.ticks=TRUE,
         xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
         value.labels=NULL, label.max=20,
         rotate.values=0, offset=0.5, proportion=FALSE,

         size=NULL, shape="circle", means=TRUE,
         sort.yx=FALSE,
         segments.y=FALSE, segments.x=FALSE,

         bubble.scale=0.25, bubble.power=0.6, bubble.text=NULL,
         low.color=NULL, hi.color=NULL,

         smooth=FALSE, smooth.points=100, smooth.trans=0.25,
         smooth.bins=128,

         fit=NULL, stroke.fit=getOption("stroke.bar"),
         se.fit=0,

         ellipse=FALSE, stroke.ellipse=getOption("stroke.pt"),
         fill.ellipse=getOption("fill.ellipse"),

         method="overplot", pt.reg="circle", pt.out="circle",
         out30="firebrick2", out15="firebrick4", new=TRUE,
         boxplot=FALSE,

         line.chart=FALSE, line.width=2, area=FALSE, 
         center.line=c("default", "mean", "median", "zero", "off"),
         show.runs=FALSE, stack=FALSE,

         breaks="Sturges", bin.start=NULL, bin.width=NULL, bin.end=NULL,
         cumul=FALSE,

         digits.d=NULL, quiet=getOption("quiet"),
         width=NULL, height=NULL, pdf.file=NULL, 
         fun.call=NULL, ...) {

  # ------
  # set parameter values 
  
  if (is.null(fun.call)) fun.call <- match.call()
   
  # limit actual argument to alternatives, perhaps abbreviated
  values <- match.arg(values)
  center.line <- match.arg(center.line)

  df.name <- deparse(substitute(data))
  options(dname = df.name)

  # if a tibble convert to data frame
  # data.frame is already #3 in class(data), so no char --> factor conversion 
  if (class(data)[1] == "tbl_df") {
    data <- as.data.frame(data, stringsAsFactors=TRUE)
  }

  # missing function only reliable if arg not modified, so capture 
  x.miss <- ifelse (missing(x), TRUE, FALSE)
  y.miss <- ifelse (missing(y), TRUE, FALSE)
  by.miss <- ifelse (missing(by), TRUE, FALSE)
  values.miss <- ifelse (missing(values), TRUE, FALSE)
  seg.y.miss <- ifelse (missing(segments.y), TRUE, FALSE)  # for Cleveland plot
  seg.x.miss <- ifelse (missing(segments.x), TRUE, FALSE)
  sort.yx.miss <- ifelse (missing(sort.yx), TRUE, FALSE)
  col.grid.miss <- ifelse (missing(grid), TRUE, FALSE)
  
  date.ts <- FALSE  # default is not a time series
  freq.poly <- FALSE  # default is not a frequency polygon

  if (grid == "on") grid <- getOption("grid") # for Cleveland

  if (se.fit[1] > 0) if (is.null(fit)) fit <- TRUE

  if (show.runs) line.chart <- TRUE

  # area
  if (stack) if (missing(area)) area <- TRUE  # stack default
  if (is.logical(area))
    area <- ifelse (area, getOption("fill.pt"), "off")
  if (area != "off") if (missing(stack)) stack <- TRUE

  if (values != "data") if (is.null(trans)) trans <- 0

  # any bin parameter activates bins
  if (!missing(breaks)  ||  !missing(bin.start)  ||  !missing(bin.width)  ||
      !missing(bin.end))
    values <- "count"

  object <- "default"
  
  if (line.chart) object <- "both"

  # any bubble parameter activates a bubble plot
  if (!missing(bubble.scale) || !missing(bubble.scale) || !missing(bubble.text)) {
    object <- "bubble"
  }

  # any ellipse parameter actives an ellipse
  if (missing(ellipse)) 
    if (!missing(stroke.ellipse) || !missing(fill.ellipse))
      ellipse <- TRUE

  if (is.null(fit)) fit.ln <- "off"
  if (is.logical(fit))
    fit.ln <- ifelse (fit, "loess", "off")
  if (is.character(fit)) {
    if (!(fit %in% c("loess", "ls", "off"))) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "fit applies only for  loess  or  ls  (least squares)\n\n")
    }
    fit.ln <- fit  # fit.ln passed to .plt.main
  }
  
  # "off" substitutes for official value of "transparent"
  for (i in 1:length(fill))
    if (fill[i] == "off") fill[i] <- "transparent"
  for (i in 1:length(stroke))
    if (stroke[i] == "off") stroke[i] <- "transparent"
  if (bg == "off") bg <- "transparent"
  if (grid == "off" ) grid <- "transparent"
  if (box == "off") box <- "transparent"
  if (stroke.ellipse == "off") stroke.ellipse <- "transparent"
  if (fill.ellipse == "off") fill.ellipse <- "transparent"
  if (area == "off") area <- "transparent"

  # populate color parameters
  if (!is.null(color)) {
    stroke <- color
    fill <- color
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
  .plt.zbad(x.miss, y.miss, values, method, breaks, bin.start, ...)


  # ------
  # evaluate x
  
  # get actual variable name before potential call of data$x
  x.name <- deparse(substitute(x))
  options(xname = x.name)

  # get data to be analyzed into data.x data frame
  
  # process row.names if specified
  if (deparse(substitute(x)) == "row.names") {
    # retain order of row names, otherwise will be alphabetical
    data.x <- data.frame(factor(row.names(data), levels=row.names(data)))
    if (is.null(xlab)) xlab <- ""  # unless specified, drop the axis label
    cat.x <- TRUE
  }

  else if (!exists(x.name, where=.GlobalEnv)) {  # x not in global env, in df
      .nodf(df.name)  # check to see if data frame container exists
      .xcheck(x.name, df.name, data)  # var in df?, vars lists not checked
      all.vars <- as.list(seq_along(data))  # even if only a single var
      names(all.vars) <- names(data)  # all data in data frame
      x.col <- eval(substitute(x), envir=all.vars)  # col num of selected vars
      if (!("list" %in% class(data))) {
        data.x <- data[, x.col]
        data.x <- data.frame(data.x)
     }      
     else {  # class of data is "list"
        data.x <- data.frame(data[[x.col]])
      }
      names(data.x) <- names(all.vars)[x.col]

    }  # end x not in global

    # x is in the global environment (vector or data frame)
    # can only access x directly if it is not in a data frame
 
    else if (is.data.frame(x)) {
        # x a data frame
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Need to specify variables for analysis,\n",
          "not a full data frame\n\n")
    }
      
    else if (is.ts(x)) {  #  time series in global
        data.x <- data.frame(.ts.dates(x))  # just the dates for x var
        names(data.x) <- "date"
        # x.col <- 1  # just put dates in data.x
        # n.x_var <- 1  # number of x-variables
        date.ts <- TRUE
        if (is.null(xlab)) xlab <- ""  # unless specified, drop the axis label

       nc <- ifelse (is.matrix(x), ncol(x), 1)
        if (nc == 1)
          y.call <- x
        else {
          y.call <- x[,1]
          if (nc > 1) for (i in 2:nc) y.call <- cbind(y.call, x[,i])
        }
        y.call <- data.frame(y.call)
        y.col <- ncol(y.call)
        cat.y <- FALSE
        if (nc == 1) {
          names(y.call) <- x.name
          y.name <- x.name
        }
        else
          names(y.call) <- colnames(x)

    y.name <- deparse(substitute(x))
    options(yname = y.name)
    }
      
      else {  # x a not ts vector in global
        if (!is.function(x))
          data.x <- data.frame(x)  # x is 1 var, including a ts
        else
          data.x <- data.frame(eval(substitute(data$x)))  # x is 1 var
        names(data.x) <- x.name
      }
      
      n.x_var <- ncol(data.x)  # number of x-variables
      x.call <- data.x
      
    # end get data.x
    

  # get data.x data to be analyzed into x.call, except for BPFM
  BPFM <- FALSE

  # just one x variable for now, a vector of cat or num values
  if (n.x_var == 1) {
    date.ts <- ifelse (.is.date(x.call[,1]), TRUE, FALSE) 
    if (!date.ts) {
      nu <- length(unique(na.omit(x.call[,1])))
      num.cat.x <- .is.num.cat(x.call[,1], n.cat)
    }
    else {  # process ts
      nu <- length(unique(x.call[,1]))
      num.cat.x <- FALSE
    }

    if (!num.cat.x && is.integer(x.call[,1]) && !date.ts && nu<= n.cat) {
      cat("\n")
      cat(">>> ", x.name, " has only only ", nu, " unique ",
          "integer values, but not equally spaced,\n",
          "      so treat as numerical in this analysis\n",
          "    Maybe convert to an R factor to treat as categorical\n",
          sep="")
    }

    if (num.cat.x && !quiet) .ncat("Plot", x.name, nu, n.cat)
    cat.x <- ifelse (num.cat.x || is.factor(x.call[,1]), TRUE, FALSE)

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
    # all to numeric, NA if char, internal code if factor
    # if a mts, data.matrix does nothing as is already a matrix
    else {
      # x.call <- data.frame(data.x)
      # x.call <- data.matrix(data.x, rownames.force=FALSE)
      cat.x <- FALSE
    }
  }  # end more than 1 x-variable
 
  
  if (!BPFM)
    n.rows <- ifelse (is.matrix(x.call[,1]), nrow(x.call[,1]), length(x.call[,1]))
  else
    n.rows <- nrow(data.x)
    #n.rows <- length(data.x)


  # smooth default
  if (n.rows > 2499) if (missing(smooth)) smooth <- TRUE
  if (smooth) if (missing(grid)) grid <- "transparent"

  if(date.ts) {
    object <- "both"
  }


  # evaluate y
  #-----------
  if (!y.miss) {
    # get actual variable name before potential call of data$y
    y.name <- deparse(substitute(y))
    options(yname = y.name)

    if (deparse(substitute(y)) == "row.names") {
      # retain order of row names, otherwise will be alphabetical
      y.call <- factor(row.names(data), levels=row.names(data))
      if (is.null(ylab)) ylab <- ""  # unless specified, drop the axis label
      cat.y <- TRUE
      data.y <- data.frame(y.call)
    }
      
    else if (!exists(y.name, where=.GlobalEnv)) {  # y not in global env, in df
        .nodf(df.name)  # check to see if data frame container exists
        .xcheck(y.name, df.name, data)  # var in df?, vars lists not checked
        all.vars <- as.list(seq_along(data))  # even if only a single var
        names(all.vars) <- names(data)  # all data in data frame
        y.col <- eval(substitute(y), envir=all.vars)  # col num selected vars
       if (!("list" %in% class(data))) {
          data.y <- data[, y.col]
          data.y <- data.frame(data.y)
       }      
       else {  # class of data is "list"
          data.y <- data.frame(data[[y.col]])
        }
        names(data.y) <- names(all.vars)[y.col]        
      }  # end global y

      else if (is.data.frame(y)){ # y is in the global env (vector or data frame)
          # y a data frame
          data.y <- y
       }
       
        else {  # y a vector in global
          if (!is.function(y))
            data.y <- data.frame(y)  # y is 1 var
          else
            data.y <- data.frame(eval(substitute(data$y)))  # y is 1 var
          names(data.y) <- y.name
        }

      n.y_var <- ncol(data.y)  # number of y-variables
      y.call <- data.y
    # end get data.y
 

      if (ncol(y.call) == 1) { # y is one variable
        # y.call <- data.y[,1]

        nu <- length(unique(na.omit(y.call)))
        num.cat.y <- .is.num.cat(y.call[,1], n.cat)

        if (!num.cat.y && is.integer(y.call[,1]) && nu <= n.cat) {
          cat("\n")
          cat(">>> ", y.name, " has only only ", nu, " unique ",
              "integer values, but not equally spaced,\n",
              "      so treat as numerical in this analysis\n",
              "    Maybe convert to an R factor to treat as categorical\n",
              sep="")
        }

        if (num.cat.y && !quiet) .ncat("Plot", y.name, nu, n.cat)
        cat.y <- ifelse (num.cat.y || is.factor(y.call[,1]), TRUE, FALSE)
      }
      
      else {  # 2 or more y vars
        cat.y <- FALSE  #  multiple y-vars must be numerical
        if (missing(ylab)) ylab <- ""  # use legend instead
        y.call <- data.frame(data.y)
        # y.call <- data.matrix(data.y, rownames.force=FALSE)
      }
  }  # end y not missing
  
  else { # missing y
    if (!date.ts) y.call <- NULL

  }  # end missing y

    if (cat.x  &&  line.chart) {
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
  # -----------------------------------------------------------------

  
  # --------
  # manage regular-R or PDF graphics window size
  if (is.null(height)) { 
    if (is.null(y.call)  &&  !BPFM  &&  method != "stack"  &&  values == "data"
        &&  object != "both"  &&  !.is.date(x.call))
      height <- ifelse (is.null(main), 2.5, 3.1)  # narrow for 1-D dotplot
    else
      height <- 4.5

    if (BPFM)  # more than 7 variables, make plot extra long
      height <- height + ((ncol(data.x) - 7) * 0.5)
  }

  if (is.null(width)) width <- 4.5
  if (!by.miss) width <- width + .85  # wider plot

  # prepare graphics window, dev or pdf
  .opendev(pdf.file, width, height)


  # ------------------------------------------------
  # set object and values where needed

  # prep 1-variable bubble plot to call regular scatter plot function
  # y.call to 0
  if (is.null(y.call)  &&  cat.x  &&  n.x_var == 1  &&  values == "data") {
    y.call <- data.frame(rep(0, nrow(x.call)))
    cat.y <- FALSE
    if (object == "default") object <- "bubble"
  }

  # if numeric x is sorted with equal intervals, set as line chart
  # x.call does not exist for BPFM
  if (!cat.x) if (is.numeric(x.call[,1])  &&  n.rows > 2) {
    if (object == "default") {
      eq.int <- ifelse (sum(is.na(x.call[,1])) > 0, FALSE, TRUE)
      if (eq.int) {
        d.x <- diff(x.call[,1])  # only look at first x-variable
        for (i in 2:(length(d.x)))
          if ((abs(d.x[i-1] - d.x[i]) > 0.0000000001)) eq.int <- FALSE
        rm(d.x)
      }  # also no y missing

      if(!is.unsorted(x.call) && eq.int && sum(is.na(y))==0) {
        object <- "both"
        if (is.null(size)) size <- 0  # by default, just plot a line w/o points
      }
    }
  }

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
    if (!y.miss) {
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
  

  # bubble plot frequency matrix (BPFM)
  if (ncol(data.x) > 1  &&  y.miss  &&  object == "bubble") {
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
      fill, stroke, bg, grid, trans,
      shape, area, box,
      cex.axis, axes, low.color, hi.color,
      xy.ticks, xlab, ylab, main, sub, size,
      bubble.scale, bubble.text, bubble.power,
      value.labels, rotate.values, offset, quiet, fun.call, ...)
  }


  else {  # all the other analyses

    # line chart prep of x.call and y.call
    if (y.miss  &&  !date.ts) {  

      if (!cat.x) { 

        if (object == "both"  &&  values == "data") {
          y.call <- x.call
          cat.y <- cat.x
          options(yname = x.name)
          
            options(xname = "Index")
              x.call <- data.frame(1:nrow(x.call))
              names(x.call) <- "Index"
        }

        else if (values %in% c("count", "prop")) {  # frequency polygon

          ssstuff <- .ss.numeric(x.call[,1], digits.d=digits.d, brief=TRUE)
          txss <- ssstuff$tx  # stats output before reduce data
         
          hist.counts <- FALSE
          hist.cumul <- ifelse(cumul, "on", "off")
          reg <- "snow2"  # applies to cumulative histogram
          h <- .hst.main(x.call[,1], fill, stroke, bg, grid,
             box, reg,
             over.grid=FALSE, cex.axis, axes, rotate.values, offset,
             breaks, bin.start, bin.width, bin.end, proportion, hist.counts,
             hist.cumul, xlab, ylab, main, sub, quiet, fun.call=NULL,
             do.plot=FALSE, ...) 

  
          n.cat <- 0  # not many midpoints, do not want to trigger num.cat
          x.call <- h$mids
          y.call <- h$counts
          if (values == "count")
            ylab <- paste("Count of", x.name)
          else {
            y.call <- y.call / sum(y.call)
            ylab <- paste("Proportion of", x.name)
          }

          object <- "both"  # do freq poly as a line chart
          freq.poly <- TRUE  # need to indicate fill possibility
          center.line <- "off"  # not meaningful here
    
          txsug <- ""
          if (getOption("suggest")) {
            txsug <- ">>> Suggestions"

            fc <- ""
            if (!grepl("area", fncl))
              fc <- paste(fc, ", area=TRUE", sep="")
            if (nzchar(fc)) {
              fc <- paste(fncl, fc, ") ", sep="")
              txsug <- paste(txsug, "\n", fc, sep="")
            }
              
            fc <- ""
            if (!grepl("size", fncl)  &&  !grepl("area", fncl))
              fc <- paste(fc, ", size=0", sep="")
            if (nzchar(fc)) {
              fc <- gsub(" = ", "=", fc)
              fc <- paste(fncl, fc, ")   # just line segments, no points", sep="")
              txsug <- paste(txsug, "\n", fc, sep="")
            }

            bw.new <- pretty((x.call[2] - x.call[1]) / 1.5)  # arbitrary new bw
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

          x.call <- data.frame(x.call)
          y.call <- data.frame(y.call)

        }  # end freq polygon
      }  # end !cat.x

      else {  # cat.x
        # just x variable, so set y.call to plot points for count and prop
        if (values %in% c("count", "prop")) {
          cat.y <- FALSE
          if (seg.x.miss) segments.x <- TRUE
          ylab <- ifelse (values=="count", "Count of", "Proportion of")
          ylab <- paste(ylab, x.name)
          frq <- table(x.call)
          if (values == "prop") frq <- frq / sum(frq)
          y.call <- data.frame(as.vector(frq))
          if (is.factor(x.call))  # preserve ordering, will lose order attribute
            x.call <- factor(names(frq), levels=levels(x.call))
          else
            x.call <- factor(names(frq))
          x.call <- data.frame(x.call)
        }
      }

    }  # end is null y.call
    

    else if (values %in% c("sum", "mean", "sd", "min", "median", "max")) {

      n.cat <- 0
      means <- FALSE
      if (seg.x.miss) segments.x <- TRUE

      # do stats console output before reducing data
      if (!quiet) {
        if (!missing(y)) {
          if (cat.y) {
            cat("\n"); stop(call.=FALSE, "\n","------\n",
            y.name, " is not numerical, so cannot compute its mean\n\n")
          }
          options(yname = x.name)  # reverse order of x and y for .ss.numeric
          options(xname = y.name)
          stats <- .ss.numeric(y.call[,1], by=x.call[,1],
                               digits.d=digits.d, brief=TRUE)
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
        out <- tapply(y.call[,1], x.call[,1], sum, na.rm=TRUE)
      }
      if (values == "mean") {
        ylab <- paste("Mean of", y.name)
        out <- tapply(y.call[,1], x.call[,1], mean, na.rm=TRUE)
      }
      if (values == "sd") {
        ylab <- paste("Standard Deviation of", y.name)
        out <- tapply(y.call[,1], x.call[,1], sd, na.rm=TRUE)
      }
      if (values == "min") {
        ylab <- paste("Minimum of", y.name)
        out <- tapply(y.call[,1], x.call[,1], min, na.rm=TRUE)
      }
      if (values == "median") {
        ylab <- paste("Median of", y.name)
        out <- tapply(y.call[,1], x.call[,1], median, na.rm=TRUE)
      }
      if (values == "max") {
        ylab <- paste("Maximum of", y.name)
        out <- tapply(y.call[,1], x.call[,1], max, na.rm=TRUE)
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

      x.call <- data.frame(x.call)
      y.call <- data.frame(y.call)
    }  # sum, mean, sd, min, median, max


    # 2-variable scatter plot
    # bubble plot for 1-variable (y.call=0) and 2-variable
    # line chart
    if ((!is.null(y.call) || date.ts) &&
        object %in% c("point", "bubble", "both", "sunflower")) {

      lx <- length(x.call[,1])
      ly <- length(y.call[,1])
      unique.x <- ifelse (length(unique(x.call[,1])) == lx, TRUE, FALSE)
      unique.y <- ifelse (length(unique(y.call[,1])) == ly, TRUE, FALSE)

      if (object == "point"  &&  values == "data"){  # for Cleveland dot plot
        if (!cat.x && cat.y && unique.y) {  # no sort.xy option
          if (seg.y.miss) if (unique.y && cat.y) segments.y <- TRUE
          if (seg.x.miss) if (unique.x && cat.x) segments.x <- TRUE
          if (sort.yx.miss) if (n.x_var <= 2) sort.yx <- TRUE
          if (col.grid.miss) grid <- "transparent"
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
        y.c <- y.call[,1]
        y.c <- factor(y.c, levels=y.c[ord])
        y.call[,1] <- y.c
      }

      # bigger point for scatterplot of stats
      if (values != "data"  &&  object == "point")
        if (is.null(size)) size <- 2.75

      .plt.main(x.call, y.call, by.call, n.cat,
         object, values,
         fill, stroke, bg, grid, box,
         trans, segment, area,
         cex.axis, axes, xy.ticks,
         xlab, ylab, main, sub, value.labels, label.max,
         rotate.values, offset, proportion,
         size, shape, means, sort.yx, segments.y, segments.x, line.width,
         smooth, smooth.points, smooth.trans, smooth.bins,
         bubble.scale, bubble.power, bubble.text, low.color, hi.color,
         fit.ln, stroke.fit, se.fit,
         ellipse, stroke.ellipse, fill.ellipse,
         center.line, show.runs, stack,
         method, pt.reg, pt.out, out30, out15,
         freq.poly, quiet, fun.call, ...)
    }

    # 1-D traditional scatter plot (not bubble plot)
    else if (!cat.x  &&  is.null(y.call)  &&  values == "data") {

      .dp.main(x.call[,1], by.call, size,
         fill, stroke, bg, grid, trans,
         shape, cex.axis, axes, xlab, main, sub,
         rotate.values, offset, method, pt.reg, pt.out,
         out30, out15, boxplot, quiet, new, 
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
