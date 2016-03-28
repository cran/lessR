ScatterPlot <-
function(x, y=NULL, by=NULL, data=mydata, type=NULL,
         n.cat=getOption("n.cat"), digits.d=NULL,
         stat=c("default", "count", "mean", "sd", "min", "max"),

         col.fill=getOption("col.fill.pt"),
         col.stroke=getOption("col.stroke.pt"),
         col.bg=getOption("col.bg"),
         col.grid=getOption("col.grid"),
         col.trans=NULL, col.area=NULL, col.box="black",

         cex.axis=0.75, col.axis="gray30", xy.ticks=TRUE,
         xlab=NULL, ylab=NULL, main=NULL, sub=NULL, cex=NULL,
         value.labels=NULL, rotate.values=0, offset=0.5,

         style=c("default", "regular", "bubble", "sunflower", "off"),

         fit.line=NULL, col.fit.line="grey55",

         shape.pts="circle", method="overplot",
         means=TRUE, sort.y=FALSE,
         segments.y=FALSE, segments.x=FALSE,

         bubble.size=0.25, bubble.power=0.6,  bubble.counts=TRUE,
         col.low=NULL, col.hi=NULL,

         ellipse=FALSE, col.ellipse="lightslategray",
         col.fill.ellipse="transparent", 

         pt.reg="circle", pt.out="circle", 
         col.out30="firebrick2", col.out15="firebrick4", new=TRUE,

         diag=FALSE, col.diag=par("fg"), lines.diag=FALSE,

         quiet=getOption("quiet"),
         pdf.file=NULL, pdf.width=NULL, pdf.height=NULL,
         fun.call=NULL, ...) {


  if (is.null(fun.call)) fun.call <- match.call()

  #n.cat.set <- ifelse(missing(n.cat), FALSE, TRUE)
  

  style <- match.arg(style)
  stat <- match.arg(stat)

  if (stat == "default") stat <- ""

  # any bubble parameter actives a bubble plot
  if (!missing(bubble.size) || !missing(bubble.size) || !missing(bubble.counts))
    style <- "bubble"

  if (is.null(fit.line)) fit.ln <- "none"
  if (is.logical(fit.line))
    fit.ln <- ifelse (fit.line, "loess", "none")
  if (is.character(fit.line)) {
    if (!(fit.line %in% c("loess", "ls", "none"))) { 
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "fit.line only for  loess  or  ls  (least squares)\n\n")
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
      if (names(dots)[i] %in% c("x.start","x.end","y.start","y.end")) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "x.start, x.end, y.start, and y.end no longer used\n\n",
          "Instead use the standard R xlim and ylim parameters,\n",
          "such as xlim=c(0,40) to specify from 0 to 40. Same for ylim.\n\n")
      }
      if (names(dots)[i] == "kind") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  kind  is renamed  style\n\n")
      }
      for (i in 1:length(dots)) {
        if (names(dots)[i] == "knitr.file") {
          cat("\n"); stop(call.=FALSE, "\n","------\n",
            "knitr.file  no longer used\n",
            "Instead use  Rmd  for R Markdown file\n\n")
        }
      }
    }
  }

  if (missing(x)) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Must specify at least one variable to analyze\n\n")
  }

  if (stat %in% c("mean", "sd", "min", "max") && missing(y)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Must specify a y-variable to analyze: ", stat, "\n\n")
  }

  if (!is.null(type)) if (type != "p" && type != "l" && type != "b") { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Option 'type' can only be \"p\" for points,\n",
        "  \"l\" for line or \"b\" for both\n\n")
  }

  # conflict between graphical and cor parameters, so avoid
  if (method %in% c("spearman", "kendall")) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "The  method  parameter has another meaning for ScatterPlot\n\n",
      "To compute another type of correlation\n",
      "  access the Correlation function\n\n")
  }


  # process shapes
  bad.shape <- NULL
  shapes <- c("circle", "square", "diamond", "triup", "tridown")
  shapes.all <- c(shapes, c(21:25), letters, LETTERS, 1:9, "+", "*", "#")

  num.flag <- FALSE
  for (i in 1:length(shape.pts)) {
    if (!(shape.pts[i] %in% shapes.all)) 
      bad.shape <- shape.pts[i]
    else
      if (shape.pts[i] %in% shapes) {
        shape.pts[i] <- which(shape.pts[i]==shapes)+20
        num.flag <- TRUE
      }
  }
  if (num.flag) shape.pts <- as.numeric(shape.pts)

  if (pt.reg %in% shapes) 
    pt.reg <- which(pt.reg==shapes) + 20
  else
    if (!(pt.reg %in% c(21:25))) bad.shape <- pt.reg

  if (pt.out %in% shapes) 
    pt.out <- which(pt.out==shapes) + 20
  else
    if (!(pt.out %in% c(21:25))) bad.shape <- pt.out

  if (!is.null(bad.shape)) {
      message("\nValid shapes: ") 
      for (j in 1:length(shapes)) message(shapes[j], " ")
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


  if (!exists(x.name, where=.GlobalEnv)) {  # x not in global env, in df
    .nodf(df.name)  # check to see if data frame container exists 
    .xcheck(x.name, df.name, data)  # var in df?, vars lists not checked
    all.vars <- as.list(seq_along(data))  # even if only a single var
    names(all.vars) <- names(data)  # all data in data frame
    x.col <- eval(substitute(x), envir=all.vars)  # col num selected vars
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

  else { # x is in the global environment (vector or data frame)
    if (is.data.frame(x))  # x a data frame
      data.x <- x
    else {  # x a vector in global
      if (!is.function(x))
        data.x <- data.frame(x)  # x is 1 var
      else
        data.x <- data.frame(eval(substitute(data$x)))  # x is 1 var
      names(data.x) <- x.name
    }
  }

  # just one x variable for now
  if (ncol(data.x) == 1) { 
    x.call <- data.x[,1]

    nu <- length(unique(na.omit(x.call)))
    numcat.x <- .is.num.cat(x.call, n.cat)
    if (numcat.x) .ncat("ScatterPlot", x.name, nu, n.cat)
    cat.x <- ifelse (numcat.x || is.factor(x.call), TRUE, FALSE)
  }

  else { # ncol > 1, check to make sure all selected vars are categorical
    for (i in 1:length(x.col)) {
      nu <- length(unique(na.omit(data.x[,i])))
      num.cat <- .is.num.cat(data.x[,i], n.cat)
      cat.all <- ifelse (num.cat || is.factor(data.x[,i]), TRUE, FALSE)
      if (!cat.all) { 
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "All variables for multiple x-variables must be categorical\n\n",
          "A categorical variable is either an R factor variable,\n",
          "  or is numeric with not more than  n.cat  unique values\n\n",
          "Can set  n.cat  locally when calling  ScatterPlot,\n",
          "  or globally with the function:  set\n\n")
      }
    }
  }



  # evaluate y
  #-----------
  if (!missing(y)) {

    if (ncol(data.x) > 1) { 
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Multiple x-variables cannot be paired with a y-variable\n\n")
    }

    # get actual variable name before potential call of data$x
    y.name <- deparse(substitute(y)) 
    options(yname = y.name)

    if (deparse(substitute(y)) == "row.names")
       y.call <- factor(row.names(data))
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

      # just one y variable for now
      if (ncol(data.y) == 1) { 
        y.call <- data.y[,1]

        nu <- length(unique(na.omit(y.call)))
        numcat.y <- ifelse(.is.integer(y.call) && nu <= n.cat, TRUE, FALSE)
        if (numcat.y) .ncat("ScatterPlot", y.name, nu, n.cat)
        cat.y <- ifelse (numcat.y || is.factor(y.call), TRUE, FALSE)
      }
      else { # ncol > 1, check to make sure all selected vars are categorical
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Multiple y-variables not allowed at this time\n\n")
      }
    }
  }
  else
    y.call <- NULL


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


  # graphics 
  if (is.null(y.call)  &&  ncol(data.x) == 1  &&  method!="stack")
    plt.h <- ifelse(is.null(main), 2.5, 3.1)  # narrow for 1-D plot
  else
    plt.h <- 5
  # for BPFM with more than 7 variables, make extra long
  if (ncol(data.x) > 7) plt.h <- plt.h + ((ncol(data.x) - 7) * 0.5)

  if (is.null(pdf.file)) {  
    if (options("device") != "RStudioGD"  &&  is.null(options()$knitr.in.progress)) {
      if (missing(by)) {  # set up graphics system to manage
        if (is.null(y.call)) {  # works for 1-D scatter plots and BPFM
          .graphwin(1, d.h=plt.h) 
        }
        else
          .graphwin(1) 
      }
      else
        .graphwin(d.w=pdf.width)  # add width to default of 4.5 for legend

      }
    }
  else  {  # pdf file
    if (is.null(pdf.width)) pdf.width <- 4.5
    if (!missing(by.call)) pdf.width <- pdf.width + 0.6
    if (is.null(pdf.height)) pdf.height <- plt.h
    pdf(file=pdf.file, width=pdf.width, height=pdf.height)
  }


  # ------------------------------------------------
  # analysis

  if (is.data.frame(data.x) == "data.frame") {  # correlation matrix
    .cr.data.frame(x, miss="pairwise", show.n=FALSE, n.cat, digits.d=2,
                   heat.map=FALSE, colors=NULL,
                   main=NULL, bottom=NULL, right=NULL, ...)
  }
 
  # bubble plot for multiple x variables
  else if (ncol(data.x) > 1) { 

    # get labels just for subset data matrix
    mylabels <- attr(data, which="variable.labels")
    nm <- names(data.x)
    if (!is.null(mylabels)) {
      mylabs <- character(length=0) 
      for (i in 1:length(nm))
        mylabs[i] <- mylabels[which(names(mylabels) == nm[i])]
    }
    else
      mylabs <- NULL
    if (length(mylabs) == 0) mylabs <- NULL  # when labels, but not relevant

    if (is.null(xlab)) xlab <- ""  # suppress x-axis label if not specified

    .dpmat.main(data[,x.col], mylabs, nm,
      col.fill, col.stroke, col.bg, col.grid, col.trans,
      shape.pts, col.area, col.box, 
      cex.axis, col.axis, col.low, col.hi,
      xy.ticks, xlab, ylab, main, sub, cex,
      bubble.size, bubble.counts,
      value.labels, rotate.values, offset, quiet, ...)

  }

  else {

    if (stat != "") {
      n.cat <- 0
      type <- "p"
      means <- FALSE

      # do stats output before graphics to retain all data values
      if (!missing(y)) {
        options(yname = x.name)  # reverse order of x and y for .ss.numeric
        options(xname = y.name)
        stats <- .ss.numeric(y.call, by=x.call, digits.d=digits.d, brief=TRUE)
        txout <- stats$tx
      }
      else  {
        stats <- .ss.factor(x.call, digits.d=digits.d, x.name=x.name, brief=TRUE)
        txout <- stats$counts
      }

      class(txout) <- "out_piece"

      output <- list(out_txt=txout)
      class(output) <- "out_all"
      print(output)
    }

    if (is.null(y.call) && cat.x) {
      if (stat == "")
        y.call <- rep(0, length(x.call)) # 1-D bubble plot, need y
      else if (stat == "count") {
        ylab <- "Count"
        frq <- table(x.call)
        x.call <- factor(names(frq))
        y.call <- as.vector(frq)
      }
    }
    if (stat %in% c("mean", "sd", "min", "max")) {
      if (stat == "mean") {
        ylab <- paste("Mean", y.name) 
        out <- tapply(y.call, x.call, mean, na.rm=TRUE)
      }
      if (stat == "sd") {
        ylab <- paste("Standard Deviation", y.name)
        out <- tapply(y.call, x.call, sd, na.rm=TRUE)
      }
      if (stat == "min") {
        ylab <- paste("Minimum", y.name)
        out <- tapply(y.call, x.call, min, na.rm=TRUE)
      }
      if (stat == "max") {
        ylab <- paste("Maximum", y.name)
        out <- tapply(y.call, x.call, max, na.rm=TRUE)
      }
      x.call <- factor(names(out))
      y.call <- as.vector(out)
    }

    # 2-variable plot
    if (!is.null(y.call)) {  
      .plt.main(x.call, y.call, by.call, type, n.cat,
         col.fill, col.stroke, col.bg, col.grid,
         shape.pts, col.area, col.box, col.trans, 
         cex.axis, col.axis, col.low, col.hi, 
         xy.ticks, xlab, ylab, main, sub, cex,
         value.labels, rotate.values, offset, style, means, stat,
         fit.ln, col.fit.line, bubble.size, bubble.power, bubble.counts,
         ellipse, col.ellipse, col.fill.ellipse,
         diag, col.diag, lines.diag, sort.y, segments.y, segments.x,
         quiet, ...)
    }

    else {  # 1-D traditional scatter plot (not bubble plot)
      if (!cat.x) { 
        .dp.main(x.call, by.call,
           col.fill, col.stroke, col.bg, col.grid, col.trans,
           shape.pts, cex.axis, col.axis, xlab, main, sub, cex, 
           rotate.values, offset, method, pt.reg, pt.out, 
           col.out30, col.out15, quiet, new, ...)

        # terminate pdf graphics system if used
        if (!is.null(pdf.file)) {
          dev.off()
          .showfile(pdf.file, "plot")
        }

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
      }  # !is.factor(x.call)
 
    }  # end 1-D plot

  }

  # terminate pdf graphics system if used
  if (!is.null(pdf.file)) {
    dev.off()
    .showfile(pdf.file, "plot")
  }

}
