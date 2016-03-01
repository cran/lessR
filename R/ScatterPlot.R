ScatterPlot <-
function(x, y=NULL, by=NULL, data=mydata, type=NULL,
         n.cat=10, digits.d=NULL,

         col.fill=getOption("col.fill.pt"),
         col.stroke=getOption("col.stroke.pt"),
         col.bg=getOption("col.bg"),
         col.grid=getOption("col.grid"),

         col.area=NULL, col.box="black",

         cex.axis=0.75, col.axis="gray30",
         xy.ticks=TRUE,
         xlab=NULL, ylab=NULL, main=NULL, sub=NULL, cex=NULL,
         value.labels=NULL, rotate.values=0, offset=0.5,

         kind=c("default", "regular", "bubble", "sunflower"),

         fit.line=NULL, col.fit.line="grey55",

         shape.pts="circle", method="overplot", means=TRUE,

         bubble.size=0.25, bubble.counts=TRUE,
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

  kind <- match.arg(kind)

  sys.n.cat <- getOption("n.cat")  # if n.cat has been re-set, use that value
  if (sys.n.cat != 0) n.cat <- sys.n.cat

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
          "such as xlim=c(0,40) to specify from 0 to 40. Same for ylim\n\n")
      }
    }
  }
 
  if (missing(x)) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Must specify a variable to analyze\n\n")
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

  dots <- list(...)  # check for deprecated parameters
  if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (names(dots)[i] == "knitr.file") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "knitr.file  no longer used\n",
          "Instead use  Rmd  for R Markdown file\n\n")
      }
    }
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
    if (class(data) != "list") {
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
    numcat.x <- ifelse(!is.factor(x.call) && nu <= n.cat, TRUE, FALSE)
    if (numcat.x) .ncat("ScatterPlot", x.name, nu, n.cat)
    cat.x <- ifelse (numcat.x || is.factor(x.call), TRUE, FALSE)
  }

  else { # ncol > 1, check to make sure all selected vars are categorical
    for (i in 1:length(x.col)) {
      nu <- length(unique(na.omit(data.x[,i])))
      num.cat <- ifelse(!is.factor(data.x[,i]) && nu <= n.cat, TRUE, FALSE)
      cat.all <- ifelse (num.cat || is.factor(data.x[,i]), TRUE, FALSE)
      if (!cat.all) { 
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "All variables for multiple x-variables must be categorical\n\n",
          "A categorical variable is either an R factor variable,\n",
          "  or a numerical variable with not more than  n.cat  unique values\n\n",
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

    # get conditions and check for data existing
    xs <- .xstatus(y.name, df.name, quiet)
    in.global <- xs$ig 

    # see if var exists in data frame, if x not in Global Env or function call 
    if (!missing(x) && !in.global)
      .xcheck(y.name, df.name, data)

    if (!in.global)
      y.call <- eval(substitute(data$y))
    else {  # vars that are function names get assigned to global
      y.call <- y
      if (is.function(y.call)) y.call <- eval(substitute(data$y))
    }

    nu <- length(unique(na.omit(y.call)))
    numcat.y <- ifelse(!is.factor(y.call) && nu <= n.cat, TRUE, FALSE)
    if (numcat.y) .ncat("ScatterPlot", y.name, nu, n.cat, brief=TRUE)
    cat.y <- ifelse (numcat.y || is.factor(y.call), TRUE, FALSE)
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
  tm <- ifelse (is.null(main), 2, 4)
  lm <- ifelse (is.null(y.call), 2, 4)
  if (ncol(data.x) > 1) lm <- 3  # no y, but allow room for variable names
  if (is.null(y.call) &&  ncol(data.x) == 1)
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

  orig.params <- par(no.readonly=TRUE)
  on.exit(par(orig.params))
  par(mar=c(4,lm,tm,2)+0.1) 


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

    .dpmat.main(data[,x.col], n.cat, mylabs, nm,
      col.fill, col.stroke, col.bg, col.grid,
      shape.pts, col.area, col.box, 
      cex.axis, col.axis, col.low, col.hi,
      xy.ticks, xlab, ylab, main, sub, cex,
      bubble.size, bubble.counts,
      value.labels, rotate.values, offset, quiet, ...)

  }

  else {

    # 1-D bubble plot of a categorical variable, need second variable
    if (is.null(y.call) && cat.x) y.call <- rep(0, length(x.call))

    # 2-variable plot
    if (!is.null(y.call)) {  
      .plt.main(x.call, y.call, by.call, type, n.cat,
         col.fill, col.stroke, col.bg, col.grid,
         shape.pts, col.area, col.box, 
         cex.axis, col.axis, col.low, col.hi, 
         xy.ticks, xlab, ylab, main, sub, cex,
         value.labels, rotate.values, offset, kind, means,
         fit.ln, col.fit.line, bubble.size, bubble.counts,
         ellipse, col.ellipse, col.fill.ellipse,
         diag, col.diag, lines.diag, quiet, ...)
    }

    else {  # 1-D traditional scatter plot (not bubble plot)
      if (!cat.x) { 
        .dp.main(x.call, by.call,
           col.fill, col.stroke, col.bg, col.grid, shape.pts,
           cex.axis, col.axis, xlab, main, sub, cex, 
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
