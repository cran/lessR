ScatterPlot <-
function(x, y=NULL, by=NULL, data=mydata, type=NULL, n.cat=getOption("n.cat"),

         col.fill=getOption("col.fill.pt"),
         col.stroke=getOption("col.stroke.pt"),
         col.bg=getOption("col.bg"),
         col.grid=getOption("col.grid"),

         col.area=NULL, col.box="black",

         shape.pts="circle", cex.axis=.85, col.axis="gray30",
         xy.ticks=TRUE,
         xlab=NULL, ylab=NULL, main=NULL, cex=NULL,

         kind=c("default", "regular", "bubble", "sunflower"),

         fit.line=c("none", "loess", "ls"), col.fit.line="grey55",

         bubble.size=.25, method="overplot",

         ellipse=FALSE, col.ellipse="lightslategray", fill.ellipse=TRUE, 

         pt.reg="circle", pt.out="circle", 
         col.out30="firebrick2", col.out15="firebrick4", new=TRUE,

         diag=FALSE, col.diag=par("fg"), lines.diag=TRUE,

         quiet=getOption("quiet"),
         pdf.file=NULL, pdf.width=5, pdf.height=5, ...) {


  fit.line <- match.arg(fit.line)
  kind <- match.arg(kind)

  dots <- list(...)  # check for deprecated parameters
  if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (names(dots)[i] %in% c("x.start","x.end","y.start","y.end")) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "x.start, x.end, y.start, and y.end no longer used.\n",
          "Instead use the standard R xlim and ylim parameters,\n",
          "such as xlim=c(0,40) to specify from 0 to 40. Same for ylim.\n\n")
      }
    }
  }
  if (method %in% c("spearman", "kendall")) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "The  method  parameter has another use for ScatterPlot.\n",
      "To compute another type of correlation, directly\n",
      "access the Correlation function.\n\n")
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
  if (num.flag) shape.pts <- suppressWarnings(as.numeric(shape.pts))

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

  # get conditions and check for data existing
  xs <- .xstatus(x.name, df.name, quiet)
  in.global <- xs$ig 

  # see if variable exists in data frame, if x not in Global Env or function call 
  if (!missing(x) && !in.global) .xcheck(x.name, df.name, data)

  if (!in.global) x.call <- eval(substitute(data$x))
  else {  # vars that are function names get assigned to global
    x.call <- x
    if (is.function(x.call)) x.call <- eval(substitute(data$x))
  }

  # evaluate y
  #-----------
  if (!missing(y)) {

    # get actual variable name before potential call of data$x
    y.name <- deparse(substitute(y)) 
    options(yname = y.name)

    # get conditions and check for data existing
    xs <- .xstatus(y.name, df.name, quiet)
    in.global <- xs$ig 

    # see if var exists in data frame, if x not in Global Env or function call 
    if (!missing(x) && !in.global)
      .xcheck(y.name, df.name, data)

    if (!in.global) y.call <- eval(substitute(data$y))
    else {  # vars that are function names get assigned to global
      y.call <- y
      if (is.function(y.call)) y.call <- eval(substitute(data$y))
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

    if (!in.global) by.call <- eval(substitute(data$by))
    else {  # vars that are function names get assigned to global
      by.call <- by
      if (is.function(by.call)) by.call <- eval(substitute(data$by))
    }

    if (!is.factor(by.call)) by.call <- factor(by.call)
  }
  else
   by.call <- NULL


  if ( (kind == "bubble") || (kind == "sunflower") ) {
    if (missing(y))  {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Option 'bubble' or 'sunflower' are only used in scatterplots.\n\n")
    }
    if ( (!is.integer(x.call)) || !is.integer(y.call)) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Option 'bubble' or 'sunflower' can only be used with integer data.\n\n")
    }
  }

  # graphics 
  if (is.null(pdf.file))  {  
    if (options("device") != "RStudioGD") {
      orig.params <- par(no.readonly=TRUE)
      on.exit(par(orig.params))
      if (missing(by))  # set up graphics system to manage
        .graphwin(1) 
      else
        .graphwin(d.w=pdf.width)  # add width to default of 4.5 for legend
      }
    }
  else  {
    if (!missing(by)) pdf.width <- pdf.width + 0.6
    pdf(file=pdf.file, width=pdf.width, height=pdf.height)
  }


  if (class(x.call)[1] == "data.frame") {
#   pairs(x)  # x is a data frame
    .cr.data.frame(x, miss="pairwise", show.n=FALSE, n.cat, digits.d=2,
                   heat.map=FALSE, colors=NULL,
                   main=NULL, bottom=NULL, right=NULL, ...)
  }

  else {

    if (!missing(y)) {
      .plt.main(x.call, y.call, by.call, data, type, n.cat,
         col.fill, col.stroke, col.bg, col.grid,
         shape.pts, col.area, col.box, 
         cex.axis, col.axis, 
         xy.ticks, xlab, ylab, main, cex, kind,
         fit.line, col.fit.line, bubble.size,
         ellipse, col.ellipse, fill.ellipse,
         diag, col.diag, lines.diag, quiet, ...)
    }

    else
      .dp.main(x.call, by.call,
         col.fill, col.stroke, col.bg, col.grid, shape.pts,
         cex.axis, col.axis, xlab, main, cex, 
         method, pt.reg, pt.out, 
         col.out30, col.out15, quiet, new, ...)
  }

  # terminate pdf graphics system
  if (!is.null(pdf.file)) {
    dev.off()
    .showfile(pdf.file, "plot")
  }

}
