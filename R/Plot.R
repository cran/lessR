Plot <-
function(x, y=NULL, by=NULL, dframe=mydata, type=NULL, n.cat=getOption("n.cat"),

         col.pts=NULL, col.fill=NULL, trans.pts=getOption("trans.pts"),
         shape.pts="circle", 

         col.line=NULL, col.area=NULL, col.box="black",
         col.grid=NULL, col.bg=NULL,
         colors=c("blue", "gray", "rose", "green", "gold", "red"),

         cex.axis=.85, col.axis="gray30",
         col.ticks="gray30", xy.ticks=TRUE,
         xlab=NULL, ylab=NULL, main=NULL, cex=NULL,
         x.start=NULL, x.end=NULL, y.start=NULL, y.end=NULL,
         time.start=NULL, time.by=NULL, time.reverse=FALSE,

         kind=c("default", "regular", "bubble", "sunflower"),

         fit.line=c("none", "loess", "ls"), col.fit.line="grey55",

         col.bubble=NULL, bubble.size=.25, col.flower=NULL,

         ellipse=FALSE, col.ellipse="lightslategray", fill.ellipse=TRUE, 

         pt.reg="circle", pt.out="circle", 
         col.out30="firebrick2", col.out15="firebrick4", new=TRUE,

         text.out=TRUE, 

         pdf.file=NULL, pdf.width=5, pdf.height=5, ...) {


  if (missing(colors)) 
    colors <- getOption("colors")
  else
    colors <- match.arg(colors)

  fit.line <- match.arg(fit.line)
  kind <- match.arg(kind)

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

  if ( (kind == "bubble") || (kind == "sunflower") ) {
    if (is.null(y))  {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Option 'bubble' or 'sunflower' are only used in scatterplots.\n\n")
    }
    if ( (!is.integer(x)) || !is.integer(y)) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Option 'bubble' or 'sunflower' can only be used with integer data.\n\n")
    }
  }

  # get actual variable name before potential call of dframe$x
  x.name <- deparse(substitute(x)) 
  options(xname = x.name)
  # get data frame name
  dframe.name <- deparse(substitute(dframe))

  # get conditions and check for dframe existing
  xs <- .xstatus(x.name, dframe.name)
  in.global <- xs$ig 

  # see if variable exists in data frame, if x not in Global Env or function call 
  if (!missing(x) && !in.global)  .xcheck(x.name, dframe.name, dframe)

  if (!in.global) x.call <- eval(substitute(dframe$x))
  else {  # vars that are function names get assigned to global
    x.call <- x
    if (is.function(x.call)) x.call <- eval(substitute(dframe$x))
  }

  # evaluate y
  #-----------
  if (!missing(y)) {

    # get actual variable name before potential call of dframe$x
    y.name <- deparse(substitute(y)) 
    options(yname = y.name)

    # get conditions and check for dframe existing
    xs <- .xstatus(y.name, dframe.name)
    in.global <- xs$ig 

    # see if var exists in data frame, if x not in Global Env or function call 
    if (!missing(x) && !in.global)
      .xcheck(y.name, dframe.name, dframe)

    if (!in.global) y.call <- eval(substitute(dframe$y))
    else {  # vars that are function names get assigned to global
      y.call <- y
      if (is.function(y.call)) y.call <- eval(substitute(dframe$y))
    }
  }

  # evaluate by
  #-----------
  if (!missing(by)) {

    # get actual variable name before potential call of dframe$x
    by.name <- deparse(substitute(by)) 
    options(byname = by.name)

    # get conditions and check for dframe existing
    xs <- .xstatus(by.name, dframe.name)
    in.global <- xs$ig 

    # see if var exists in data frame, if x not in Global Env or function call 
    if (!missing(x) && !in.global)
      .xcheck(by.name, dframe.name, dframe)

    if (!in.global) by.call <- eval(substitute(dframe$by))
    else {  # vars that are function names get assigned to global
      by.call <- by
      if (is.function(by.call)) by.call <- eval(substitute(dframe$by))
    }
  }
  else
   by.call <- NULL

  orig.params <- par(no.readonly=TRUE)
  on.exit(par(orig.params))

  # set up graphics system
  if (is.null(pdf.file))  {
    if (missing(by)) 
      .graphwin() 
    else
      .graphwin(d.w=5.1)  # add .6 in width to default of 4.5 for legend
  }
  else 
    pdf(file=pdf.file, width=pdf.width, height=pdf.height)


  if (class(x.call)[1] == "data.frame") {
    pairs(x)  # x is a data frame
    .cr.data.frame(x, miss="pairwise", show.n=FALSE, n.cat, digits.d=2,
                   heat.map=FALSE, colors=NULL,
                   main=NULL, bottom=NULL, right=NULL,...)
  }

  else {

    if (!missing(y)) {
      .plt.main(x.call, y.call, by.call, dframe, type, n.cat,
         col.line, col.area, col.box, col.pts, col.fill,
         trans.pts, shape.pts, col.grid, col.bg, colors, 
         cex.axis, col.axis, col.ticks, 
         xy.ticks, xlab, ylab, main, cex,
         x.start, x.end, y.start, y.end, kind,
         fit.line, col.fit.line, 
         col.bubble, bubble.size, col.flower,
         ellipse, col.ellipse, fill.ellipse, text.out, ...)
    }

    else
      .dp.main(x.call, by.call,
         col.pts, col.fill, trans.pts, shape.pts,
         col.bg, col.grid, colors,
         cex.axis, col.axis, col.ticks, xlab, main, cex, 
         pt.reg, pt.out, 
         col.out30, col.out15, text.out, new, ...)
  }

  # terminate pdf graphics system
  if (!is.null(pdf.file)) {
    dev.off()
    .showfile(pdf.file, "plot")
  }

}
