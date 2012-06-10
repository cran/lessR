Plot <-
function(x, y=NULL, dframe=mydata, type=NULL, ncut=4,

         col.line=NULL, col.area=NULL, col.box="black",
         col.pts=NULL, col.fill=NULL, trans.pts=NULL,
         pch=NULL, col.grid=NULL, col.bg=NULL,
         colors=c("blue", "gray", "rose", "green", "gold", "red"),

         cex.axis=.85, col.axis="gray30",
         col.ticks="gray30", xy.ticks=TRUE,
         xlab=NULL, ylab=NULL, main=NULL, cex=NULL,
         x.start=NULL, x.end=NULL, y.start=NULL, y.end=NULL,
         time.start=NULL, time.by=NULL, time.reverse=FALSE,

         kind=c("default", "regular", "bubble", "sunflower"),

         fit.line=c("none", "loess", "ls"), col.fit.line="grey55",
         center.line=NULL,

         col.bubble=NULL, bubble.size=.25, col.flower=NULL,

         ellipse=FALSE, col.ellipse="lightslategray", fill.ellipse=TRUE, 

         pt.reg=21, pt.out=19, 
         col.out30="firebrick2", col.out15="firebrick4", new=TRUE,

         text.out=TRUE, ...) {


  if (missing(colors)) col.default <- TRUE else col.default <- FALSE
  colors <- match.arg(colors)
  if (col.default && !is.null(getOption("colors"))) 
    colors <- getOption("colors")

  fit.line <- match.arg(fit.line)
  kind <- match.arg(kind)

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

  if (class(x.call)[1] == "data.frame") {
    .graphwin()
    pairs(x)  # x is a data frame
    cr.data.frame(x, ...)
  }
  else {
    .graphwin()
    if (!missing(y))
      .plt.main(x.call, y.call, dframe, type, ncut,
         col.line, col.area, col.box, col.pts, col.fill, trans.pts,
         pch, col.grid, col.bg, colors, cex.axis, col.axis, col.ticks, xy.ticks,
         xlab, ylab, main, cex, x.start, x.end, y.start, y.end,
         time.start, time.by, time.reverse, kind,
         fit.line, col.fit.line, center.line, col.bubble, bubble.size, col.flower,
         ellipse, col.ellipse, fill.ellipse, text.out, ...)
    else
      .dp.main(x.call,
         col.pts, col.fill, trans.pts, col.bg, col.grid, colors,
         cex.axis, col.axis, col.ticks, xlab, main, 
         pt.reg, pt.out, 
         col.out30, col.out15, text.out, new, ...)
  }

}
