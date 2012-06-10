RunChart <-
function(y, dframe=mydata, type=NULL, 

         col.line=NULL, col.area=NULL, col.box="black",
         col.pts=NULL, col.fill=NULL, trans.pts=NULL,
         pch=NULL, col.grid=NULL, col.bg=NULL,
         colors=c("blue", "gray", "rose", "green", "gold", "red"),

         cex.axis=.85, col.axis="gray30",
         col.ticks="gray30", xy.ticks=TRUE,
         xlab=NULL, ylab=NULL, main=NULL, cex=NULL,
         x.start=NULL, x.end=NULL, y.start=NULL, y.end=NULL,
         time.start=NULL, time.by=NULL, time.reverse=FALSE,

         center.line=NULL,

         text.out=TRUE, ...) {


  if (missing(colors)) col.default <- TRUE else col.default <- FALSE
  colors <- match.arg(colors)
  if (col.default && !is.null(getOption("colors"))) 
    colors <- getOption("colors")

  # get actual variable name before potential call of dframe$y
  y.name <- deparse(substitute(y)) 
  options(yname = y.name)
  # get data frame name
  dframe.name <- deparse(substitute(dframe))

  # get conditions and check for dframe existing
  ys <- .xstatus(y.name, dframe.name)
  in.global <- ys$ig 

  # see if variable exists in data frame, if y not in Global Env or function call 
  if (!missing(y) && !in.global)  .xcheck(y.name, dframe.name, dframe)

  if (!in.global) y.call <- eval(substitute(dframe$y))
  else {  # vars that are function names get assigned to global
    y.call <- y
    if (is.function(y.call)) y.call <- eval(substitute(dframe$y))
  }

  .graphwin()
  .rc.main(y.call, dframe, type,
       col.line, col.area, col.box, col.pts, col.fill, trans.pts,
       pch, col.grid, col.bg, colors, cex.axis, col.axis, col.ticks, xy.ticks,
       xlab, ylab, main, cex, x.start, x.end, y.start, y.end,
       time.start, time.by, time.reverse, 
       center.line, text.out, ...)

}
