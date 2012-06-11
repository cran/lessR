Density <-
function(x, dframe=mydata, 
         bw="nrd0", type=c("both", "general", "normal"),
         bin.start=NULL, bin.width=NULL, text.out=TRUE,

         col.bg=NULL, col.grid=NULL, col.bars=NULL,
         col.nrm="black", col.gen="black",
         col.fill.nrm=NULL, col.fill.gen=NULL,
         colors=c("blue", "gray", "rose", "green", "gold", "red"), 

         cex.axis=.85, col.axis="gray30", col.ticks="gray30",
         x.pt=NULL, xlab=NULL, main=NULL, y.axis=FALSE, 
         x.min=NULL, x.max=NULL, band=FALSE, ...) {


  if (missing(colors)) col.default <- TRUE else col.default <- FALSE
  colors <- match.arg(colors)
  if (col.default && !is.null(getOption("colors"))) colors <- getOption("colors")

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
  else {
    x.call <- x
    if (is.function(x)) x.call <- eval(substitute(dframe$x))
  }

  .graphwin()
  .den.main(x.call, dframe=mydata, 
            bw, type, bin.start, bin.width, text.out,
            col.bg, col.grid, col.bars, col.nrm, col.gen,
            col.fill.nrm, col.fill.gen, colors, 
            cex.axis, col.axis, col.ticks,
            x.pt, xlab, main, y.axis, x.min, x.max, band, ...)

}
