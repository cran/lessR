LineChart <-
function(y, data=mydata, type=NULL, 

         col.fill=getOption("col.fill.bar"), 
         col.stroke=getOption("col.stroke.pt"),
         col.bg=getOption("col.bg"),
         col.grid=getOption("col.grid"),
         col.line=getOption("col.stroke.pt"),

         col.area=NULL, col.box="black",

         shape.pts=21, cex.axis=.85, col.axis="gray30",
         col.ticks="gray30", xy.ticks=TRUE, line.width=1.1,
         xlab=NULL, ylab=NULL, main=NULL, cex=NULL,

         time.start=NULL, time.by=NULL, time.reverse=FALSE,

         center.line=c("default", "mean", "median", "off"),

         quiet=getOption("quiet"),
         pdf.file=NULL, pdf.width=5, pdf.height=5, ...) {


  center.line <- match.arg(center.line)

  # get actual variable name before potential call of data$y
  y.name <- deparse(substitute(y)) 
  options(yname = y.name)

  # get data frame name
  dname <- deparse(substitute(data))
  options(dname = dname)

  # get conditions and check for data existing
  ys <- .xstatus(y.name, dname, quiet)
  in.global <- ys$ig 

  # see if variable exists in data frame, if y not in Global Env or function call 
  if (!missing(y) && !in.global)  .xcheck(y.name, dname, data)

  if (!in.global) y.call <- eval(substitute(data$y))
  else {  # vars that are function names get assigned to global
    y.call <- y
    if (is.function(y.call)) y.call <- eval(substitute(data$y))
  }

  # set up graphics system
  .opendev(pdf.file, pdf.width, pdf.height)

  .lc.main(y.call, type,
       col.line, col.area, col.box, col.stroke, col.fill, shape.pts,
       col.grid, col.bg, cex.axis, col.axis, col.ticks, xy.ticks,
       line.width, xlab, ylab, main, cex,
       time.start, time.by, time.reverse, 
       center.line, quiet, ...)

  # terminate pdf graphics system
  if (!is.null(pdf.file)) {
    dev.off()
    .showfile(pdf.file, "line chart")
  }


}
