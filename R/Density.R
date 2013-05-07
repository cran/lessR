Density <-
function(x, data=mydata, 

         bw="nrd0", type=c("both", "general", "normal"),
         bin.start=NULL, bin.width=NULL,

         col.fill=getOption("col.fill.pt"),
         col.bg=getOption("col.bg"),
         col.grid=getOption("col.grid"),

         col.nrm="black", col.gen="black",
         col.fill.nrm="transparent", col.fill.gen="transparent",

         cex.axis=.85, col.axis="gray30", col.ticks="gray30",
         x.pt=NULL, xlab=NULL, main=NULL, y.axis=FALSE, 
         x.min=NULL, x.max=NULL, band=FALSE, 

         quiet=getOption("quiet"),
         pdf.file=NULL, pdf.width=5, pdf.height=5, ...) {

  clr <- getOption("colors") 
  if (clr == "blue") {
    if (col.fill == getOption("col.fill.pt")) col.fill <- "gray86"
    col.fill.nrm <- rgb(80,150,200, alpha=70, maxColorValue=255)
    col.fill.gen <- rgb(250,210,230, alpha=70, maxColorValue=255)
  }
  if (clr == "gray.black" || clr == "orange.black") {
    col.nrm <- getOption("col.stroke.pt")
    col.gen <- getOption("col.stroke.pt")
  }

  # get actual variable name before potential call of data$x
  x.name <- deparse(substitute(x)) 
  options(xname = x.name)

  # get data frame name
  dname <- deparse(substitute(data))
  options(dname = dname)

  # get conditions and check for data existing
  xs <- .xstatus(x.name, dname, quiet)
  in.global <- xs$ig 

  # see if variable exists in data frame, if x not in Global Env or function call 
  if (!missing(x) && !in.global) .xcheck(x.name, dname, data)

  if (!in.global) x.call <- eval(substitute(data$x))
  else {
    x.call <- x
    if (is.function(x)) x.call <- eval(substitute(data$x))
  }

  # set up graphics system
  .opendev(pdf.file, pdf.width, pdf.height)

  orig.params <- par(no.readonly=TRUE)
  on.exit(par(orig.params))

  d <- .den.main(x.call, data=mydata, 
            bw, type, bin.start, bin.width, quiet,
            col.fill, col.bg, col.grid, col.nrm, col.gen,
            col.fill.nrm, col.fill.gen, 
            cex.axis, col.axis, col.ticks,
            x.pt, xlab, main, y.axis, x.min, x.max, band, ...)

  # terminate pdf graphics system
  if (!is.null(pdf.file)) {
    dev.off()
    .showfile(pdf.file, "density plot")
  }

  invisible(d)

}
