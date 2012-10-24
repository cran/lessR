Density <-
function(x, dframe=mydata, 
         bw="nrd0", type=c("both", "general", "normal"),
         bin.start=NULL, bin.width=NULL, text.out=TRUE,

         col.fill=getOption("col.fill.pt"),
         col.bg=getOption("col.bg"), col.grid=getOption("col.grid"),

         col.nrm="black", col.gen="black",
         col.fill.nrm="transparent", col.fill.gen="transparent",

         cex.axis=.85, col.axis="gray30", col.ticks="gray30",
         x.pt=NULL, xlab=NULL, main=NULL, y.axis=FALSE, 
         x.min=NULL, x.max=NULL, band=FALSE, 

         pdf.file=NULL, pdf.width=5, pdf.height=5, ...) {


  # get actual variable name before potential call of dframe$x
  x.name <- deparse(substitute(x)) 
  options(xname = x.name)

  # get data frame name
  dframe.name <- deparse(substitute(dframe))

  # get conditions and check for dframe existing
  xs <- .xstatus(x.name, dframe.name)
  in.global <- xs$ig 

  # see if variable exists in data frame, if x not in Global Env or function call 
  if (!missing(x) && !in.global) .xcheck(x.name, dframe.name, dframe)

  if (!in.global) x.call <- eval(substitute(dframe$x))
  else {
    x.call <- x
    if (is.function(x)) x.call <- eval(substitute(dframe$x))
  }

  # set up graphics system
  .opendev(pdf.file, pdf.width, pdf.height)

  orig.params <- par(no.readonly=TRUE)
  on.exit(par(orig.params))

  .den.main(x.call, dframe=mydata, 
            bw, type, bin.start, bin.width, text.out,
            col.fill, col.bg, col.grid, col.nrm, col.gen,
            col.fill.nrm, col.fill.gen, 
            cex.axis, col.axis, col.ticks,
            x.pt, xlab, main, y.axis, x.min, x.max, band, ...)

  # terminate pdf graphics system
  if (!is.null(pdf.file)) {
    dev.off()
    .showfile(pdf.file, "density plot")
  }

}
