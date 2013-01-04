PieChart <-
function(x, data=mydata, 
         col.fill=NULL, col.low=NULL, col.hi=NULL,
         colors=c("rainbow", "terrain", "heat"),
         random.col=FALSE, main=NULL,
         quiet=FALSE, pdf.file=NULL, pdf.width=5, pdf.height=5, ...) {


  if (missing(colors)) 
    colors <- getOption("colors")
  else
    colors <- match.arg(colors)

  x.name <- deparse(substitute(x)) 
  options(xname = x.name)

  # get data frame name
  dname <- deparse(substitute(data))
  options(dname = dname)

  # get conditions and check for data existing
  xs <- .xstatus(x.name, dname)
  is.frml <- xs$ifr
  from.data <- xs$fd
  in.global <- xs$ig 

  # see if variable exists in the data frame, if x not in Global Env or function call 
  if (!missing(x) && !in.global)  .xcheck(x.name, dname, data)

  if (!in.global) x.call <- eval(substitute(data$x))
  else {  # vars that are function names get assigned to global
    x.call <- x
    if (is.function(x.call)) x.call <- eval(substitute(data$x))
  }

  # set up graphics system
  .opendev(pdf.file, pdf.width, pdf.height)

  #orig.params <- par(no.readonly=TRUE)
  #on.exit(par(orig.params))

  .pc.main(x.call, 
       random.col, col.fill, col.low, col.hi,
       colors, quiet, main, 
       pdf.file, pdf.width, pdf.height, ...)

  # terminate pdf graphics system
  if (!is.null(pdf.file)) {
    dev.off()
    .showfile(pdf.file, "pie chart")
  }

}
