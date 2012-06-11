PieChart <-
function(x, dframe=mydata, 
         random.col=FALSE,
         col.slices=NULL, col.low=NULL, col.hi=NULL,
         colors=c("blue", "gray", "rose", "green", "gold", "red",
                  "rainbow", "terrain", "heat"),
         text.out=TRUE, main=NULL, ...) {


  colors <- match.arg(colors)

  x.name <- deparse(substitute(x)) 
  options(xname = x.name)

  # get data frame name
  dframe.name <- deparse(substitute(dframe))

  # get conditions and check for dframe existing
  xs <- .xstatus(x.name, dframe.name)
  is.frml <- xs$ifr
  from.data <- xs$fd
  in.global <- xs$ig 

  # see if variable exists in the data frame, if x not in Global Env or function call 
  if (!missing(x) && !in.global)  .xcheck(x.name, dframe.name, dframe)

  if (!in.global) x.call <- eval(substitute(dframe$x))
  else {  # vars that are function names get assigned to global
    x.call <- x
    if (is.function(x.call)) x.call <- eval(substitute(dframe$x))
  }

  .graphwin()
  .pc.main(x.call, 
       random.col, col.slices, col.low, col.hi,
       colors, text.out, main, ...)

}
