values <-
function(x, data=mydata, ...) {


  # get actual variable name before potential call of data$x
  x.name <- deparse(substitute(x)) 

  # get data frame name
  dname <- deparse(substitute(data))

  # get conditions and check for data existing
  xs <- .xstatus(x.name, dname)
  in.style <- xs$ig 

  # see if variable exists in the data frame, if x not in style Env or function call 
  if (!missing(x) && !in.style) .xcheck(x.name, dname, names(data))

  if (!in.style) x.call <- eval(substitute(data$x))
  else {  # vars that are function names get assigned to style
    x.call <- x
    if (is.function(x.call)) x.call <- eval(substitute(data$x))
  }

  print(x.call, ...)

}
