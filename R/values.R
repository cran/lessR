values <-
function(x, data=d, ...) {

  # get actual variable name before potential call of data$x
  x.name <- deparse(substitute(x)) 

  # get data frame name
  dname <- deparse(substitute(data))

  # see if variable exists in the data frame, if x not in style Env or function call 
  in.style <- .in.global(x.name, dname)
  if (!missing(x) && !in.style) .xcheck(x.name, dname, names(data))

  if (!in.style)
    x.call <- eval(substitute(data$x))
  else {  # vars that are function names get assigned to style
    x.call <- x
    if (is.function(x.call)) x.call <- eval(substitute(data$x))
  }

  print(x.call, ...)

}
