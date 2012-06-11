values <-
function(x, dframe=mydata, ...) {

  # get actual variable name before potential call of dframe$x
  x.name <- deparse(substitute(x)) 

  # get data frame name
  dframe.name <- deparse(substitute(dframe))

  # get conditions and check for dframe existing
  xs <- .xstatus(x.name, dframe.name)
  in.global <- xs$ig 

  # see if variable exists in the data frame, if x not in Global Env or function call 
  if (!missing(x) && !in.global)  .xcheck(x.name, dframe.name, dframe)

  if (!in.global) x.call <- eval(substitute(dframe$x))
  else {  # vars that are function names get assigned to global
    x.call <- x
    if (is.function(x.call)) x.call <- eval(substitute(dframe$x))
  }

  print(x.call, ...)

}
