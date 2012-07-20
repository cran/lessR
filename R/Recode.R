Recode <-
function(x, new.name=NULL, old, new, dframe=mydata) {

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

  .rec.main(x.call, x.name, new.name, old, new, dframe, dframe.name)

}
