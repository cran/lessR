Correlation <-
function(x, y, dframe=mydata,
         miss=c("pairwise", "listwise", "everything"),
         show.n=NULL, brief=FALSE, n.cat=getOption("n.cat"), digits.d=NULL,
         colors=c("blue", "gray", "rose", "green", "gold", "red"),
         heat.map=TRUE, main=NULL, bottom=3, right=3,
         pdf.file=NULL, pdf.width=5, pdf.height=5, ...) {
# x can be a data frame, or x can be a variable in a specified data frame

  miss <- match.arg(miss)

  if (missing(colors)) 
    colors <- getOption("colors")
  else
    colors <- match.arg(colors)

  is.dtfrm <- FALSE  # is data frame

  if (missing(x)) {
    x.name <- ""  # in case x is missing, i.e., data frame mydata
    is.dtfrm <- TRUE
    dframe <- eval(substitute(mydata))
  }
  else {
    # get actual variable name or get data frame name
    x.name <- deparse(substitute(x)) 
    options(xname = x.name)
    if (exists(x.name, where=1)) if (is.data.frame(x)) {
       is.dtfrm <- TRUE
       dframe <- x
    }
  }

  if (!is.dtfrm) {

    dframe.name <- deparse(substitute(dframe))

    # get conditions and check for dframe existing
    xs <- .xstatus(x.name, dframe.name)
    is.frml <- xs$ifr
    in.global <- xs$ig 

    # see if the variable exists in data frame, if x not in Global Env 
    if (!in.global) .xcheck(x.name, dframe.name, dframe)

    if (in.global) x.call <- x else x.call <- eval(substitute(dframe$x))

    # evaluate y
    if (!missing(y)) {

      # get actual variable name before potential call of dframe$x
      y.name <- deparse(substitute(y)) 
      options(yname = y.name)

      # see if y exists from a function call
      # indicate a function call with sys.frame returns larger than 1 
      if (exists(y.name, where=parent.frame(n=1)) && sys.nframe() > 1) 
        in.call <- TRUE else in.call <- FALSE

      # get conditions and check for dframe existing
      if (!in.call) {
        xs <- .xstatus(y.name, dframe.name)
        in.global <- xs$ig 
      }
      else in.global <- FALSE

      # see if var exists in data frame, if x not in Global Env or function call 
      if (!in.global && !in.call) .xcheck(y.name, dframe.name, dframe)

      if (in.global) y.call <- y 
      else y.call <- eval(substitute(dframe$y))
    }
    else {
      y.call <- NULL
      if (is.null(show.n))
        if (nrow(dframe) <= 15) show.n <- TRUE else show.n <- FALSE
    }

  }  # x not data frame

  if (is.dtfrm) 
      .cr.data.frame(dframe, miss, show.n=show.n, n.cat, digits.d,
                     heat.map, colors, main, bottom, right, 
                     pdf.file, pdf.width, pdf.height, ...) 

  else .cr.default(x.call, y.call, brief, ...) 

}
