PieChart <-
function(x, y=NULL, data=mydata,

         radius=1, hole=0.65, hole.fill=getOption("panel.fill"),

         fill=NULL, 
         color=getOption("bar.color"),
         trans=getOption("trans.bar.fill"),

         density=NULL, angle=45,
         lty="solid", lwd=1, edges=200,

         clockwise=FALSE, init.angle=ifelse (clockwise, 90, 0), 

         values=c("none", "percent", "prop", "input"),
         values.pos=c("pie", "labels"), values.color="white",
         values.cex=.85,

         main=NULL, main.cex=1.2, labels.cex=0.9, cex,

         add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,

         quiet=getOption("quiet"),
         width=5, height=5, pdf.file=NULL, ...) {


  values <- match.arg(values)
  values.pos <- match.arg(values.pos)

  if (!missing(cex)) {
    main.cex <- cex * main.cex
    labels.cex <- cex * labels.cex
    values.cex <- cex * values.cex
  }

  if (missing(x)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Need a variable from which to calculate the pie chart\n\n")
  }

  main.miss <- ifelse (missing(main), TRUE, FALSE)

  # default color scale
  if (is.null(fill)) {
    theme <- getOption("theme")
    fill <- ifelse (theme %in% c("gray", "white"), "grayscale", "hcl")
  }

  if (hole < 0  ||  hole >= 1) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "size of hole is a proportion, so must be between 0 and 1\n\n")
  }

  .param.old(...)


  # evaluate x
  #-------------
  x.name <- deparse(substitute(x)) 
  options(xname = x.name)

  # get data frame name
  df.name <- deparse(substitute(data))
  options(dname = df.name)

  if (!exists(x.name, where=.GlobalEnv)) {  # x not in global env, in df
    .nodf(df.name)  # check to see if data frame container exists 
    .xcheck(x.name, df.name, data)  # see if var in df, vars lists not checked
    vars.list <- as.list(seq_along(data))
    names(vars.list) <- names(data)
    x.col <- eval(substitute(x), envir=vars.list)  # col num of each var
    if (length(x.col) > 1) data <- data[, x.col]  # x is a vars list
    if (length(x.col) == 1) x.call <- eval(substitute(data$x))  # x is 1 var
  }
  else {  # x is in the global environment (vector, matrix or data frame)
    if (is.data.frame(x))  # x a data frame
      data <- x
    else {  # x a vector or matrix in global
      .xstatus(x.name, df.name, quiet)
      if (exists(x.name, where=.GlobalEnv)) if (is.matrix(x)) { 
        x.name <- xlab
        xlab <- NULL
        options(xname = x.name)
      }
      x.call <- x
      if (is.function(x.call)) x.call <- eval(substitute(data$x))
    }
  }


  # evaluate y
  #-------------
  if (!missing(y)) {

    # get actual variable name before potential call of data$x
    y.name <- deparse(substitute(y)) 
    options(yname = y.name)

    # get conditions and check for data existing
    xs <- .xstatus(y.name, df.name, quiet)
    in.global <- xs$ig 

    # see if var exists in data frame, if x not in global Env or function call 
    if (!in.global) .xcheck(y.name, df.name, data)
    if (!in.global)
      y.call <- eval(substitute(data$y))
    else {  # vars that are function names get assigned to global
      y.call <- y
      if (is.function(y.call)) y.call <- eval(substitute(data$y))
    }

  }
  else
    y.call <- NULL


  # set up graphics system
  if (!is.null(pdf.file))
    if (!grepl(".pdf", pdf.file)) pdf.file <- paste(pdf.file, ".pdf", sep="")
  .opendev(pdf.file, width, height)

  hole <- hole * radius
  .pc.main(x.call, y.call, 
        fill, color, trans, 
        radius, hole, hole.fill, edges, 
        clockwise, init.angle, 
        density, angle, lty, lwd,
        values, values.pos, values.color, values.cex,
        labels.cex, main.cex, main, main.miss,
        add, x1, x2, y1, y2,
        quiet, pdf.file, width, height, ...)

  # terminate pdf graphics system
  if (!is.null(pdf.file)) {
    dev.off()
    .showfile(pdf.file, "pie chart")
  }

}
