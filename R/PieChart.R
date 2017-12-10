PieChart <-
function(x, y=NULL, data=mydata,

         radius=1, hole=0.65, hole.fill=getOption("panel.fill"),

         fill=NULL, low.fill=NULL, hi.fill=NULL,
         colors=c("rainbow", "terrain", "heat"),
         random.color=FALSE,

         clockwise=FALSE, init.angle=ifelse (clockwise, 90, 0), 
         density=NULL, angle=45, border="black", lty="solid",
         edges=200, 

         main=NULL, cex=1, cex.main=1,

         quiet=getOption("quiet"),
         width=5, height=5, pdf.file=NULL, ...) {


  if (missing(colors)) 
    colors <- getOption("theme")
  else
    colors <- match.arg(colors)

  if (border[1] == "off") border[1] <- "transparent"

  if (hole < 0  ||  hole >= 1) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "size of hole is a proportion, so must be between 0 and 1\n\n")
  }

  dots <- list(...)  # check for deprecated parameters
  if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      old.nm <- c("col.fill", "col.low", "col.hi")
      if (names(dots)[i] %in% old.nm) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "options that began with the abbreviation  col  now begin with  ",
          "color \n\n")
      }
    }
  }


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

  #orig.params <- par(no.readonly=TRUE)
  #on.exit(par(orig.params))

  hole <- hole * radius
  .pc.main(x.call, y.call, 
       random.color, fill, low.fill, hi.fill, colors,
       radius, hole, hole.fill, edges, 
       clockwise, init.angle, 
       density, angle, border, lty,
       cex, cex.main, quiet, main,
       pdf.file, width, height, ...)

  # terminate pdf graphics system
  if (!is.null(pdf.file)) {
    dev.off()
    .showfile(pdf.file, "pie chart")
  }

}
