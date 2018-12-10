PieChart <-
function(x, y=NULL, data=mydata, rows=NULL,

         radius=1, hole=0.65, hole.fill=getOption("panel.fill"),

         fill=NULL, 
         color="lightgray",
         trans=getOption("trans.bar.fill"),

         density=NULL, angle=45,
         lty="solid", lwd=1, edges=200,

         clockwise=FALSE, init.angle=ifelse (clockwise, 90, 0), 

         values=getOption("values"),
         values.color=getOption("values.color"), 
         values.size=getOption("values.size"),
         values.digits=getOption("values.digits"),
         values.pos=getOption("values.pos"),

         main=NULL, main.cex=1.2, labels.cex=0.9, cex,

         add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,

         eval.df=NULL, quiet=getOption("quiet"),
         width=6.5, height=6, pdf.file=NULL, ...) {


  if (!missing(cex)) {
    main.cex <- cex * main.cex
    labels.cex <- cex * labels.cex
    values.size <- cex * values.size
  }

  fill.miss <- ifelse (missing(fill), TRUE, FALSE)
  main.miss <- ifelse (missing(main), TRUE, FALSE)

  color[which(color == "off")] <- "transparent"

  if (is.null(values.digits)) {
    if (values == "%") values.digits <- 0
    if (values == "prop") values.digits <- 2
  }

  if (missing(values) && (!missing(values.color) || !missing(values.size)
      || !missing(values.digits) || !missing(values.pos)))
    values <- "%"

  if (is.null(values.digits)) {
    if (values == "%") values.digits <- 0
    if (values == "prop") values.digits <- 2
  }

  if (missing(values.color)) {
    values.color <- "white" 
    if (values.pos == "out") values.color <- getOption("axis.text.color")
  }

  if (missing(x)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Need a variable from which to calculate the pie chart\n\n")
  }

  if (hole < 0  ||  hole >= 1) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "size of hole is a proportion, so must be between 0 and 1\n\n")
  }

  if (!(values %in% c("off", "%", "prop", "input"))) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Valid values of values: \"off\",  \"%\", \"prop\", and \"input\"\n\n")
  }

  .param.old(...)


  shiny <- ifelse (isNamespaceLoaded("shiny"), TRUE, FALSE) 
  if (is.null(eval.df))  # default values
    eval.df <- ifelse (shiny, FALSE, TRUE)

  # get actual variable name before potential call of data$x
  x.name <- deparse(substitute(x))  # could be a list of var names
  options(xname = x.name)

  df.name <- deparse(substitute(data))  # get name of data table
  options(dname = df.name)
  if ((missing(data) && shiny))  # force evaluation (not lazy) if data not specified
    data <- eval(substitute(data), envir=parent.frame())
 
  # if a tibble convert to data frame
  # data.frame is already #3 in class(data), so no char --> factor conversion 
  if (class(data)[1] == "tbl_df") {
    data <- as.data.frame(data, stringsAsFactors=TRUE)
  }

  x.in.global <- .in.global(x.name)  # see if in global, includes vars list


  # -----------------------------------------------------------
  # establish if a data frame, if not then identify variable(s)

  if (!x.in.global) {
    if (eval.df) {
      .nodf(df.name)  # check to see if data frame container exists 
      .xcheck(x.name, df.name, names(data))  # x-var in df?
    }
    data.vars <- as.list(seq_along(data))
    names(data.vars) <- names(data)
    ind <- eval(substitute(x), envir=data.vars)  # col num of each var
    if (!missing(rows)) {  # subset rows
      r <- eval(substitute(rows), envir=data, enclos=parent.frame())
      r <- r & !is.na(r)  # set missing for a row to FALSE
      data <- data[r,,drop=FALSE]
    }
  if (length(ind) > 1) data <- data[, ind]  # x is a vars list
  if (length(ind) == 1) x.call <- eval(substitute(data$x))  # x is 1 var
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
    if (!in.global) .xcheck(y.name, df.name, names(data))
    if (!in.global)
      y.call <- eval(substitute(data$y))
    else {  # vars that are function names get assigned to global
      y.call <- y
      if (is.function(y.call)) y.call <- eval(substitute(data$y))
    }

  }
  else
    y.call <- NULL


  # evaluate fill (NULL, numeric constant or a variable)
  #--------------
  if (!fill.miss) {
    fill.name <- deparse(substitute(fill))
    in.df <- ifelse (exists(fill.name, where=data), TRUE, FALSE)

    # only works for y given, not tabulated
    if (in.df) {
      fill.val <- eval(substitute(data$fill))
      fill <- .getColC(fill.val)
    }

    # or do a tabulation to get value of y
    if (fill.name == "(count)") {
      xtb <- table(x.call)
      fill <- .getColC(xtb)
    }  # end .count 
  }  # end !fill.miss

  if (!is.null(pdf.file)) {
    if (!grepl(".pdf", pdf.file)) pdf.file <- paste(pdf.file, ".pdf", sep="")
    .opendev(pdf.file, width, height)
  }
  else {
    if (!shiny) {  # not dev.new for shiny
      pdf.fnm <- NULL
      .opendev(pdf.file, width, height)
    }
  }


  # if (!shiny)
  #   dev.set(which=2)  # reset graphics window for standard R functions

   hole <- hole * radius
  .pc.main(x.call, y.call, 
        fill, color, trans, 
        radius, hole, hole.fill, edges, 
        clockwise, init.angle, 
        density, angle, lty, lwd,
        values, values.pos, values.color, values.size, values.digits,
        labels.cex, main.cex, main, main.miss,
        add, x1, x2, y1, y2,
        quiet, pdf.file, width, height, ...)

  # terminate pdf graphics system
  if (!is.null(pdf.file)) {
    dev.off()
    .showfile(pdf.file, "pie chart")
  }

}
