PieChart <-
function(x, y=NULL, data=d, rows=NULL,

         radius=1, hole=0.65, hole_fill=getOption("panel_fill"),

         theme=getOption("theme"),
         fill=NULL, 
         color="lightgray",
         trans=getOption("trans_bar_fill"),

         density=NULL, angle=45,
         lty="solid", lwd=1, edges=200,

         clockwise=FALSE, init_angle=ifelse (clockwise, 90, 0), 

         values=getOption("values"),
         values_color=getOption("values_color"), 
         values_size=getOption("values_size"),
         values_digits=getOption("values_digits"),
         values_position=getOption("values_position"),

         main=NULL, main_cex=getOption("main_cex"),
         labels_cex=getOption("lab_cex"), cex,

         add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,

         eval_df=NULL, quiet=getOption("quiet"),
         width=6.5, height=6, pdf_file=NULL, ...) {


  # a dot in a parameter name to an underscore
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (length(grep(".", names(dots)[i], fixed=TRUE)) > 0) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }

  if (!missing(cex)) {
    main_cex <- cex * main_cex
    labels_cex <- cex * labels_cex
    values_size <- cex * values_size
  }

  fill.miss <- ifelse (missing(fill), TRUE, FALSE)
  color.miss <- ifelse (missing(color), TRUE, FALSE)
  trans.miss <- ifelse (missing(trans), TRUE, FALSE)
  main.miss <- ifelse (missing(main), TRUE, FALSE)

  color[which(color == "off")] <- "transparent"

  if (theme != getOption("theme")) {  # not the current theme
    sty <- style(theme, reset=FALSE)
    if (fill.miss) fill <- sty$bar$bar.fill.discrete
    if (color.miss) color <- sty$bar$color
    if (trans.miss) trans <- sty$bar$trans.fill
  }

  if (is.null(values_digits)) {
    if (values == "%") values_digits <- 0
    if (values == "prop") values_digits <- 2
  }

  if (missing(values) && (!missing(values_color) || !missing(values_size)
      || !missing(values_digits) || !missing(values_position)))
    values <- "%"

  if (is.null(values_digits)) {
    if (values == "%") values_digits <- 0
    if (values == "prop") values_digits <- 2
  }

  if (missing(values_color)) {
    values_color <- "white" 
    if (values_position == "out") values_color <- getOption("axis_text_color")
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
  if (is.null(eval_df))  # default values
    eval_df <- ifelse (shiny, FALSE, TRUE)

  # get actual variable name before potential call of data$x
  x.name <- deparse(substitute(x))  # could be a list of var names
  options(xname = x.name)

  # let deprecated mydata work as default
  dfs <- .getdfs() 
  mydata.ok <- FALSE
  if (!is.null(dfs)) {
    if ("mydata" %in% dfs  &&  !("d" %in% dfs)) {
      d <- mydata
      df.name <- "mydata"
      mydata.ok <- TRUE
      options(dname = df.name)
    }
  }

  if (!mydata.ok) {
    df.name <- deparse(substitute(data))  # get name of data table
    options(dname = df.name)
  }
 
  # if a tibble convert to data frame
  if (!is.null(dfs)) {
    if (df.name %in% dfs) {  # tibble to df
      if (any(grepl("tbl", class(data), fixed=TRUE))) {
        data <- data.frame(data, stringsAsFactors=FALSE)
      }
    }
  }

  if ((missing(data) && shiny))  # force evaluation (not lazy) if data not specified
    data <- eval(substitute(data), envir=parent.frame())

  x.in.global <- .in.global(x.name, quiet)  # in global?, includes vars list


  # -----------------------------------------------------------
  # establish if a data frame, if not then identify variable(s)

  if (!x.in.global) {
    if (eval_df) {
      if (!mydata.ok) .nodf(df.name)  # check to see if df exists 
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
    fill_name <- deparse(substitute(fill))
    in.df <- ifelse (exists(fill_name, where=data), TRUE, FALSE)

    # only works for y given, not tabulated
    if (in.df) {
      fill_val <- eval(substitute(data$fill))
      fill <- .getColC(fill_val)
    }

    # or do a tabulation to get value of y
    if (substr(fill_name, 1, 6) == "(count") {
      xtb <- table(x.call)
      fill <- .getColC(xtb, fill_name=fill_name)
    }  # end .count 
  }  # end !fill.miss

  if (!is.null(pdf_file)) {
    if (!grepl(".pdf", pdf_file)) pdf_file <- paste(pdf_file, ".pdf", sep="")
    .opendev(pdf_file, width, height)
  }
  else {
    if (!shiny) {  # not dev.new for shiny
      pdf.fnm <- NULL
      .opendev(pdf_file, width, height)
    }
  }

  n.levels <- length(unique(x.call))
  if (fill.miss) {
    is.ord <- ifelse (is.ordered(x.call), TRUE, FALSE)
    ordYN <- ifelse (is.ord, TRUE, FALSE)
    fill <- .color_range(.get_fill(theme, ordYN), n.levels)  # do default range
  }
  else
    fill <- .color_range(fill, n.levels)

  # if (!shiny)
  #   dev.set(which=2)  # reset graphics window for standard R functions

   hole <- hole * radius
  .pc.main(x.call, y.call, 
        fill, color, trans, 
        radius, hole, hole_fill, edges, 
        clockwise, init_angle, 
        density, angle, lty, lwd,
        values, values_position, values_color, values_size, values_digits,
        labels_cex, main_cex, main, main.miss,
        add, x1, x2, y1, y2,
        quiet, pdf_file, width, height, ...)

  # terminate pdf graphics system
  if (!is.null(pdf_file)) {
    dev.off()
    .showfile(pdf_file, "pie chart")
  }

}
