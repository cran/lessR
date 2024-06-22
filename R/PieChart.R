PieChart <-
function(x, y=NULL, data=d, filter=NULL,

         radius=1, hole=0.65, hole_fill=getOption("panel_fill"),

         theme=getOption("theme"),
         fill=NULL, 
         color="lightgray",
         transparency=getOption("trans_bar_fill"),

         density=NULL, angle=45,
         lty="solid", lwd=1, edges=200,

         clockwise=FALSE, init_angle=ifelse (clockwise, 90, 0), 

         labels=getOption("labels"),
         labels_color=getOption("labels_color"), 
         labels_size=getOption("labels_size"),
         labels_digits=getOption("labels_digits"),
         labels_position=getOption("labels_position"),

         main=NULL, main_cex=getOption("main_cex")*1.2,
         labels_cex=getOption("lab_cex"), cex,

         add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,

         rows=NULL,

         eval_df=NULL, quiet=getOption("quiet"),
         width=6.5, height=6, pdf_file=NULL, ...) {


  # ------------ Old Stuff ----------------------------------
  if (!missing(rows)) 
    message(">>> Parameter  rows  renamed to:  filter.\n",
            "    Change to  filter,  rows  will stop working in the future.\n")

  # a dot in a parameter name to an underscore
  dots <- list(...)
  n.values <- 0
  if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (grepl("values", names(dots)[i], fixed=TRUE)) {
        n.values <- n.values + 1
        if (n.values == 1) 
          message(">>> Parameters  values, values_color, etc. now ",
                  "renamed to:  labels, labels_color, etc.\n",
                  "    Old parameter names will stop working in the future.\n")
        if (names(dots)[i] == "values") labels <- dots[[i]]
        if (names(dots)[i] == "values_color") labels_color <- dots[[i]]
        if (names(dots)[i] == "values_size") labels_size <- dots[[i]]
        if (names(dots)[i] == "values_digits") labels_decimals <- dots[[i]]
        if (names(dots)[i] == "values_position") labels_position <- dots[[i]]
        if (names(dots)[i] == "values_cut") labels_cut <- dots[[i]]
      }
    }
    if (names(dots)[i] == "addtop") pad_y_max <- dots[[i]] 
    if (names(dots)[i] == "add_top") pad_y_max <- dots[[i]] 
    if (names(dots)[i] == "stat_yx") stat <- dots[[i]]
    if (grepl(".", names(dots)[i], fixed=TRUE)) {
      nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)  # dot to _
      assign(nm, dots[[i]])
      get(nm)
    }
  }

  if (!missing(cex)) {
    main_cex <- cex * main_cex
    labels_cex <- cex * labels_cex
    labels_size <- cex * labels_size
  }

  trans <- transparency

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

  if (is.null(labels_digits)) {
    if (labels == "%") labels_digits <- 0
    if (labels == "prop") labels_digits <- 2
  }

  if (missing(labels) && (!missing(labels_color) || !missing(labels_size)
      || !missing(labels_digits) || !missing(labels_position)))
    labels <- "%"

  if (is.null(labels_digits)) {
    if (labels == "%") labels_digits <- 0
    if (labels == "prop") labels_digits <- 2
  }

  if (missing(labels_color)) {
    labels_color <- "white" 
    if (labels_position == "out") labels_color <- getOption("axis_text_color")
  }

  if (missing(x)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Need a variable from which to calculate the pie chart\n\n")
  }

  if (hole < 0  ||  hole >= 1) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Size of hole is a proportion, so must be between 0 and 1\n\n")
  }

  if (!(labels %in% c("off", "%", "prop", "input"))) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Valid labels of labels: \"off\",  \"%\", \"prop\", and \"input\"\n\n")
  }

  .param.old(...)


  # --------- data frame stuff
  
  data.miss <- ifelse (missing(data), TRUE, FALSE) 

  # let deprecated mydata work as default
  dfs <- .getdfs() 
  mydata.ok <- FALSE
  if (!is.null(dfs)) {
    if ("mydata" %in% dfs  &&  !("d" %in% dfs)) {
      d <- mydata
      rm(mydata)
      df.name <- "mydata"
      mydata.ok <- TRUE
      options(dname = df.name)
    }
  }

  # get name of data table
  if (!mydata.ok) {
    df.name <- deparse(substitute(data))
    options(dname = df.name)
  }

  shiny <- FALSE
  if (!is.null(sys.call(-1)))  # is NULL when called directly from R console
    if (sys.call(-1) == "renderPlot()") {  # from shiny, user or interact()
      shiny <- TRUE
      data <- eval(substitute(data), envir=parent.frame())
    }
 
  # if a tibble, convert to data frame
  if (!shiny) {
    if (exists(df.name, envir=.GlobalEnv)) {
      if (any(grepl("tbl", class(data), fixed=TRUE)))
        data <- data.frame(data)
    }
  }
  else  # no check for existence of df.name
    if (any(grepl("tbl", class(data), fixed=TRUE)))
      data <- data.frame(data)

  x.name <- deparse(substitute(x), width.cutoff = 120L)
  options(xname = x.name)

    if (!is.null(x.name))
      x.in.global <- .in.global(x.name, quiet)  # in global?, includes vars list
    else
      x.in.global <- FALSE

  if (!x.in.global)  {
    if (df.name != "NULL") {  # if NULL, force global (shiny, from interact() )
    # force evaluation (not lazy) if data not specified but relies on default d
    if (data.miss) {
      data <- eval(substitute(data), envir=parent.frame())
      # the 1.201 comes from Shiny, need to reset
      # l.cex and l.axc are set in interact() before shiny run
      if (getOption("lab_cex") == 1.201) {
       if (getOption("l.cex") != 1.201) {
          style(lab_cex=getOption("l.cex"))
          style(axis_cex=getOption("l.axc"))
        }
        else
          style()
        }
      }
    }
    else # df.name is NULL
      x.in.global <- TRUE
  }
    
  eval_df <- !x.in.global 
    

  # -----------------------------------------------------------
  # establish if a data frame, if not then identify variable(s)

  if (!x.in.global) {
    if (eval_df) {
      if (!mydata.ok) .nodf(df.name)  # check to see if df exists 
      .xcheck(x.name, df.name, names(data))  # x-var in df?
    }

    # subset filter (with deprecated rows parameter also)
    if (!missing(filter) || !missing(rows)) {
      txt <- .filter(deparse(substitute(filter)))
      if (!missing(filter))  # subset filter
        r <- eval(str2expression(txt), envir=data, enclos=parent.frame())
      if (!missing(rows))
        r <- eval(substitute(rows), envir=data, enclos=parent.frame())
      r <- r & !is.na(r)  # set missing for a row to FALSE
      nr.before <- nrow(data)
      if (any(r))
        data <- data[r,,drop=FALSE]
      if (!quiet) {
        if (!missing(filter))  # filter parameter present 
          cat("\nfilter: ",  txt, "\n-----\n")
        cat("Rows of data before filtering: ", nr.before, "\n")
        cat("Rows of data after filtering:  ", nrow(data), "\n\n")
      }
    }  # end filter

    data.vars <- as.list(seq_along(data))
    names(data.vars) <- names(data)
    ind <- eval(substitute(x), envir=data.vars)  # col num of each var
    if (length(ind) > 1) data <- data[, ind]  # x is a vars list
    if (length(ind) == 1) x.call <- eval(substitute(data$x))  # x is 1 var
  }
  else {  # x is in the global environment (vector, matrix or data frame)
    if (is.data.frame(x))  # x a data frame
      data <- x
    else {  # x a vector or matrix in global
      .in.global(x.name, quiet)  # x.name an expression?
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
    in.global <- .in.global(y.name, quiet)

    # see if var exists in data frame, if y not in global Env or function call 
      if (!in.global) {
        if (eval_df)
          .xcheck(y.name, df.name, names(data))
        y.call <- eval(substitute(data$y))
      }
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
#   in.df <- ifelse (exists(fill_name, where=data), TRUE, FALSE)
    in.df <- FALSE

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


  # set up pdf_file if needed
  if (!is.null(pdf_file)) {
    if (!grepl(".pdf", pdf_file))
      pdf_file <- paste(pdf_file, ".pdf", sep="")
    pdf(file=pdf_file, width=width, height=height, onefile=FALSE)
  }
  else {
    if (df.name != "NULL")  # not dev.new for shiny
        .opendev(pdf_file, width, height)
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
  pc <- .pc.main(x.call, y.call, 
        fill, color, trans, 
        radius, hole, hole_fill, edges, 
        clockwise, init_angle, 
        density, angle, lty, lwd,
        labels, labels_position, labels_color, labels_size, labels_digits,
        labels_cex, main_cex, main, main.miss,
        add, x1, x2, y1, y2,
        quiet, pdf_file, width, height, ...)

  # terminate pdf graphics system
  if (!is.null(pdf_file)) {
    dev.off()
    if (!quiet) .showfile(pdf_file, "PieChart")
  }

  # if attached -- from interact() -- de-attach to be safe
  if ("shiny" %in% .packages()) detach(package:shiny)
  return(invisible(pc))

}
