LineChart <-
function(x, data=d, rows=NULL,
         n_cat=getOption("n_cat"), type=NULL, 

         line_color=getOption("pt_color"), area=NULL, 

         shape_pts=21, lab_cex=1.0, axis_cex=0.75,
         axis_text_color=getOption("axis_x_text_color"),

         rotate_x=0, rotate_y=0, offset=.5,

         xy_ticks=TRUE, line_width=1,
         xlab=NULL, ylab=NULL, main=NULL, sub=NULL, cex=NULL,

         time_start=NULL, time_by=NULL, time_reverse=FALSE,

         center_line=c("default", "mean", "median", "zero", "off"),

         show_runs=FALSE, eval_df=NULL, quiet=getOption("quiet"),
         width=6, height=6, pdf=FALSE, ...) {


  message(">>> Deprecated<<<\n\n",
      "Instead use the  Plot()  function to plot line charts.\n",
      "For explanation see the vignettes.\n",
      "Enter: browseVignettes(\"lessR\")\n\n",
      "See vignette: Visualize: Time Data.\n\n")

  # a dot in a parameter name to an underscore
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    change <- c("n.cat", "line.color", "shape.pts", "lab.cex", "axis.cex",
                "axis.text.color",  "rotate.x", "rotate.y",
                "time.reverse", "center.line", "show.runs")
    for (i in 1:length(dots)) {
      if (names(dots)[i] %in% change) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }


  center_line <- match.arg(center_line)

  fill <- getOption("bar_fill_cont") 
  color <- getOption("pt_color_ordered")
  panel_fill <- getOption("panel_fill")
  panel_color <- getOption("panel_color")

  if (line_color == "off") line_color <- "transparent"
  if (!is.null(area)) if (area == "off") area <- "transparent"

  dots <- list(...)  # check for deprecated parameters
  if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (grepl("color_", names(dots)[i], fixed=TRUE)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "color options dropped the  color_ prefix\n",
          "eg., fill, instead of color_fill\n\n")
      }
      if (names(dots)[i] == "line") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "line  is now  line_color\n\n")
      }
      if (grepl("col.", names(dots)[i], fixed=TRUE)) 
        if (names(dots)[i] != "col.main"  &&
            names(dots)[i] != "col.lab"  &&
            names(dots)[i] != "col.sub") {
          cat("\n"); stop(call.=FALSE, "\n","------\n",
            "color options dropped the  col. prefix\n",
            "eg., fill, instead of col_fill\n\n")
      }
    }
  }

  shiny <- ifelse (isNamespaceLoaded("shiny"), TRUE, FALSE) 
  if (is.null(eval_df))  # default values
    eval_df <- ifelse (shiny, FALSE, TRUE)
   # get actual variable name before potential call of data$x
  if (!missing(x))  # can't do is.null or anything else with x until evaluated
    x.name <- deparse(substitute(x))  # could be a list of var names
  else
    x.name <- NULL  # otherwise is actually set to "NULL" if NULL
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


  if (!is.null(x.name))
    x.in.global <- .in.global(x.name, quiet)  # in global?, includes vars list
  else
    x.in.global <- FALSE
    
# -----------------------------------------------------------
# establish if a data frame, if not then identify variable(s)
# x can be missing entirely, with a data frame passed instead
# if x a vector, then x.name not in data, but also not in global

  if (!missing(x)) {

    # x not in global env, in df, specify data= forces to data frame
    if (!x.in.global) {
      if (!mydata.ok) .nodf(df.name)  # check to see if df exists 
     .xcheck(x.name, df.name, names(data))  # x-var in df?
      data.vars <- as.list(seq_along(data))
      names(data.vars) <- names(data)
      ind <- eval(substitute(x), envir=data.vars)  # col num of each var      
      if (!missing(rows)) {  # subset rows
        r <- eval(substitute(rows), envir=data, enclos=parent.frame())
        r <- r & !is.na(r)  # set missing for a row to FALSE
        data <- data[r,,drop=FALSE]
      }
      if (!("list" %in% class(data))) {
        data <- data[, ind]
        if (length(ind) == 1) {
          data <- data.frame(data, stringsAsFactors=TRUE)  # x is 1 var
          names(data) <- x.name
         }
      }
      else {
        data <- data.frame(data[[ind]], stringsAsFactors=TRUE)
        names(data) <- x.name
      }
    }
    else { # x is in the global environment (vector or data frame)
      if (is.data.frame(x))  # x a data frame
        data <- x
      else {  # x a vector in global
        .in.global(x.name, quiet)  # x.name an expression?
        data <- data.frame(x, stringsAsFactors=TRUE)  # x is 1 var
        names(data) <- x.name
      }
    }
  }


# ---------------
# do the analysis

    # set up graphics
    manage.gr <- .graphman()  # manage graphics?
    if (manage.gr) {
      i.win <- 0
      for (i in 1:ncol(data)) {
        if (is.numeric(data[,i])  &&  !.is.num.cat(data[,i], n_cat)) 
          i.win <- i.win + 1
      }
      .graphwin(i.win, d.w=width, d.h=height)
      open.win <- 2
    }

    plot.i <- 0  # keep track of generated graphics
    plot.title  <- character(length=0)

    # no suggestions if multiple variables
    if (ncol(data) > 1) {
      sug <- getOption("suggest")
      options(suggest = FALSE)
    }

    for (i in 1:ncol(data)) {

      if (!is.ts(data[,i]))
        nu <- length(unique(na.omit(data[,i])))
      else
        nu <- length(unique(data[,i]))

      x.name <- names(data)[i]
      options(xname = x.name)
      options(yname = x.name)  # just needed for lc()

      if (is.numeric(data[,i])) {
        # let 1 variable go through, even if num.cat
        if (ncol(data) == 1  ||  !.is.num.cat(data[,i], n_cat)) {

        if (pdf) {
          pdf.fnm <- paste("LC", "_", x.name, ".pdf", sep="") 
          .opendev(pdf.fnm, width, height)
        }
        else {
          pdf.fnm <- NULL
          if (ncol(data) > 1) {
            plot.i <- plot.i + 1
            plot.title[plot.i] <- paste("Line Chart of ", x.name, sep="")
            if (manage.gr) {
              open.win <- open.win + 1
              dev.set(which = open.win)
            }
          }
        }

      .lc.main(data[,i], type,
         line_color, area, color, fill, shape_pts,
         panel_color, panel_fill,
         lab_cex, axis_cex, axis_text_color,
         rotate_x, rotate_y, offset, xy_ticks,
         line_width, xlab, ylab, main, sub, cex,
         time_start, time_by, time_reverse, 
         center_line, show_runs, quiet, ...)

      if (pdf) {
        dev.off()
        if (!quiet) .showfile(pdf.fnm, "Line Chart")
      }

    }  # nu > n_cat
    else
      if (!quiet) .ncat("Line Chart", x.name, nu, n_cat)

    }  # is.numeric(data[,i])

  }  # for

  if (ncol(data) > 1) {
    if (!pdf) if (is.null(options()$knitr.in.progress))
      .plotList(plot.i, plot.title)
  }

}
