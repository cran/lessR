LineChart <-
function(x, data=mydata, n.cat=getOption("n.cat"), type=NULL, 

         fill=getOption("bar.fill"), 
         stroke=getOption("pt.stroke"),
         bg.fill=getOption("bg.fill"),
         bg.stroke=getOption("bg.stroke"),
         line=getOption("pt.stroke"),
         area=NULL, 

         shape.pts=21, cex.axis=0.75, values.stroke="gray30",

         rotate.x=0, rotate.y=0, offset=.5,

         xy.ticks=TRUE, line.width=1,
         xlab=NULL, ylab=NULL, main=NULL, sub=NULL, cex=NULL,

         time.start=NULL, time.by=NULL, time.reverse=FALSE,

         center.line=c("default", "mean", "median", "zero", "off"),

         show.runs=FALSE, quiet=getOption("quiet"),
         width=4.5, height=4.5, pdf=FALSE, ...) {


  center.line <- match.arg(center.line)

  for (i in 1:length(fill))
    if (fill[i] == "off") fill[i] <- "transparent"
  for (i in 1:length(stroke))
    if (stroke[i] == "off") stroke[i] <- "transparent"
  if (bg.fill == "off") bg.fill <- "transparent"
  if (bg.stroke == "off") bg.stroke <- "transparent"
  if (line == "off") bg.stroke <- "transparent"
  if (!is.null(area)) if (area == "off") area <- "transparent"

  dots <- list(...)  # check for deprecated parameters
  if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (grepl("color.", names(dots)[i], fixed=TRUE)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "color options dropped the  color. prefix\n",
          "eg., fill, instead of color.fill\n\n")
      }
      if (grepl("col.", names(dots)[i], fixed=TRUE)) 
        if (names(dots)[i] != "col.main"  &&
            names(dots)[i] != "col.lab"  &&
            names(dots)[i] != "col.sub") {
          cat("\n"); stop(call.=FALSE, "\n","------\n",
            "color options dropped the  col. prefix\n",
            "eg., fill, instead of col.fill\n\n")
      }
    }
  }

  # get actual variable name before potential call of data$x
  x.name <- deparse(substitute(x))
  options(xname = x.name)
  options(yname = x.name)  # for .lc.main, which uses y as the var

  df.name <- deparse(substitute(data))
  options(dname = df.name)


# -----------------------------------------------------------
# establish if a data frame, if not then identify variable(s)

  if (!missing(x)) {
    if (!exists(x.name, where=.GlobalEnv)) {  # x not in global env, in df
      .nodf(df.name)  # check to see if data frame container exists 
      .xcheck(x.name, df.name, data)  # see if var in df, vars lists not checked
      vars.list <- as.list(seq_along(data))
      names(vars.list) <- names(data)
      x.col <- eval(substitute(x), envir=vars.list)  # col num of each var
      if (!("list" %in% class(data))) {
        data <- data[, x.col]
        if (length(x.col) == 1) {
          data <- data.frame(data)  # x is 1 var
          names(data) <- x.name
         }
      }
      else {
        data <- data.frame(data[[x.col]])
        names(data) <- x.name
      }
    }
    else { # x is in the global environment (vector or data frame)
      if (is.data.frame(x))  # x a data frame
        data <- x
      else {  # x a vector in global
        data <- data.frame(x)  # x is 1 var
        names(data) <- x.name
      }
    }
  }


# ---------------
# do the analysis

  if (ncol(data) > 1) {
    plot.i <- 0  # keep track of generated graphics
    plot.title  <- character(length=0)
  }

  for (i in 1:ncol(data)) {
    cat("\n")
    
    if (!is.ts(data[,i]))
      nu <- length(unique(na.omit(data[,i])))
    else
      nu <- length(unique(data[,i]))
      
    x.name <- names(data)[i]
    options(xname = x.name)

    if (is.numeric(data[,i])) {
      # let 1 variable go through, even if num.cat
      if (ncol(data) == 1  ||  !.is.num.cat(data[,i], n.cat)) {


      if (pdf)
        pdf.fnm <- paste("LC", "_", x.name, ".pdf", sep="") 
      else {
        pdf.fnm <- NULL
        if (ncol(data) > 1) {
          plot.i <- plot.i + 1
          plot.title[plot.i] <- paste("LineChart of ", x.name, sep="")
        }
      }
      .opendev(pdf.fnm, width, height)


      .lc.main(data[,i], type,
         line, area, stroke, fill, shape.pts,
         bg.stroke, bg.fill,
         cex.axis, values.stroke,
         rotate.x, rotate.y, offset, xy.ticks,
         line.width, xlab, ylab, main, sub, cex,
         time.start, time.by, time.reverse, 
         center.line, show.runs, quiet, ...)

      if (pdf) {
        dev.off()
        if (!quiet) .showfile(pdf.fnm, "Line Chart")
      }

    }  # nu > n.cat
    else
      if (!quiet) .ncat("Line Chart", x.name, nu, n.cat)

    }  # is.numeric(data[,i])

  }  # for

  if (ncol(data) > 1) {
    if (!pdf) if (is.null(options()$knitr.in.progress))
      .plotList(plot.i, plot.title)
  }

}
