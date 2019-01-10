Density <-
function(x, data=d, rows=NULL,
         n.cat=getOption("n.cat"), Rmd=NULL,

       bw=NULL, type=c("both", "general", "normal"),
       histogram=TRUE, bin.start=NULL, bin.width=NULL,

       color.nrm="black", color.gen="black",
       fill.nrm=NULL, fill.gen=NULL,

       axis.text.color="gray30", rotate.x=0, rotate.y=0, offset=0.5,

       x.pt=NULL, xlab=NULL, main=NULL, sub=NULL, y.axis=FALSE,
       x.min=NULL, x.max=NULL,
       rug=FALSE, color.rug="black", size.rug=0.5,

       eval.df=NULL, digits.d=NULL, quiet=getOption("quiet"),
       width=4.5, height=4.5, pdf=FALSE,
       fun.call=NULL, ...) {


  type <- match.arg(type)
  if (is.null(fun.call)) fun.call <- match.call()

  fill <- getOption("se.fill")
  panel.fill <- getOption("panel.fill")
  panel.color <- getOption("panel.color")
  lab.cex <- getOption("lab.cex")
  axis.cex <- getOption("axis.cex")

  if (!missing(color.rug) || !missing(size.rug)) rug <- TRUE

  bw.miss <- ifelse (missing(bw), TRUE, FALSE)

  # see if dated parameter values
  .param.old(...)

  clr <- getOption("theme")  # color theme not used except for monochrome

# if (missing(fill))
#   if (.Platform$OS == "windows")
#     fill <- "gray80"
#   else
#     fill <- "gray86"

  if (missing(fill.nrm)) {
      fill.nrm <- rgb(80,150,200, alpha=70, maxColorValue=255)
    if (clr == "gray" ||
       (getOption("theme") == "gray"  &&  getOption("sub.theme") == "black")) {
      fill.nrm <- "transparent"
    }
  }

  if (missing(fill.gen)) {
      fill.gen <- rgb(250,210,230, alpha=70, maxColorValue=255)
    if (clr == "gray" ||
       (getOption("theme") == "gray"  &&  getOption("sub.theme") == "black")) {
      fill.gen <- rgb(.75,.75,.75, .5)
    }
  }

  
  shiny <- ifelse (isNamespaceLoaded("shiny"), TRUE, FALSE) 
  if (is.null(eval.df))  # default values
    eval.df <- ifelse (shiny, FALSE, TRUE)
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
        data <- data.frame(data, stringsAsFactors=TRUE)
      }
    }
  }

  if ((missing(data) && shiny))  # force evaluation (not lazy) if data not specified
    data <- eval(substitute(data), envir=parent.frame())


  if (!is.null(x.name))
    x.in.global <- .in.global(x.name)  # see if in global, includes vars list
  else
    x.in.global <- FALSE
    
  # -----------------------------------------------------------
  # establish if a data frame, if not then identify variable(s)
  # x can be missing entirely, with a data frame passed instead
  # if x a vector, then x.name not in data, but also not in global

  if (!missing(x)) {

    # x not in global env, in df, specify data= forces to data frame
    if (!x.in.global) {
      if (eval.df) {
        if (!mydata.ok) .nodf(df.name)  # check to see if data frame container exists
        .xcheck(x.name, df.name, names(data))  # var in df?, vars lists not checked
      }
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
        if (length(ind) == 1) {  # x is 1 var
          data <- data.frame(data)
          names(data) <- x.name
         }
      }
      else {  # class of data is "list"
        data <- data.frame(data[[ind]])
        names(data) <- x.name
      }
    }

    else { # x is in the global environment (vector or data frame)
      if (is.data.frame(x))  # x a data frame
        data <- x
      else {  # x a vector in style
        .xstatus(x.name, df.name, quiet)
        if (!is.function(x))
          data <- data.frame(x)  # x is 1 var
        else
          data <- data.frame(eval(substitute(data$x)))  # x is 1 var
        names(data) <- x.name
      }
    }

  }


    # ---------------
    # do the analysis

    plot.i <- 0  # keep track of generated graphics
    plot.title  <- character(length=0)
    manage.gr <- .graphman()  # see if graphics are to be managed
    if (manage.gr  &&  !pdf  && !shiny) {
      i.win <- 0
      for (i in 1:ncol(data)) {
        if (is.numeric(data[,i])  &&  !.is.num.cat(data[,i], n.cat))
          i.win <- i.win + 1
      }
      .graphwin(i.win, width, height)
    }
    open.win <- 2

    if (is.null(digits.d)) {
      dig.dec <- .max.dd(data[,1]) + 1
      if (dig.dec == 1) dig.dec <- 2
    }
    else
      dig.dec <- digits.d
    options(digits.d=dig.dec)


  for (i in 1:ncol(data)) {

    nu <- length(unique(na.omit(data[,i])))

    x.name <- names(data)[i]
    options(xname = x.name)

    if (is.numeric(data[,i])) {  # has to be a numeric variable
      # do not do num.cat vars, unless only 1 variable to analyze
      if (ncol(data) == 1  ||  !.is.num.cat(data[,i], n.cat)) {

      if (pdf) {
        pdf.fnm <- paste("Density", "_", x.name, ".pdf", sep="")
        .opendev(pdf.fnm, width, height)
      }
      else {
        pdf.fnm <- NULL
        plot.i <- plot.i + 1
        plot.title[plot.i] <- paste("Density of ", x.name, sep="")
        if (manage.gr && !shiny) {
          open.win <- open.win + 1
          dev.set(which = open.win)
        }
      }

      gl <- .getlabels()
      x.name <- gl$xn; x.lbl <- gl$xl;
      y.name <- gl$yn; y.lbl <- gl$yl
      if (!quiet  &&  ncol(data) > 1) {
        ttlns <- .title2(x.name, y.name, x.lbl, y.lbl, TRUE)
        ttlns <- paste(" ", "\n", ttlns, sep="")
      }
      else
        ttlns <- ""

      # get bandwidth
      if (bw.miss) bw <- .band.width(data[,i], ...)

      stuff <- .dn.main(data[,i], bw, type, histogram, bin.start, bin.width,
            fill, panel.fill, panel.color,
            color.nrm, color.gen, fill.nrm, fill.gen,
            lab.cex, axis.cex, axis.text.color, rotate.x, rotate.y, offset,
            x.pt, xlab, main, sub, y.axis, x.min, x.max,
            rug, color.rug, size.rug, quiet, ...)

      txdst <- ""
      txotl <- ""
      if (!quiet) {
        txdst <- stuff$tx

        txotl <- .bx.stats(data[,i])$txotl
        if (txotl[1] == "") txotl <- "No (Box plot) outliers"

        class(txdst) <- "out"
        class(txotl) <- "out"
      }


      if (ncol(data) > 1) {  # for a variable range, just text output
        class(ttlns) <- "out"  # title only for multiple variables

        output <- list(out_title=ttlns, out_stats=txdst, out_outliers=txotl)
        class(output) <- "out_all"
        print(output)
      }

      if (pdf) {
        dev.off()
        if (!quiet) .showfile(pdf.fnm, "density curve")
      }

    }  # nu > n.cat
    else
      if (!quiet) .ncat("Density curve", x.name, nu, n.cat)

    }  # is.numeric(data[,i])
  }  # for (i in 1:ncol(data)), cycle through all the variables


  if (ncol(data) > 1) {
    if (!pdf) if (is.null(options()$knitr.in.progress)) if (plot.i > 0)
      .plotList(plot.i, plot.title)
  }

  if (!shiny)
    dev.set(which=2)  # reset graphics window for standard R functions


  # now further processing if only a single numerical variable to process
  if (ncol(data) == 1  &&  nu > n.cat) {

    # R Markdown
    txkfl <- ""
    if (!is.null(Rmd)) {
      if (!grepl(".Rmd", Rmd)) Rmd <- paste(Rmd, ".Rmd", sep="")
      txknt <- .dist.Rmd(x.name, df.name, fun.call, digits.d)
      cat(txknt, file=Rmd, sep="\n")
      txkfl <- .showfile2(Rmd, "R Markdown instructions")
    }

    class(txkfl) <- "out"

    output <- list(type="Density",
      out_title=ttlns, out_stats=txdst, out_outliers=txotl, out_file=txkfl,
      bw=stuff$bw, n=stuff$n, n.miss=stuff$n.miss, W=stuff$W,
         pvalue=stuff$pvalue)

    class(output) <- "out_all"

    return(output)

  }

  # dev.set(which=2)  # reset graphics window for standard R functions

}
