Density <-
function(x, data=mydata, n.cat=getOption("n.cat"), Rmd=NULL, 

       bw="nrd0", type=c("both", "general", "normal"),
       histogram=TRUE, bin.start=NULL, bin.width=NULL,

       fill=getOption("pt.fill"),
       bg.fill=getOption("bg.fill"),
       bg.stroke=getOption("bg.stroke"),

       nrm.color="black", gen.color="black",
       fill.nrm=NULL, fill.gen=NULL,

       cex.axis=0.75, values.stroke="gray30",

       rotate.x=0, rotate.y=0, offset=0.5,

       x.pt=NULL, xlab=NULL, main=NULL, sub=NULL, y.axis=FALSE, 
       x.min=NULL, x.max=NULL, band=FALSE, 

       digits.d=NULL, quiet=getOption("quiet"),
       width=4.5, height=4.5, pdf=FALSE,
       fun.call=NULL, ...) {


  if (is.null(fun.call)) fun.call <- match.call()

  for (i in 1:length(fill))
    if (fill[i] == "off") fill[i] <- "transparent"
  if (bg.fill == "off") bg.fill <- "transparent"
  if (bg.stroke == "off") bg.stroke <- "transparent"

  dots <- list(...)  # check for deprecated parameters
  if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      old.nm <- c("col.fill", "col.bg", "col.grid", "col.box", "col.nrm",
                  "col.gen", "col.fill.nrm", "col.fill.gen", "col.axis")
      if (names(dots)[i] %in% old.nm) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "options that began with the abbreviation  col  replaced with  ",
          "new options, see ?dn \n\n")
      }
      if (grepl("color.", names(dots)[i], fixed=TRUE)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "color options dropped the  color. prefix\n",
          "eg., fill, instead of color.fill.\n\n")
      }
      if (names(dots)[i] == "knitr.file") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "knitr.file  no longer used\n",
          "Instead use  Rmd  for R Markdown file\n\n")
      }
      if (names(dots)[i] == "pdf.file") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "pdf.file  changed to  pdf, either TRUE or FALSE\n\n")
      }
    }
  }

  clr <- getOption("theme")  # color theme not used except for monochrome 

  if (missing(fill))
    if (.Platform$OS == "windows")
      fill <- "gray80"
    else
      fill <- "gray86"

  if (missing(fill.nrm))
      fill.nrm <- rgb(80,150,200, alpha=70, maxColorValue=255)

  if (missing(fill.gen))
      fill.gen <- rgb(250,210,230, alpha=70, maxColorValue=255)

  if (clr == "gray" ||
     (getOption("theme") == "gray"  &&  getOption("sub.theme") == "black")) {
    fill.nrm <- "transparent"
    fill.gen <- rgb(.75,.75,.75, .5)
  }

  #orig.params <- par(no.readonly=TRUE)
  #on.exit(par(orig.params))
  #par(bg=getOption("device.fill"))

  # get actual variable name before potential call of data$x
  x.name <- deparse(substitute(x)) 
  options(xname = x.name)

  df.name <- deparse(substitute(data))
  options(dname = df.name)


# -----------------------------------------------------------
# establish if a data frame, if not then identify variable(s)

  if (!missing(x)) {

    if (!exists(x.name, where=.GlobalEnv)) {  # x not in style env, in df
      .nodf(df.name)  # check to see if data frame container exists 
      .xcheck(x.name, df.name, data)  # var in df?, vars lists not checked
      all.vars <- as.list(seq_along(data))  # even if only a single var
      names(all.vars) <- names(data)  # all data in data frame
      x.col <- eval(substitute(x), envir=all.vars)  # col num selected vars
      if (!("list" %in% class(data))) {
        data <- data[, x.col]
        if (length(x.col) == 1) {  # x is 1 var
          data <- data.frame(data)
          names(data) <- x.name
         }
      }
      else {  # class of data is "list"
        data <- data.frame(data[[x.col]])
        names(data) <- x.name
      }
    }

    else { # x is in the global environment (vector or data frame)
      if (is.data.frame(x))  # x a data frame
        data <- x
      else {  # x a vector in style
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
    if (manage.gr  &&  !pdf) {
      i.win <- 0
      for (i in 1:ncol(data)) {
        if (is.numeric(data[,i])  &&  !.is.num.cat(data[,i], n.cat)) 
          i.win <- i.win + 1
      }
      .graphwin(i.win, width, height)
    }
    open.win <- 2

    if (is.null(digits.d)) {
      dig.dec <- .max.dd(data[,i]) + 1
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
        if (manage.gr) {
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
 
      stuff <- .dn.main(data[,i], bw, type, histogram, bin.start, bin.width, 
            fill, bg.fill, bg.stroke,
            nrm.color, gen.color, fill.nrm, fill.gen, 
            cex.axis, values.stroke, rotate.x, rotate.y, offset, 
            x.pt, xlab, main, sub, y.axis, x.min, x.max, band, quiet, ...)

      txdst <- ""
      txotl <- ""
      if (!quiet) {
        txdst <- stuff$tx

        txotl <- .outliers(data[,i])
        if (txotl[1] == "") txotl <- "No (Box plot) outliers"

        class(txdst) <- "out_piece"
        class(txotl) <- "out_piece"
      }

      
      if (ncol(data) > 1) {  # for a variable range, just text output
        class(ttlns) <- "out_piece"  # title only for multiple variables
        
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

    }  # is.nmeric(data[,i])
  }  # for (i in 1:ncol(data)), cycle through all the variables

  if (ncol(data) > 1) {
    if (!pdf) if (is.null(options()$knitr.in.progress))
      .plotList(plot.i, plot.title)
  }


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

    class(txkfl) <- "out_piece"

    output <- list(type="Density",
      out_title=ttlns, out_stats=txdst, out_outliers=txotl, out_file=txkfl,
      bw=stuff$bw, n=stuff$n, n.miss=stuff$n.miss, W=stuff$W,
         pvalue=stuff$pvalue)

    class(output) <- "out_all"

    return(output)

  }

  # dev.set(which=2)  # reset graphics window for standard R functions

}
