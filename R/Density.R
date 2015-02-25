Density <-
function(x, data=mydata, n.cat=getOption("n.cat"), 

         bw="nrd0", type=c("both", "general", "normal"),
         bin.start=NULL, bin.width=NULL,

         col.fill=getOption("col.fill.pt"),
         col.bg=getOption("col.bg"),
         col.grid=getOption("col.grid"),

         col.nrm="black", col.gen="black",
         col.fill.nrm=NULL, col.fill.gen=NULL,

         cex.axis=.85, col.axis="gray30",
         x.pt=NULL, xlab=NULL, main=NULL, y.axis=FALSE, 
         x.min=NULL, x.max=NULL, band=FALSE, 

         quiet=getOption("quiet"),
         pdf.file=NULL, pdf.width=5, pdf.height=5, ...) {

  clr <- getOption("colors")  # color theme not used except for monochrome 

  if (missing(col.fill))
    if (.Platform$OS == "windows")
      col.fill <- "gray80"
    else
      col.fill <- "gray86"

  if (missing(col.bg)) col.bg <- "ghostwhite"

  if (missing(col.fill.nrm))
      col.fill.nrm <- rgb(80,150,200, alpha=70, maxColorValue=255)

  if (missing(col.fill.gen))
      col.fill.gen <- rgb(250,210,230, alpha=70, maxColorValue=255)

  if (clr == "gray" || clr == "gray.black") {
    col.fill.nrm <- "transparent"
    col.fill.gen <- "transparent"
  }


  # get actual variable name before potential call of data$x
  x.name <- deparse(substitute(x)) 
  options(xname = x.name)

  df.name <- deparse(substitute(data))
  options(dname = df.name)

  pdf.nm <- FALSE
  if (!missing(pdf.file)) pdf.nm <- TRUE


# -----------------------------------------------------------
# establish if a data frame, if not then identify variable(s)

if (!missing(x)) {
    if (!exists(x.name, where=.GlobalEnv)) {  # x not in global env, in df
      .nodf(df.name)  # check to see if data frame container exists 
      .xcheck(x.name, df.name, data)  # see if var in df, vars lists not checked
      vars.list <- as.list(seq_along(data))
      names(vars.list) <- names(data)
      x.col <- eval(substitute(x), envir=vars.list)  # col num of each var
      if (class(data) != "list") {
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

  go.pdf <- FALSE
  if (pdf.nm || ncol(data) > 1) go.pdf <- TRUE

  for (i in 1:ncol(data)) {
    cat("\n")

    nu <- length(unique(na.omit(data[,i])))

    x.name <- names(data)[i]
    options(xname = x.name)

    if (is.numeric(data[,i])) {
      if (nu > n.cat) {

      pdf.fnm <- .pdfname("Density", x.name, go.pdf, pdf.nm, pdf.file)
     .opendev(pdf.fnm, pdf.width, pdf.height)

      d.gen <- .dn.main(data[,i], bw, type, bin.start, bin.width, 
            col.fill, col.bg, col.grid, col.nrm, col.gen,
            col.fill.nrm, col.fill.gen, 
            cex.axis, col.axis, 
            x.pt, xlab, main, y.axis, x.min, x.max, band, quiet, ...)

      if (go.pdf) {
        dev.off()
        if (!quiet) .showfile(pdf.fnm, "density curve")
      }

      if (ncol(data) == 1) invisible(d.gen)
    }  # nu > n.cat
    else
      .ncat("Density curve", x.name, nu, n.cat)

    }  # is.numeric(data[,i])
  }  # for

  if (ncol(data)==1  && nu>n.cat) invisible(d.gen)
}
