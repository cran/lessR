if (getRversion() >= "2.15.1") 
  globalVariables(c("mydata", "mycor"))


.onAttach <-
function(...) {

  packageStartupMessage(
      "--------------------------------------------------------\n",
      "For help, enter, after the >:  Help()\n",
      "To read a text, SPSS or R data file:  > mydata <- Read()\n",
      "--------------------------------------------------------\n\n")

  options(colors="blue")
  options(trans.fill.bar=0.00)
  options(trans.fill.pt=0.66)
  options(col.fill.bar = "#A1B4CCFF")  # .maketrans of lightsteelblue3"
  options(col.fill.pt = "#A1B4CC57")  # .maketrans of "lightsteelblue3"
  options(col.stroke.bar="slategray")
  options(col.stroke.pt="darkblue")
  options(col.bg="ghostwhite")
  options(col.grid="gray90")
  options(col.ghost=FALSE)
  options(col.heat="darkblue")

  options(n.cat=0)

  options(quiet=FALSE)
  options(brief=FALSE)

  options(show.signif.stars=FALSE)
  options(scipen=30)

}


.max.dd <- function(x) {

 n.dec <-function(xn) {
    xc <- format(xn)  # as.character(51.45-48.98) does not work
    nchar(xc)
    ipos <- 0
    for (i in 1:nchar(xc)) if (substr(xc,i,i)==".") ipos <- i
    if (ipos > 0) n.dec <- nchar(xc)-ipos else n.dec <- 0
    return(n.dec)
  }

  max.dd <- 0
  for (i in 1:length(x))
    if (!is.na(x[i])) if (n.dec(x[i]) > max.dd) max.dd <- n.dec(x[i])

  return(max.dd)
}


.getdigits <- function(x, min.digits) {
    digits.d <- .max.dd(x) + 1
    if (digits.d < min.digits) digits.d <- min.digits
    return(digits.d)
}


.dash <- function(ndash, cc) {
  if (missing(cc)) cc <- "-" 
  for (i in 1:(ndash)) cat(cc)
  cat("\n") 
}


.fmt <- function(k, d=getOption("digits.d"), w=0) {
  format(sprintf("%.*f", d, k), width=w, justify="right", scientific=FALSE)
}


.fmti <- function(k, w=0) {
  format(sprintf("%i", k), width=w, justify="right")
}


.fmtc <- function(k, w=0, j="right") {
  format(sprintf("%s", k), width=w, justify=j)
}


.xstatus <- function(var.name, dname, quiet=FALSE) {

  # see if analysis from data is based on a formula
  if (grepl("~", var.name)) is.frml <- TRUE else is.frml <- FALSE

  # see if analysis is from descriptive stats or from data 
  if (var.name == "NULL") from.data <- FALSE else from.data <- TRUE

  # see if the variable exists in the Global Environment
  if (exists(var.name, where=.GlobalEnv)) {
    in.global <- TRUE
    if (!quiet)
      cat("\n>>> Note: ", var.name, "created outside of a data",
          "frame (table)\n")
  }
  else
    in.global <- FALSE

  # see if "variable" is really a function call
  if (grepl("(", var.name, fixed=TRUE))  {
    txtA <- paste("The referenced variable in a lessR function can only be\n",
            "a variable name.\n\n", sep="")
    txtB <- "For example, this does not work:\n  > Histogram(rnorm(50))\n\n"
    txtC <- "Instead do this:\n  > Y <- rnorm(50)\n  > Histogram(Y)"
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        txtA, txtB, txtC, "\n")
  }

  # see if the data frame exists (mydata default), if x from data, not in Global Env
  if (!in.global && from.data) {
    if (!exists(dname, where=.GlobalEnv)) {
      if (dname == "mydata") 
        txtA <- ", the default data frame name, " else txtA <- " "
      txtB1 <- "So either create the data frame with the Read function, or\n"
      txtB2 <- "  specify the actual data frame with the parameter: data\n"
      txtB <- paste(txtB1, txtB2, sep="")
      cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Data frame ", dname, txtA, "does not exist\n\n", txtB, "\n")
    }
  }

  return(list(ifr=is.frml, fd=from.data, ig=in.global))
}


.xcheck <- function(var.name, dname, data) {

# see if variable exists in the data frame
  if (!exists(var.name, where=data)) { 
    if (dname == "mydata") {
      txt1 <- ", the default name \n\n"
      txt2 <- "Either make sure to use the correct variable name, or\n"
      txt3 <- "specify the actual data frame with the parameter: data\n"
      txt <- paste(txt1, txt2, txt3, sep="")
    }
    else 
      txt <- "\n"
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Variable ", var.name, " does not exist either by itself ",
        "(in the user's workspace),\n",
        "or in the data frame with the name of ", dname, txt, "\n",
        "To view the existing variable names enter: > names(", dname, ")\n\n")
  }
}


.getlabels <- function(xlab, ylab, main) {

  # get variable labels if they exist

  x.name <- getOption("xname")
  y.name <- getOption("yname")
  x.lbl <- NULL
  y.lbl <- NULL

  dname <- getOption("dname")  # not set for dependent option on tt
  if (!is.null(dname)) {
    if (exists(dname, where=.GlobalEnv))
      mylabels <- attr(get(dname, pos=.GlobalEnv), which="variable.labels")
    else
      mylabels <- NULL
  }
  else
    mylabels <- NULL

  if (!is.null(mylabels)) {
    x.lbl <- mylabels[which(names(mylabels) == x.name)]
    if (length(x.lbl) == 0) x.lbl <- NULL
    y.lbl <- mylabels[which(names(mylabels) == y.name)]
    if (length(y.lbl) == 0) y.lbl <- NULL
  }

  # axis and legend labels
  if (!missing(xlab)) {
    if (!is.null(xlab)) x.lab <- xlab 
    else if (is.null(x.lbl)) x.lab <- x.name else x.lab <- x.lbl
    if (length(x.lab) == 1) if (nchar(x.lab) > 45)  # power.ttest: len > 1
      x.lab <- paste(substr(x.lab,1,45), "...")
  }
  else x.lab <- NULL

  if (!missing(ylab)) {
    if (!is.null(ylab)) y.lab <- ylab
    else if (is.null(y.lbl)) y.lab <- y.name else y.lab <- y.lbl
    if (nchar(y.lab) > 50)
      y.lab <- paste(substr(y.lab,1,50), "...")
  }
  else y.lab <- NULL

  if (!missing(main)) {
    if (!is.null(main)) main.lab <- main else main.lab <- ""
  }
  else main.lab <- NULL

  return(list(xn=x.name, xl=x.lbl, xb=x.lab, 
              yn=y.name, yl=y.lbl, yb=y.lab, mb=main.lab))
}


.varlist <- function(n.pred, i, var.name, pred.lbl, n.obs, n.keep, lvls=NULL) {

  if (i == 1) txt <- "Response Variable:  "
  else 
    if (n.pred > 1) txt <- paste(pred.lbl, " ", 
      toString(i-1), ": ", sep="")
    else txt <- "Predictor Variable: "
  cat(txt, var.name)

  dname <- getOption("dname")
  if (exists(dname, where=.GlobalEnv))
    mylabels <- attr(get(dname, pos=.GlobalEnv), which="variable.labels")
  else
    mylabels <- NULL

  if (!is.null(mylabels)) {
    lbl <- mylabels[which(names(mylabels) == var.name)]
    if (!is.null(lbl)) cat(", ", as.character(lbl))
  }

  if (!is.null(lvls)) if (i > 1) cat("\n  Levels:", lvls)
  cat("\n")

  if (i == n.pred+1) {
    cat("\n")
    cat("Number of cases (rows) of data: ", n.obs, "\n")
    cat("Number of cases retained for analysis: ", n.keep, "\n")
  }
}


.title <- function(x.name, y.name, x.lbl, y.lbl, isnullby) {

  txt1 <- x.name
  if (!is.null(x.lbl)) txt1 <- paste(txt1, ", ", x.lbl, sep="")

  if (isnullby) txt1 <- paste("---", txt1, "---")
  else {
    txt2 <- paste(y.name, sep="")
    if (!is.null(y.lbl)) txt2 <- paste(txt2, ", ", y.lbl, sep="") 
  }

  cat("\n")
  cat(txt1, "\n")
  if (!isnullby) {
    cat(txt2, "\n")
    ndash <- max(nchar(txt1),nchar(txt2))
    .dash(ndash)
  }
  cat("\n")

}


.showfile <- function(fname, txt) {
  if (getwd() == "/")
    workdir <- "top level (root) of your file system"
  else
    workdir <- getwd()
  cat("\n")
  cat("The", txt, "was written at the current working directory.\n")
  cat("       ", fname, " in:  ", workdir, "\n")
  cat("\n")
}


.graphwin <- function(wnew=1, d.w=NULL, d.h=NULL) {

  dl <- dev.list()
  dl2 <- dl[which(dl==2)]  # device #2
  dl.more <- dl[which(dl>2)]  # devices larger than #2

  # remove all open windows past device 2
  if (length(dl.more) > 0) {
    min.dd <- dl.more[which(dl.more==min(dl.more))]
    max.dd <- dl.more[which(dl.more==max(dl.more))]
    for (i in min.dd:max.dd) dev.off(which=i)
  }

  if (length(dl2) == 0) off.two <- TRUE else off.two <- FALSE

  # if not already present, generate a null window for #2 and then remove
  if (off.two) wnew <- wnew + 1
  for (i in 1:wnew) 
    if (is.null(d.w) && is.null(d.h))
      dev.new()
    else
      dev.new(width=d.w, height=d.h)
  if (off.two) dev.off(which=2)

}


.opendev <- function(pdf.file, pdf.width, pdf.height) {

  if (is.null(pdf.file)) {
    .graphwin(1)
    orig.params <- par(no.readonly=TRUE)
    on.exit(par(orig.params))
  }
  else 
    pdf(file=pdf.file, width=pdf.width, height=pdf.height)

}


.corcolors <- function(R, NItems, main, bm=3, rm=3, diag=NULL,
                       pdf.file, pdf.width, pdf.height) {

    if (!is.null(diag)) {
      for (i in 1:NItems) R[i,i] <- diag
      cat("\nNote: To provide more color separation for off-diagonal\n",
          "      elements, the diagonal elements of the matrix for\n",
          "      computing the heat map are set to 0.\n", sep="")
    }

    max.color <- getOption("col.heat")
    hmcols <- colorRampPalette(c("white",max.color))(256)
    
    .opendev(pdf.file, pdf.width, pdf.height)  # set up graphics

    heatmap(R[1:NItems,1:NItems], Rowv=NA, Colv="Rowv", symm=TRUE,
      col=hmcols, margins=c(bm,rm), main=main)

    if (!is.null(pdf.file)) {  # terminate pdf graphics
      dev.off()
      .showfile(pdf.file, "plot")
    }
}


.maketrans <- function(col.name, trans.level) {
  r.tr <- col2rgb(col.name)[1]
  g.tr <- col2rgb(col.name)[2]
  b.tr <- col2rgb(col.name)[3]

  #trans.level <- (1-trans.level) * 256
  col.trans <- rgb(r.tr, g.tr, b.tr, alpha=trans.level, maxColorValue=256)

  return(col.trans)
}
