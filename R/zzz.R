if(getRversion() >= "2.15.1") 
  globalVariables(c("mydata", "mylabels", "mycor", "lm.out", "av.out"))


.onAttach <-
function(...) {

  packageStartupMessage("\nTo get help, enter, after the >,  Help()\n")

  options(colors="blue")
  options(trans.pts=.66)
  options(n.cat=4)

  options(show.signif.stars=FALSE)
  options(scipen=30)

}


.max.dd <- function(x) {

 n.dec <-function(xn) {
    xc <- as.character(xn)
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


.xstatus <- function(var.name, dframe.name) {

  # see if analysis from data is based on a formula
  if (grepl("~", var.name)) is.frml <- TRUE else is.frml <- FALSE

  # see if analysis is from descriptive stats or from data 
  if (var.name == "NULL") from.data <- FALSE else from.data <- TRUE

  # see if the variable exists in the Global Environment
  if (exists(var.name, where=.GlobalEnv)) in.global <- TRUE  else in.global <- FALSE

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
    if (!exists(dframe.name, where=.GlobalEnv)) {
      if (dframe.name == "mydata") 
        txtA <- ", the default data frame name, " else txtA <- " "
      txtB1 <- "So either create the data frame by reading with the rad function, or\n"
      txtB2 <- "  specify the actual data frame with the parameter: dframe\n"
      txtB <- paste(txtB1, txtB2, sep="")
      cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Data frame ", dframe.name, txtA, "does not exist\n\n", txtB, "\n")
    }
  }

  return(list(ifr=is.frml, fd=from.data, ig=in.global))
}


.xcheck <- function(var.name, dframe.name, dframe) {

# see if variable exists in the data frame
  if (!exists(var.name, where=dframe)) { 
    if (dframe.name == "mydata") {
      txt1 <- ", the default name \n\n"
      txt2 <- "So either make sure you are using the correct variable name, or\n"
      txt3 <- "  specify the actual data frame with the parameter: dframe\n"
      txt <- paste(txt1, txt2, txt3, sep="")
    }
    else 
      txt <- " "
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Variable ", var.name, " does not exist either by itself ",
        "or in the data frame ", dframe.name, txt, "\n\n")
  }
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

  if (is.null(pdf.file)) 
    .graphwin()
  else 
    pdf(file=pdf.file, width=pdf.width, height=pdf.height)

}


.corcolors <- function(R, NItems, colors, main,
                       bm=3, rm=3, diag=NULL,
                       pdf.file, pdf.width, pdf.height) {

    if (!is.null(diag)) {
      for (i in 1:NItems) R[i,i] <- diag
      cat("\nNote: To provide more color separation for off-diagonal\n",
          "      elements, the diagonal elements of the matrix for\n",
          "      computing the heat map are set to 0.\n", sep="")
    }

    if (colors == "blue") max.color <- "darkblue"
    else if (colors == "gray") max.color <- "gray5"
    else if (colors == "rose") max.color <- "rosybrown4"
    else if (colors == "green") max.color <- "darkgreen"
    else if (colors == "gold") max.color <- "goldenrod4"
    else if (colors == "red") max.color <- "darkred"

    hmcols <- colorRampPalette(c("white",max.color))(256)
    
    .opendev(pdf.file, pdf.width, pdf.height)  # set up graphics

    heatmap(R[1:NItems,1:NItems], Rowv=NA, Colv="Rowv", symm=TRUE,
      col=hmcols, margins=c(bm,rm), main=main)

    if (!is.null(pdf.file)) {  # terminate pdf graphics
      dev.off()
      .showfile(pdf.file, "plot")
    }
}


.getlabels <- function(xlab, ylab, main) {

  # get variable labels if they exist
  x.name <- getOption("xname")
  y.name <- getOption("yname")
  x.lbl <- NULL
  y.lbl <- NULL
  if (exists("mylabels", where=.GlobalEnv)) {
    x.lbl <- as.character(mylabels[which(row.names(mylabels)==x.name), "label"])
    if (length(x.lbl) == 0) x.lbl <- NULL
    y.lbl <- as.character(mylabels[which(row.names(mylabels)==y.name), "label"])
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


.varlist <- function(n.pred, i, var.name, lvls=NULL) {
  
  if (i == 1) txt <- "Response Variable:  "
  else 
    if (n.pred > 1) txt <- paste("\nPredictor Variable ", 
      toString(i-1), ": ", sep="")
    else txt <- "\nPredictor Variable: "
  cat(txt, var.name)

  if (exists("mylabels")) {  # use variable label if it exists
    lbl <- mylabels[which(row.names(mylabels)==var.name), "label"]
    if (!is.null(lbl)) cat(", ", as.character(lbl))
    if (!is.null(lvls)) if (i > 1) cat("\n  Levels:", lvls)
  }
  cat("\n")
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


.maketrans <- function(col.name, trans.pts) {
  r.tr <- col2rgb(col.name)[1]
  g.tr <- col2rgb(col.name)[2]
  b.tr <- col2rgb(col.name)[3]

  mc <- 256
  trans.pts <- (1-trans.pts) * mc

  col.trans <- rgb(r.tr, g.tr, b.tr, alpha=trans.pts, maxColorValue=mc)
  return(col.trans)
}


.clr <- function(theme, trans.pts=NULL) {

  palette <- character(length=9)

  mc <- 256
  if (is.null(trans.pts)) trans.pts <- getOption("trans.pts")

  trans <- trans.pts  # before transformation

  trans.pts <- (1-trans.pts) * mc

  if (theme == "blue") {
    palette[1] <- "lightsteelblue"
    palette[2] <- "lightsteelblue4"
    palette[3] <- "gray90"
    palette[4] <- "ghostwhite"
    palette[5] <-  "darkblue"
    palette[6] <- .maketrans("darkblue", trans) 
    palette[7] <- "steelblue"
    palette[8] <- "plum"
    palette[9] <- "lightsteelblue"
  }
  if (theme == "gray") {
    palette[1] <- "gray30"
    palette[2] <- "white"
    palette[3] <- "white"
    palette[4] <- "gray90"
    palette[5] <- "gray15"
    palette[6] <- .maketrans("gray15", trans)
    palette[7] <- "gray15"
    palette[8] <- "transparent"
    palette[9] <- "gray60"
  }
  if (theme == "rose") {
    palette[1] <- rgb(245,213,210, maxColorValue=mc)
    palette[2] <- "mistyrose4"
    palette[3] <- "snow2"
    palette[4] <- "snow1"
    palette[5] <- "coral3"
    palette[6] <- .maketrans("coral3", trans)
    palette[7] <- "lightsteelblue"
    palette[8] <- "transparent"
    palette[9] <- "mistyrose1"
  }
  if (theme == "gold") {
    palette[1] <- "goldenrod2"
    palette[2] <- "goldenrod4"
    palette[3] <- rgb(220,222,200, maxColorValue=mc)
    palette[4] <- rgb(255,250,245, maxColorValue=mc)
    palette[5] <- "goldenrod4"
    palette[6] <- .maketrans("goldenrod4", trans)
    palette[7] <- "lightsteelblue"
    palette[8] <- "transparent"
    palette[9] <- "moccasin"
  }
  if (theme == "green") {
    palette[1] <- "darkseagreen3"
    palette[2] <- "darkseagreen4"
    palette[3] <- "darkseagreen1"
    palette[4] <- rgb(246,255,246, maxColorValue=mc)
    palette[5] <- "darkgreen"
    palette[6] <- .maketrans("darkgreen", trans)
    palette[7] <- "lightsteelblue"
    palette[8] <- "transparent"
    palette[9] <- "darkseagreen3"
  }
  if (theme == "red") {
    palette[1] <- "firebrick2"
    palette[2] <- "firebrick4"
    palette[3] <- "lightpink"
    palette[4] <- rgb(255,251,251, maxColorValue=mc)
    palette[5] <- "darkred"
    palette[6] <- .maketrans("darkred", trans)
    palette[7] <- "lightgoldenrod2"
    palette[8] <- "transparent"
    palette[9] <- "lightpink"
  }

  return(palette)

}
