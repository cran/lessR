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


.fmtc <- function(k, w=0) {
  format(sprintf("%s", k), width=w, justify="right")
}


.xstatus <- function(var.name, dframe.name) {

  # see if analysis from data is based on a formula
  if (grepl("~", var.name)) is.frml <- TRUE else is.frml <- FALSE

  # see if analysis is from descriptive stats or from data 
  if (var.name == "NULL") from.data <- FALSE else from.data <- TRUE

  # see if the variable exists in the Global Environment
  if (exists(var.name, where=1)) in.global <- TRUE  else in.global <- FALSE

  # see if the data frame exists (mydata default), if x from data, not in Global Env
  if (!in.global && from.data) {
    if (!exists(dframe.name)) {
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


.graphwin <- function(wnew=1) {

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
  for (i in 1:wnew) dev.new()
  if (off.two) dev.off(which=2)

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
  }
  else x.lab <- NULL
  if (!missing(ylab)) {
    if (!is.null(ylab)) y.lab <- ylab
    else if (is.null(y.lbl)) y.lab <- y.name else y.lab <- y.lbl
  }
  else y.lab <- NULL
  if (!missing(main)) {
    if (!is.null(main)) main.lab <- main else main.lab <- ""
  }
  else main.lab <- NULL

  return(list(xn=x.name, xl=x.lbl, xb=x.lab, 
              yn=y.name, yl=y.lbl, yb=y.lab, mb=main.lab))
}


.showfile <- function(fname, txt) {
  if (getwd() == "/")
    workdir <- "top level (root) of your fname system"
  else
    workdir <- getwd()
  cat("\n")
  cat("fname of", txt, "written at current working directory.\n")
  cat("       ", fname, "at:  ", workdir, "\n")
  cat("\n")
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
    txt2 <- paste("  by\n", y.name, sep="")
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


.clr <- function(theme) {

  palette <- character(length=9)
  trans <- getOption("trans")

  mc <- 256

  if (is.null(trans)) trans <- 0
  if (trans != "gray") trans <- 1 - trans
  trans <- trans * mc

  if (theme == "blue") {
    palette[1] <- "lightsteelblue"
    palette[2] <- "lightsteelblue4"
    palette[3] <- "gray90"
    palette[4] <- "ghostwhite"
    palette[5] <-  "darkblue"
    palette[6] <- rgb(0,0,139, alpha=trans, maxColorValue=mc)  # darkblue 
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
    palette[6] <- rgb(38,38,38, alpha=trans, maxColorValue=mc)  # gray15
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
    palette[6] <- rgb(205,91,69, alpha=trans, maxColorValue=mc)  # coral3
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
    palette[6] <- rgb(139,105,20, alpha=trans, maxColorValue=mc)  # goldenrod4
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
    palette[6] <- rgb(0,100,0, alpha=trans, maxColorValue=mc)  # darkgreen
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
    palette[6] <- rgb(139,0,0, alpha=trans, maxColorValue=mc)  # darkred
    palette[7] <- "lightgoldenrod2"
    palette[8] <- "transparent"
    palette[9] <- "lightpink"
  }

  return(palette)

}


