if (getRversion() >= "2.15.1") 
  globalVariables(c("mydata", "mycor"))


.onAttach <-
function(...) {

  packageStartupMessage("\n",
      "lessR 3.3.4     RStudio, knitr compatible      www.lessRstats.com\n",
      "------------------------------------------------------------------\n",
      "To get started, and for help in general, enter:  Help()\n",
      "To read a text, Excel, SPSS, SAS or R data file:  mydata <- Read()\n",
      "------------------------------------------------------------------\n",
      "Automatically generate an R Markdown file for knitr, enter:  ?reg\n",
      "------------------------------------------------------------------\n\n")

  options(colors="dodgerblue")
  options(trans.fill.bar=0.25)
  options(trans.fill.pt=0.66)
  options(col.fill.bar = "#1874CCBF")  # .maketrans of dodgerblue3"
  options(col.fill.pt = "#1874CCBF")  # .maketrans of "dodgerblue3"
  options(col.stroke.bar="steelblue4")
  options(col.stroke.pt="steelblue4")
  options(col.bg="#EDEFF1")
  options(col.grid="snow3")
  options(col.ghost=FALSE)
  options(col.heat="dodgerblue4")

  options(n.cat=0)
  options(quiet=FALSE)
  options(brief=FALSE)

  options(explain=TRUE)
  options(interpret=TRUE)
  options(results=TRUE)
  options(document=TRUE)
  options(code=TRUE)

  options(show.signif.stars=FALSE)
  options(scipen=30)

}


.max.dd <- function(x) {

 n.dec <-function(xn) {
    xc <- format(xn)  # as.character(51.45-48.98) does not work
    nchar(xc)
    ipos <- 0
    for (i in 1:nchar(xc)) if (substr(xc,i,i)==".") ipos <- i
    n.dec <- ifelse (ipos > 0, nchar(xc)-ipos, 0)
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


.dash <- function(ndash, cc, newline=TRUE) {
  if (missing(cc)) cc <- "-" 
  for (i in 1:(ndash)) cat(cc)
  if (newline) cat("\n") 
}

.dash2 <- function(ndash, cc="-") {
  tx <- ""
  if (!is.null(cc)) for (i in 1:(ndash)) tx <- paste(tx, cc, sep="")
  return(tx)
}


.plotList <- function(plot.i, plot.title) {
  mxttl <- 0
  for (i in 1:plot.i)
    if (nchar(plot.title[i]) > mxttl) mxttl <- nchar(plot.title[i])
  mxttl <- mxttl + 8
  cat("\n")
  .dash(mxttl, newline=FALSE)
  for (i in 1:plot.i) {
    cat("\n", "Plot ", i,": ", plot.title[i], sep="")
  }
  cat("\n")
  .dash(mxttl, newline=FALSE)
  cat("\n\n")

}


.plotList2 <- function(plot.i, plot.title) {
  tx <- character(length = 0)

  mxttl <- 0
  for (i in 1:plot.i)
    if (nchar(plot.title[i]) > mxttl) mxttl <- nchar(plot.title[i])
  mxttl <- mxttl + 8

  tx[length(tx)+1] <- .dash2(mxttl)
  for (i in 1:plot.i)
    tx[length(tx)+1] <- paste("Plot ", i,": ", plot.title[i], sep="")
  tx[length(tx)+1] <- .dash2(mxttl)

  return(tx)
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


.fmtNS <- function(k) {
  format(k, scientific=FALSE )
}


.xstatus <- function(var.name, dname, quiet=FALSE) {

  # see if analysis from data is based on a formula
  is.frml <- ifelse (grepl("~", var.name), TRUE, FALSE)

  # see if analysis is from descriptive stats or from data 
  from.data <- ifelse (var.name == "NULL", FALSE, TRUE)

  # see if the variable exists in the Global Environment
  in.global <- FALSE
  if (nchar(var.name)>0) if (exists(var.name, where=.GlobalEnv)) {
    if (!is.function(var.name)) { # a global "var" could be a function call 
      in.global <- TRUE
      if (!quiet)
        cat(">>> Note: ", var.name, "exists in the workspace, outside of",
            "a data frame (table)\n")
    }
  }

  # see if "variable" is really an expression
  if (grepl("(", var.name, fixed=TRUE) ||  grepl("[", var.name, fixed=TRUE))  {
    txtA <- paste("A referenced variable in a lessR function can only be\n",
            "a variable name.\n\n", sep="")
    txtB <- "For example, this does not work:\n  > Histogram(rnorm(50))\n\n"
    txtC <- "Instead do this:\n  > Y <- rnorm(50)\n  > Histogram(Y)"
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        txtA, txtB, txtC, "\n")
  }

  if (!in.global && from.data) .nodf(dname)

  return(list(ifr=is.frml, fd=from.data, ig=in.global))
}


.nodf <- function(dname) {

  # see if the data frame exists (mydata default), if x from data, not in Global Env
  if (!exists(dname, where=.GlobalEnv)) {
    if (dname == "mydata") 
      txtA <- ", the default data table name, " else txtA <- " "
    txtB1 <- "So either create the data table with the Read function, or\n"
    txtB2 <- "  specify the actual data table with the parameter: data\n"
    txtB <- paste(txtB1, txtB2, sep="")
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Data frame (table) ", dname, txtA, "does not exist\n\n", txtB, "\n")
  }

}


.xcheck <- function(var.name, dname, data) {

  if ( (!grepl(":", var.name) && !grepl(",", var.name)) ) { # x not var list

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
}


# see if cor matrix exists as stand-alone or embedded in list structure
.cor.exists <- function(cor.nm) {

  if (!grepl("$cors", cor.nm, fixed=TRUE))  # no $cors in name
    is.there <- exists(cor.nm, where=.GlobalEnv)

  else {
    nm <- sub("$cors", "", cor.nm, fixed=TRUE)  # remove $cors from name
    if (!exists(nm, where=.GlobalEnv))  # root list exists?
      is.there <- FALSE
    else
      is.there  <- exists("cors", where=eval(parse(text=nm)))  #  cors inside?
  }
  if (!is.there) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "No correlation matrix entered.\n\n",
      "No object called ", cor.nm, " exists.\n\n",
      "Either enter the correct name, or calculate with: Correlation\n",
      "Or read the correlation matrix with: corRead\n\n", sep="")
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

  if (i == 1)
    txt <- "Response Variable:  "
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


.varlist2 <- function(n.pred, i, var.name, pred.lbl, n.obs, lvls=NULL) {
  tx <- character(length = 0)

  if (i == 1)
    txt <- "Response Variable:  "
  else
    if (n.pred > 1) txt <- paste(pred.lbl, " ", 
      toString(i-1), ": ", sep="")
    else txt <- "Predictor Variable: "
  tx[length(tx)+1] <- paste(txt, var.name, sep="")

  dname <- getOption("dname")
  if (exists(dname, where=.GlobalEnv))
    mylabels <- attr(get(dname, pos=.GlobalEnv), which="variable.labels")
  else
    mylabels <- NULL
  if (exists(dname, where=.GlobalEnv))
    myunits <- attr(get(dname, pos=.GlobalEnv), which="variable.units")
  else
    myunits <- NULL

  if (!is.null(mylabels)) {
    lbl <- mylabels[which(names(mylabels) == var.name)]
    unt <- myunits[which(names(myunits) == var.name)] 
    if (!is.null(unt)) if (nzchar(unt))
      lbl <- paste(lbl, " (", unt, ")", sep="")
    else
      lbl <- lbl 
    if (!is.null(lbl))
      tx[length(tx)] <- paste(tx[length(tx)], ", ", as.character(lbl), sep="")
  }

  if (!is.null(lvls)) if (i > 1) tx[length(tx)+1] <- paste("\n  Levels:", lvls)

  return(tx)
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
    cat("  - by levels of - \n")
    cat(txt2, "\n")
    ndash <- max(nchar(txt1),nchar(txt2))
    .dash(ndash)
  }
  cat("\n")

}

.title2 <- function(x.name, y.name, x.lbl, y.lbl, isnullby, new.ln=TRUE) {

  txt1 <- x.name
  if (!is.null(x.lbl)) txt1 <- paste(txt1, ", ", x.lbl, sep="")

  if (isnullby) {
    txt1 <- paste("---", txt1, "---")
    if (new.ln) txt1 <- paste(txt1, "\n", sep="")
  }
  else {
    txt2 <- paste(y.name, sep="")
    if (!is.null(y.lbl)) txt2 <- paste(txt2, ", ", y.lbl, sep="") 
  }

  tx <- character(length = 0)

  tx[length(tx)+1] <- txt1
  if (!isnullby) {
    if (is.null(y.lbl))
      tx[length(tx)+1] <- "  - by levels of - "
    else
      tx[length(tx)+1] <- "\n  - by levels of - \n"
    tx[length(tx)+1] <- txt2
    if (is.null(y.lbl))
      tx[length(tx)+1] <- .dash2(max(nchar(txt1),nchar(txt2)))
  }

  return(tx)

}


.showfile <- function(fname, txt) {
  if (getwd() == "/")
    workdir <- "top level (root) of your file system"
  else
    workdir <- getwd()
  cat("\n")
  cat("The", txt, "written at the current working directory.\n")
  cat("       ", fname, " in:  ", workdir, "\n")
  cat("\n")
}


.showfile2 <- function(fname, txt) {
  if (getwd() == "/")
    workdir <- "top level (root) of your file system"
  else
    workdir <- getwd()

  tx <- character(length = 0)

  tx[length(tx)+1] <- paste("The", txt, "written at the current working directory.")
  tx[length(tx)+1] <- paste("       ", fname, " in:  ", workdir)

  return(tx)

}


.pdfname <- function(analysis, x.name, go.pdf, pdf.nm, pdf.file) {
  if (go.pdf) {
    if (pdf.nm)
      if (!grepl(".pdf", pdf.file))
        pdf.fnm <- paste(pdf.file, ".pdf", sep="")
      else
        pdf.fnm <- pdf.file
    else
      pdf.fnm <- paste(analysis, "_", x.name, ".pdf", sep="")
  }
  else
    pdf.fnm <- NULL

  return(pdf.fnm)
}


# see if manage graphics or just sequentially plot
.graphman <- function() {

  in.RStudio <- ifelse (options("device") != "RStudioGD", FALSE, TRUE)

  in.knitr <- ifelse (is.null(options()$knitr.in.progress), FALSE, TRUE)

  manage.gr <- ifelse (!in.RStudio  &&  !in.knitr, TRUE, FALSE)

  return(manage.gr)
}


# manages the graphics system (not in RStudio or knitr)
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

  off.two <- ifelse (length(dl2) == 0, TRUE, FALSE)

  # open graphics windows
  # if not already present, generate a null window for #2 and then remove
  if (off.two) wnew <- wnew + 1
  for (i in 1:wnew) 
    if (is.null(d.w) && is.null(d.h))
      dev.new()
    else
      dev.new(width=d.w, height=d.h)
  if (off.two) dev.off(which=2)

}


.opendev <- function(pdf.fnm, pdf.width, pdf.height) {

  if (is.null(pdf.fnm)) {
    if (options("device") != "RStudioGD"  &&  is.null(options()$knitr.in.progress)) {
      .graphwin(1)
      orig.params <- par(no.readonly=TRUE)
      on.exit(par(orig.params))
    }
  }
  else 
    pdf(file=pdf.fnm, width=pdf.width, height=pdf.height)

}


.ncat <- function(analysis, x.name, nu, n.cat) {

      cat("\n>>>", x.name,  "is numeric,",
          "but only has", nu, "<= n.cat =", n.cat, "levels,",
          "so treat as categorical.\n\n",
          "   If categorical, can make this variable a",
          "factor with R function: factor\n\n",
          "   If numerical, to obtain the", tolower(analysis),
          "decrease  n.cat ",
          "to specify\n",
          "   a lower number of unique values.\n")

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


.to256 <- function(trans.level)
   trn <- (1-getOption(trans.level))*256


# change class call to class character
.fun.call.deparse <- function(fun.call) {

  fc.d <- deparse(fun.call)
  if (length(fc.d) > 1) {  # multiple lines
    fc <- fc.d[1]
    for (i in 2:length(fc.d)) fc <- paste(fc, fc.d[i], sep="")  
  }
  else
    fc <- fc.d

  fc <- sub("     ", " ", fc, fixed=TRUE)
  fc <- sub("    ", " ", fc, fixed=TRUE)
  fc <- sub("  ", " ", fc, fixed=TRUE)

  return(fc)

}


# get the value for a specified function argument
.get.arg <- function(argm, fc) {

  loc <- regexec(argm, fc)
  strt1 <- loc[[1]]  # beginning of argument
  if (strt1 > 0) {
    j <- strt1
    while(substr(fc, start=j, stop=j) != "\"") j <- j + 1 
    strt <- j
    j <- j + 1  # first " after ,
    while(substr(fc, start=j, stop=j) != "\"") j <- j + 1 
    stp <- j  # second " after ,
    value <- substr(fc, start=strt, stop=stp)
  }
  else
    value <- ""

  return(value)
}


# remove argument and character value from a function call
.rm.arg <-  function(argm, fc) {

  loc <- regexec(argm, fc)[[1]]  # beginning of argument

  if (loc > 0) {

    first.arg <- ifelse (substr(fc, loc-1, loc-1) == "(", TRUE, FALSE)

    j <- loc
    if (!first.arg)  # is not first argument, start at preceding comma
      while(substr(fc, start=j, stop=j) != ",") if (j > 0) j <- j - 1 
    strt <- j  #  closing parentheses or comma before argument

    while(substr(fc, start=j, stop=j) != "\"") if (j < 1000) j <- j + 1 
    j <- j + 1  # first " after ,
    while(substr(fc, start=j, stop=j) != "\"") if (j < 1000) j <- j + 1 
    stp <- j  # second " after ,

    if (first.arg) stp <- stp + 2  # remove trailing comma and space

    remv <- substr(fc, start=strt, stop=stp)
    fc.new <- sub(remv, "", fc, fixed=TRUE)

  }

  return(fc.new)
}


# remove argument and Non-String value from a function call
.rm.arg.ns <-  function(argm, fc) {

  loc <- regexec(argm, fc)[[1]]  # beginning of argument

  if (loc > 0) {

    first.arg <- ifelse (substr(fc, loc-1, loc-1) == "(", TRUE, FALSE)

    j <- loc
    if (!first.arg)  # is not first argument, start at preceding comma
      while(substr(fc, start=j, stop=j) != ",") if (j > 0) j <- j - 1 
    strt <- j  #  closing parentheses or comma before argument

    dlm <- c(",", ")")

    j <- j + 1
    while(!(substr(fc, start=j, stop=j) %in% dlm))
      if (j < 1000) j <- j + 1 

    stp <- j  # got a "," or a ")" 
    stp <- stp - 1  # retain the "," or ")"

    if (first.arg) stp <- stp + 2  # remove trailing comma and space

    remv <- substr(fc, start=strt, stop=stp)
    fc.new <- sub(remv, "", fc, fixed=TRUE)

  return(fc.new)
  }

}




.outliers <- function(x) {

  outliers <- boxplot.stats(x)$out
  if (length(outliers>0) && unique(na.omit(x)>3)) {
    cat("\nNumber of outliers:", length(outliers), "\n")

    lo.whisker <- boxplot.stats(x)$stats[1]
    lo.out <- outliers[outliers < lo.whisker]
    lo.out <- sort(lo.out, decreasing=FALSE)
    lo.len <- length(lo.out)
    cat("Small: ")
    if (lo.len > 0) {
      if (lo.len <= 25)
        for (i in 1:lo.len) cat(format(lo.out[i], scientific=FALSE), " ")
      else {
        for (i in 1:16) cat(format(lo.out[i], scientific=FALSE), " ")
        cat ("...  ")
        for (i in (lo.len-5):lo.len) cat(format(lo.out[i], scientific=FALSE), " ")
      }
    }
    else
      cat("none")
    cat("\n")

    hi.whisker <- boxplot.stats(x)$stats[5]
    hi.out <- outliers[outliers > hi.whisker]
    hi.out <- sort(hi.out, decreasing=FALSE)
    hi.len <- length(hi.out)
    cat("Large: ")
    if (hi.len > 0) {
      if (hi.len <= 25)
        for (i in 1:hi.len) cat(format(hi.out[i], scientific=FALSE), " ")
      else {
        for (i in 1:16) cat(format(hi.out[i], scientific=FALSE), " ")
        cat ("...  ")
        for (i in (hi.len-5):hi.len) cat(format(hi.out[i], scientific=FALSE), " ")
      }
    }
    else
      cat(" none\n")

  }

  cat("\n")
}


.outliers2 <- function(x) {

  tx <- character(length = 0)

  outliers <- boxplot.stats(x)$out

  if (length(outliers>0) && length(unique(na.omit(x)>3))) {
    tx[length(tx)+1] <- paste("Number of outliers:", length(outliers))

    lo.whisker <- boxplot.stats(x)$stats[1]
    lo.out <- outliers[outliers < lo.whisker]
    lo.out <- sort(lo.out, decreasing=FALSE)
    lo.len <- length(lo.out)
    tx[length(tx)+1] <- "Small: "
    if (lo.len > 0) {
      if (lo.len <= 25) {
        for (i in 1:lo.len)
          tx[length(tx)] <- paste(tx[length(tx)], .fmtNS(lo.out[i]))
      }
      else {
        for (i in 1:16) 
          tx[length(tx)] <- paste(tx[length(tx)], .fmtNS(lo.out[i]))
        tx[length(tx)] <- "...  "
        for (i in (lo.len-5):lo.len)
          tx[length(tx)] <- paste(tx[length(tx)], .fmtNS(lo.out[i]))
      }
    }
    else
      tx[length(tx)] <- paste(tx[length(tx)], "none")

    hi.whisker <- boxplot.stats(x)$stats[5]
    hi.out <- outliers[outliers > hi.whisker]
    hi.out <- sort(hi.out, decreasing=FALSE)
    hi.len <- length(hi.out)
    tx[length(tx)+1] <- "Large:"
    if (hi.len > 0) {
      if (hi.len <= 25) {
        for (i in 1:hi.len) 
          tx[length(tx)] <- paste(tx[length(tx)], .fmtNS(hi.out[i]))
      }
      else {
        for (i in 1:16)
          tx[length(tx)] <- paste(tx[length(tx)], .fmtNS(hi.out[i]))
        tx[length(tx)] <- paste(tx[length(tx)], "...  ")
        for (i in (hi.len-5):hi.len)
          tx[length(tx)] <- paste(tx[length(tx)], .fmtNS(hi.out[i]))
      }
    }
    else
      tx[length(tx)] <- paste(tx[length(tx)], "none")

  }

  else
    tx <- ""

  return(tx)
}


.prntbl <- function(x, digits.d=2, cut=0, cc="-", cors=FALSE,
                    brk=NULL, bnd=NULL) {
  tx <- character(length = 0)

  max.ch <- ifelse (cors, 3, 0)  # max char per column, 0 is not applicable

  # width of column 1
  max.c1 <- 0
  for (i in 1:nrow(x)) {
    c1 <- nchar(rownames(x)[i])
    if (c1 > max.c1) max.c1 <- c1
  }
  max.c1 <- max.c1 + 2

  # widths of variable names
  colnm.w <- integer(length=ncol(x))
  for (i in 1:ncol(x)) 
    colnm.w[i] <- nchar(colnames(x)[i])

  # width of columns
  max.ln <- integer(length=ncol(x))
  for (j in 1:ncol(x)) {
    if (is.numeric(x[,j])) {
      c.val <- 0
      for (i in 1:nrow(x)) {
        i.val <- nchar(formatC(x[i,j], digits=digits.d, format="f"))
        if (i.val > c.val) c.val <- i.val
      }
    }
    else
      c.val <- 4
    if (!cors)
      max.ln[j] <- max(colnm.w[j], c.val) + 1
    else {
      max.ln[j] <- max(colnm.w[j], 4)
      if (max.ch > 0) max.ln[j] <- max.ch
      if (max.ln[j] > 4) max.ln[j] <- max.ln[j] + 1
    }
    if (max.ln[j] < 4) max.ln[j] <- 4
  }

  if (!is.null(cc)) tx[length(tx)+1] <- .dash2(sum(max.ln)+max.c1, cc=cc)

  # matrix for potentially multi-row column names
  if (max.ch > 0) {
    nr.ind.lbl <- integer(length=ncol(x))
    for (i in 1:ncol(x))
      nr.ind.lbl[i] <- ((nchar(colnames(x)[i]) + (max.ch-1)) %/% max.ch)

    nr.lbl <- max(nr.ind.lbl)  # n row of labels
    col.nm <- matrix(nrow=nr.lbl, ncol=ncol(x))
    for (i in 1:nrow(col.nm)) {
      for (j in 1:ncol(col.nm)) { 
        srt <- ((i-1)*max.ch) + 1
        stp <- srt + (max.ch - 1) 
        col.nm[i,j] <- substr(colnames(x)[j], srt, stp) 
        #if (nchar(col.nm[i,j]) > 0)  # left adjust within column
          #while (nchar(col.nm[i,j]) <= (max.ch-1))
            #col.nm[i,j] <- paste(col.nm[i,j], " ", sep="")
      }
    }
  }
  else {
    nr.lbl <- 1
    col.nm <- matrix(nrow=1, ncol=ncol(x))
    for (j in 1:ncol(col.nm)) col.nm[1,j] <- colnames(x)[j] 
  }
  # for each row, shift down value if next row is "", repeat
  if (nr.lbl > 1) { 
    for (k in 2:nrow(col.nm)) {  # repeat for each row
      for (i in 2:nrow(col.nm)) {
        for (j in 1:ncol(col.nm)) { 
          if (nchar(col.nm[i,j]) == 0) {
            col.nm[i,j] <- col.nm[i-1,j]  
            col.nm[i-1,j] <- ""
          }
        }
      }
    }
  }

  # write col labels
  for (i in 1:nr.lbl) {  # for each row of column labels
    tx[length(tx)+1] <- format("", width=max.c1)
    for (j in 1:ncol(x)) {
      wd <- max.ln[j]
      tx[length(tx)] <- paste(tx[length(tx)], .fmtc(col.nm[i,j], w=wd), sep="")
      if (!is.null(bnd)) if (j %in% bnd)
        if (i == nr.lbl)
          tx[length(tx)] <- paste(tx[length(tx)], "|", sep="")
        else
          tx[length(tx)] <- paste(tx[length(tx)], " ", sep="")
    }
  }
  if (!is.null(bnd))
    tx[length(tx)+1] <- .dash2(sum(max.ln)+max.c1+length(bnd), cc="-")

  # factor vars to char vars
  if (is.data.frame(x)) { 
    i.col <- sapply(x, is.factor)
    x[i.col] <- lapply(x[i.col], as.character)
  }

  # write values
  for (i in 1:nrow(x)) {
    if (i %in% brk) tx[length(tx)+1] <- "..."
    rwnm <- paste(" ", rownames(x)[i])
    tx[length(tx)+1] <- format(rwnm, width=max.c1, justify="right")

    for (j in 1:ncol(x)) {
      if (is.integer(x[i,j]))
        tx[length(tx)] <- paste(tx[length(tx)], .fmti(x[i,j], w=max.ln[j]), sep="")

      else if (is.numeric(x[i,j])) {
        wd <- max.ln[j] 
        if (cors) {
          if (max.ln[j] > 5) wd <- max(6, max.ln[j]+1) + 1 
          else wd <- max(6, max.ln[j]+1)
          cs <- .fmt(x[i,j], d=digits.d, w=wd)
          cs <- sub("0.", "", cs, fixed=TRUE)
          cs <- sub(" 1.00", "100", cs, fixed=TRUE)
        }
        else
          cs <- .fmt(x[i,j], d=digits.d, w=wd)
        if (abs(x[i,j]) < cut) cs <- paste(rep(" ", wd-2), collapse="")
        tx[length(tx)] <- paste(tx[length(tx)], cs, sep="")
        if (!is.null(bnd)) if (j %in% bnd)
          tx[length(tx)] <- paste(tx[length(tx)], "|", sep="") 
      }

      else if (is.character(x[i,j]))
        tx[length(tx)] <- paste(tx[length(tx)], .fmtc(x[i,j], w=max.ln[j]) , sep="") 
    }

    if (!is.null(bnd)) if (i %in% bnd)
      tx[length(tx)+1] <- .dash2(sum(max.ln)+max.c1+length(bnd), cc="-")
  }

  return(tx)

}  # end .prntbl
