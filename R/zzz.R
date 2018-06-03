if (getRversion() >= "2.15.1")
  globalVariables(c("mydata", "mycor", "P1", "P2", "P3"))
# Pn is for latticeExtra layer function


.onAttach <-
function(...) {

  packageStartupMessage("\n",
      "lessR 3.7.5     feedback: gerbing@pdx.edu     web: lessRstats.com/new\n",
      "---------------------------------------------------------------------\n",
      "1. mydata <- Read()        Read text, Excel, SPSS, SAS or R data file\n",
      "2. Help()                  Get help\n",
      "3. hs(), bc(), or ca()     All histograms, all bar charts, or both\n",
      "4. Plot(X) or Plot(X,Y)    For continuous and categorical variables\n",
      "                           numerical X: Violin, Box, Scatter plot \n",
      "5. by1= , by2=             Trellis graphics, a plot for each by1, by2\n",
      "6. reg(Y ~ X, Rmd=\"eg\")    Regression + R markdown file that, when\n",
      "                           knit, provides full interpretative output\n",
      "7. style(\"darkred\", sub.theme=\"colors\") Set theme, sub.theme\n",
      "   style(show=TRUE)        all color/style options and current values\n",
	  "8. getColors()             create discrete, continuous color scales\n")

  options(theme = "lightbronze")
  options(sub.theme = "default")

  options(window.fill = "#F7F2E6")  # rgb(247,242,230, maxColorValue=255)
  options(panel.fill = "#F7F2E6")
  #options(panel.fill = "transparent")
  options(panel.color = "#DED9CD")  # rgb(222,217,205, maxColorValue=255)
  options(panel.lwd = 0.5)
  options(panel.lty = "solid")

  # .maketrans("gray50", .to256("trans.bar.fill"))
  options(bar.fill = rgb(121,138,148, maxColorValue=255))  
  options(bar.fill.discrete = rgb(121,138,148, maxColorValue=255))  
  options(bar.fill.ordered = rgb(121,138,148, maxColorValue=255))
  options(trans.bar.fill = 0.0)
  options(bar.color = rgb(108,126,144, maxColorValue=255))
  options(values = "off")
  options(values.color = "white")
  options(values.cex = 0.85)
  options(values.digits = NULL)
  options(values.pos = "in")

  options(pt.fill = rgb(121,138,148, maxColorValue=255))  # old: gray20
  options(trans.pt.fill = 0.00)
  options(pt.color = rgb(121,138,148, maxColorValue=255))
  options(out.fill = "firebrick4")
  options(out.color = "firebrick4")
  options(out2.fill = "firebrick2")
  options(out2.color = "firebrick2")

  options(violin.fill = rgb(144,165,175, maxColorValue=255))
  options(violin.color = "gray15") 
  options(box.fill = "#7F7F7F23")  # .maketrans("gray10", 35)) 
  options(box.color = "gray15") 

  options(bubble.text.color = "#F7F2E6")  # rgb(247,242,230, maxColorValue=255)
  options(ellipse.fill = "#8B8B8B37")   # .maketrans("gray55", 55)
  options(ellipse.color = "gray20")
  options(ellipse.lwd = 1)
  options(se.fill = "#1A1A1A19")   # .maketrans("gray10", 25)
  options(fit.color = "gray15")
  options(fit.lwd = 1.5)
  options(area.fill = "#7F7F7FE6")
  options(heat = "gray30")
  options(segment.color = "gray15")
  options(ID.color = "gray50")

  options(main.color = "gray15")
  options(main.cex = 1)
  options(lab.color = "gray15")
  options(lab.x.color = NULL)
  options(lab.y.color = NULL)
  options(lab.cex = .95)
  options(lab.x.cex = NULL)
  options(lab.y.cex = NULL)

  options(axis.color = "gray15")
  options(axis.x.color = NULL)
  options(axis.y.color = NULL)
  options(axis.lwd = 1)
  options(axis.x.lwd = NULL)
  options(axis.y.lwd = NULL)
  options(axis.lty = "solid")
  options(axis.x.lty = NULL)
  options(axis.y.lty = NULL)
  options(axis.cex = 0.75)
  options(axis.x.cex = NULL)
  options(axis.y.cex = NULL)
  options(axis.text.color = "gray15")
  options(axis.x.text.color = NULL)
  options(axis.y.text.color = NULL)
  options(rotate.x = 0)
  options(rotate.y = 0)
  options(offset = 0.5)

  options(grid.color = "#DED9CD")  # rgb(222,217,205, maxColorValue=255)
  options(grid.x.color = NULL)  # rgb(222,217,205, maxColorValue=255)
  options(grid.y.color = NULL)  # rgb(222,217,205, maxColorValue=255)
  options(grid.lwd = 0.5)
  options(grid.x.lwd = NULL)
  options(grid.y.lwd = NULL)
  options(grid.lty = "solid")
  options(grid.x.lty = NULL)
  options(grid.y.lty = NULL)

  options(strip.fill = "#7F7F7F37")  # .maketrans("gray50", 55)) 
  options(strip.color = "gray55") 
  options(strip.text.color = "gray15") 

  #Plot(Years, Salary, bg="grey85", grid="grey77") on cheap Dell monitor

  options(add.fill = "gray20")
  options(add.trans = 0.0)
  options(add.color = "gray60")
  options(add.cex = 0.9)
  options(add.lwd = 0.5)
  options(add.lty = "solid")

  options(n.cat = 1)
  options(suggest = TRUE)
  options(quiet = FALSE)
  options(brief = FALSE)

  options(explain = TRUE)
  options(interpret = TRUE)
  options(results = TRUE)
  options(document = TRUE)
  options(code = TRUE)

  options(show.signif.stars = FALSE)
  options(scipen = 30)

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

# get number of decimal digits, trailing and leading 0's deleted
# x a scalar
.num.dec <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5)
    nchar(strsplit(as.character(x), ".", fixed=TRUE)[[1]][[2]])
  else
    return(0)
}

.fmt <- function(k, d=getOption("digits.d"), w=0) {
  format(sprintf("%.*f", d, k), width=w, justify="right", scientific=FALSE)
}


.fmt0 <- function(k, d=getOption("digits.d"), w=0) {
  a <- format(sprintf("%.*f", d, k), width=w, justify="right", scientific=FALSE)
  a <- substr(a,2,nchar(a))
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


# abbreviate column labels for cross-tab and related tables
.abbrev <- function(nms, mx.len=8) {

  if (max(nchar(nms)) > mx.len) {
    nms <- gsub("Strongly", "Strng", nms)
    nms <- gsub("Slightly", "Slght", nms)
    nms <- abbreviate(nms, mx.len)
  }
  
  # value returned is of type character
  return(nms)
}


# is date function
.is.date <- function(x) {

  isdate <- ifelse(class(x) == "Date", TRUE, FALSE)

  if (!isdate[1])  # ordered factor has more than 1 class
    isdate <- ifelse(grepl("POSIX",  class(x), fixed=TRUE)[1], TRUE, FALSE)  

  return(isdate)

}


# extract sequence of dates from time series y
.ts.dates <- function(y) {

  date_num <- as.numeric(time(y))
  year <- floor(date_num)
  year_beg <- as.POSIXct(paste0(year, '-01-01'))
  year_end <- as.POSIXct(paste0(year+1, '-01-01'))
  diff.yr <- year_end - year_beg
  dates <- year_beg + ((date_num %% 1) * diff.yr)
  dates <- as.Date(format(dates, format='%Y-%m-%d')) # from POSIX to Date
  x <- dates  # dates to be on x-axis
  
  return(x)
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



.is.integer <- function(x, tol= .Machine$double.eps^0.5) {

  if (is.numeric(x)) {
    x <- na.omit(x)
    int.flg <- ifelse (abs(x - round(x)) < tol, TRUE, FALSE)  # each i of vector
    result.flg <- ifelse (all(int.flg), TRUE, FALSE)
  }
  else
    result.flg <- FALSE

  return(result.flg)
}


.xstatus <- function(var.name, dname, quiet=FALSE) {

  # see if analysis from data is based on a formula
  is.frml <- ifelse (grepl("~", var.name), TRUE, FALSE)

  # see if analysis is from descriptive stats or from data 
  from.data <- ifelse (var.name == "NULL", FALSE, TRUE)

  # see if the variable exists in the global environment
  in.style <- FALSE
  if (nchar(var.name)>0) if (exists(var.name, where=.GlobalEnv)) {
    # a style "var" could be the name of a function
    # var.name is a character string, so convert to an object
    if (!is.function(eval(parse(text=var.name)))) {
      in.style <- TRUE
  #   if (!quiet)  # only want to do this if var.name is in a df also
  #     cat(">>>", var.name, "analyzed from the workspace, not in",
  #         "a data frame (table)\n")
    }
  }

  # see if "variable" is really an expression
  if (grepl("(", var.name, fixed=TRUE) ||  
      grepl("[", var.name, fixed=TRUE) ||  
      grepl("$", var.name, fixed=TRUE))  {
    txtA <- paste("A referenced variable in a lessR function can only be\n",
            "a variable name.\n\n", sep="")
    txtB <- "For example, this does not work:\n  > Histogram(rnorm(50))\n\n"
    txtC <- "Instead do this:\n  > Y <- rnorm(50)\n  > Histogram(Y)"
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        txtA, txtB, txtC, "\n")
  }

  if (!in.style && from.data) .nodf(dname)
 
  return(list(ifr=is.frml, fd=from.data, ig=in.style))
}


.getdfs <- function() {  # get list of data frames in style env

  objs <- function(x) class(get(x))

  dfs <- ls(.GlobalEnv)[sapply(ls(.GlobalEnv), objs) == 'data.frame']

  return(dfs)
}


.nodf <- function(dname) {

  # see if the data frame exists (mydata default), if x from data, not in style Env
  if (!exists(dname, where=.GlobalEnv)) {
    dfs <- .getdfs()  # list of data frames in style env
    txtA <- ifelse (dname == "mydata", ", the default data table name, ", " ") 

    if ("Mydata" %in% dfs)
      txtM <- paste("Because you have a data table called Mydata,\n",
        " perhaps you meant to call it mydata, if so just re-read \n",
        " into mydata instead of Mydata")
    else
      txtM <- paste(
        "If a data table is not named the default mydata, then to\n",
        "  analyze the variables in that data table, in the function call\n",
        "  for the analysis specify the actual data table name with\n",
        "  the data option\n",
        "For the data table called ", dfs[1], "\n",
        "  add the following to your function call:  , data=", dfs[1], "\n\n",
        "Or, just re-read the data into the mydata data table\n\n", sep="")

    if (length(dfs) == 0) { 
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "An analysis is of data values for one or more variables found\n",
        "  in a rectangular data table, with the data values for a \n",
        "  variable located in a column\n\n",
        "You have not yet read data into a data table for analysis,\n", 
        "  so the data table called ", dname, txtA, "is\n",
        "  not available for analysis\n\n",
        "Read the data into an R data table with the Read function, usually\n",
        "  reading the data into an R data table called mydata\n\n",
        "To read a data file on your computer system into the mydata data\n", 
        "  table, in which you browse your file folders to locate the\n",
        "  desired date file, enter:\n",
        "     mydata <- Read()\n\n",
        "To specify a data table from your computer or the web, enter:\n",
        "     mydata <- Read(\"path name\") \n",
        "  or \n",
        "     mydata <- Read(\"web address\") \n",
        "In the web address include the http:// at the beginning\n",
        "  and also include the quotes around the web address\n\n")
    }
    else if (length(dfs) == 1) {
      nm <- parse(text=paste("names(", dfs[1],")"))
      nm <- eval(nm)
      for (i in 1:length(nm)) nm[i] <- paste(nm[i], " ")
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Data table ", dname, txtA, "does not exist\n\n",
        "You have read data into one data table, ", dfs[1], ", but that\n", 
        "  is not the data table ", dname, " that was to be analyzed\n\n", 
        "Following are the names of the variables that are available\n",
        "  for analysis in the ", dfs[1], " data table\n\n",
        "  ", nm, "\n\n",
        txtM, "\n\n")
    }
    else if (length(dfs) > 1) {
      dts <- ""
      for (i in 1:length(dfs)) dts <- paste(dts, dfs[i])
      if (dname == "mydata") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Data table ", dname, txtA, "does not exist\n\n",
          "Data tables you read and/or created: ", dts, "\n\n",
          "Perhaps you have a data table that contains the variables\n",
          "  of interest to be analyzed, but it is not named ", dname, "\n",
          "Can specify the actual name with the data option\n",
          "For example, for a data table named ", dfs[1], "\n",
          "  add the following to your function call:  , data=", dfs[1], "\n\n",
          "Or, just re-read the data into the mydata data table\n\n")
        }
      else {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Data table ", dname, txtA, "does not exist\n\n",
          "Perhaps you have a data table that contains the variables\n",
          "  of interest to be analyzed, but it is not named ", dname, "\n\n",
          "Data tables you have already read and/or created: ", dts, "\n\n",
          "Use an available data table, or read data into a new table\n\n")
      }
    }
  }
}


.xcheck <- function(var.name, dname, data) {

  if ( (!grepl(":", var.name, fixed=TRUE) &&
        !grepl("c(", var.name, fixed=TRUE)) ) { # x not var list

    if (grepl("\"", var.name)) { 
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "No quotes allowed with the variable name.\n\n")
    }

  # see if "variable" is really an expression
  if (grepl("(", var.name, fixed=TRUE) ||  grepl("[", var.name, fixed=TRUE))  {
    txtA <- paste("A referenced variable in a lessR function can only be\n",
            "a variable name\n\n", sep="")
    txtB <- paste("For example, for the Histogram function, this does not work:\n",
            "  > Histogram(rnorm(50))\n\n", sep="")
    txtC <- "Instead do this:\n  > Y <- rnorm(50)\n  > Histogram(Y)"
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        txtA, txtB, txtC, "\n\n")
  }

  # see if "variable" includes a $ 
  if ( grepl("$", var.name, fixed=TRUE))  {
    txtA <- paste("A referenced variable in a lessR function just includes\n",
            "the variable name\n\n", sep="")
    txtB <- paste("For example, for the Histogram function, this does not work:\n",
            "  > Histogram(mydata$Y)\n\n", sep="")
    txtC <- "Instead do this:\n  > Histogram(Y, data=mydata)"
    txtD <- "If you wish to specify a data table, use option: data"
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        txtA, txtB, txtC, "\n", txtD, "\n\n")
  }

    # see if variable exists in the data frame
    if (!exists(var.name, where=data)) { 
      dfs <- .getdfs()  # data frames in style

      txt1 <- ", the default name \n\n"
      txt2 <- "Either make sure to use the correct variable name, or\n"
      txt3 <- "specify the data table that contains the variable with option:  data\n"
      txt <- ifelse (dname == "mydata",  paste(txt1, txt2, txt3, sep=""), "\n")

      nm <- parse(text=paste("names(", dname,")"))
      nm <- eval(nm)
      for (i in 1:length(nm)) nm[i] <- paste(nm[i], " ")

      if (dname == "mydata")
         txtDef <- ", which is the default data table name\n"
      else
         txtDef <- ""

      if (length(dfs) == 1) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "You are attempting to analyze the variable ", var.name, " in the\n", 
          "  data table called ", dname, txtDef, "\n",
          "Unfortunately, variable ", var.name, " does not exist in ", dname, "\n\n",
          "The following variables are currently in the ", dname, " data table,\n",
          "  available for analysis:\n\n",
          "  ", nm,  "\n\n")
      }

      else if (length(dfs) > 1) {
        nm2 <- parse(text=paste("names(", dfs[1],")"))
        nm2 <- eval(nm2)
        for (i in 1:length(nm2)) nm2[i] <- paste(nm2[i], " ")
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "You are attempting to analyze the variable ", var.name, " in the\n", 
          "  data table called ", dname, txtDef, "\n",
          "Unfortunately, variable ", var.name, " does not exist in ", dname, "\n\n",
          "The following variables are currently in the ", dname, " data table,\n",
          "  available for analysis:\n\n",
          "  ", nm,  "\n\n",
          "You do have another data table, but it is named ", dfs[1], "\n",
          "The following variables are currently in the ", dfs[1], " data table,\n",
          "  available for analysis:\n\n",
          "  ", nm2,  "\n\n",
          "If a data table is not named the default mydata, then to\n",
          "  analyze the variables in that data table, in the function call\n",
          "  for the analysis specify the actual data table name with\n",
          "  the data option\n",
          "For the data table called ", dfs[1], "\n",
          "  add the following to your function call:  , data=", dfs[1],
          "\n\n", sep="") 
      }
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


# get values for passed par parameters because ... cannot be passed to title 
#   in calling routine as each title will invoke sub if active,
#   and setting the line forces everything on the same line
.getdots <- function(...) {

  col.main <- NULL
  col.lab <- NULL
  sub.lab <- NULL
  col.sub <- NULL
  cex.main <- NULL

  dots <- list(...)  
  if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (names(dots)[i] == "col.main")  col.main <- dots[[i]] 
      if (names(dots)[i] == "col.lab")  col.lab <- dots[[i]] 
      if (names(dots)[i] == "sub.lab")  sub.lab <- dots[[i]] 
      if (names(dots)[i] == "col.sub")  col.sub <- dots[[i]] 
      if (names(dots)[i] == "cex.main")  cex.main <- dots[[i]] 
    }
  }

  return(list(col.main=col.main, col.lab=col.lab, sub.lab=sub.lab,
              col.sub=col.sub, cex.main=cex.main))

}


.varlist <- function(n.pred, i, var.name, pred.lbl, n.obs, n.keep, lvls=NULL) {

  if (i == 1)
    txt <- "Response Variable:  "
  else
      txt <- paste(pred.lbl, " Variable ", toString(i-1), ": ", sep="")
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


.varlist2 <- function(n.pred, ind, var.name, pred.lbl, n.obs, n.keep, lvls=NULL) {
  tx <- character(length = 0)

  if (ind == 1)
    txt <- "Response Variable: "
  else {
    if (n.pred > 1)
      txt <- paste(pred.lbl, " Variable ", toString(ind-1), ": ", sep="")
    else
      txt <- paste(pred.lbl, " Variable: ", sep="")
  }
  if (pred.lbl == "Factor"  &&  ind > 1) tx[length(tx)+1] <- ""
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
    if (!is.null(unt)) if (nzchar(unt))  if(!is.na(unt))
      lbl <- paste(lbl, " (", unt, ")", sep="")
    if (!is.null(lbl))
      tx[length(tx)] <- paste(tx[length(tx)], ", ", as.character(lbl), sep="")
  }

  if (!is.null(lvls)) {
    tx2 <- "  Levels:"
    for (i in 1:length(lvls)) tx2 <- paste(tx2, lvls[i])
    tx[length(tx)+1] <- tx2 
  }

  if (ind == n.pred+1) {
    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste("Number of cases (rows) of data: ", n.obs)
    tx[length(tx)+1] <- paste("Number of cases retained for analysis: ", n.keep)
  }

  return(tx)
}


.title <- function(x.name, y.name, x.lbl, y.lbl, isnullby) {

  txt1 <- x.name
  if (!is.null(x.lbl)) txt1 <- paste(txt1, ": ", x.lbl, sep="")

  if (isnullby) txt1 <- paste("---", txt1, "---")
  else {
    txt2 <- paste(y.name, sep="")
    if (!is.null(y.lbl)) txt2 <- paste(txt2, ": ", y.lbl, sep="") 
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


.title2 <- function(x.name, y.name, x.lbl, y.lbl, isnullby, new.ln=FALSE) {

  txt1 <- x.name
  if (!is.null(x.lbl)) txt1 <- paste(txt1, ": ", x.lbl, sep="")

  if (isnullby) {
    txt1 <- paste("---", txt1, "---")
    if (new.ln) txt1 <- paste(txt1, "\n", sep="")
  }
  else {
    txt2 <- paste(y.name, sep="")
    if (!is.null(y.lbl)) txt2 <- paste(txt2, ": ", y.lbl, sep="") 
  }

  tx <- character(length = 0)

  tx[length(tx)+1] <- txt1
  if (!isnullby) {
    #if (is.null(y.lbl))
      #tx[length(tx)+1] <- "  - by levels of - \n"
    #else
    tx[length(tx)+1] <- "\n  - by levels of - \n"
    tx[length(tx)] <- paste(tx[length(tx)], txt2, sep="")  # no leading blank
    #if (is.null(y.lbl))
      #tx[length(tx)+1] <- .dash2(max(nchar(txt1),nchar(txt2)))
  }

  return(tx)

}


# get variable labels if they exist
.getlabels <- function(xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
                       y.nm=FALSE, by.nm=FALSE, by1.nm=FALSE,
                       lab.x.cex=NULL, lab.y.cex=NULL, labels=mylabels,
                       graph.win=TRUE, flip=FALSE, ...) {

  if (graph.win) {  # do not open a graphics window if no plot
    fig.width <-  par("fin")[1]
    fig.ht <-  par("fin")[2]
    marg.x <- par("mai")[2] + par("mai")[4]
    axis.x <- fig.width - marg.x
    marg.y <- par("mai")[1] + par("mai")[3]
    axis.y <- fig.ht - marg.y
    cut.x <- 0.90 * axis.x
    cut.y <- 0.95 * axis.y
  }
  else {
    cut.x <- 3.75
    cut.y <- 3.75
  }

  # strwidth function not working in regular R, lab.cex has no affect
  regR <- FALSE
  in.RStudio <- ifelse (options("device") != "RStudioGD", FALSE, TRUE)
  in.knitr <- ifelse (is.null(options()$knitr.in.progress), FALSE, TRUE)
  if (!in.RStudio && !in.knitr) regR <- TRUE 

  x.name <- getOption("xname")
  if (y.nm)
    y.name <- getOption("yname")  # y.name is specified
  else if (!by.nm && !by1.nm)
    y.name <- getOption("yname")  # y.name by default
  else if (by.nm && !by1.nm)
    y.name <- getOption("byname")
  else if (!by.nm && by1.nm)
    y.name <- getOption("by1name")

  # get variable labels, x.lbl and y.lbl
  x.lbl <- NULL
  y.lbl <- NULL
  l.name <- deparse(substitute(labels))
  if (exists(l.name, where=.GlobalEnv)) {
    mylabels <- get(l.name, pos=.GlobalEnv)

    i.row <- which(row.names(mylabels) == x.name)
    if (length(i.row) > 0) if (is.numeric(i.row))
      if (!is.na(mylabels[i.row, ])) x.lbl <- mylabels[i.row,1]

    i.row <- which(row.names(mylabels) == y.name)
    if (length(i.row) > 0) if (is.numeric(i.row))
      if (!is.na(mylabels[i.row, ])) y.lbl <- mylabels[i.row,1]
  }

  else {  # labels embedded in data
    dname <- getOption("dname")  # not set for dependent option on tt
    if (!is.null(dname)) {
      if (exists(dname, where=.GlobalEnv)) {
        mylabels <- attr(get(dname, pos=.GlobalEnv), which="variable.labels")
        myunits <- attr(get(dname, pos=.GlobalEnv), which="variable.units")
      }
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
  }  # end labels embedded in data


  # ------------------------
  # get x.lab
  if (is.null(x.lbl) && is.null(xlab))
    x.lab <- x.name
  else {
    if (!is.null(xlab))
      x.lab <- xlab  # xlab specified
    else if (!is.null(x.lbl)) 
      x.lab <- x.lbl
  }

  # get y.lab
  if (is.null(y.lbl) && is.null(ylab))
    y.lab <- y.name
  else {
    if (!is.null(ylab))
      y.lab <- ylab  # ylab specified
    else if (!is.null(y.lbl)) 
      y.lab <- y.lbl

  }

  if (flip) {
    temp <- ylab;  ylab <- xlab;  xlab <- temp
    temp <- y.lab;  y.lab <- x.lab;  x.lab <- temp
    temp <- y.lbl;  y.lbl <- x.lbl;  x.lbl <- temp 
    temp <- lab.y.cex;  lab.y.cex <- lab.x.cex;  lab.x.cex <- temp 
    temp <- y.name;  y.name <- x.name; #  x.name <- temp 
  }

  # ------------------------
  # x-axis and legend labels

  if (!is.null(x.lbl) || !is.null(xlab)) {
    # power.ttest: len > 1;  # add var name to label?
    if (length(x.lab) == 1  &&  !is.null(lab.x.cex)  &&  graph.win) { 
      var.nm <- ifelse (is.null(x.lbl) && !is.null(x.name), FALSE, TRUE) 
      if (!is.null(xlab)) var.nm <- FALSE  # xlab is the complete label
      al <- .adjlbl(x.lab, lab.x.cex, cut=cut.x, x.name, var.nm, units="inches")
      x.lab <- al$lab
    }
  }  # end get x.lab


  # ------------------------
  # y-axis and legend labels

    if (!is.null(y.lab)) { # power.ttest: len > 1
    if (length(y.lab) == 1  &&  !is.null(lab.y.cex)  &&  graph.win) { 
      var.nm <- ifelse (is.null(y.lbl) && !is.null(y.name), FALSE, TRUE) 
      if (!is.null(ylab)) var.nm <- FALSE  # ylab is the complete label
      al <- .adjlbl(y.lab, lab.y.cex, cut=cut.y, y.name, var.nm, units="inches")
      y.lab <- al$lab
    }
  }  # end process y-axis label


  if (!missing(main)) {
    if (!is.null(main))
      main.lab <- main
    else 
      main.lab <- NULL
  }
  else
    main.lab <- NULL

  if (!missing(sub)) {
    if (!is.null(sub)) sub.lab <- sub else sub.lab <- NULL
  }
  else
    sub.lab <- NULL

  return(list(xn=x.name, xl=x.lbl, xb=x.lab, yn=y.name, yl=y.lbl, yb=y.lab,
     mb=main.lab, sb=sub.lab))
}  # end .getlabels




# get the lines of the axis label, prefix with variable name
.adjlbl <-
function(lab, labcex, cut, nm, var.nm, units) {

  # add variable name to label
  if (grepl("Count", lab, fixed=TRUE)) var.nm <- FALSE 
  if (grepl("Proportion", lab, fixed=TRUE)) var.nm <- FALSE 
  if (grepl("Alternative", lab, fixed=TRUE)) var.nm <- FALSE 
  if (var.nm) {
    lab <- paste(nm, ": ", lab, sep="")
#   if (!grepl("\n", lab, fixed=TRUE))  # bquote removes the \n
#     lab <- bquote(paste(italic(.(nm)), ": ", .(lab))) 
  }
  strw <- strwidth(lab, units=units, cex=labcex)
  n.lab.ln <- (strw %/% cut) + 1

  if (strw > cut) {
    line <- character(length=n.lab.ln)
    s <- unlist(strsplit(lab, " "))
    il <- 1
    for (iw in 1:(length(s))) {
      if (strwidth(line[il], units=units, cex=labcex) > cut)
        il <- il + 1
      line[il] <- paste(line[il], s[iw])  
    } 
    # trim a possible trailing blank line
    if (line[n.lab.ln] == "") line <- line[1:(n.lab.ln-1)]

    if (length(line) == 1 ) {
      lab <- line
    }
    else if (length(line) == 2 ) {  # break label down the middle
      brk <- nchar(lab) %/% 2
      while (substr(lab,brk,brk) != " ") brk <- brk-1  # break at word boundary
      line1 <- substr(lab, 1, brk)
      line2 <- substr(lab, brk+1, nchar(lab))
      lab <- paste(line1, "\n",  line2)
    }
    else if (length(line) > 2 ) {  # use re-constructed lines
      lab <- ""
      for (i in 1:length(line)) {
        lab <- paste(lab, line[i])
        if (i < length(line)) lab <- paste(lab, "\n", sep="")
      }
    }
  }

  return(list(lab=lab))
}


# plot a set of vertical or horizontal grid lines
.grid <- 
function(dir, axT) {

  # possible inheritance

  grid.x.color <- ifelse(is.null(getOption("grid.x.color")), 
    getOption("grid.color"), getOption("grid.x.color"))
  grid.y.color <- ifelse(is.null(getOption("grid.y.color")), 
    getOption("grid.color"), getOption("grid.y.color"))
 
  grid.x.lwd <- ifelse(is.null(getOption("grid.x.lwd")), 
    getOption("grid.lwd"), getOption("grid.x.lwd"))
  grid.y.lwd <- ifelse(is.null(getOption("grid.y.lwd")), 
    getOption("grid.lwd"), getOption("grid.y.lwd"))

  grid.x.lty <- ifelse(is.null(getOption("grid.x.lty")), 
    getOption("grid.lty"), getOption("grid.x.lty"))
  grid.y.lty <- ifelse(is.null(getOption("grid.y.lty")), 
    getOption("grid.lty"), getOption("grid.y.lty"))

  if (dir == "h") if (grid.y.lwd > 0) 
    abline(h=axT, col=grid.y.color, lwd=grid.y.lwd, lty=grid.y.lty)

  if (dir == "v") if (grid.x.lwd > 0) 
    abline(v=axT, col=grid.x.color, lwd=grid.x.lwd, lty=grid.x.lty)

}


.axes <- function(x.lvl, y.lvl, axT1, axT2, par1, par3,
         rotate.x=0, rotate.y=0, offset=0.5,
         y.only=FALSE, ...) {

  axis.x.color <- ifelse(is.null(getOption("axis.x.color")), 
    getOption("axis.color"), getOption("axis.x.color"))
  axis.y.color <- ifelse(is.null(getOption("axis.y.color")), 
    getOption("axis.color"), getOption("axis.y.color"))
 
  axis.x.lwd <- ifelse(is.null(getOption("axis.x.lwd")), 
    getOption("axis.lwd"), getOption("axis.x.lwd"))
  axis.y.lwd <- ifelse(is.null(getOption("axis.y.lwd")), 
    getOption("axis.lwd"), getOption("axis.y.lwd"))

  axis.x.lty <- ifelse(is.null(getOption("axis.x.lty")), 
    getOption("axis.lty"), getOption("axis.x.lty"))
  axis.y.lty <- ifelse(is.null(getOption("axis.y.lty")), 
    getOption("axis.lty"), getOption("axis.y.lty"))

  axis.x.cex <- ifelse(is.null(getOption("axis.x.cex")), 
    getOption("axis.cex"), getOption("axis.x.cex"))
  adj <- .RSadj(axis.cex=axis.x.cex); axis.x.cex <- adj$axis.cex
  axis.y.cex <- ifelse(is.null(getOption("axis.y.cex")), 
    getOption("axis.cex"), getOption("axis.y.cex"))
  adj <- .RSadj(axis.cex=axis.y.cex); axis.y.cex <- adj$axis.cex

  axis.x.text.color <- ifelse(is.null(getOption("axis.x.text.color")), 
    getOption("axis.text.color"), getOption("axis.x.text.color"))
  axis.y.text.color <- ifelse(is.null(getOption("axis.y.text.color")), 
    getOption("axis.text.color"), getOption("axis.y.text.color"))

  
  if (is.null(x.lvl)  &&  !is.null(axT1)) {  # numeric, uses axT1
    if (!y.only) {  # do x axis in calling routine for time series
      axis(1, at=axT1, labels=FALSE, tck=-.01, col=axis.x.color,
        lwd=axis.x.lwd, lty=axis.x.lty)
      dec.d <- .getdigits(round(axT1,6),1) - 1
      text(x=axT1, y=par3, labels=.fmt(axT1,dec.d),
           pos=1, xpd=TRUE, cex=axis.x.cex, col=axis.x.text.color,
           srt=rotate.x, offset=offset, ...)
    }
  }
  
  else if (!is.null(x.lvl)) {  # categorical, uses x.lvl
    axis(1, at=axT1, labels=FALSE, tck=-.01, col=axis.x.color,
        lwd=axis.x.lwd, lty=axis.x.lty)
    text(x=axT1, y=par3, labels=x.lvl,
         pos=1, xpd=TRUE, cex=axis.x.cex, col=axis.x.text.color,
         srt=rotate.x, offset=offset, ...)
  }

  if (is.null(y.lvl)  &&  !is.null(axT2)) {
    axis(2, at=axT2, labels=FALSE, tck=-.01, col=axis.y.color,
        lwd=axis.y.lwd, lty=axis.y.lty)
    dec.d <- .getdigits(round(axT2,6),1) - 1
    text(x=par1, y=axT2, labels=.fmt(axT2,dec.d),
         pos=2, xpd=TRUE, cex=axis.y.cex, col=axis.y.text.color,
         srt=rotate.y, ...)
  }
  else if (!is.null(y.lvl)) {
    axis(2, at=axT2, labels=FALSE, tck=-.01, col=axis.y.color,
        lwd=axis.y.lwd, lty=axis.y.lty)
    text(x=par1, y=axT2, labels=y.lvl,
         pos=2, xpd=TRUE, cex=axis.y.cex, col=axis.y.text.color,
         srt=rotate.y, ...)
  }
}


# axis labels
.axlabs <- function(x.lab, y.lab, main.lab, sub.lab, max.lbl.y,
                    x.val=NULL, xy.ticks=TRUE, offset=0.5,
                    lab.x.cex=NULL, lab.y.cex=NULL, main.cex=NULL,
                    n.lab.x.ln=1, n.lab.y.ln=1, xlab.adj=0, ylab.adj=0,
                    ...) {

  lab.x.color <- ifelse(is.null(getOption("lab.x.color")), 
    getOption("lab.color"), getOption("lab.x.color"))
  lab.y.color <- ifelse(is.null(getOption("lab.y.color")), 
    getOption("lab.color"), getOption("lab.y.color"))

  if (is.null(lab.x.cex)) {  # temp until all .axes calls provide lab.x.cex
    lab.x.cex <- ifelse(is.null(getOption("lab.x.cex")), 
      getOption("lab.cex"), getOption("lab.x.cex"))
  }
  if (is.null(lab.y.cex)) { 
    lab.y.cex <- ifelse(is.null(getOption("lab.y.cex")), 
      getOption("lab.cex"), getOption("lab.y.cex"))
  }

  adj <- .RSadj(lab.cex=lab.x.cex); lab.x.cex <- adj$lab.cex
  adj <- .RSadj(lab.cex=lab.y.cex); lab.y.cex <- adj$lab.cex

  # xlab positioning
  lblx.lns <-  par("mar")[1] - 1.3   # par("mar")[1] is bm in lines
  ln.ht.x <- par('cin')[2] * lab.x.cex * par('lheight')  # line ht inches
  xlab.adj <- xlab.adj / ln.ht.x

  # ylab positioning
  lbly.lns <- par("mar")[2] - (0.3 + 0.9*n.lab.y.ln)  # mar 2: lm in lines
  ln.ht.y <- par('cin')[2] * lab.y.cex * par('lheight')  # line ht inches
  ylab.adj <- ylab.adj / ln.ht.y

  regR <- FALSE  # regular R by itself adjustment
  in.RStudio <- ifelse (options("device") == "RStudioGD", TRUE, FALSE)
  in.knitr <- ifelse (!is.null(options()$knitr.in.progress), TRUE, FALSE)
  if (!in.RStudio && !in.knitr) regR <- TRUE  
  if (regR) ylab.adj <- ylab.adj + .2

  title(xlab=x.lab, line=lblx.lns-xlab.adj, 
        col.lab=lab.x.color, cex.lab=lab.x.cex, ...)
  title(sub=sub.lab, line=lblx.lns+1, cex.sub=0.76,
        col.lab=lab.x.color, ...)
  title(ylab=y.lab, line=lbly.lns-ylab.adj,
        col.lab=lab.y.color, cex.lab=lab.y.cex, ...)
  title(main=main.lab, cex.main= getOption("main.cex"),
        col.main=getOption("main.color"), ...)

}


# get number of lines in value labels
.get.val.ln <- function (val.lab, var.name) {

  ln.val <- integer(length=length(val.lab))

  for (i in seq_along(val.lab)) {  
    val.lab[i] <- gsub(" ", "\n", val.lab[i])  # space to new line
    val.lab[i] <- gsub("_", " ", val.lab[i])  # underline to space
    ln.br <- 0
    for (j in 1:nchar(val.lab[i]))
      if (substr(val.lab[i], j, j)=="\n") ln.br <- ln.br + 1 
    ln.val[i] <- ln.br + 1
  }
  mx.val.ln <- max(ln.val)  # largest number of value label lines

  if (is.infinite(mx.val.ln)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "No value labels, ", var.name, " not properly specified\n\n")
  }

  return(list(val.lab=val.lab, mx.val.ln=mx.val.ln))
}


# margins
.marg <- function(max.y.width, y.lab, x.lab, main,
                  rotate.x=0, mx.x.val.ln=1, mx.y.val.ln=1,
                  lab.x.cex=0.95, lab.y.cex=0.95, max.x.width=NULL) {

  # top margin
  tm <- 0.15
  if (!is.null(main)) tm <- tm + .25
  # if (options("device") == "RStudioGD") {
    # tm <- ifelse(.Platform$OS == "windows", tm-.15, 0)
  # }

  # right margin
  rm <- 0.1


  # bottom margin
  n.lab.x.ln <- 0  # in case x.lab is null
  if (!is.null(x.lab)) {
    strn <- unlist(gregexpr("\n", x.lab, fixed=TRUE))
    if (strn[1] == -1) strn <- NULL  # return of -1 means no occurrence
    n.lab.x.ln <- length(strn) + 1
  }

  ln.ht <- par('cin')[2] * lab.x.cex * par('lheight')  # line ht inches
  if (rotate.x != 90) {
    bm <- ((n.lab.x.ln + mx.x.val.ln) * ln.ht) + (1.5*ln.ht)
  }
  else {
    bm <- max.x.width + (ln.ht * n.lab.x.ln) + 0.25 
  }
  tm <- ifelse (is.null(main), tm+.05, tm+.25)  #  adjust tm for increased bm
  if (rotate.x != 0) bm <- bm + .15


  # left margin
  n.lab.y.ln <- 0  # in case x.lab is null
  if (!is.null(y.lab)) {
    strn <- unlist(gregexpr("\n", y.lab, fixed=TRUE))
    if (strn[1] == -1) strn <- NULL  # return of -1 means no occurrence
    n.lab.y.ln <- length(strn) + 1
  }

  mm <- max.y.width + 0.45 
  if (!is.null(y.lab)) mm <- mm + (n.lab.y.ln * .18)

  return(list(lm=mm, tm=tm, rm=rm, bm=bm,
              n.lab.x.ln=n.lab.x.ln, n.lab.y.ln=n.lab.y.ln))
}


# enlarge scale for R
.RSadj <- function(radius=0.25, axis.cex=NULL, cex.names=NULL, lab.cex=NULL) {

  regR <- FALSE  # regular R by itself
  in.RStudio <- ifelse (options("device") == "RStudioGD", TRUE, FALSE)
  in.knitr <- ifelse (!is.null(options()$knitr.in.progress), TRUE, FALSE)
  if (!in.RStudio && !in.knitr) regR <- TRUE 

  if (regR) {
    radius <- radius*1.6
  }

  if (!is.null(axis.cex))
    size.axis <- ifelse (regR, axis.cex*1.3, axis.cex)
  else
    size.axis <- NULL

   if (!is.null(lab.cex))
    size.lab <- ifelse (regR, lab.cex*1.3, lab.cex)
  else
   size.lab <- NULL

  return(list(radius=radius, axis.cex=size.axis, lab.cex=size.lab))
}


.showfile <- function(fname, txt) {
  if (getwd() == "/")
    workdir <- "top level (root) of your file system"
  else
    workdir <- getwd()
  cat("The", txt, "written at the current working directory\n")
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


.band.width <- function(x, details=FALSE, ...) {

  if (details) {
    cat("\n")
    cat("iterate for smoother density bandwidth (bw)\n")
    cat("changes: number of times densities change sign\n")
    cat("----------------------------------------------\n")
  }

  x <- na.omit(x)
  bw <- bw.nrd0(x)
  irep <- 0

  repeat {
    irep <- irep + 1
    d.gen <- suppressWarnings(density(x, bw, ...))  # no missing data
    xd <- diff(d.gen$y)

    flip <- 0
    for (j in 2:length(xd))
      if (sign(xd[j-1]) != sign(xd[j])) flip <- flip + 1
    if (details)
      cat(irep, " changes:", .fmti(flip,3), "   bw: ", .fmt(bw,4), "\n", sep="")
    if (flip > 1  &&  irep < 25)
{
      bw <- 1.1 * bw
}
    else
      break;
  }  # end repeat

  return(bw)
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

  manage.gr <- ifelse (!in.RStudio && !in.knitr, TRUE, FALSE)

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
    for (i in 1:wnew) {
      if (is.null(d.w) && is.null(d.h))
        dev.new()
      else if (is.null(d.w))  # BPFM and 1 cat var have reduced height only
        dev.new(height=d.h)
      else
        dev.new(width=d.w, height=d.h)
    }
  if (off.two) dev.off(which=2)

}


.opendev <- function(pdf.fnm, width, height) {

  if (is.null(pdf.fnm)) {
    if (options("device") != "RStudioGD" && is.null(options()$knitr.in.progress)) {
      .graphwin(1, d.w=width, d.h=height)
      orig.params <- par(no.readonly=TRUE)
      on.exit(par(orig.params))
    }
  }
  else  # windows puts a blank first page without onefile=FALSE 
    pdf(file=pdf.fnm, width=width, height=height, onefile=FALSE)

}


# num.cat var is integer with small number of unique values
.is.num.cat <- function(x, n.cat) {

  x <- sort(unique(na.omit(x)))

  nu.x <- length(x)

  if (.is.integer(x)  &&  nu.x <= n.cat) {
    eq.int <- TRUE
    d.x <- diff(x)  # check for equal intervals
    if (nu.x > 2) {
      for (i in 2:(length(x)-1)) {
        if ((abs(d.x[i-1] - d.x[i]) > 0.0000000001)) eq.int <- FALSE
      }
      status <- eq.int  # num.cat var has equal intervals
    }
    else
      status <- TRUE

  }
  else
    status <- FALSE 

  return(status)

}


.ncat <- function(analysis, x.name, nu, n.cat, brief=FALSE) {

  cat("\n")
  cat(">>> ", x.name, " has only only ", nu, " equally spaced unique ",
      "integer values <= n.cat=", n.cat, "\n",
      "    so treat as categorical, and perhaps convert to an R factor\n", sep="")

  if (!brief)
    cat("    For numeric, set n.cat smaller than ", nu, 
        " with ", analysis, " or globally with  style", sep="")

  cat("\n")

}

        
.corcolors <- function(R, NItems, main, bm=3, rm=3, diag=NULL,
                       pdf.file, width, height) {

    if (!is.null(diag)) {
      for (i in 1:NItems) R[i,i] <- diag
      cat("\nNote: To provide more color separation for off-diagonal\n",
          "      elements, the diagonal elements of the matrix for\n",
          "      computing the heat map are set to 0.\n", sep="")
    }

    max.color <- getOption("heat")
    hmcols <- colorRampPalette(c("white",max.color))(256)
    
    .opendev(pdf.file, width, height)  # set up graphics

    heatmap(R[1:NItems,1:NItems], Rowv=NA, Colv="Rowv", symm=TRUE,
      col=hmcols, margins=c(bm,rm), main=main)

    if (!is.null(pdf.file)) {  # terminate pdf graphics
      dev.off()
      .showfile(pdf.file, "plot")
    }
}


.maketrans <- function(col.name, trans.level) {

  col.trans <- numeric(length(col.name))

  for (i in 1:length(col.name)) {
    r.tr <- col2rgb(col.name[i])[1]
    g.tr <- col2rgb(col.name[i])[2]
    b.tr <- col2rgb(col.name[i])[3]

    col.trans[i]  <- rgb(r.tr, g.tr, b.tr, alpha=trans.level, maxColorValue=256)
  }

  return(col.trans)
}


# generate a pre-defined color range if requested
.color.range <- function(fill, n.clr, no.change=FALSE) {
  f1 <- fill[1]

       if (f1 == "colors") clrs <- getColors("colors", n=n.clr)
  else if (f1 == "reds") clrs <- getColors("reds", n=n.clr)
  else if (f1 == "yellows") clrs <- getColors("yellows", n=n.clr)
  else if (f1 == "rusts") clrs <- getColors("rusts", n=n.clr)
  else if (f1 == "olives") clrs <- getColors("olives", n=n.clr)
  else if (f1 == "greens") clrs <- getColors("greens", n=n.clr)
  else if (f1 == "turquoises") clrs <- getColors("turquoises", n=n.clr)
  else if (f1 == "aquas") clrs <- getColors("aquas", n=n.clr)
  else if (f1 == "blues") clrs <- getColors("blues", n=n.clr)
  else if (f1 == "purples") clrs <- getColors("purples", n=n.clr)
  else if (f1 == "grays") clrs <- getColors("grays", n=n.clr)
  else if (f1 == "heat") clrs <- getColors("heat", n=n.clr)
  else if (f1 == "terrain") clrs <- getColors("terrain", n=n.clr)
  else if (f1 == "rainbow") clrs <- getColors("rainbow", n=n.clr)
  else {
    if (!no.change)
      clrs <- NULL
     else  # for style, lines 374+
      clrs <- fill
  }

  return(clrs)
}


.to_rgb <- function(color) {

  if (is.null(color))
    rgb.color <- "NULL"
  else {  # preserve color name if it exists
    if (!(color %in% colors()))
      rgb.color <- col2rgb(color, alpha=TRUE)
    else
      rgb.color <- color
  }

  return(rgb.color)

}


.to_num <- function(k, d=1, w=0) {
  if (!is.null(k)) 
    val <- format(sprintf("%.*f", d, k), width=w, justify="right",
                  scientific=FALSE)
  else
    val <- "NULL" 
  return(val)
}


.to_str <- function(cc) {
  if (is.null(cc)) cc <- "NULL"
  return(cc)
}


.to256 <- function(trans.level)
   trn <- (1-getOption(trans.level))*256

.to256n <- function(trans.level)
   trn <- (1-trans.level) * 256


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


# remove argument and logical value from a function call
.rm.arg.l <-  function(argm, fc) {

  loc <- regexec(argm, fc)[[1]]  # beginning of argument

  if (loc > 0) {

    first.arg <- ifelse (substr(fc, loc-1, loc-1) == "(", TRUE, FALSE)

    j <- loc
    if (!first.arg)  # is not first argument, start at preceding comma
      while(substr(fc, start=j, stop=j) != "," &&  substr(fc, start=j, stop=j) != "")
         if (j < 1000) j <- j + 1 
    stp <- j  #  closing parentheses or comma before argument
    if (first.arg) stp <- stp + 2  # remove trailing comma and space
    strt <- loc - 1

    remv <- substr(fc, start=strt, stop=stp)
    fc.new <- sub(remv, "", fc, fixed=TRUE)
    fc.new <- sub(",,", "", fc.new, fixed=TRUE)  # hack

  }

  return(fc.new)
}


# remove x=  and y= for suggestions for Plot
.rm.arg.2 <-  function(argm, fc) {

  fc <- sub(",,", ",", fc, fixed=TRUE)
 
  fc1 <- gsub(argm, "", fc, fixed=TRUE)  # remove all argm from fc
  fc2 <- gsub(",", ", ", fc1, fixed=TRUE)  # each , goes to , space
  fc3 <- gsub("  ", " ", fc2, fixed=TRUE)
  fc3 <- gsub(") #", ")  #", fc3, fixed=TRUE)  # restore blank before #

  if (grepl("(", argm, fixed=TRUE)) fc3 <- gsub("Plot", "Plot(", fc3)
  fc3 <- gsub("((", "(", fc3, fixed=TRUE)
  fc3 <- sub(", ,", ",", fc3, fixed=TRUE)

  return(fc3)
}


# remove argument and non-string value from a function call
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


.prntbl <- function(x, digits.d=2, cut=0, cc="-", cors=FALSE,
                    brk=NULL, bnd=NULL, v1.nm=NULL, v2.nm=NULL) {

# brk: ... replaces rows not printed 
# bnd: boundary between groups

  tx <- character(length = 0)

  max.ch <- ifelse (cors, 3, 0)  # max char per column, 0 is not applicable

  # width of column 1
  max.c1 <- 0
  for (i in 1:nrow(x)) {
    c1 <- nchar(rownames(x)[i])
    if (c1 > max.c1) max.c1 <- c1
  }
  if (!is.null(v2.nm)) if (nchar(v2.nm) > max.c1) max.c1 <- nchar(v2.nm) 
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
    else {
      c.val <- 0        
      for (i in 1:nrow(x)) {
        i.val <- nchar(as.character(x[i,j]))
        if (i.val > c.val) c.val <- i.val
      }
    }
      #c.val <- 4
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

  blnk <- format("", width=max.c1-1)
  if (!is.null(v1.nm)) tx[length(tx)+1] <- paste(blnk, v1.nm)
  # write col labels
  for (i in 1:nr.lbl) {  # for each row of column labels
    if (is.null(v2.nm))
      tx[length(tx)+1] <- format("", width=max.c1)
    else
      tx[length(tx)+1] <- paste(" ", v2.nm,
           format("", width=max.c1-nchar(v2.nm)-2), sep="")
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
    if (is.null(v2.nm))
      tx[length(tx)+1] <- format(rwnm, width=max.c1, justify="right")
    else
      tx[length(tx)+1] <- format(rwnm, width=max.c1-1, justify="right")

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
        tx[length(tx)] <- paste(tx[length(tx)], .fmtc(x[i,j], w=max.ln[j]),
                                sep="") 
    }

    if (!is.null(bnd)) if (i %in% bnd)
      tx[length(tx)+1] <- .dash2(sum(max.ln)+max.c1+length(bnd), cc="-")
  }

  return(tx)

}  # end .prntbl
