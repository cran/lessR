# need d as global because data=d in function def does not 
#  have a value assigned to d, d <- mydata does do that, even if conditional
#  generates a Note
if (getRversion() >= "3.5.0")
  globalVariables(c("d", "mydata", "l", "mylabels", "mycor"))


.onAttach <-
function(...) {

  packageStartupMessage("\n",
      "lessR 4.0.8  feedback: gerbing@pdx.edu  web: lessRstats.com/new\n",
      "---------------------------------------------------------------\n",
      "> d <- Read(\"\")   Read text, Excel, SPSS, SAS, or R data file\n",
      "  d is default data frame, data= in analysis routines optional\n",
      "\n",
      "Learn about reading, writing, and manipulating data, graphics,\n",
      "testing means and proportions, regression, factor analysis,\n",
      "customization, and descriptive statistics from pivot tables.\n",
      "  Enter:  browseVignettes(\"lessR\")\n\n",
      "View changes in this new version of lessR.\n",
      "  Enter: help(package=lessR)  Click: Package NEWS\n", 
      "  Enter: interact()  for access to interactive graphics\n") 


  # imports shiny loads the package, but prefer not have loaded until
  #   actually run, such as with interact()
  # the reason is that there is less data frame and variable error checking
  #   when shiny is loaded
  unloadNamespace("shiny")

  options(warn = -1)  # suppress warnings while bin.width, etc., allowed

  options(theme = "colors")
  options(sub_theme = "default")

  options(panel_fill = "white")
  options(window_fill = getOption("panel_fill"))
  options(panel_color = "gray45")  # lattice
  options(panel_lwd = 1.0)
  options(panel_lty = "solid")

  # .maketrans("gray50", .to256("trans_bar_fill"))
  options(bar_fill = NULL)
  options(bar_fill_discrete = c("#4398D0", "#B28B2A", "#5FA140", "#D57388",
           "#9A84D6", "#00A898", "#C97E5B", "#909711", "#00A3BA", "#D26FAF",
           "#00A76F", "#BD76CB" ))  # getColors("hues")
  options(bar_fill_ordered = rgb(144,165,195, maxColorValue=255))
  options(trans_bar_fill = 0.0)
  options(bar_color = rgb(126,144,168, maxColorValue=255))
  options(bar_color_discrete = "transparent")
  options(bar_color_ordered = rgb(126,144,168, maxColorValue=255))
  options(values = "%")
  options(values_color = "white")
  options(values_size = 0.75)
  options(values_digits = NULL)
  options(values_position = "in")

  options(pt_fill = rgb(50,78,92, maxColorValue=255))
  options(trans_pt_fill = 0.00)
  options(pt_color = rgb(50,78,92, maxColorValue=255))  # old 70 80 90
  options(out_fill = "firebrick4")
  options(out_color = "firebrick4")
  options(out2_fill = "firebrick2")
  options(out2_color = "firebrick2")

  options(violin_fill = "#7485975A")  # .maketrans(hcl(240,20,55), 90)
  options(violin_color = "gray15")
  options(box_fill = rgb(65,155,210, maxColorValue=255)) # old getColors("hues")
  options(box_color = "gray15")
  options(line_color = "gray15")

  options(bubble_text_color = "#F7F2E6")  # rgb(247,242,230, maxColorValue=255)
  options(ellipse_fill = "#92806F28")   # .maketrans(hcl(50,20,55), 40)
  options(ellipse_color = "gray20")
  options(ellipse_lwd = 1)
  options(se_fill = "#1A1A1A19")  # old .maketrans("gray10", 40) "darkblue", 25
  options(fit_color = rgb(92,64,50, maxColorValue = 255))
  options(fit_lwd = 2)
  options(heat = "gray30")
  options(segment_color = "gray40")
  options(ID_color = "gray50")

  options(main_color = "gray15")
  options(main_cex = 1)
  options(lab_color = "gray15")
  options(lab_x_color = NULL)
  options(lab_y_color = NULL)
  options(lab_cex = .88)
  options(lab_x_cex = NULL)
  options(lab_y_cex = NULL)

  options(axis_color = "gray15")
  options(axis_x_color = NULL)
  options(axis_y_color = NULL)
  options(axis_lwd = 1)
  options(axis_x_lwd = NULL)
  options(axis_y_lwd = NULL)
  options(axis_lty = "solid")
  options(axis_x_lty = NULL)
  options(axis_y_lty = NULL)
  options(axis_cex = 0.75)
  options(axis_x_cex = NULL)
  options(axis_y_cex = NULL)
  options(axis_text_color = "gray20")
  options(axis_x_text_color = NULL)
  options(axis_y_text_color = NULL)
  options(rotate_x = 0)
  options(rotate_y = 0)
  options(offset = 0.5)

  options(grid_color = "#DED9CD")  # rgb(222,217,205, maxColorValue=255)
  options(grid_x_color = NULL)
  options(grid_y_color = NULL)
  options(grid_lwd = 0.5)
  options(grid_x_lwd = NULL)
  options(grid_y_lwd = NULL)
  options(grid_lty = "solid")
  options(grid_x_lty = NULL)
  options(grid_y_lty = NULL)

  options(strip_fill = "#7F7F7F37")  # .maketrans("gray50", 55))
  options(strip_color = "gray40")
  options(strip_text_color = "gray15")

  #Plot(Years, Salary, bg="grey85", grid="grey77") on cheap Dell monitor

  options(add_fill = "gray20")
  options(add_trans = 0.0)
  options(add_color = "gray30")
  options(add_cex = 0.75)
  options(add_lwd = 0.5)
  options(add_lty = "solid")

  options(n_cat = 1)
  options(suggest = TRUE)
  options(note = TRUE)
  options(quiet = FALSE)
  options(brief = FALSE)

  options(explain = TRUE)
  options(interpret = TRUE)
  options(results = TRUE)
  options(document = TRUE)
  options(code = TRUE)

  options(show.signifstars = FALSE)
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


.getdigits <- function(x, min_digits) {
  digits_d <- .max.dd(x) + 1
  if (digits_d < min_digits) digits_d <- min_digits
  return(digits_d)
}

# get number of decimal digits, trailing and leading 0's deleted
# x a scalar
.num.dec <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5)
    nchar(strsplit(as.character(x), ".", fixed=TRUE)[[1]][[2]])
  else
    return(0)
}

.fmt <- function(k, d=getOption("digits_d"), w=0, j="right") {
  format(sprintf("%.*f", d, k), width=w, justify=j, scientific=FALSE)
}

# display large number with separating commas
.fmt_cm <- function(k, d=getOption("digits_d")) {
  formatC(k, big.mark=",", format="f", digits=d)
}


.fmt0 <- function(k, d=getOption("digits_d"), w=0) {
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

  isdate <- ifelse("Date" %in% class(x), TRUE, FALSE)

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
  #if (is.integer(type.convert(as.character(d[1:rows,i]))))

  if (is.numeric(x)) {
    x <- na.omit(x)
    int.flg <- ifelse (abs(x-round(x)) < tol, TRUE, FALSE)  # each i of vector
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
  in.global <- FALSE
  if (nchar(var.name)>0) if (var.name %in% ls(name=.GlobalEnv)) {
    in.global <- TRUE
    # a style "var" could be the name of a function
    # var.name is a character string, so convert to an object
    if (!is.function(eval(parse(text=var.name)))) {
      in.global <- TRUE
#      if (!quiet)  # only want to do this if var.name is in a df also
#        cat(">>> Note:", var.name, "from the workspace, not in",
#            "a data frame (table)\n")
    }
  }

  # see if "variable" is really an expression
  if ((grepl("(", var.name, fixed=TRUE) ||
      grepl("[", var.name, fixed=TRUE) ||
      grepl("$", var.name, fixed=TRUE)) && substr(var.name, 1, 1) != "c")  {
    txtA <- paste("A referenced variable in a lessR function can only be\n",
            "a variable name.\n\n", sep="")
    txtB <- "For example, this does not work:\n  > Histogram(rnorm(50))\n\n"
    txtC <- "Instead do this:\n  > Y <- rnorm(50)\n  > Histogram(Y)"
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        txtA, txtB, txtC, "\n")
  }

  # let deprecated mydata work as default
  dfs <- .getdfs() 
  mydata.ok <- FALSE
  if ("mydata" %in% dfs  &&  !("d" %in% dfs)) {
    d <- mydata 
    mydata.ok <- TRUE
  }
  if (!mydata.ok) if (!in.global && from.data) .nodf(dname)

  return(list(ifr=is.frml, fd=from.data, ig=in.global))
}


# get list of data frames in global environment
# include both R data frames and tidyverse tibbles
.getdfs <- function() { 

  objs <- function(x) class(get(x))

  inGlb <- ls(name=.GlobalEnv)
  if (length(inGlb) > 0) {
    dfs <- character(length=0)
    k <- 0
    for (i in 1:length(inGlb)) {
      if (any(class(get(inGlb[i])) == "data.frame")) {
        k <- k + 1
        dfs[k] <- inGlb[i]
      } 
    }

#   mylbl <- which(dfs == "l")
#   if (length(mylbl) > 0) dfs <- dfs[-mylbl]
  }
  else
    dfs <- NULL

  return(dfs)
}


.nodf <- function(dname) {

  # see if df exists (d default), if x from data, not in style Env
  if (!exists(dname, where=.GlobalEnv)) {  # search Global and inside
    dfs <- .getdfs()  # list of data frames in style env
    txtA <- ifelse (dname == "d", ", the default data table name, ", " ")

    if ("D" %in% dfs)
      txtM <- paste("Because you have a data table called D,\n",
        " perhaps you meant to call it d, if so just re-read \n",
        " into d instead of D")
    else
      txtM <- paste(
        "If a data table is not named the default d, then to\n",
        "  analyze the variables in that data table, in the function call\n",
        "  for the analysis specify the actual data table name with\n",
        "  the data option\n",
        "For the data table called ", dfs[1], "\n",
        "  add the following to your function call:  , data=", dfs[1], "\n\n",
        "Or, just re-read the data into the d data table\n\n", sep="")

    if (length(dfs) == 0) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "An analysis is of data values for one or more variables found\n",
        "  in a rectangular data table, with the data values for a \n",
        "  variable located in a column.\n\n",
        "You have not yet read data into a data table for analysis,\n",
        "  so the data table called ", dname, txtA, "is\n",
        "  not available for analysis.\n\n",
        "Read the data into an R data table with the Read function, usually\n",
        "  reading the data into an R data table called d.\n\n",
        "To read a data file on your computer system into the d data\n",
        "  table, in which you browse your file folders to locate the\n",
        "  desired date file, enter:\n",
        "     d <- Read(\"\")\n\n",
        "To specify a data table from your computer or the web, enter:\n",
        "     d <- Read(\"path name\") \n",
        "  or \n",
        "     d <- Read(\"web address\") \n",
        "In the web address include the http:// at the beginning\n",
        "  and also include the quotes around the web address.\n\n")
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
      if (dname == "d") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Data table ", dname, txtA, "does not exist\n\n",
          "Data tables you read and/or created: ", dts, "\n\n",
          "Perhaps you have a data table that contains the variables\n",
          "  of interest to be analyzed, but it is not named ", dname, "\n",
          "Can specify the actual name with the data option\n",
          "For example, for a data table named ", dfs[1], "\n",
          "  add the following to your function call:  , data=", dfs[1], "\n\n",
          "Or, just re-read the data into the d data table\n\n")
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


.in.global <- function(var.name, quiet) {

  # see if "variable" includes a $
  for (i in 1:length(var.name)) {
    if (grepl("$", var.name[i], fixed=TRUE))  {
      txtA <- paste("A referenced variable in a lessR function just includes\n",
                    "the variable name\n\n", sep="")
      txtB <- paste("e.g., for the Histogram function, this does not work:\n",
                    "  > Histogram(d$Y)\n\n", sep="")
      txtC <- "Instead do this:\n  > Histogram(Y, data=d)"
      txtD <- "If you wish to specify a data table, use option: data"
      cat("\n"); stop(call.=FALSE, "\n","------\n",
                      txtA, txtB, txtC, "\n", txtD, "\n\n")
    }
  }

  to.expr <- parse(text=var.name)  # convert char string to expression
  var.nm <- all.vars(to.expr)  # get >= 1 variable names, 1st and last for :

  in.global <- logical(length=length(var.nm))
  for (i in 1:length(var.nm)) {  # each variable in var list one at a time
    in.global[i] <- FALSE
    if (!is.null(var.nm[i])) if (!is.na(var.nm[i])) if (nchar(var.nm[i]) > 0) {
      if (var.nm[i] %in% ls(name=.GlobalEnv)) {
        if (!is.function(eval(parse(text=var.nm[i])))) in.global[i] <- TRUE
      }
    }
    
    if (length(.getdfs()) > 0) {  # if not data frame, no point to message
      if (in.global[i] && !quiet)
         cat(">>> Note:", var.nm[i], "is from the workspace, not in",
         "a data frame (table)\n")
      else
        if (any(in.global) && !quiet)
          cat(">>> Note:", var.nm[i], "is NOT in the workspace\n")
    }
  }  # end for
  
  if (any(in.global) && !all(in.global)) {  # eval $ in .xcheck
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Some variables are in the user workspace,\n",
      "  and other variables are not. All must exist and\n",
      "  be in the workspace or in a data frame, not both.\n\n")
  }
  
  in.global <- ifelse (all(in.global), TRUE, FALSE)
  
  return(in.global)    
}


# check to see if var.name is just a single name
# if yes, then see if it is in the data frame of dname
# nms contains the variable names in dname
.xcheck <- function(var.name, dname, nms) {

  to.expr <- parse(text=var.name)  # convert char string to expression
  var.nm <- all.vars(to.expr)  # get >= 1 variable names, 1st and last for :

  for (i in 1:length(var.nm)) {  # each variable one at a time
    # see if "variable" is an expression
    if (grepl("(", var.nm[i], fixed=TRUE) || grepl("[", var.nm[i],fixed=TRUE)) {
      txtA <- paste("A referenced variable in a lessR function can only be\n",
                    "a variable name\n\n", sep="")
      txtB <- paste("e.g., for the Histogram function, this does not work:\n",
                    "  > Histogram(rnorm(50))\n\n", sep="")
      txtC <- "Instead do this:\n  > Y <- rnorm(50)\n  > Histogram(Y)"
      cat("\n"); stop(call.=FALSE, "\n","------\n",
                      txtA, txtB, txtC, "\n\n")
    }

    # see if variable exists in the data frame
    ind <- which(nms == var.nm[i])
    if (length(ind) == 0) {
      dfs <- .getdfs()  # data frames in style
      txt1 <- ", the default name \n\n"
      txt2 <- "Either make sure to use the correct variable name, or\n"
      txt3 <- "specify the data table that contains the variable with: data=\n"
      txt <- ifelse (dname == "d",  paste(txt1, txt2, txt3, sep=""), "\n")

#      nm <- eval(parse(text=paste("names(", dname,")")))
      nm <- paste(nms, " ")  # add a space for output listing

      if (dname == "d")
        txtDef <- ", which is the default data table name\n"
      else
        txtDef <- ""

      if (length(dfs) == 1) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "You are attempting to analyze the variable ", var.nm[i], " in the\n",
          "  data table called ", dname, txtDef, "\n",
          "Unfortunately, variable ", var.nm[i], " does not exist in ", dname,
          "\n\n",
          "The following variables are currently in the ", dname,
          " data table,\n",
          "  available for analysis:\n\n", "  ", nm, "\n\n")
      }

      else if (length(dfs) > 1) {
        nm2 <- parse(text=paste("names(", dfs[1],")"))
        nm2 <- eval(nm2)
        nm2 <- paste(nm2, " ")
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "You are attempting to analyze the variable ", var.nm[i], " in the\n",
          "  data table called ", dname, txtDef, "\n",
          "Unfortunately, variable ", var.nm[i], " does not exist in ",
          dname, "\n\n",
          "The following variables are currently in the ", dname,
          " data table,\n",
          "  available for analysis:\n\n",
          "  ", nm,  "\n\n",
          "You do have another data table, but it is named ", dfs[1], "\n",
          "The following variables are currently in the ", dfs[1],
          " data table,\n",
          "  available for analysis:\n\n",
          "  ", nm2,  "\n\n",
          "If a data table is not named the default d, then to\n",
          "  analyze the variables in that data table, in the function call\n",
          "  for the analysis specify the actual data table name with\n",
          "  the data option\n",
          "For the data table called ", dfs[1], "\n",
          "  add the following to your function call:  , data=", dfs[1],
          "\n\n", sep="")
      }
    }
  }  # end var by var
}


# see if cor matrix exists as stand-alone or embedded in list structure
.cor.exists <- function(cor.nm) {

  if (!grepl("$R", cor.nm, fixed=TRUE))  # no $R in name
    is.there <- cor.nm %in% ls(name=.GlobalEnv)

  else {
    nm <- sub("$R", "", cor.nm, fixed=TRUE)  # remove $R from name
    if (!(nm %in% ls(name=.GlobalEnv)))  # root list exists?
      is.there <- FALSE
    else
      is.there  <- exists("R", where=eval(parse(text=nm)))  #  R inside?
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
  if (dname %in% ls(name=.GlobalEnv))
    l <- attr(get(dname, pos=.GlobalEnv), which="variable.labels")
  else
    l <- NULL

  if (!is.null(l)) {
    lbl <- l[which(names(l) == var.name)]
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


.varlist2 <- function(n.pred, ind, var.name, pred.lbl, n.obs, n.keep,
                      lvls=NULL) {
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

  if (dname %in% ls(name=.GlobalEnv))
    l <- attr(get(dname, pos=.GlobalEnv), which="variable.labels")
  else
    l <- NULL
  if (dname %in% ls(name=.GlobalEnv))
    myunits <- attr(get(dname, pos=.GlobalEnv), which="variable.units")
  else
    myunits <- NULL

  if (!is.null(l)) {
    lbl <- l[which(names(l) == var.name)]
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
                       lab_x_cex=NULL, lab_y_cex=NULL, labels=l,
                       graph.win=TRUE, flip=FALSE, ...) {

  if (graph.win) {
    fig.width <-  par("fin")[1]
    fig.ht <-  par("fin")[2]
    marg.x <- par("mai")[2] + par("mai")[4]
    axis_x <- fig.width - marg.x
    marg.y <- par("mai")[1] + par("mai")[3]
    axis_y <- fig.ht - marg.y
    cut.x <- 0.90 * axis_x
    cut.y <- 0.95 * axis_y
  }
  else {  # do not open a graphics window if no plot
    cut.x <- 3.75
    cut.y <- 3.75
  }


  # strwidth function not working in regular R, lab_cex has no affect
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

  x.lbl <- NULL
  y.lbl <- NULL

  # let deprecated mylabels work as default
  dfs <- .getdfs() 
  mylabels.ok <- FALSE
  if (!is.null(dfs)) {
    if ("mylabels" %in% dfs  &&  !("l" %in% dfs)) {
      l <- mylabels
      l.name <- "mylabels"
      mylabels.ok <- TRUE
    }
  }
  if (!mylabels.ok)
    l.name <- deparse(substitute(labels))

  if (l.name %in% ls(name=.GlobalEnv)) {
    l <- get(l.name, pos=.GlobalEnv)

    i.row <- which(row.names(l) == x.name)
    if (length(i.row) > 0) if (is.numeric(i.row))
      if (!is.na(l[i.row, ])) x.lbl <- l[i.row,1]

    i.row <- which(row.names(l) == y.name)
    if (length(i.row) > 0) if (is.numeric(i.row))
      if (!is.na(l[i.row, ])) y.lbl <- l[i.row,1]
  }

  else {  # labels embedded in data
    dname <- getOption("dname")  # not set for dependent option on tt
    if (!is.null(dname)) {
      if (dname %in% ls(name=.GlobalEnv)) {
        l <- attr(get(dname, pos=.GlobalEnv), which="variable.labels")
        myunits <- attr(get(dname, pos=.GlobalEnv), which="variable.units")
      }
      else
        l <- NULL
    }
    else
      l <- NULL

    if (!is.null(l)) {
      x.lbl <- l[which(names(l) == x.name)]
      if (length(x.lbl) == 0) x.lbl <- NULL
      y.lbl <- l[which(names(l) == y.name)]
      if (length(y.lbl) == 0) y.lbl <- NULL
    }
  }  # end labels embedded in data

  # ------------------------
  # get x.lab
  st.nya <- ifelse (getOption("sub_theme") == "wsj", TRUE, FALSE)
  if (is.null(x.lbl) && is.null(xlab)) {
    x.lab <- x.name
  }
  else {
    if (!is.null(xlab))
      x.lab <- xlab  # xlab specified
    else if (!is.null(x.lbl))
      x.lab <- x.lbl
  }
  if (is.null(xlab)) if (st.nya) x.lab <- ""

  # get y.lab
  if (is.null(y.lbl) && is.null(ylab))
      y.lab <- y.name
  else {
    if (!is.null(ylab))
      y.lab <- ylab  # ylab specified
    else if (!is.null(y.lbl))
      y.lab <- y.lbl
  }
  if (is.null(ylab)) if (st.nya) y.lab <- ""

  if (flip) {  # is this doing any good???  should it be x.lab???
    temp <- ylab;  ylab <- xlab;  xlab <- temp
    temp <- y.lab;  y.lab <- x.lab;  x.lab <- temp
    temp <- y.lbl;  y.lbl <- x.lbl;  x.lbl <- temp
    temp <- lab_y_cex;  lab_y_cex <- lab_x_cex;  lab_x_cex <- temp
    temp <- y.name;  y.name <- x.name; #  x.name <- temp
  }

  # ------------------------
  # x-axis and legend labels

  if ((!is.null(x.lbl) || !is.null(xlab)) && !st.nya) {
    # power.ttest: len > 1;  # add var name to label?
    if (length(x.lab) == 1  &&  !is.null(lab_x_cex)  &&  graph.win) {
      var.nm <- ifelse (is.null(x.lbl) && !is.null(x.name), FALSE, TRUE)
      if (!is.null(xlab)) var.nm <- FALSE  # xlab is the complete label
      al <- .adjlbl(x.lab, lab_x_cex, cut=cut.x, x.name, var.nm, units="inches")
      x.lab <- al$lab
    }
  }  # end get x.lab

  # ------------------------
  # y-axis and legend labels

    if (!is.null(y.lab)  &&  !st.nya) { # power.ttest: len > 1
    if (length(y.lab) == 1  &&  !is.null(lab_y_cex)  &&  graph.win) {
      var.nm <- ifelse (is.null(y.lbl) && !is.null(y.name), FALSE, TRUE)
      if (!is.null(ylab)) var.nm <- FALSE  # ylab is the complete label
      al <- .adjlbl(y.lab, lab_y_cex, cut=cut.y, y.name, var.nm, units="inches")
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
    sub.lab <- ifelse (!is.null(sub), sub, NULL)
  }
  else
    sub.lab <- NULL

  return(list(xn=x.name, xl=x.lbl, xb=x.lab, yn=y.name, yl=y.lbl, yb=y.lab,
     mb=main.lab, sb=sub.lab, lab_x_cex=lab_x_cex, lab_y_cex=lab_y_cex))
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
  n.lab_ln <- (strw %/% cut) + 1
  if (n.lab_ln < 1) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "No room for axis labels, readjust.\n\n")
  }

  if (strw > cut) {
    line <- character(length=n.lab_ln)
    s <- unlist(strsplit(lab, " "))
    il <- 1
    for (iw in 1:(length(s))) {
      if (strwidth(line[il], units=units, cex=labcex) > cut)
        il <- il + 1
      line[il] <- paste(line[il], s[iw])
    }
    # trim a possible trailing blank line
    if (line[n.lab_ln] == "") line <- line[1:(n.lab_ln-1)]

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

  grid_x_color <- ifelse(is.null(getOption("grid_x_color")),
    getOption("grid_color"), getOption("grid_x_color"))
  grid_y_color <- ifelse(is.null(getOption("grid_y_color")),
    getOption("grid_color"), getOption("grid_y_color"))

  grid_x_lwd <- ifelse(is.null(getOption("grid_x_lwd")),
    getOption("grid_lwd"), getOption("grid_x_lwd"))
  grid_y_lwd <- ifelse(is.null(getOption("grid_y_lwd")),
    getOption("grid_lwd"), getOption("grid_y_lwd"))

  grid_x_lty <- ifelse(is.null(getOption("grid_x_lty")),
    getOption("grid_lty"), getOption("grid_x_lty"))
  grid_y_lty <- ifelse(is.null(getOption("grid_y_lty")),
    getOption("grid_lty"), getOption("grid_y_lty"))

  if (dir == "h") if (grid_y_lwd > 0)
    abline(h=axT, col=grid_y_color, lwd=grid_y_lwd, lty=grid_y_lty)

  if (dir == "v") if (grid_x_lwd > 0)
    abline(v=axT, col=grid_x_color, lwd=grid_x_lwd, lty=grid_x_lty)

}


.axes <- function(x.lvl, y.lvl, axT1, axT2,
         rotate_x=0, rotate_y=0, offset=0.5, y.only=FALSE, ...) {

  axis_x_color <- ifelse(is.null(getOption("axis_x_color")),
    getOption("axis_color"), getOption("axis_x_color"))
  axis_y_color <- ifelse(is.null(getOption("axis_y_color")),
    getOption("axis_color"), getOption("axis_y_color"))

  axis_x_lwd <- ifelse(is.null(getOption("axis_x_lwd")),
    getOption("axis_lwd"), getOption("axis_x_lwd"))
  axis_y_lwd <- ifelse(is.null(getOption("axis_y_lwd")),
    getOption("axis_lwd"), getOption("axis_y_lwd"))

  axis_x_lty <- ifelse(is.null(getOption("axis_x_lty")),
    getOption("axis_lty"), getOption("axis_x_lty"))
  axis_y_lty <- ifelse(is.null(getOption("axis_y_lty")),
    getOption("axis_lty"), getOption("axis_y_lty"))

  axis_x_cex <- ifelse(is.null(getOption("axis_x_cex")),
    getOption("axis_cex"), getOption("axis_x_cex"))
  adj <- .RSadj(axis_cex=axis_x_cex); axis_x_cex <- adj$axis_cex
  axis_y_cex <- ifelse(is.null(getOption("axis_y_cex")),
    getOption("axis_cex"), getOption("axis_y_cex"))
  adj <- .RSadj(axis_cex=axis_y_cex); axis_y_cex <- adj$axis_cex

# if (is.null(getOption("axis_text_color"))) options(axis_text_color = "gray15")
  axis_x_text_color <- ifelse(is.null(getOption("axis_x_text_color")),
    getOption("axis_text_color"), getOption("axis_x_text_color"))
  axis_y_text_color <- ifelse(is.null(getOption("axis_y_text_color")),
    getOption("axis_text_color"), getOption("axis_y_text_color"))

  fnt <- ifelse (getOption("sub_theme") == "wsj", 2, 1) # bold

  usr <- par("usr")

  if (is.null(x.lvl)  &&  !is.null(axT1)) {  # numeric, uses axT1
    if (!y.only) {  # do x axis in calling routine for time series
      axis(1, at=axT1, labels=FALSE, tck=-.01, col=axis_x_color,
        lwd=axis_x_lwd, lty=axis_x_lty)
      dec.d <- .getdigits(round(axT1,6),1) - 1
      axT <- axT1[which(axT1 >= usr[1]  &  axT1 <= usr[2])]
      text(x=axT, y=usr[3], labels=.fmt(axT,dec.d),
           pos=1, xpd=TRUE, cex=axis_x_cex, col=axis_x_text_color,
           srt=rotate_x, offset=offset, font=fnt, ...)
    }
  }

  else if (!is.null(x.lvl)) {  # categorical, uses x.lvl
    axis(1, at=axT1, labels=FALSE, tck=-.01, col=axis_x_color,
        lwd=axis_x_lwd, lty=axis_x_lty)
    text(x=axT1, y=usr[3], labels=x.lvl,
         pos=1, xpd=TRUE, cex=axis_x_cex, col=axis_x_text_color,
         srt=rotate_x, offset=offset, font=fnt, ...)
  }

  if (is.null(y.lvl)  &&  !is.null(axT2)) {
    axis(2, at=axT2, labels=FALSE, tck=-.01, col=axis_y_color,
        lwd=axis_y_lwd, lty=axis_y_lty)
    dec.d <- .getdigits(round(axT2,6),1) - 1
    axT <- axT2[which(axT2 >= usr[3]  &  axT2 <= usr[4])]
    text(x=usr[1], y=axT, labels=.fmt(axT,dec.d),
         pos=2, xpd=TRUE, cex=axis_y_cex, col=axis_y_text_color,
         srt=rotate_y, font=fnt, ...)
  }
  else if (!is.null(y.lvl)) {
    axis(2, at=axT2, labels=FALSE, tck=-.01, col=axis_y_color,
        lwd=axis_y_lwd, lty=axis_y_lty)
    text(x=usr[1], y=axT2, labels=y.lvl,
         pos=2, xpd=TRUE, cex=axis_y_cex, col=axis_y_text_color,
         srt=rotate_y, font=fnt, ...)
  }
}


# axis labels
.axlabs <- function(x.lab, y.lab, main.lab, sub.lab, 
                    x.val=NULL, xy_ticks=TRUE, offset=0.5,
                    lab_x_cex=NULL, lab_y_cex=NULL, main_cex=NULL,
                    n.lab_x.ln=1, n.lab_y.ln=1, xlab_adj=0, ylab_adj=0,
                    ...) {
#   max.lbl <- max(nchar(axTicks(2)))

  lab_x_color <- ifelse(is.null(getOption("lab_x_color")),
    getOption("lab_color"), getOption("lab_x_color"))
  lab_y_color <- ifelse(is.null(getOption("lab_y_color")),
    getOption("lab_color"), getOption("lab_y_color"))

  if (is.null(lab_x_cex)) {  # temp until all .axes calls provide lab_x_cex
    lab_x_cex <- ifelse(is.null(getOption("lab_x_cex")),
      getOption("lab_cex"), getOption("lab_x_cex"))
  }
  if (is.null(lab_y_cex)) {
    lab_y_cex <- ifelse(is.null(getOption("lab_y_cex")),
      getOption("lab_cex"), getOption("lab_y_cex"))
  }

  adj <- .RSadj(lab_cex=lab_x_cex); lab_x_cex <- adj$lab_cex
  adj <- .RSadj(lab_cex=lab_y_cex); lab_y_cex <- adj$lab_cex
  lblx.lns <- par("mar")[1] - 1.15

# xlab_adj <- xlab_adj / ln.ht.x
  # ylab positioning
  ln.ht.y <- par('cin')[2] * lab_y_cex * par('lheight')  # line ht inches
  lby <- (.9*ln.ht.y) / 0.19
  lbly.lns <- par("mar")[2] - (0.3 + 1*n.lab_y.ln) * lby  # mar 2: lm lines
  ylab_adj <- ylab_adj / ln.ht.y

  regR <- FALSE  # regular R by itself adjustment
  in.RStudio <- ifelse (options("device") == "RStudioGD", TRUE, FALSE)
  in.knitr <- ifelse (!is.null(options()$knitr.in.progress), TRUE, FALSE)
  if (!in.RStudio && !in.knitr) regR <- TRUE
  if (regR) ylab_adj <- ylab_adj + .2

  title(xlab=x.lab, line=lblx.lns-xlab_adj,
        col.lab=lab_x_color, cex.lab=lab_x_cex, ...)
  if (!is.null(sub.lab)) 
    title(sub=sub.lab, line=lblx.lns+1-xlab_adj, cex.sub=0.75,
          col.lab=lab_x_color, ...)
  title(ylab=y.lab, line=lbly.lns-ylab_adj+.1,
        col.lab=lab_y_color, cex.lab=lab_y_cex, ...)
  if (!is.null(main.lab)) 
    title(main=main.lab, cex.main= getOption("main_cex"),
          col.main=getOption("main_color"), ...)

}


# get number of lines in value labels
.get.val.ln <- function (val.lab, var.name) {

  ln.val <- integer(length=length(val.lab))

  for (i in seq_along(val.lab)) {
    if (!is.na(val.lab[i])) {
      val.lab[i] <- gsub(" ", "\n", val.lab[i])  # space to new line
      val.lab[i] <- gsub("~", " ", val.lab[i])  # ~ to space
      ln.br <- 0
      for (j in 1:nchar(val.lab[i]))
        if (substr(val.lab[i], j, j)=="\n") ln.br <- ln.br + 1
      ln.val[i] <- ln.br + 1
    }
    else
      val.lab[i] <- "<NA>"  # when y is given and a value of x is missing
  }
  mx.val.ln <- max(ln.val)  # largest number of value label lines

  if (is.infinite(mx.val.ln)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "No value labels, ", var.name, " not properly specified\n\n")
  }

  return(list(val.lab=val.lab, mx.val.ln=mx.val.ln))
}


# margins
.marg <- function(max.y.width, y.lab, x.lab, main, sub,
                  rotate_x=0, mx.x.val.ln=1, mx.y.val.ln=1,
                  lab_x_cex=0.95, lab_y_cex=0.95, max.x.width=NULL) {
# not processing sub at this time

  # top margin
  tm <- 0.05  # old is 0.15
  if (!is.null(main)) tm <- tm + .25
  # if (options("device") == "RStudioGD") {
    # tm <- ifelse(.Platform$OS == "windows", tm-.15, 0)
  # }

  # right margin
  rm <- 0.15

  # bottom margin
  n.lab_x.ln <- 0  # in case x.lab is null
  if (!is.null(x.lab)) {
    if (x.lab != "") {
      strn <- unlist(gregexpr("\n", x.lab, fixed=TRUE))
      if (strn[1] == -1) strn <- NULL  # return of -1 means no \n
      n.lab_x.ln <- length(strn) + 1
    }
    else  # such as from default for time series
      n.lab_x.ln <- -0.6 + (.1 * lab_x_cex)
  }

  ln.ht <- par('cin')[2] * lab_x_cex * par('lheight')  # lin ht inches

  # rotate_x==90 and horiz=TRUE not compatible, leads to NULL max.x.width
  if (rotate_x != 90  ||  is.null(max.x.width))
    bm <- ((n.lab_x.ln + mx.x.val.ln) * .70 * ln.ht) + 0.30  # inches
  else
    bm <- max.x.width + (ln.ht * n.lab_x.ln) + 0.28
  bm <- bm + (-0.065 +(.055* n.lab_x.ln))
  tm <- ifelse (is.null(main), tm+.05, tm+.25)  #  adjust tm for increased bm
  if (rotate_x != 0) bm <- bm + .15
  if (lab_x_cex > 1.1) bm <- bm + .04  # actually should be axis_cex

  # left margin
  n.lab_y.ln <- 0  # in case x.lab is null
  if (!is.null(y.lab)) {
    if (y.lab != "") {
      strn <- unlist(gregexpr("\n", y.lab, fixed=TRUE))
      if (strn[1] == -1) strn <- NULL  # return of -1 means no \n
      n.lab_y.ln <- length(strn) + 1
    }
  }

  mm <- max.y.width + 0.24
  if (max.y.width < .10) mm <- mm + .02
  if (lab_y_cex > 1) mm <- mm + .10
  if (!is.null(y.lab)) mm <- mm + (n.lab_y.ln * .20)

  return(list(lm=mm, tm=tm, rm=rm, bm=bm,
              n.lab_x.ln=n.lab_x.ln, n.lab_y.ln=n.lab_y.ln))
}


# enlarge scale for R
.RSadj <- function(radius=0.25, axis_cex=NULL, cex.names=NULL, lab_cex=NULL) {

  if (is.null(radius)) radius <- 0.25

  regR <- FALSE  # regular R by itself
  in.RStudio <- ifelse (options("device") == "RStudioGD", TRUE, FALSE)
  in.knitr <- ifelse (!is.null(options()$knitr.in.progress), TRUE, FALSE)
  if (!in.RStudio && !in.knitr) regR <- TRUE

  if (regR) {
    radius <- radius*1.6
  }

  if (!is.null(axis_cex))
    size.axis <- ifelse (regR, axis_cex*1.3, axis_cex)
  else
    size.axis <- NULL

   if (!is.null(lab_cex))
    size.lab <- ifelse (regR, lab_cex*1.3, lab_cex)
  else
   size.lab <- NULL

  return(list(radius=radius, axis_cex=size.axis, lab_cex=size.lab))
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

  txt.wrt <- "written at the current working directory." 
  tx[length(tx)+1] <- paste("The", txt, txt.wrt)
  tx[length(tx)+1] <- paste("       ", fname, " in:  ", workdir)

  return(tx)

}


.band.width <- function(x, bw_iter=25, details=FALSE, ...) {

  if (details) {
    cat("\n")
    cat("iterate for smoother density bandwidth (bw)\n")
    cat("flips: number of times densities change sign\n")
    cat("--------------------------------------------\n")
  }

  x <- na.omit(x)
  bw <- bw.nrd0(x)
  irep <- 0
  if (details)
    cat(irep, .fmtc(" ", 10) , "   bw: ", .fmt(bw,4), "\n", sep="")

  repeat {
    irep <- irep + 1
    d.gen <- suppressWarnings(density(x, bw, ...))  # no missing data
    xd <- diff(d.gen$y)

    flip <- 0
    for (j in 2:length(xd))
      if (sign(xd[j-1]) != sign(xd[j])) flip <- flip + 1
    if (flip > 1  &&  irep <= bw_iter) {
      bw <- 1.1 * bw
      if (details)
        cat(irep, "  flips:", .fmti(flip,3), "  bw: ", .fmt(bw,4), "\n", sep="")
    }
    else
      break;
  }  # end repeat

  return(bw)
}


.pdfname <- function(analysis, x.name, go.pdf, pdf.nm, pdf_file) {
  if (go.pdf) {
    if (pdf.nm)
      if (!grepl(".pdf", pdf_file))
        pdf.fnm <- paste(pdf_file, ".pdf", sep="")
      else
        pdf.fnm <- pdf_file
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
    min_dd <- dl.more[which(dl.more==min(dl.more))]
    max.dd <- dl.more[which(dl.more==max(dl.more))]
    for (i in min_dd:max.dd) dev.off(which=i)
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


.opendev <- function(pdf_file, width, height) {

  if (is.null(pdf_file)) {
    if (options("device") != "RStudioGD" &&
        is.null(options()$knitr.in.progress)) {
      .graphwin(1, d.w=width, d.h=height)
      orig.params <- par(no.readonly=TRUE)
      on.exit(par(orig.params))
    }
  }
  else  # windows puts a blank first page without onefile=FALSE
    pdf(file=pdf_file, width=width, height=height, onefile=FALSE)

}


# num.cat var is integer with small number of unique values
.is.num.cat <- function(x, n_cat) {

  x <- sort(unique(na.omit(x)))

  nu.x <- length(x)

  if (.is.integer(x)  &&  nu.x <= n_cat) {
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


.ncat <- function(analysis, x.name, nu, n_cat, brief=FALSE) {

  cat("\n")
  cat(">>> ", x.name, " has only only ", nu, " equally spaced unique ",
      "integer values <= n_cat=", n_cat, "\n",
      "    so treat as categorical, and perhaps convert to an R factor\n", sep="")

  if (!brief)
    cat("    For numeric, set n_cat smaller than ", nu,
        " with ", analysis, " or globally with  style", sep="")

  cat("\n")

}


.corcolors <- function(R, NItems, main, bm=NULL, rm=NULL, diag=NULL,
                       pdf_file, width, height) {

  if (!is.null(diag)) {
    for (i in 1:NItems) R[i,i] <- diag
    cat("\nNote: To provide more color separation for off-diagonal\n",
        "      elements, the diagonal elements of the matrix for\n",
        "      computing the heat map are set to 0.\n", sep="")
  }

  fill_low <- NULL
  fill_hi <- NULL

  thm <- (getOption("theme"))
  if (is.null(fill_low) && is.null(fill_hi)) {      
    if (thm %in% c("colors", "dodgerblue", "blue", "lightbronze")) {
      fill_low <- "rusts"
      fill_hi <- "blues"
      hmcols <- getColors(fill_low, fill_hi, l=c(10,90), n=100, output=FALSE)
    }
    else if (thm %in% c("darkred", "red", "rose", "slatered")) {
      fill_low <- "turquoises" 
      fill_hi <- "reds"
      hmcols <- getColors(fill_low, fill_hi, l=c(10,90), n=100, output=FALSE)
    }
    else if (thm %in% c("darkgreen", "green")) {
      fill_low <- "violets" 
      fill_hi <- "greens"
      hmcols <- getColors(fill_low, fill_hi, l=c(10,90), n=100, output=FALSE)
    }
    else if (thm %in% c("gold", "brown", "sienna")) {
      fill_low <- "blues" 
      fill_hi <- "browns"
      hmcols <- getColors(fill_low, fill_hi, l=c(10,90), n=100, output=FALSE)
    }
    else if (thm %in% c("gray", "white")) {
      fill_low <- "white"
      fill_hi <- "black"
      hmcols <- colorRampPalette(c("white", "gray75", "black"))(100)
    }
  }
  else if (is.null(fill_low) || is.null(fill_hi)) { 
    fill_low <- "white"
    fill_hi <- "gray20"
    hmcols <- colorRampPalette(c("white", "gray75", "black"))(100)
  }

  # fill_low and fill_hi "blues", etc, then divergent, else sequential`

  axis_x_cex <- ifelse(is.null(getOption("axis_x_cex")),
      getOption("axis_cex"), getOption("axis_x_cex"))
  axis_y_cex <- ifelse(is.null(getOption("axis_y_cex")),
      getOption("axis_cex"), getOption("axis_y_cex"))

  cnm <- colnames(R)
  max.num <- max(nchar(cnm))
  mrg <- 1.3 + .38*max.num
  if (is.null(bm)) bm <- mrg
  if (is.null(rm)) rm <- mrg
  if (axis_x_cex > 0.75) axis_x_cex <- axis_x_cex + 1  # hack
  if (axis_y_cex > 0.75) axis_y_cex <- axis_y_cex + 1

  heatmap(R[1:NItems,1:NItems], Rowv=NA, Colv="Rowv", symm=TRUE,
    col=hmcols, margins=c(bm,rm), main=main,
    cexRow=axis_x_cex+.2, cexCol=axis_y_cex+.2)

  if (!is.null(pdf_file)) {  # terminate pdf graphics
    dev.off()
    .showfile(pdf_file, "plot")
  }
}


.maketrans <- function(col.name, trans_level) {

  col.trans <- numeric(length(col.name))

  for (i in 1:length(col.name)) {
    r.tr <- col2rgb(col.name[i])[1]
    g.tr <- col2rgb(col.name[i])[2]
    b.tr <- col2rgb(col.name[i])[3]

    col.trans[i] <- rgb(r.tr, g.tr, b.tr, alpha=trans_level, maxColorValue=256)
  }

  return(col.trans)
}


# from the theme, get the name of the corresponding pre-set color range 
.get_fill <- function(theme=getOption("theme"), seq.pal=TRUE) {

  # for ordinal variables, or color theme not default, get sequential palette
  # for not ordinal and default color theme, qualitative palette
  if (theme == "colors" ) {
    if (seq.pal)
      clrs <- "blues"
    else
      clrs <- getOption("bar_fill_discrete")
  }
  else if (theme %in% c("gray", "white")) clrs <- "grays"
  else if (theme %in% c("lightbronze", "dodgerblue", "blue")) clrs <- "blues"
  else if (theme %in% c("gold", "brown", "sienna")) clrs <- "browns"
  else if (theme == "orange") clrs <- "rusts"
  else if (theme %in% c("darkred", "red", "rose", "slatered")) clrs <- "reds"
  else if (theme %in% c("darkgreen", "green")) clrs <- "greens"
  else if (theme == "purple") clrs <- "violets"
  else clrs <- "blues"

  return(clrs)

}


# from a pre-defined color palette name, generate the palette
# if not a palette name, then return the name, i.e., do nothing
.color_range <- function(fill, n.clr) {

  # names of color palettes generated by getColors
  nm <- c("reds", "rusts", "browns", "olives", "greens", "emeralds",
          "turquoises", "aquas", "blues", "purples", "violets", "magentas",
          "grays", "hues")
  nmR <- c("rainbow", "heat", "terrain")
  nmV<- c("viridis", "cividis", "magma", "inferno", "plasma")
  nmD<- c("distinct")
  nmW<- c("BottleRocket1", "BottleRocket2", "Rushmore1", "Rushmore",
          "Royal1", "Royal2", "Zissou1", "Darjeeling1", "Darjeeling2",
          "Chevalier1", "FantasticFox1", "Moonrise1", "Moonrise2",
          "Moonrise3", "Cavalcanti1", "GrandBudapest1", "GrandBudapest2",
          "IsleofDogs1", "IsleofDogs2")

  # fill is a function such as hcl or is a named vector
  if (is.call(fill) || is.name(fill)) {
    clrs <- eval(fill)
  }

  # or evaluate the character string fill
  else {
    if (!is.null(fill[1])) {
      if (fill[1] == "colors") fill[1] <- "hues"   # new names
      if (fill[1] == "yellows")  fill[1] <- "browns" 

      if (fill[1] %in% nm  ||  fill[1] %in% nmR  ||  fill[1] %in% nmV  ||
          fill[1] %in% nmW  ||  fill[1] %in% nmD) {
        clrs <- getColors(fill[1], n=n.clr, output=FALSE)  # sequential palette
      }
      else {
        clrs <- fill  # not an identified name of a color range
      }

      if (length(fill == 2)) {  # divergent
        if (fill[2] %in% nm)
          clrs <- getColors(fill[1], fill[2], n=n.clr, output=FALSE)
      }
    }  # fill[1] not NULL

    else  # fill[1] is NULL
      clrs <- NULL
  }

  return(clrs)
}


# match a hue to the color theme
.get.h <- function(theme=getOption("theme")) {

       if (theme %in% c("gray", "white")) h=0  # any value for h works
  else if (theme %in% c("colors", "lightbronze", "dodgerblue", "blue")) h <- 240
  else if (theme %in% c("gold", "brown", "sienna")) h <- 60
  else if (theme == "orange") h <- 30
  else if (theme %in% c("darkred", "red", "rose", "slatered")) h <- 0
  else if (theme %in% c("darkgreen", "green")) h <- 120
  else if (theme == "purple") h <- 300
  else h <- 240

  return(h)

}


# continuous color scale
.getColC <- function(x, chroma=55, fill_name) {

  if (getOption("theme") %in% c("gray", "white")) chroma <- 0

  if (!grepl(".v", fill_name, fixed=TRUE)) {
    xp <- pretty(x)
    xp.mn <- min(xp)
    xp.mx <- max(xp)
    xp.rn <- xp.mx - xp.mn

    x.nrm <- (x - xp.mn) / xp.rn

    lum <- 100 - (100*x.nrm)  # scale each value, light to dark flip
    expn <- (82 + (2 * length(x))) / 100
    if (expn > .96) expn <- .96  # hack
    lum <- (lum**expn) + 9  # compress, which darkens, then lighten a bit
    cc <- hcl(h=.get.h(), c=chroma, l=lum)
    clr <- cc
  }

  else {  # (count.v) so do viridis scaling
    vir <- viridisLite::viridis(100)
    x.nrm <- x / max(x)
    cc <- double(length=length(x.nrm))
    for (i in 1:length(cc)) cc[i] <-  viridisLite::viridis(1, begin=x.nrm[i]) 
    clr <- cc
  }

  return(clr)

}


.to_rgb <- function(color) {

  clr <- color[1]  # box_fill is qualitative color scale

  if (is.null(color))
    rgb_color <- "NULL"
  else {  # preserve color name if it exists
    if (!(color[1] %in% colors()))
      rgb_color <- col2rgb(clr, alpha=TRUE)
    else
      rgb_color <- clr
  }

  return(rgb_color)

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


.to256 <- function(trans_level)
   trn <- (1-getOption(trans_level))*256

.to256n <- function(trans_level)
   trn <- (1-trans_level) * 256


# change class call to class character
.fun_call.deparse <- function(fun_call) {

  fc.d <- deparse(fun_call)
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
    fc_new <- sub(remv, "", fc, fixed=TRUE)

  }

  return(fc_new)
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
    fc_new <- sub(remv, "", fc, fixed=TRUE)
    fc_new <- sub(",,", "", fc_new, fixed=TRUE)  # hack

  }

  return(fc_new)
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
    fc_new <- sub(remv, "", fc, fixed=TRUE)

  return(fc_new)
  }

}


.prntbl <- function(x, digits_d=2, cut=0, cc="-", cors=FALSE,
                    brk=NULL, bnd=NULL, v1.nm=NULL, v2.nm=NULL,
                    from_efa=FALSE) {

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
        i.val <- nchar(formatC(x[i,j], digits=digits_d, format="f"))
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

  if (!is.null(cc))
    tx[length(tx)+1] <- .dash2(sum(max.ln)+max.c1, cc=cc)

  # matrix for potentially multi-row column names
  if (max.ch > 0) {
    nr.ind.lbl <- integer(length=ncol(x))
    for (i in 1:ncol(x))
      nr.ind.lbl[i] <- ((nchar(colnames(x)[i]) + (max.ch-1)) %/% max.ch)

    nr.lbl <- max(nr.ind.lbl)  # n_row of labels
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
          cs <- .fmt(x[i,j], d=digits_d, w=wd)
          cs <- sub("0.", "", cs, fixed=TRUE)
          cs <- sub(" 1.00", "100", cs, fixed=TRUE)
        }
        else
          cs <- .fmt(x[i,j], d=digits_d, w=wd)
        wd2 <- ifelse (!from_efa, wd-2, wd)
        if (abs(x[i,j]) < cut) cs <- paste(rep(" ", wd2), collapse="")
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


# debug cat
p <- function(x) {

  xstr <- deparse(substitute(x))
  cat(paste(xstr,":", sep=""), x, "\n")

}

pn <- function(x) {

  xstr <- deparse(substitute(x))
  cat("\n", paste(xstr,":", sep=""), x, "\n")

}
