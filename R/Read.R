Read <-
function(from=NULL, format=NULL, var_labels=FALSE,

         widths=NULL, stringsAsFactors=FALSE,

         missing="", n_mcut=1,
         miss_show=30, miss_zero=FALSE, miss_matrix=FALSE,

         max_lines=30, sheet=1,

         brief=TRUE, quiet=getOption("quiet"),

         fun_call=NULL, ...) {

        
  # a dot in a parameter name to an underscore
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    change <- c("in.lessR", "var.labels", "n.mcut", "miss.show", "miss.zero",
                "miss.matrix", "max.lines", "fun.call")
    for (i in 1:length(dots)) {
      if (names(dots)[i] %in% change) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }

  if (is.null(fun_call)) fun_call <- match.call()

  if (!is.null(from))
    if (nchar(from) == 0) from <- NULL  #  "" to NULL

  if (hasArg(labels)) {
     cat("\n"); stop(call.=FALSE, "\n","------\n",
         ">>> To read a csv or Excel file of variable labels, each row a\n",
         "    variable name and then variable label, and then optionally\n",
         "    a variable unit in the third column, just set\n",
         "    var_labels=TRUE in the Read statement\n\n")
  }

  .param.old(...)

  miss.format <- ifelse (missing(format), TRUE, FALSE)
  fmts <- c("text", "csv", "tsv", "Excel", "R", "SPSS", "SAS")
  if (!is.null(format)) {
    if (!(format %in% c(fmts, "lessR"))) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
          ">>> Specified format must be one of:\n",
               "\"text\", \"Excel\", \"R\", \"SPSS\", \"SAS\",\n\n")
    }
    if (format %in% c("text", "tsv")) format <- "csv"

    if (is.null(from) && format=="lessR") {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Cannot browse for a data file that is part of lessR.\n",
          "Specify the file name.\n\n")
    }
  }  # end !is.null(format)

  # option to browse for data file, and then display file name
  browse <- FALSE
  if (is.null(from)) {
    if (options("device") == "RStudioGD")
      cat(">>> Browse window may be hidden behind RStudio window\n")
    browse <- TRUE
    from <- file.choose()
    fncl <- paste("Read(", "from = \"", from,  "\", quiet = TRUE)", sep="")
#   fncl2 <- paste("Read(", "from = \"", from,  "\")", sep="")
  }
  else {
    fncl <- .fun_call.deparse(fun_call)
#   fncl2 <- .fun_call.deparse(fun_call)  # for read_excel currency bug
  }

  if (!var_labels) options(read.call=fncl)  # save only data file for knitr run

  if (miss.format) {
         if (!is.null(widths)) format <- "fwd"
    else if (grepl(".csv$", from)) format <- "csv"
    else if (grepl(".tsv$", from)) format <- "csv"
    else if (grepl(".txt$", from)) format <- "csv"
    else if (grepl(".sav$", from)) format <- "SPSS"
    else if (grepl(".zsav$", from)) format <- "SPSS"
    else if (grepl(".dta$", from)) format <- "Stata"
    else if (grepl(".rda$", from)) format <- "R"
    else if (grepl(".xls$", from) || grepl(".xlsx$", from)) format <- "Excel"
    else if (grepl(".sas7bdat$", from)) format <- "SAS"

    if (is.null(format)) {
      if (from %in% c("BodyMeas", "Cars93",
                      "Employee", "Employee_lbl", "Mach4", "Mach4_lbl",
                      "Jackets", "Learn", "Reading", "StockPrice")) {
        format <- "lessR"  # presume exact name not exist in current wd
      }
      else { 
        cat("\n"); stop(call.=FALSE, "\n","------\n",
            "Need to specify  format  or specify the name of\n",
            "  a built-in lessR data file.\n\n")
      }
    }
  }  # end miss format

  if (var_labels && format=="R") {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "An R data file should already contain labels before it was created.\n",
        "Or add them manually with the lessR function: label\n\n")
  }

  if (!quiet) {
    max.chr <- nchar(from)
    if (format == "Excel"  &&  !quiet) {
      txt <- "Alexander Walker's openxlsx package]"
      cat("[with the read.xlsx function from", txt, "\n")
    }

    if (browse) {
      cat("\n")
      cat("Data File:  ", from, "\n")
    }
  }


  # read the data (into object d)
  # -----------------------------

  if (format %in% c("SPSS", "SAS", "Stata")) {
    if (!requireNamespace("haven", quietly=TRUE)) {
      stop("Package \"haven\" needed to read this file\n",
           "Please install it:  install.packages(\"haven\")\n\n",
           call. = FALSE)
    }
  }

  if (format %in% c("fwd", "csv")) {  # text file

    if (format=="fwd")
      d <- read.fwf(file=from, widths=widths, ...)

    else if (format=="csv") {

      # get delimiter
      line1 <- scan(from, what="character", nlines=1, sep="\t", quiet=TRUE)
      if (length(line1) > 1) {
        message(">>> A tab character detected in the first row of the\n",
            "     data file. Presume tab delimited data.\n", sep="")
        delim <- "\t"
      }
      else
        delim <- ","

      # read data or labels/units  
      if (!var_labels)
        d <- read.csv(file=from, na.strings=missing, sep=delim,
                      stringsAsFactors=stringsAsFactors, ...)
      else {
        d <- read.csv(file=from, header=FALSE,
                  row.names=1, 
                  sep=delim, stringsAsFactors=FALSE, ...)
        names(d)[1] <- "label" 
        if (ncol(d) == 2) names(d)[2] <- "unit" 
      }
   }  # end csv

 }  # end text file

  else if (format == "Excel") {

    if (!var_labels)
      d <- openxlsx::read.xlsx(from, sheet=sheet, detectDates=TRUE, ...)

    else {
      d <- openxlsx::read.xlsx(from, sheet=sheet,
                               colNames=FALSE, rowNames=TRUE)
      names(d)[1] <- "label" 
      for (i in 1:nrow(d)) if (is.na(d[i,1])) d[i,2] <- ""

      if (ncol(d) == 2) {
         names(d)[2] <- "unit" 
         for (i in 1:nrow(d)) if (is.na(d[i,2])) d[i,2] <- ""
      }
    }
#     fnu.col <- logical(length=ncol(d))  # reset to FALSE
#     for (i in 1:ncol(d)) if (.is.date(d[,i])) fnu.col[i] <- TRUE
#     d[fnu.col] <- lapply(d[fnu.col], function(x) x <- x+1)  # read.xlsx bug?
#     d[fnu.col] <- lapply(d[fnu.col], type.convert) # as in read.csv

    if (!is.null(list(...)$row.names))  # add any row.names to data frame
      d <- data.frame(d, row.names=list(...)$row.names, stringsAsFactors=TRUE)

    # if true integer, then convert from type double to integer
    rows <- min(50, nrow(d))  # save some time scanning
    fnu.col <- logical(length=ncol(d))
    for (i in 1:ncol(d))
      if (.is.integer(d[1:rows,i]))
        fnu.col[i] <- TRUE
     d[fnu.col] <- lapply(d[fnu.col], as.integer) # move to integer

  }  # end (format == "Excel")

  else if (format == "SPSS") {  # data and any labels
    cat("[with read_spss() from the haven package]\n")
    dt <- haven::read_spss(file=from, ...)  # a tibble with label attribute
    d <- data.frame(row.names=1:nrow(dt))
    d <- d[1:nrow(dt), ]
    i.new <- 0
    for (i in 1:ncol(dt)) {
      i.new <- i.new + 1
      d[[i.new]] <- dt[[i]]
      names(d)[i.new] <- names(dt)[i]
      if ("haven_labelled" %in% class(dt[[i]])) {
        if (.is.integer(d[[i.new]]))
          d[[i.new]] <- as.integer(d[[i.new]])  # to standard R integer var
        else
          d[[i.new]] <- as.numeric(d[[i.new]])
        i.new <- i.new + 1
        d[[i.new]] <- haven::as_factor(dt[[i]])   # labels to a factor var
        names(d)[i.new] <- paste(names(dt)[i], "_f", sep="")
      }
    }
    haven <- FALSE
    for (i in 1:ncol(dt)) if  ("haven_labelled" %in% class(dt[[i]]))
      haven <- TRUE
    if (haven) {
      cat("\nVariable and Variable Label  --> See vignette(\"Read\"),",
          "SPSS section\n")
      cat(.dash(27))
      for (i in 1:ncol(dt)) {
        if ("haven_labelled" %in% class(dt[[i]])) {
          lbl <- attr((dt[[i]]), "label")
          cat(paste(names(dt)[i], ", ", sep=""), lbl, "\n")
          cat(paste(names(dt)[i], "_f, ", sep=""), lbl, "\n")
        }
      }
    }  # end haven
    rm(dt)  # no longer need the tibble that was read
  }  # end SPSS

  else if (format == "SAS"  &&  !quiet) { # data
    cat("[with read_stata() from the haven package]\n")
    d <- haven::read_sas(data_file=from, ...)
  }

  else if (format == "Stata"  &&  !quiet) { # data
    cat("[with read_stata() from the haven package]\n")
    d <- haven::read_stata(file=from, ...)
  }

  else if (format == "R") {  # data and any labels
    x.env <- new.env()  # scratch environment
    load(from, envir=x.env)
    dname <- ls(x.env)
    d <- get(dname, pos=x.env)
  }

  else if (format == "lessR") {  # data and any labels
    txt <- ifelse (substr(from,1,4) == "data", "", "data")
    file.name <- paste(txt, from, ".rda", sep="")

    path.name <- paste(find.package("lessR"), "/data/",  file.name, sep="")

    if (!file.exists(path.name)) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "No lessR data file with that name.\n\n",
        "To view the list of lessR data files,\n",
        "    enter  > help(package=lessR)\n",
        "The data file names begin with  'data.'\n\n")
    }

    x.env <- new.env()  # scratch environment
    load(path.name, envir=x.env)

    dname <- paste(txt, from, sep="")
    d <- get(dname, pos=x.env)
  }  # end (format == "lessR")

  # check for valid characters in the variable names
  if (format %in% c("csv", "Excel")) {
    dg <- character(length=10)
    for (i in 0:9) dg[i+1] <- as.character(i)
    legal <- c(letters, LETTERS, dg, "_", ".", "(", ")")

    # get rid of variables that are not named (NA's)
    nm.miss <- is.na(names(d))
    d[nm.miss] <- NULL

    badchar <- FALSE
    for (i in 1:length(names(d))) {
      cc <- names(d)[i]
      if (!is.na(cc)) {
        for (j in 1:nchar(cc)) {
          if (!(substr(cc,j,j) %in% legal)) {
            badchar <- TRUE
            names(d)[i] <- gsub(substr(cc,j,j), "", names(d)[i])
            if (substr(cc,j,j) == " ")
              txt <- "Removed the blank space,"
            else
              txt <- paste("Removed character, ", substr(cc,j,j), ",", sep="")
            cat(txt, "new variable name: ", names(d)[i], "\n")
          }  # char not legal  
        }  # sequence through characters of ith name
      }  # cc cannot be missing
      if (substr(cc,1,1) == ".") {  # remove any beginning .
         while(substr(cc,1,1) == ".") cc <- substr(cc, 2, nchar(cc))
         cat("\nRemoved leading dots of variable name:", names(d)[i], "\n")
      }
    }
    if (badchar)
      cat("\nR only allows letters, digits and . or  _ in variable names\n\n")
  }


  # function call for suggestions
  fncl <- .fun_call.deparse(fun_call)

  if (!quiet) {
    if (getOption("suggest")) {
      if (brief || !grepl("var_labels", fncl))
        cat("\n>>> Suggestions\n")
      if (!grepl("var_labels", fncl)  &&  format != "lessR")
        cat("To read a csv or Excel file of variable labels, var_labels=TRUE\n",
            "  Each row of the file:  Variable Name, Variable Label\n")
      if (brief) {
        cat("Details about your data, Enter:  details()  for d, or",
           " details(name)\n")
      }
      cat("\n")
    }
  }


  # feedback
  # --------
  if (!quiet  &&  is.data.frame(d))
    details(d, n_mcut, miss_zero, max_lines, miss_show,
                      miss_matrix, brief)
  else
    cat("\n")

  return(d)

}

