Read <-
function(from=NULL, format=NULL, in_lessR=FALSE,

         var_labels=FALSE, widths=NULL, stringsAsFactors=FALSE,
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
         "    variable name and then variable label, just set\n",
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
  }

  # option to browse for data file, and then display file name
  browse <- FALSE
  if (is.null(from)) {
    if (options("device") == "RStudioGD")
      cat(">>> Browse window may be hidden behind RStudio window\n")
    browse <- TRUE
    from <- file.choose()
    fncl <- paste("Read(", "from = \"", from,  "\", quiet = TRUE)", sep="")
    fncl2 <- paste("Read(", "from = \"", from,  "\")", sep="")
  }
  else {
    fncl <- .fun_call.deparse(fun_call)
    fncl2 <- .fun_call.deparse(fun_call)  # for read_excel currency bug
  }

  options(read.call=fncl)  # save for knitr run

  if (miss.format) {
         if (in_lessR) format <- "lessR"
    else if (!is.null(widths)) format <- "fwd"
    else if (grepl(".csv$", from)) format <- "csv"
    else if (grepl(".tsv$", from)) format <- "csv"
    else if (grepl(".txt$", from)) format <- "csv"
    else if (grepl(".sav$", from)) format <- "SPSS"
    else if (grepl(".sas7bdat$", from)) format <- "SAS"
    else if (grepl(".rda$", from)) format <- "R"
    else if (grepl(".xls$", from) || grepl(".xlsx$", from)) format <- "Excel"
  }
  if (var_labels && format=="R") {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "An R data file should already contain labels before it was created.\n",
        "Or add them manually with the lessR function: label\n\n")
  }

  if (!quiet) {
    max.chr <- nchar(from)
    if (format == "Excel") {
      txt <- "Alexander Walker's openxlsx package]"
      cat("[with the read.xlsx function from", txt, "\n")
    }

    if (browse) {
      cat("\n")
      cat("Data File:  ", from, "\n")
    }

    # function call for suggestions
    fncl <- .fun_call.deparse(fun_call)

    if (getOption("suggest")) {
      if (brief || !grepl("var_labels", fncl))
        cat("\n>>> Suggestions\n")
      if (!grepl("var_labels", fncl)  &&  format != "lessR")
        cat("To read a csv or Excel file of variable labels, var_labels=TRUE\n",
            "  Each row of the file:  Variable Name, Variable Label\n")
      if (brief) {
        cat("Details about your data, Enter:  details()  for d, or",
           " details(name)\n")
        if (options("device") == "RStudioGD")
          cat("To view your data, Enter:  View(name)  such as ",
          " View(d)\n")
      }
      cat("\n")
    }
  }


  # read the data (into object d)
  # ---------------------__------

  if (format %in% c("fwd", "csv")) {  # text file

    if (format=="fwd")
      d <- read.fwf(file=from, widths=widths, ...)

    else if (format=="csv") {
      line1 <- scan(from, what="character", nlines=1, sep="\t", quiet=TRUE)
      if (length(line1) > 1) {
        message(">>> A tab character detected in the first row of the\n",
            "     data file. Presume tab delimited data.\n", sep="")
        delim <- "\t"
      }
      else
        delim <- ","
      if (!var_labels)
        d <- read.csv(file=from, na.strings=missing, sep=delim,
                      stringsAsFactors=stringsAsFactors, ...)
      else
        d <- read.csv(file=from, header=FALSE,
                  row.names=1, col.names=c("x", "label"),
                  sep=delim, stringsAsFactors=FALSE, ...)
    }

  }  # end text file

  else if (format == "Excel") {

    if (!var_labels)
      d <- openxlsx::read.xlsx(from, sheet=sheet, detectDates=TRUE, ...)
    else {
      d <- openxlsx::read.xlsx(from, sheet=sheet,
                               colNames=FALSE, rowNames=TRUE)
      names(d) <- "label" 
    }
#     fnu.col <- logical(length=ncol(d))  # reset to FALSE
#     for (i in 1:ncol(d)) if (.is.date(d[,i])) fnu.col[i] <- TRUE
#     d[fnu.col] <- lapply(d[fnu.col], function(x) x <- x+1)  # read.xlsx bug?
    # d[fnu.col] <- lapply(d[fnu.col], type.convert) # as in read.csv

    # d <- readxl::read_excel(path=from, sheet=sheet)
    if (!is.null(list(...)$row.names))  # add any row.names to data frame
      d <- data.frame(d, row.names=list(...)$row.names)
    # class(d) <- "data.frame"  # otherwise nonstandard class from read_excel

      # if true integer, then convert from type double to integer
      rows <- min(50, nrow(d))  # save some time scanning
      fnu.col <- logical(length=ncol(d))
      for (i in 1:ncol(d))
        if (is.double(d[,i]))
          if (is.integer(type.convert(as.character(d[1:rows,i]))))
            fnu.col[i] <- TRUE
       d[fnu.col] <- lapply(d[fnu.col], as.integer) # move to integer

  }  # end (format == "Excel")

  else if (format == "SPSS")  # data and any labels
    d <- read.spss(file=from, to.data.frame=TRUE, use.value_labels=TRUE, ...)

  else if (format == "SAS") { # data
    d <- read.sas7bdat(file=from, ...)
    txt <- "Matt Shotwell's sas7bdat package]"
    cat("[with the read.sas7bdat function from", txt, "\n")
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
        "To view the list of data files, enter  > Help(lessR)\n",
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


  # feedback
  # --------
  if (!quiet  &&  is.data.frame(d))
    details(d, n_mcut, miss_zero, max_lines, miss_show,
                      miss_matrix, brief)
  else
    cat("\n")

  return(d)

}

