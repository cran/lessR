Read <-
function(from=NULL, format=NULL, var_labels=FALSE, widths=NULL,

         missing=NULL, n_mcut=1,
         miss_show=30, miss_zero=FALSE, miss_matrix=FALSE,

         max_lines=30, sheet=1, row_names=NULL, header=TRUE,

         brief=TRUE, quiet=getOption("quiet"),

         fun_call=NULL, ...) {


  miss.format <- ifelse (missing(format), TRUE, FALSE)
  miss.header <- ifelse (missing(header), TRUE, FALSE)


  # a dot in a parameter name to an underscore
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    change <- c("var.labels", "n.mcut", "miss.show", "miss.zero",
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
         "    a variable unit in the third column, set  var_labels=TRUE \n",
         "    in the Read statement and read into the l data frame\n\n")
  }

  .param.old(...)

  fmts <- c("text", "csv", "tsv", "Excel", "ODS", "R", "SPSS", "SAS", "Stata",
            "feather", "parquet")
  if (!is.null(format)) {
    if (!(format %in% c(fmts, "lessR"))) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
          ">>> Specified format must be one of:\n",
               "\"text\", \"Excel\", \"ODS\", \"R\", \"feather\", \"parquet\",
                \"SPSS\", \"SAS\"\n\n")
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
    browse <- TRUE
    from <- file.choose()
    fncl <- paste("Read(", "from = \"", from,  "\", quiet = TRUE)", sep="")
  }
  else {
    fncl <- .fun_call.deparse(fun_call)
  }

  # save only data file for knitr run
  # reading from "Employee_lbl" does not require var_labels
  if (!var_labels && !grepl("_lbl", from, fixed=TRUE))
    options(read.call=fncl)

  if (miss.format) {
         if (!is.null(widths)) format <- "fwd"
    else if (grepl(".csv$", from)) format <- "csv"
    else if (grepl(".tsv$", from)) format <- "tsv"
    else if (grepl(".prn$", from)) format <- "prn"
    else if (grepl(".txt$", from)) format <- "txt"  # comma or tab separated
    else if (grepl(".sav$", from)) format <- "SPSS"
    else if (grepl(".zsav$", from)) format <- "SPSS"
    else if (grepl(".dta$", from)) format <- "Stata"
    else if (grepl(".rda$", from)) format <- "R"
    else if (grepl(".sas7bdat$", from)) format <- "SAS"
    else if (grepl(".ods", from)) format <- "ODS"
    else if (grepl(".xls$", from) || grepl(".xlsx$", from)) format <- "Excel"
    else if (grepl(".feather", from)) format <- "feather"
    else if (grepl(".parquet", from)) format <- "parquet"

    if (is.null(format)) {
      df <- list.files(system.file("data", package="lessR"))
      df <- df[!(df %in%"dataFreqTable99.rda")]
      df <- gsub(".rda", "", df, fixed=FALSE)
      df <- gsub("data", "", df, fixed=FALSE)
      if (from %in% df) {
        format <- "lessR"
        if (grepl("lbl", from, fixed=TRUE)) var_labels <- TRUE
      }
      else {
        df <- paste(" ", df)
        cat("\n"); stop(call.=FALSE, "\n","------\n",
            "Not a recognizable data file. Specify the  format parameter\n",
            "  to identify the file type or specify the name of\n",
            "  a built-in lessR data file without the data prefix.\n\n",
            "Available lessR data files:\n\n", df, "\n\n")
      }
    }
  }  # end miss format

  if (var_labels && format=="R") {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "An R data file should already contain labels before it was created.\n",
        "Or add them manually with the lessR function: label\n\n")
  }

  # give credit
  # -----------

  if (!quiet) {
    if (format == "Excel") {
      txt <- "Schauberger and Walker's openxlsx package]"
      cat("[with the read.xlsx() function from", txt, "\n")
    }
    if (format == "ODS") {
      txt <- "Schutten and Chan's readODS package]"
      cat("[with the read_ods() function from", txt, "\n")
    }
    if (format == "feather") {
      txt <- "Richardson's et al. arrow package]"
      cat("[with the read_feather() function from", txt, "\n")
    }
    if (format == "parquet") {
      txt <- "Richardson's et al. arrow package]"
      cat("[with the read_parquet() function from", txt, "\n")
    }

    if (browse) {
      cat("\n")
      cat("Data File:  ", from, "\n\n")
    }
  }


  # see if needed package is installed
  # ----------------------------------

  if (format %in% c("SPSS", "SAS", "Stata")) {
    if (!requireNamespace("haven", quietly=TRUE)) {
      stop("Packages \"haven\" and \"tzdb\" needed to read this file\n",
           "Please install them:  install.packages(c(\"haven\", \"tzdb\"))\n\n",
           call. = FALSE)
    }
  }

  if (format == "ODS") {
    if (!requireNamespace("readODS", quietly=TRUE)) {
      stop("Package \"readODS\" needed to read this file\n",
           "Please install:  install.packages(\"readODS\")\n\n",
           call. = FALSE)
    }
  }

  if (format %in% c("feather", "parquet")) {
    if (!requireNamespace("arrow", quietly=TRUE)) {
      stop("Package \"arrow\" needed to read this file\n",
           "Please install:  install.packages(\"arrow\")\n\n",
           call. = FALSE)
    }
  }


  # get type of txt file
  # --------------------
  if (format == "txt") {  # see if tab delimiter
    line1 <- scan(from, what="character", nlines=1, sep="\t", quiet=TRUE)
    format <- "csv"
    if (length(line1) > 1) {
      message(">>> A tab character detected in the first row of the\n",
          "     data file. Presume tab delimited data.\n", sep="")
      format <- "tsv"
    }
  }


  # read the data (into object d)
  # -----------------------------

  if (format=="fwd")
    d <- read.fwf(file=from, widths=widths, ...)


  else if (format %in% c("csv", "tsv", "prn")) {

    if (is.null(missing)) {
      if (format %in% c("csv", "tsv")) missing <- " "
    }
    else if (missing == "prn") missing <- "NA"

    # read data or labels/units
    if (!var_labels) {
      if (missing(row_names)) {
        row.names <- NULL
        if (format == "csv")
          d <- read.csv(file=from, na.strings=missing, header=header,...)
        else if (format == "tsv")
          d <- read.delim(file=from, na.strings=missing, header=header, ...)
        else if (format == "prn")
          d <- read.table(file=from, na.strings=missing, header=header, ...)
      }
      else {  # row names
        if (is.logical(row_names)) {  # can specify as TRUE
          if (row_names) row.names <- 1
        }
        else
          row.names <- row_names
        if (format == "csv")
          d <- read.csv(file=from, na.strings=missing, row.names=row.names, ...)
        else if (format == "tsv")
          d <- read.delim(file=from, na.strings=missing,
                          row.names=row.names, ...)
      }  # end row names
    }  # not reading var labels

    else {  # read var_labels
      d <- read.csv(file=from, header=FALSE, row.names=1, ...)
      names(d)[1] <- "label"
      if (ncol(d) == 2) names(d)[2] <- "unit"
    }
  }  # end csv etc.


  else if (format %in% c("feather", "parquet")) {  # arrow file

    if (format=="feather")
      d <- arrow::read_feather(file=from, ...)
    else if (format=="parquet")
      d <- arrow::read_parquet(file=from, ...)
    d <- data.frame(d)  # default is a tibble
    if (names(d)[1] == "RowName") {  # when created with Write()
      rownames(d) <- d$RowName
      d$RowName <- NULL
    }
  }


  else if (format == "ODS") {

    if (!var_labels) {
      if (missing(row_names))
        row_names <- FALSE
      else {
        if (row_names == 1) row_names <- TRUE
      }
      if (row_names)
        d <- readODS::read_ods(from, sheet=sheet, row_names=TRUE,
                               as_tibble=FALSE, ...)
      else
        d <- readODS::read_ods(from, sheet=sheet, row_names=FALSE, ...)
      d <- data.frame(d)  # default is a tibble
    }


    else {
      d <- readODS::read_ods(from, sheet=sheet,
                               col_names=FALSE, row_names=TRUE)
      names(d)[1] <- "label"
      for (i in 1:nrow(d)) if (is.na(d[i,1])) d[i,2] <- ""

      if (ncol(d) == 2) {
         names(d)[2] <- "unit"
         for (i in 1:nrow(d)) if (is.na(d[i,2])) d[i,2] <- ""
      }
    }
  }   # end (format == "ODS")


  else if (format == "Excel") {

    if (!var_labels) {
      row.names <- ifelse (missing(row_names), FALSE, TRUE)
      d <- openxlsx::read.xlsx(from, sheet=sheet, detectDates=TRUE,
              rowNames=row.names, ...)
    }

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
    d <- data.frame(dt)
    i.new <- 0
    for (i in 1:ncol(dt)) {
      i.new <- i.new + 1
      d[[i.new]] <- dt[[i]]
      names(d)[i.new] <- names(dt)[i]
      if ("haven_labelled" %in% class(dt[[i]])) {
        i.new <- i.new + 1
        d[[i.new]] <- haven::as_factor(dt[[i]])   # labels to a factor var
        names(d)[i.new] <- paste(names(dt)[i], "_f", sep="")
      }
    }  # end col by col 
    if (names(d)[1] == "RowName") {  # when created with Write()
      rownames(d) <- d$RowName
      d$RowName <- NULL
    }
    d[] <- lapply(d, function(col) {
      if (is.character(col)) col[col == ""] <- NA
      col
    })
    d[] <- lapply(d, function(col) {
      if (.is.integer(col)) col <- as.integer(col)
      col
    })

    haven <- FALSE
    for (i in 1:ncol(dt)) if ("haven_labelled" %in% class(dt[[i]]))
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
    cat("[with read_sas() from the haven package]\n")
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

    if (!file.exists(path.name)) {  # manually had specified  format="lessR"
      df <- list.files(system.file("data", package="lessR"))
      df <- df[!(df %in%"dataFreqTable99.rda")]
      df <- gsub(".rda", "", df, fixed=FALSE)
      df <- gsub("data", "", df, fixed=FALSE)
      if (from %in% df)
        format <- "lessR"  # presume exact name not exist in current wd
      else {
        df <- paste(" ", df)
        cat("\n"); stop(call.=FALSE, "\n","------\n",
            "Need to specify  format  or specify the name of\n",
            "  a built-in lessR data file without the data prefix.\n\n",
            "Available lessR data files:\n\n", df, "\n\n")
      }
    }

    x.env <- new.env()  # scratch environment
    load(path.name, envir=x.env)

    dname <- paste(txt, from, sep="")
    d <- get(dname, pos=x.env)
  }  # end (format == "lessR")



  # check for valid characters in the variable names for text files
  # ---------------------------------------------------------------

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
            if (!quiet) cat(txt, "new variable name: ", names(d)[i], "\n")
          }  # char not legal
        }  # sequence through characters of ith name
      }  # cc cannot be missing
      if (substr(cc,1,1) == ".") {  # remove any beginning .
         while(substr(cc,1,1) == ".") cc <- substr(cc, 2, nchar(cc))
         if (!quiet)
           cat("\nRemoved leading dots of variable name:", names(d)[i], "\n")
      }
    }
    if (badchar && !quiet)
      cat("\nR only allows letters, digits and . or  _ in variable names\n\n")
  }


  # function call for suggestions
  # -----------------------------
  fncl <- .fun_call.deparse(fun_call)

  if (!quiet) {
    if (getOption("suggest")) {
      if (brief || !grepl("var_labels", fncl))
        cat("\n>>> Suggestions\n")
        cat("Recommended binary format for data files: feather\n",
            " Create with Write(d, \"your_file\", format=\"feather\")\n")
        if (!grepl("var_labels", fncl)  &&  format != "lessR")
          cat("To read a csv or Excel file of variable labels",
              "var_labels=TRUE\n",
              "  Each row of the file:  Variable Name, Variable Label\n",
              "Read into a data frame named l  (the letter el)\n\n", sep="")
      if (brief) {
        cat("More details about your data, Enter:  details()  for d, or",
           " details(name)\n")
      }
      cat("\n")
    }
  }


  # feedback
  # --------
  if (!quiet  &&  is.data.frame(d))
    details(d, n_mcut, max_lines, miss_show,
                      miss_matrix, var_labels, brief)
  else
    cat("\n")

  return(d)

}

