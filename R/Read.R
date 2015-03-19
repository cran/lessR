Read <- 
function(ref=NULL, format=c("csv", "SPSS", "R", "Excel", "lessR"),

         labels=NULL, widths=NULL, missing="", n.mcut=1, 

         miss.show=30, miss.zero=FALSE, miss.matrix=FALSE, 
      
         max.lines=30, sheet=1,

         brief=getOption("brief"), quiet=getOption("quiet"), ...) {


  format <- match.arg(format)

  if (is.null(ref) && format=="lessR") {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Cannot browse for a data file that is part of lessR.\n",
        "Specify the file name.\n\n")
  }

# option to browse for data file, and then display file name
  cat("\n")
  if (is.null(ref)) {
    ref <- file.choose()
    cat("\n")
  }

  options(data.file=ref)  # save for knitr run

  if (grepl(".sav$", ref)) format <- "SPSS" 
  if (grepl(".rda$", ref)) format <- "R" 
  if (grepl(".xls$", ref) || grepl(".xlsx$", ref)) format <- "Excel" 
  if (!is.null(widths)) format <- "fwd"

  # construct full path name for label file if not already
  if (!is.null(labels)) {
    if (labels == "")
      ref.lbl <- file.choose()
    else {
      if (labels!="row2") {
        if (!grepl("/", labels) && !grepl("\\\\", labels)) {  # not full path
          nc <- nchar(ref)
          ch <- substr(ref, start=1, stop=nc)
          n.gone <- 0
          while ((ch != "/")  &&  (ch != "\\")) {
            n.gone <- n.gone + 1 
            ch <- substr(ref, start=nc-n.gone, stop=nc-n.gone)
          }
          ref.lbl <- substr(ref, start=1, stop=nc-n.gone)
          ref.lbl <- paste(ref.lbl, labels, sep="")
        }
        else
          ref.lbl <- labels
      }
      else
        ref.lbl <- "labels in second row of data file"
    }
  }

  if (!quiet) {
    max.chr <- nchar(ref)
    if (!is.null(labels))
      if (nchar(ref.lbl) > max.chr) max.chr <- nchar(ref.lbl)
    .dash(max.chr + 14)
    cat("Data File:   ", ref, "\n")
    if (!is.null(labels))  cat("Label File:  ", ref.lbl, "\n")
    .dash(max.chr + 14)
  }

  # see if labels=="row2"
  if (is.null(labels))
    isnot.row2 <- TRUE
  else
    if (labels != "row2") isnot.row2 <- TRUE else isnot.row2 <- FALSE

  # do the read
  # -----------

  if (format=="fwd" || format=="csv") {  # text file

    if (format=="fwd")
      data <- read.fwf(file=ref, widths=widths, ...)

    else if (format=="csv") {
      line1 <- scan(ref, what="character", nlines=1, sep="\t", quiet=TRUE)
      if (length(line1) > 1) {
        message(">>> A tab character detected in the first row of the data file.\n",
            "    Presume tab delimited data.\n", sep="")
        delim <- "\t"
      }
      else
        delim <- ","
      if (isnot.row2)  # read data
         data <- read.csv(file=ref, na.strings=missing, sep=delim, ...)
    }

  }  # end text file
      
  else if (format == "Excel") { 
    txt <- "Warnes, Rogers and Grothendiek's gdata package]"
    cat("[with the read.xls function from", txt, "\n") 
    perl.test <- Sys.which("perl")
    if (nchar(perl.test) > 0) {
      if (isnot.row2)  # read data
        data <- read.xls(xls=ref, sheet=sheet,
                na.strings=c("NA","#DIV/0!", ""), ...)
    }
    else {  # no Perl
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "To read an Excel file, Read relies upon the read.xls function\n",
        "from the gdata package, which requires the Perl scripting language.\n",
        "Install the 64-bit Perl if running the 64-bit version of R.\n\n",
        "To install (Strawberry) Perl on Windows and make accessible to R:\n",
        " 1. Download and install Perl from: http://strawberryperl.com/\n",
        " 2. Copy and paste the following function calls into the R console:\n",
        "      library(gdata)\n      ",
        "installXLSXsupport(perl = 'C:\\\\strawberry\\\\perl\\\\bin\\\\perl.exe')\n",
        " 3. Restart R.\n\n")
    }
  }

  if (!is.null(labels)) {  # process labels
    if (format %in% c("fwd", "csv", "Excel")) {
      if (labels != "row2") {  # read labels file
      if (grepl(".xls$", ref.lbl) || grepl(".xlsx$", ref.lbl))
        format.lbl <- "Excel" 
      else
        format.lbl <- "csv"
        if (format.lbl != "Excel") 
          mylabels <- read.csv(file=ref.lbl, row.names=1, col.names=c("","label"),
            header=FALSE)
        else
          mylabels <- read.xls(xls=ref.lbl, row.names=1, col.names=c("","label"),
            header=FALSE)
      }
      else {  # labels == "row2"
        if (format != "Excel") 
          mylabels <- read.csv(file=ref, nrows=1, sep=delim, ...)
        else
          mylabels <- read.xls(xls=ref, nrows=1, na.strings="", ...)
        var.names <- names(mylabels)
        mylabels <- data.frame(t(mylabels))  # var names are row names
        names(mylabels) <- "label"
        if (format != "Excel") 
          data <- read.csv(file=ref, skip=1, 
                           na.strings=missing, col.names=var.names, sep=delim, ...)
        else
          data <- read.xls(xls=ref, skip=1, 
                           na.strings=missing, col.names=var.names, ...)
        }
      # transfer labels to data
      attr(data, which="variable.labels") <- as.character(mylabels$label)
      names(attr(data, which="variable.labels")) <- as.character(row.names(mylabels))
    }
  }

  else if (format == "SPSS")  # data and any labels
    data <- read.spss(file=ref, to.data.frame=TRUE, ...)

  else if (format == "R") {  # data and any labels
    x.env <- new.env()  # scratch environment
    load(ref, envir=x.env)
    dname <- ls(x.env)
    data <- get(dname, pos=x.env)
  }

  else if (format == "lessR") {  # data and any labels
    if (substr(ref,1,4) == "data")
      txt <- ""
    else
      txt <- "data"
    file.name <- paste(txt, ref, ".rda", sep="")

    path.name <- paste(find.package("lessR"), "/data/",  file.name, sep="")

    if (!file.exists(path.name)) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "No lessR data file with that name.\n\n",
        "To view the list of data files, enter  > Help(lessR)\n",
        "The data file names begin with  'data.'\n\n")
    }

    x.env <- new.env()  # scratch environment
    load(path.name, envir=x.env)

    dname <- paste(txt, ref, sep="")
    data <- get(dname, pos=x.env)
  }


  # feedback
  # --------
  if (!quiet) details(data, n.mcut, miss.zero, max.lines, miss.show,
                      miss.matrix, brief)

  return(data)

}

