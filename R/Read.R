Read <- 
function(ref=NULL, format=c("csv", "SPSS", "R", "Excel", "SAS", "lessR"),

         labels=NULL, widths=NULL, missing="", n.mcut=1, 

         miss.show=30, miss.zero=FALSE, miss.matrix=FALSE, 
      
         max.lines=30, sheet=1,

         brief=getOption("brief"), quiet=getOption("quiet"), 

         fun.call=NULL, ...) {


  if (is.null(fun.call)) fun.call <- match.call()
  fncl <- .fun.call.deparse(fun.call) 
  options(read.call=fncl)  # save for knitr run

  format <- match.arg(format)
  if (is.null(ref) && format=="lessR") {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Cannot browse for a data file that is part of lessR.\n",
        "Specify the file name.\n\n")
  }

# option to browse for data file, and then display file name
  if (is.null(ref)) {
    ref <- file.choose()
    cat("\n")
  }


  if (grepl(".sav$", ref)) format <- "SPSS" 
  if (grepl(".sas7bdat$", ref)) format <- "SAS" 
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


  # do the read (into object d)
  # ---------------------------

  if (format=="fwd" || format=="csv") {  # text file

    if (format=="fwd")
      d <- read.fwf(file=ref, widths=widths, ...)

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
         d <- read.csv(file=ref, na.strings=missing, sep=delim, ...)
    }

  }  # end text file
      
  else if (format == "Excel") { 
    txt <- "Hadley Wickham's readxl package]"
    cat("[with the read_excel function from", txt, "\n") 
      if (isnot.row2)  # read data
        d <- read_excel(path=ref, sheet=sheet)
        if (!is.null(list(...)$row.names))  #  see if do row.names 
          d <- data.frame(d, row.names=list(...)$row.names)
          class(d) <- "data.frame"  # otherwise nonstandard class from read_excel
  }


  if (!is.null(labels)) {  # process labels
    if (format %in% c("fwd", "csv", "Excel")) {
      if (labels != "row2") {  # read labels file
        if (grepl(".xls$", ref.lbl) || grepl(".xlsx$", ref.lbl))
          format.lbl <- "Excel" 
        else
          format.lbl <- "csv"
        if (format.lbl != "Excel") 
          mylabels <- read.csv(file=ref.lbl, row.names=1, header=FALSE)
        else {
          mylabels <- read_excel(path=ref.lbl, col_names=FALSE)
          mylabels <- data.frame(mylabels, row.names=1)
        }
        if (ncol(mylabels) == 1) names(mylabels) <- c("label")
        if (ncol(mylabels) == 2) names(mylabels) <- c("label", "unit")
      }
      else {  # labels == "row2"
        if (format != "Excel") 
          mylabels <- read.csv(file=ref, nrows=1, sep=delim, ...)
        else {
          mylabels <- read_excel(path=ref, ...)
          mylabels <- mylabels[1,] 
        }
        var.names <- names(mylabels)
        mylabels <- data.frame(t(mylabels))  # var names are row names
        names(mylabels) <- "label"
        if (format != "Excel") 
          d <- read.csv(file=ref, skip=1, 
                           na.strings=missing, col.names=var.names, sep=delim, ...)
        else
          d <- read_excel(path=ref, col_names=var.names)
        }
      # transfer labels and maybe units to data
      attr(d, which="variable.labels") <- as.character(mylabels$label)
      names(attr(d, which="variable.labels")) <- as.character(row.names(mylabels))
      if (ncol(mylabels) == 2) {
        attr(d, which="variable.units") <- as.character(mylabels$unit)
        names(attr(d, which="variable.units")) <- as.character(row.names(mylabels))
      }
    }
  }

  else if (format == "SPSS")  # data and any labels
    d <- read.spss(file=ref, to.data.frame=TRUE, ...)

  else if (format == "SAS") { # data 
    d <- read.sas7bdat(file=ref, ...)
    txt <- "Matt Shotwell's sas7bdat package]"
    cat("[with the read.sas7bdat function from", txt, "\n") 
  }

  else if (format == "R") {  # data and any labels
    x.env <- new.env()  # scratch environment
    load(ref, envir=x.env)
    dname <- ls(x.env)
    d <- get(dname, pos=x.env)
  }

  else if (format == "lessR") {  # data and any labels
    txt <- ifelse (substr(ref,1,4) == "data", "", "data")
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
    d <- get(dname, pos=x.env)
  }


  # if a column is unique non-numeric, convert read as factor to character
  n.col <- apply(d, 2, function(x) sum(!is.na(x)))
  nu.col <- apply(d, 2, function(x) length(unique(na.omit(x))))
  fnu.col <- logical(length=ncol(d))
  if (format != "Excel") {
    for (i in 1:ncol(d)) if (nu.col[i]==n.col[i] && (is.factor(d[,i])))
          fnu.col[i] <- TRUE 
    d[fnu.col] <- lapply(d[fnu.col], as.character)
  }
  else {  # read_excel does not convert strings to factors
    for (i in 1:ncol(d)) if (nu.col[i]!=n.col[i] && (is.character(d[,i])))
          fnu.col[i] <- TRUE 
    d[fnu.col] <- lapply(d[fnu.col], as.factor)
  }


  # feedback
  # --------
  if (!quiet) details(d, n.mcut, miss.zero, max.lines, miss.show,
                      miss.matrix, brief)

  return(d)

}

