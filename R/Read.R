Read <- 
function(ref=NULL, format=c("csv", "SPSS", "R", "lessR"),

         labels=NULL, widths=NULL, missing="", n.mcut=1, 

         miss.show=30, miss.zero=FALSE, miss.matrix=FALSE, 
      
         max.lines=30, quiet=getOption("quiet"), ...) {

  format <- match.arg(format)

  if (is.null(ref) && format=="lessR") {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Cannot browse for a file that is part of lessR.\n",
        "Specify the file name.\n\n")
  }

# option to browse for data file, and then display file name
  cat("\n")
  if (is.null(ref)) {
    ref <- file.choose()
    .dash(68)
    cat("File: \n")
    cat("   ", ref, "\n")
    .dash(68)
    cat("\n")
  }

  if (grepl(".sav$", ref)) format <- "SPSS" 
  if (grepl(".rda$", ref)) format <- "R" 
  if (!is.null(widths)) format <- "fwd"

  # construct full path name for label file if not already
  if (!is.null(labels) && labels!="row2") {
    if (!grepl(.Platform$file.sep, labels)) {
      pth <- strsplit(ref, "/")
      fp <- ""
      for (i in 2:length(pth[[1]])-1)
        fp <- paste(fp, pth[[1]][i], .Platform$file.sep, sep="")
      ref.lbl <- paste(fp, labels, sep="")
    }
    else
      ref.lbl <- labels
  }


  # do the read
  # -----------

  if (format=="fwd" || format=="csv") {  # text file

    if (format=="fwd")
      data <- read.fwf(file=ref, widths=widths, ...)

    else if (format=="csv") {
      if (is.null(labels)) 
        data <- suppressWarnings(read.csv(file=ref, na.strings=missing, ...))
      else {
        if (labels != "row2")
          data <- suppressWarnings(read.csv(file=ref, na.strings=missing, ...))
        else {
          mylabels <- read.csv(file=ref, nrows=1, ...)
          var.names <- names(mylabels)
          mylabels <- data.frame(t(mylabels))  # var names are row names
          names(mylabels) <- "label"
          data <- suppressWarnings(read.csv(file=ref, skip=1, 
                         na.strings=missing, col.names=var.names, ...))
        }
      }
    }

    if (!is.null(labels)) {
      if (labels != "row2") {
        mylabels <- 
          read.csv(file=ref.lbl, row.names=1, col.names=c("","label"), header=FALSE)
      }
      # transfer labels to data
      attr(data, which="variable.labels") <- as.character(mylabels$label)
      names(attr(data, which="variable.labels")) <- as.character(row.names(mylabels))
    }

  }  # end text file
      
  else if (format == "SPSS")  # data and any labels
      data <- suppressWarnings(read.spss(file=ref, to.data.frame=TRUE, ...))

  else if (format == "R") {  # data and any labels
    x.env <- new.env()  # scratch environment
    load(ref, envir=x.env)
    dname <- ls(x.env)
    data <- get(dname, pos=x.env)
  }

  else if (format == "lessR") {
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
  if (!quiet) details(data, n.mcut, miss.zero, max.lines, miss.show, miss.matrix)

  return(data)

}

