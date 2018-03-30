VariableLabels <-
function(x, value=NULL, quiet=getOption("quiet")) {

  missx <- ifelse (missing(x), TRUE, FALSE) 
  if (!missx) {
    x.name <- deparse(substitute(x))
    options(xname = x.name)
  }
  else
    x.name <- NULL

  # extract labels/units from a data frame
  if (!is.null(x.name)) if (exists(x.name, where=.GlobalEnv)) { 
    if (is.data.frame(x)) {
      label <- attr(get(x.name, pos=.GlobalEnv), which="variable.labels")
      unit <- attr(get(x.name, pos=.GlobalEnv), which="variable.units")
      mylbl <- cbind(label, unit)
      mylabels <- as.data.frame(mylbl, stringsAsFactors=FALSE,
                                row.names=names(label))
      if (!is.null(unit)) for (i in 1:nrow(mylabels))
        if (is.na(mylabels[i,2])) mylabels[i,2] <- ""
      if (ncol(mylabels) == 1) names(mylabels) <- "label"
      if (ncol(mylabels) == 2) names(mylabels) <- c("label", "unit")
      if (is.null(mylbl))
        cat("\nNo variable labels present in the data file\n\n")
      return(mylabels)
    }
  }

  # mylabels: modify existing or add new row
  mylbYN <- ifelse (exists("mylabels", where=.GlobalEnv), TRUE, FALSE) 
  if (mylbYN && !missx && !is.null(value)) {
    if (length(mylabels[which(row.names(mylabels) == x.name), 1]) > 0) {
      mylabels[which(row.names(mylabels) == x.name), 1] <- value
    }
    else {
      nr <- nrow(mylabels)
      mylabels[nr + 1, 1] <- value
      row.names(mylabels)[nr + 1] <- x.name
    }
  return(mylabels)
  }


  # display only or read from external file
  # --------------------------------------- 

  frm.cnsl <- FALSE
  if (!missx) 
    if (exists(x.name, where=.GlobalEnv))
      if (grepl("\n", x, fixed=TRUE)) frm.cnsl <- TRUE  # from console

  fmt <- "none"  # see if x is a file reference
  if (!is.null(x.name)) {
    if (grepl(".csv", x.name)) fmt <- "csv"
    if (grepl(".xlsx", x.name)) fmt <- "Excel"
  }

  # display only
  if (!frm.cnsl  &&  fmt=="none" && is.null(value)) {
    if (missx && is.null(x.name)) {  # display all labels
      cat("\n")
      for (i in 1:nrow(mylabels))
        cat(row.names(mylabels)[i], ": ", mylabels[i,], "\n", sep="")
    }
    else {  # display existing label
      lbl <- mylabels[which(row.names(mylabels) == x.name), 1]
      if (is.null(lbl)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        "The variable label does not exist for variable: ", x.name, "\n\n")
      }
      cat("\n")
      cat(x.name, ": ", lbl, "\n", sep="")
    }
  }

  # assign labels to vars in a data frame and return data frame
  else {

    # get labels
    if (fmt == "none")  {  # no external file, read from console
      l <- read.csv(text=x, row.names=1,
                    header=FALSE, stringsAsFactors=FALSE)
      if (ncol(l) == 1) names(l) <- "label"
      if (ncol(l) == 2) names(l) <- c("label", "unit")
    }
    else if (fmt == "csv") {  # x is a file name
      l <- read.csv(x, row.names=1,
                           header=FALSE, stringsAsFactors=FALSE)
      if (ncol(l) == 1) names(l) <- "label"
      if (ncol(l) == 2) names(l) <- c("label", "unit")
    }
    else {
#     if (fmt=="Excel"  &&  grepl("http", x, fixed=TRUE)) {
#       cat("\n"); stop(call.=FALSE, "\n","------\n",
#           "The underlying read_excel function does not\n",
#           "support reading Excel files from the web.\n",
#           "Use the  download.file  function to download\n",
#           "the file to your local file system.\n\n",
#           "  download.file(\"", x, "\", \"MYFILE.xlsx\")\n\n",
#           "Replace MYFILE with the desired file name.\n",
#           "Enter  getwd()  to see where the file was saved. \n\n")
#     }
      l <- as.data.frame(read.xlsx(x, colNames=FALSE))
#     l <- as.data.frame(read_excel(x, col_names=FALSE))

      l <- as.data.frame(l, row.names=l[,1])
      l <- l[, -1, drop=FALSE]  # keep as a data frame with drop
      if (ncol(l) == 1) names(l) <- "label"
      if (ncol(l) == 2) names(l) <- c("label", "unit")
    }

  return(l)
  }

  cat("\n")

}
