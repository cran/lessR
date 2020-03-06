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
      l <- as.data.frame(mylbl, stringsAsFactors=FALSE,
                                row.names=names(label))
      if (!is.null(unit)) for (i in 1:nrow(l))
        if (is.na(l[i,2])) l[i,2] <- ""
      if (ncol(l) == 1) names(l) <- "label"
      if (ncol(l) == 2) names(l) <- c("label", "unit")
      if (is.null(mylbl))
        cat("\nNo variable labels present in the data file\n\n")
      return(l)
    }
  }

  # l: modify existing or add new row
  mylbYN <- ifelse (exists("l", where=.GlobalEnv), TRUE, FALSE) 
  if (mylbYN && !missx && !is.null(value)) {
    if (length(l[which(row.names(l) == x.name), 1]) > 0) {
      l[which(row.names(l) == x.name), 1] <- value
    }
    else {
      nr <- nrow(l)
      l[nr + 1, 1] <- value
      row.names(l)[nr + 1] <- x.name
    }
  return(l)
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
      for (i in 1:nrow(l))
        cat(row.names(l)[i], ": ", l[i,], "\n", sep="")
    }
    else {  # display existing label
      lbl <- l[which(row.names(l) == x.name), 1]
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
      l <- as.data.frame(read.xlsx(x, colNames=FALSE), stringsAsFactors=TRUE)
      l <- as.data.frame(l, row.names=l[,1], stringsAsFactors=TRUE)
      l <- l[, -1, drop=FALSE]  # keep as a data frame with drop
      if (ncol(l) == 1) names(l) <- "label"
      if (ncol(l) == 2) names(l) <- c("label", "unit")
    }

  return(l)
  }

  cat("\n")

}
