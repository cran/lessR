factors <-
function (x, levels, labels=NULL, data=d, ordered=FALSE,
          new=FALSE, suffix="_f", var_labels=FALSE, ...) {

  # a dot in a parameter name to an underscore
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (names(dots)[i] %in% c("var.labels")) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }

  if (var_labels) new <- TRUE
  if (missing(levels) && var_labels) levels <- NULL
  if (is.null(labels)) labels <- as.character(levels)

  # get ind, the indices of selected variables
  data.vars <- as.list(seq_along(data))
  names(data.vars) <- names(data)
  ind <- eval(substitute(x), envir=data.vars, parent.frame())

  n_col <- ncol(data)
  n_row <- nrow(data)

  # create new variables for factor versions
  if (new) {
    n.add <- length(ind)

    # get old names + new names
    new.names <- paste(names(data)[ind], suffix, sep="")
    all.names <- c(names(data), new.names)

    if (!var_labels) { # do the conversion to factors
      # get old df + new df
      new.mat <- matrix(nrow=nrow(data), ncol=n.add)
      data <- cbind(data, new.mat)
      names(data) <- all.names

      # convert
      new.i.start <- n_col + 1
      new.i.end <- n_col + n.add
      data[, new.i.start:new.i.end] <-
      lapply(data[, ind, drop=FALSE], factor,
             levels=levels, labels=labels, ordered=ordered)
    }
  }  # end new

  # replace existing values
  else {
    if (!var_labels) {
      data[, ind] <-
        lapply(data[, ind, drop=FALSE], factor,
               levels=levels, labels=labels, ordered=ordered)
    }
  }  # end replace existing


  # copy the variable labels to the newly created .f vars
  if (var_labels) {

    l.name <- "l"
    if (exists(l.name, where=.GlobalEnv)) {
      l <- get(l.name, pos=.GlobalEnv)
    }
    else {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
                      "No l data frame available.\n\n")
    }

    nm.vars <- names(data)[ind]
    new.lbls <- l[which(row.names(l) %in% nm.vars), ]

    new.mat2 <- data.frame(matrix(nrow=n.add, ncol=1), stringsAsFactors=TRUE)
    names(new.mat2) <- "label"

    row.start <- n_col + 1
    row.end <- n_col + n.add
    rownames(new.mat2)[1:n.add] <- all.names[(row.start):(row.end)]
    new.mat2[, 1] <- new.lbls
    new.labels <- rbind(l, new.mat2)
  }

  if (!var_labels)
    return(data=as.data.frame(data, stringsAsFactors=TRUE))
  else
    return(new.labels)

}
