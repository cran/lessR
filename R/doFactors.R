doFactors <-
function (x, levels, labels=NULL, data=mydata, ordered=FALSE,
          new=FALSE, suffix=".f", var.labels=FALSE) {

  if (var.labels) new <- TRUE
  if (missing(levels) && var.labels) levels <- NULL
  if (is.null(labels)) labels <- as.character(levels)

  # get ind, the indices of selected variables
  data.vars <- as.list(seq_along(data))
  names(data.vars) <- names(data)
  ind <- eval(substitute(x), envir=data.vars, parent.frame())

  n.col <- ncol(data)
  n.row <- nrow(data)

  # create new variables for factor versions
  if (new) {
    n.add <- length(ind)

    # get old names + new names
    new.names <- paste(names(data)[ind], suffix, sep="")
    all.names <- c(names(data), new.names)

    if (!var.labels) { # do the conversion to factors
      # get old df + new df
      new.mat <- matrix(nrow=nrow(data), ncol=n.add)
      data <- cbind(data, new.mat)
      names(data) <- all.names

      # convert
      new.i.start <- n.col + 1
      new.i.end <- n.col + n.add
      data[, new.i.start:new.i.end] <-
      lapply(data[, ind, drop=FALSE], factor,
             levels=levels, labels=labels, ordered=ordered)
    }
  }  # end new

  # replace existing values
  else {
    if (!var.labels) {
      data[, ind] <-
        lapply(data[, ind, drop=FALSE], factor,
               levels=levels, labels=labels, ordered=ordered)
    }
  }  # end replace existing


  # copy the variable labels to the newly created .f vars
  if (var.labels) {

    l.name <- "mylabels"
    if (exists(l.name, where=.GlobalEnv)) {
      mylabels <- get(l.name, pos=.GlobalEnv)
    }
    else {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
                      "No mylabels data frame available.\n\n")
    }

    nm.vars <- names(data)[ind]
    new.lbls <- mylabels[which(row.names(mylabels) %in% nm.vars), ]

    new.mat2 <- data.frame(matrix(nrow=n.add, ncol=1))
    names(new.mat2) <- "label"

    row.start <- n.col + 1
    row.end <- n.col + n.add
    rownames(new.mat2)[1:n.add] <- all.names[(row.start):(row.end)]
    new.mat2[, 1] <- new.lbls
    new.labels <- rbind(mylabels, new.mat2)
  }

  if (!var.labels)
    return(data=as.data.frame(data))
  else
    return(new.labels)

}
