.rec.main <-
function(x, x.name, new.name, old, new, dframe, dframe.name) {

  n.values <- length(old)
  n.obs <- nrow(dframe)

  if (n.values != length(new)) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "The same number of values must be specified for both\n",
      "old and new values.\n\n")
  }

  old.unique <- sort(unique(x))
  n.unique <- length(old.unique)
  cat("\n")
  cat("---  Recode:", x.name, "---\n")
  cat("\n")

  if (class(x) == "integer")
    cat("Unique values of", x.name, "in the data:", old.unique, "\n")
  else if (class(x) == "factor")
    cat("Unique values of", x.name, "in the data:", levels(x), "\n")
  cat("Number of unique values of", x.name, "in the data:", n.unique, "\n")
  cat("\n")

  # check to ensure that all values to recode exist in the data
  for (i in 1:n.values) {
    is.in <- FALSE
    for (j in 1:n.unique) 
      if (old[i] == old.unique[j]) is.in <- TRUE 
    if (!is.in) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "A value specified to recode: ", old[i], "\n",
      "This value does not exist in the data.\n\n")
    }
  }

  cat("Number of values of", x.name, "to recode:", n.values, "\n")
  cat("Number of observations (rows) to recode:", n.obs, "\n")
  cat("\n")

  cat("Recoding Specification\n")
  .dash(22)
  for (i in 1:n.values) cat("  ", old[i], "-->", new[i], "\n")

  cat("\n")
  if (is.null(new.name))
    cat("No value specified for option: new.name\n",
        "So replace existing values of ", x.name, "\n", sep="")
  else
    cat("Recode to variable:", new.name, "\n")

  cat("\n")
 

  if (class(x) == "factor") x <- as.character(x)

  new.x <- x

  # the recode
  for (i in 1:n.values) {
    for (j in 1:n.obs) {
      if (!is.na(new.x[j])) if (x[j] == old[i]) new.x[j] <- new[i]
    } 
  }

  nms <- names(dframe)
  if (is.null(new.name)) col.num <- which(nms == x.name)

  myd <- transform(dframe, dummy.name=new.x)

  # replace dummy.name with actual name
  if (is.null(new.name)) {
    names(myd) <- c(nms, x.name)
    myd <- subset(myd, select=-c(col.num))
  }
  else
    names(myd) <- c(nms, new.name)

  # write the new data frame
  assign(dframe.name, myd, pos=.GlobalEnv)
 
  cat("\n")
  cat("First six rows of data for data frame:", dframe.name, "\n")
  .dash(45)
  cat("\n")
  print(head(get(dframe.name, pos=.GlobalEnv)))
  cat("\n")

}
