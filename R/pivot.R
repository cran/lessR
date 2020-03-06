pivot <-
  function(data, compute, value, by, filter=NULL, table_2d=FALSE) {

  response <- deparse(substitute(value))  # response variable name

  data.vars <- as.list(seq_along(data))
  names(data.vars) <- names(data)

  if (!missing(filter)) {  # subset rows
    r <- eval(substitute(filter), envir=data, enclos=parent.frame())
    r <- r & !is.na(r)  # set missing for a row to FALSE
    data <- data[r,,drop=FALSE]
  }

  # get index of response variable
  ind.smry <- eval(substitute(value), envir=data.vars, parent.frame())

  # get indices of X variables
  x_new <- deparse(substitute(by))
  x <- str2lang(x_new)  # transform character string to type calls
  ind.fe <- eval(substitute(x), envir=data.vars, parent.frame())

  if (length(ind.fe) > 1) {
    a <- aggregate(data[,ind.smry], by=as.list(data[,ind.fe]), FUN=compute)
    n <- aggregate(data[,ind.smry], by=as.list(data[,ind.fe]), FUN=length)
  }
  else {
    ind.fe <- eval(substitute(by), envir=data.vars, parent.frame())
    a <- aggregate(data[,ind.smry], by=list(data[,ind.fe]), FUN=compute)
    x.name <- deparse(substitute(by))  # name of single X variable
    names(a)[1] <- x.name
    n <- aggregate(data[,ind.smry], by=list(data[,ind.fe]), FUN=length)
  }

  a <- cbind(a[,1:(ncol(a)-1), drop=FALSE], n[,ncol(n), drop=FALSE],
             a[,ncol(a), drop=FALSE])
  names(a)[ncol(a)] <- response
  names(a)[ncol(a)-1] <- "n"

  if (table_2d && length(ind.fe)==2) {
    i_nm.smry <- which(names(a) == names(d)[ind.smry])
    i_nm.x1 <- which(names(a) == names(d)[ind.fe[1]])
    i_nm.x2 <- which(names(a) == names(d)[ind.fe[2]])

    tbl <- matrix(a[,i_nm.smry], byrow=FALSE, ncol=2)
    rownames(tbl) <- unique(a[,i_nm.x1])
    colnames(tbl) <- unique(a[,i_nm.x2])
    return(tbl)
  }

  else
    return(a)
}
