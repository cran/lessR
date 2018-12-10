.ss.real <-
function(x, y=NULL, by=NULL, digits.d=NULL, x.name, y.name=NULL, by.name=NULL,
         x.lbl=NULL, y.lbl=NULL, label.max=20, ...)  {


  n.dim <- length(dim(x))

  # -------------------------------------
  # two variables 
  if (n.dim == 2) { 

    # potential abbreviation of column labels
    mx.chr <- max(nchar(colnames(x)), na.rm=TRUE)
    if (mx.chr > label.max) {
      c.nm <- colnames(x)
      colnames(x) <- .abbrev(colnames(x), label.max)
    }

    # width of column 1
    if (!is.null(by.name))
      max.c1 <- nchar(by.name)
    else
      max.c1 <- 0
    for (i in 1:nrow(x)) {
      c1 <- nchar(rownames(x)[i])

      if (c1 > max.c1) max.c1 <- c1
    }
    max.c1 <- max.c1 + 2
    if (max.c1 < 5) max.c1 <- 5

    # width of data columns
    max.ln <- integer(length=0)
    for (i in 1:ncol(x)) {
        ln.nm <- nchar(colnames(x)[i])
      for (j in 1:nrow(x)) {
        ln.vl <- nchar(as.character(x[j,i]))
      }
        max.ln[i] <- max(ln.nm, ln.vl) + 3
        if (max.ln[i] < 4) max.ln[i] <- 4
    }

  # print table
  tx <- character(length = 0)

  # title
  tx[length(tx)+1] <- paste("Data Table of", y.name) 
  tx[length(tx)+1] <- .dash2(nchar(tx[length(tx)]))
  tx[length(tx)+1] <- ""

  # col labels
  if (!is.null(x.name))
    tx[length(tx)+1] <-  .fmtc(x.name, w=max.c1+3)
  tx[length(tx)+1] <-  format(by.name, width=max.c1, justify="left")
  w <- nchar(as.character(sum(x)))
  for (i in 1:ncol(x))
    tx[length(tx)] <- paste(tx[length(tx)], .fmtc(colnames(x)[i], w=max.ln[i]),
      sep="")

  # values
  for (i in 1:nrow(x)) {
    rwnm <- paste(" ", rownames(x)[i])
    tx[length(tx)+1] <-  format(rwnm, width=max.c1, justify="left")
    for (j in 1:ncol(x)) {
      tx[length(tx)] <- paste(tx[length(tx)], .fmt(x[i,j], d=digits.d,
             w=max.ln[j]), sep="")
    }
  }


    return(list(n.dim=n.dim, txtbl=tx))

  }  # end two variable


  else {  # one variable

    # potential abbreviation of column labels
    mx.chr <- max(nchar(names(x)), na.rm=TRUE)
    if (mx.chr > label.max) {
      c.nm <- names(x)
      names(x) <- .abbrev(names(x), label.max)
    }

     names(x)[which(is.na(names(x)))] <- "<NA>"  # for y given, a missing x
     max.ln <- integer(length=0)      
     for (i in 1:length(x)) {
       if (is.na(names(x[i]))) names(x[i]) <- "xxx"
       ln.nm <- nchar(names(x[i]))
       ln.vl <- nchar(format(x[i]))
       max.ln[i] <- max(ln.nm, ln.vl) + 1
       if (max.ln[i] < 6) max.ln[i] <- 6
     }

      tx <- character(length=0)

      tx <- paste(" Data for: ", y.name)
      tx[length(tx)+1] <- paste(" ", .dash2(nchar(tx)-1), sep="")

      tx[length(tx)+1] <- ""
      for (i in 1:length(x))
        tx[length(tx)] <- paste(tx[length(tx)], .fmtc(names(x[i]), w=max.ln[i]))

      tx[length(tx)+1] <- ""
      for (i in 1:length(x))
        tx[length(tx)] <- paste(tx[length(tx)], .fmt(x[i], d=digits.d,
             w=max.ln[i]+1), sep="")

      txtbl <- tx

    return(list(n.dim=n.dim, txtbl=tx))

  }  # one variable

}
