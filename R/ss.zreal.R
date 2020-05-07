.ss.real <-
function(x, y=NULL, by=NULL, digits_d=NULL, x.name, y.name=NULL, by.name=NULL,
         x.lbl=NULL, y.lbl=NULL, label_max=20, ...)  {


  n_dim <- length(dim(x))
  x.name <- getOption("xname")
  y.name <- getOption("yname")

  # -------------------------------------
  # two variables 
  if (n_dim == 2) { 

    # potential abbreviation of column labels
    mx.chr <- max(nchar(colnames(x)), na.rm=TRUE)
    if (mx.chr > label_max) {
      c.nm <- colnames(x)
      colnames(x) <- .abbrev(colnames(x), label_max)
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
      for (j in 1:nrow(x))
        ln.vl <- nchar(as.character(x[j,i]))
      max.ln[i] <- max(ln.nm, ln.vl) + 3
      if (max.ln[i] < 4) max.ln[i] <- 4
    }

    # print table
    tx <- character(length = 0)

    # title
    tx[length(tx)+1] <- paste("Data Table of", y.name) 
    tx[length(tx)+1] <- .dash2(nchar(tx[length(tx)]))
    tx[length(tx)+1] <- ""

    # col labels (presume first horizontal layout)
    if (!is.null(x.name))
      tx[length(tx)+1] <-  .fmtc(x.name, w=max.c1+3)
    tx[length(tx)+1] <-  format(by.name, width=max.c1, justify="left")
    w <- nchar(as.character(sum(x)))
    for (i in 1:ncol(x))
      tx[length(tx)] <- paste(tx[length(tx)], .fmtc(colnames(x)[i], w=max.ln[i]),
        sep="")

    if (max(nchar(tx)) < getOption("width")) {  # horizontal layout

      # values
      for (i in 1:nrow(x)) {
        rwnm <- paste(" ", rownames(x)[i])
        tx[length(tx)+1] <- format(rwnm, width=max.c1, justify="left")
        for (j in 1:ncol(x)) {
          tx[length(tx)] <- paste(tx[length(tx)], .fmt(x[i,j], d=digits_d,
                 w=max.ln[j]), sep="")
        }
      }
    }  # end horizontal

    else {  # vertical layout

      tx <- ""

      if (nrow(x) * (ncol(x)-1) > 20) { 
        message("Table output is vertical to fit in window, but > 20 rows\n",
                "To view the complete table, save the output\n",
                "  to an object, e.g., b <- BarChart(...)\n",
                "  then b$values\n")
      }

      else {  # write

        mx.cx <- max(nchar(x.name), max(nchar(colnames(x))))

        tx[length(tx)+1] <- paste(
           .fmtc(x.name, w=mx.cx, j="left"),
           .fmtc(by.name, w=max.c1+1, j="left"),
           getOption("yname"))
        for (i in 1:ncol(x)) {
          for (j in 1:nrow(x)) {
             tx[length(tx)+1] <- paste(
                .fmtc(colnames(x)[i], w=mx.cx, j="left"), 
                .fmtc(rownames(x)[j], w=max.c1, j="left"),
                .fmt(x[j,i], d=digits_d, w=max.ln-3))
          }
        }
      }  # end write
    }  # end vertical

    return(list(n_dim=n_dim, txtbl=tx))

  }  # end two variable


  else {  # one variable

    # potential abbreviation of column labels
    mx.chr <- max(nchar(names(x)), na.rm=TRUE)
    if (mx.chr > label_max) {
      c.nm <- names(x)
      names(x) <- .abbrev(names(x), label_max)
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

      # col labels
      tx[length(tx)+1] <- ""
      for (i in 1:length(x))
        tx[length(tx)] <- paste(tx[length(tx)], .fmtc(names(x[i]), w=max.ln[i]))

      if (max(nchar(tx)) < getOption("width")) {  # horizontal layout
        # values
        tx[length(tx)+1] <- ""
        for (i in 1:length(x))
          tx[length(tx)] <- paste(tx[length(tx)], .fmt(x[i], d=digits_d,
               w=max.ln[i]+1), sep="")
      }

      else {  # vertical

        tx <- ""

        mx.cx <- max(nchar(x.name), max(nchar(names(x))))
        mx.vl <- max(nchar(y.name), max(nchar(as.character(.fmt(x, d=digits_d)))))

        tx[length(tx)+1] <- paste(
           .fmtc(x.name, w=mx.cx+1, j="left"),
           .fmtc(y.name, w=nchar(y.name)+1, j="left"))
        for (i in 1:nrow(x)) {
           tx[length(tx)+1] <- paste(
              .fmtc(names(x)[i], w=mx.cx, j="left"), 
              .fmt(x[i], d=digits_d, w=mx.vl+1))
        }

      }  # end vertical

      txtbl <- tx

    values <- data.frame(x, stringsAsFactors=TRUE)
    names(values) <- c(x.name, "values")
    return(list(n_dim=n_dim, txtbl=tx, values=values))

  }  # end one variable

}
