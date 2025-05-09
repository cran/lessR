.prntbl <- function(x, digits_d=2, cut=0, cc="-", cors=FALSE,
                    brk=NULL, bnd=NULL, v1.nm=NULL, v2.nm=NULL,
                    from_efa=FALSE) {

# brk: ... replaces rows not printed
# bnd: boundary between groups

  max.ch <- ifelse (cors, 3, 0)  # max char per column, 0 is not applicable

  # width of column 1
  max.c1 <- 0
  for (i in 1:nrow(x)) {
    c1 <- nchar(rownames(x)[i])
    if (c1 > max.c1) max.c1 <- c1
  }
  if (!is.null(v2.nm)) if (nchar(v2.nm) > max.c1) max.c1 <- nchar(v2.nm)
  max.c1 <- max.c1 + 2

  # widths of variable names
  colnm.w <- integer(length=ncol(x))
  for (i in 1:ncol(x))
    colnm.w[i] <- nchar(colnames(x)[i])

  # width of columns
  max.ln <- integer(length=ncol(x))
  for (j in 1:ncol(x)) {
    if (is.numeric(x[,j])) {
      c.val <- 0
      for (i in 1:nrow(x)) {
        i.val <- nchar(formatC(x[i,j], digits=digits_d, format="f"))
        if (i.val > c.val) c.val <- i.val
      }
    }
    else {
      c.val <- 0
      for (i in 1:nrow(x)) {
        i.val <- nchar(as.character(x[i,j]))
        if (i.val > c.val) c.val <- i.val
      }
    }
    if (!cors)
      max.ln[j] <- max(colnm.w[j], c.val) + 1
    else {
      max.ln[j] <- max(colnm.w[j], 4)
      if (max.ch > 0) max.ln[j] <- max.ch
      if (max.ln[j] > 4) max.ln[j] <- max.ln[j] + 1
    }
    if (max.ln[j] < 4) max.ln[j] <- 4
  }

  tx <- character(length = 0)
  if (!is.null(cc))
    tx[length(tx)+1] <- .dash2(sum(max.ln)+max.c1, cc=cc)

  # matrix for potentially multi-row column names
  if (max.ch > 0) {
    nr.ind.lbl <- integer(length=ncol(x))
    for (i in 1:ncol(x))
      nr.ind.lbl[i] <- ((nchar(colnames(x)[i]) + (max.ch-1)) %/% max.ch)

    nr.lbl <- max(nr.ind.lbl)  # n_row of labels
    col.nm <- matrix(nrow=nr.lbl, ncol=ncol(x))
    for (i in 1:nrow(col.nm)) {
      for (j in 1:ncol(col.nm)) {
        srt <- ((i-1)*max.ch) + 1
        stp <- srt + (max.ch - 1)
        col.nm[i,j] <- substr(colnames(x)[j], srt, stp)
      }
    }
  }
  else {
    nr.lbl <- 1
    col.nm <- matrix(nrow=1, ncol=ncol(x))
    for (j in 1:ncol(col.nm)) col.nm[1,j] <- colnames(x)[j]
  }
  # for each row, shift down value if next row is "", repeat
  if (nr.lbl > 1) {
    for (k in 2:nrow(col.nm)) {  # repeat for each row
      for (i in 2:nrow(col.nm)) {
        for (j in 1:ncol(col.nm)) {
          if (nchar(col.nm[i,j]) == 0) {
            col.nm[i,j] <- col.nm[i-1,j]
            col.nm[i-1,j] <- ""
          }
        }
      }
    }
  }

  blnk <- format("", width=max.c1-1)
  if (!is.null(v1.nm)) tx[length(tx)+1] <- paste(blnk, v1.nm)
  # write col labels
  for (i in 1:nr.lbl) {  # for each row of column labels
    if (is.null(v2.nm))
      tx[length(tx)+1] <- format("", width=max.c1)
    else
      tx[length(tx)+1] <- paste(" ", v2.nm,
           format("", width=max.c1-nchar(v2.nm)-2), sep="")
    for (j in 1:ncol(x)) {
      wd <- max.ln[j]
      tx[length(tx)] <- paste(tx[length(tx)], .fmtc(col.nm[i,j], w=wd), sep="")
      if (!is.null(bnd)) if (j %in% bnd)
        if (i == nr.lbl)
          tx[length(tx)] <- paste(tx[length(tx)], "|", sep="")
        else
          tx[length(tx)] <- paste(tx[length(tx)], " ", sep="")
    }
  }
  if (!is.null(bnd))
    tx[length(tx)+1] <- .dash2(sum(max.ln)+max.c1+length(bnd), cc=" ")

  # factor vars to char vars
  if (is.data.frame(x)) {
    i.col <- sapply(x, is.factor)
    x[i.col] <- lapply(x[i.col], as.character)
  }

  # write values
  for (i in 1:nrow(x)) {
    if (i %in% brk) tx[length(tx)+1] <- "..."
    rwnm <- paste(" ", rownames(x)[i])
    if (is.null(v2.nm))
      tx[length(tx)+1] <- format(rwnm, width=max.c1, justify="right")
    else
      tx[length(tx)+1] <- format(rwnm, width=max.c1-1, justify="right")

    for (j in 1:ncol(x)) {
      if (is.integer(x[i,j]))
        tx[length(tx)] <- paste(tx[length(tx)], .fmti(x[i,j],
                                w=max.ln[j]), sep="")

      else if (is.numeric(x[i,j])) {
        wd <- max.ln[j]
        if (cors) {
          if (max.ln[j] > 5) wd <- max(6, max.ln[j]+1) + 1
          else wd <- max(6, max.ln[j]+1)
          cs <- .fmt(x[i,j], d=digits_d, w=wd)
          cs <- sub("0.", "", cs, fixed=TRUE)
          cs <- sub(" 1.00", "100", cs, fixed=TRUE)
        }
        else
          cs <- .fmt(x[i,j], d=digits_d, w=wd)
        wd2 <- ifelse (!from_efa, wd-2, wd)
        if (abs(x[i,j]) < cut) cs <- paste(rep(" ", wd2), collapse="")
        tx[length(tx)] <- paste(tx[length(tx)], cs, sep="")
        if (!is.null(bnd)) if (j %in% bnd)
          tx[length(tx)] <- paste(tx[length(tx)], "|", sep="")
      }

      else if (is.character(x[i,j]))
        tx[length(tx)] <- paste(tx[length(tx)], .fmtc(x[i,j], w=max.ln[j]),
                                sep="")
    }

    if (!is.null(bnd)) if (i %in% bnd)
      tx[length(tx)+1] <- .dash2(sum(max.ln)+max.c1+length(bnd), cc="-")
  }

  return(tx)

}
