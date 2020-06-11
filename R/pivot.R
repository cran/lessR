pivot <-
function(data, compute, value, rows_by, cols_by=NULL,
         na_value=FALSE, na_by=FALSE, na_remove=TRUE,
         digits_d=NULL) {

  # length(x) is count_n(x) + count_NA(x)
  count_n <- function(x) sum(!is.na(x))
  count_NA <- function(x) sum(is.na(x))

  # value of compute must be a recognized object, such as a function
  cmpt.nm <- deparse(substitute(compute))

  # missing values in by vars, automatically dropped
  # missing values in value vars, NA in aggregated output

  data.vars <- as.list(seq_along(data))
  names(data.vars) <- names(data)

  # get index of response variable(s)
  ind.smry <- eval(substitute(value), envir=data.vars, parent.frame())

  # get indices of rows_by variable(s)
  if (!missing(rows_by)) {
    x <- str2lang(deparse(substitute(rows_by)))  # char string to type calls
    ind.r.by <- eval(substitute(x), envir=data.vars, parent.frame())
  }
  else {
    ind.r.by <- ind.smry  # for tabulate do not need by vars
  }
  n.r.by <- length(ind.r.by)

  # get indices of cols_by variable(s)
  if (!missing(cols_by)) {
    x <- str2lang(deparse(substitute(cols_by)))  # char string to type calls
    ind.c.by <- eval(substitute(x), envir=data.vars, parent.frame())
    n.c.by <- length(ind.c.by)
  }
  else {
    ind.c.by <- NULL
    n.c.by <- 0
  }

  if (n.c.by > 2)  {
    nms.c <- ""
    for (i in 1:n.c.by) nms.c <- paste(nms.c, names(d)[ind.c.by[i]])
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Specified column  by  variables: ", nms.c, "\n",
      "Only two column  by  variables permitted\n\n")
  }

  # collapse by variables to a single by for aggregation into long form
  n.by <- n.r.by + n.c.by
  ind.by <- c(ind.r.by, ind.c.by)

  if (!na_by)
    exc <- NA  # standard default
  else
    exc <- NULL  # include NA's
  if (n.by > 1) {
    data[, ind.by] <- lapply(data[, ind.by], factor, exclude=exc)
    by.vars <- as.list(data[, ind.by])
  }
  else {
    data[, ind.by] <- factor(data[, ind.by], exclude=exc)
    by.vars <- list(data[, ind.by])
  }

  # tabulate
  if (cmpt.nm == "tabulate") {
    use_na <- ifelse (!na_by, "no", "ifany")
    a <- table(data[, ind.by], useNA=use_na)
    a <- as.data.frame(a)
    if (n.by == 1) names(a)[1] <- deparse(substitute(value))
    names(a)[ncol(a)] <- "n"
  }

  # aggregate over a numerical variable
  else {
    a <- aggregate(data[,ind.smry], by=by.vars, drop=FALSE, FUN=compute,
                   na.rm=na_remove)  # compute for a cell even if a missing
    n <- aggregate(data[,ind.smry], by=by.vars, drop=FALSE, FUN=count_n)
    n_NA <- aggregate(data[,ind.smry], by=by.vars, drop=FALSE, FUN=count_NA)

    if (length(ind.smry) == 1)
      names(a)[ncol(a)] <- deparse(substitute(value))
    if (length(ind.by) == 1)
      names(a)[1] <- deparse(substitute(by))

    a <- cbind(a[,1:n.by, drop=FALSE],
               n[,ncol(n), drop=FALSE],
               n_NA[,ncol(n_NA), drop=FALSE],
               a[,(1+n.by):ncol(a), drop=FALSE])
    names(a)[n.by+1] <- "n"
    names(a)[n.by+2] <- "miss"
    for (i in 1:nrow(a)) {
      if (is.na(a[i,"n"])) a[i,"n"] <- 0
      if (is.na(a[i,"miss"])) a[i,"miss"] <- 0
    }

    # round if specified
    i.s <- which(names(a) %in% names(d)[ind.smry])
    if (!is.null(digits_d)) a[, i.s] <- round(a[, i.s], digits_d)

    if (!na_value) a <- na.omit(a)
  }

  # option to reshape long form a to wide form
  # ------------------------------------------
  if (n.c.by > 0) {

    # delete unneeded variables
    n.ind <- which(names(a) == "n")
    miss.ind <- which(names(a) == "miss")
    a <- a[, -c(n.ind, miss.ind)]

    # re-reference pivot variables in aggregated data frame a
    i.s <- which(names(a) %in% names(d)[ind.smry])
    i.r <- which(names(a) %in% names(d)[ind.r.by])
    i.c <- which(names(a) %in% names(d)[ind.c.by])

    # first column by variable
    if (n.c.by == 1) {
      i1.r <- i.r
      i1.c <- i.c
    }
    else if (n.c.by == 2) {
      i1a.r <- i.r
      i1b.r <- i.c[1]  # move first col by var to the row by
      i1.r <- c(i1a.r, i1b.r)
      i1.c <- i.c[2]
    }

    # reshape only works on one col by variable at a time
    w <- reshape(a, direction="wide", idvar=c(names(a)[i1.r]),
                    timevar=names(a)[i1.c], v.names=names(a)[i.s])

   # second column by variable
    if (n.c.by == 2) {
      i2.r <- i.r
      i2.c <- i.c[1]
    w <- reshape(w, direction="wide", idvar=c(names(a)[i2.r]),
                    timevar=names(a)[i2.c])
    }
    rownames(w) <- c()  # remove row names

  # headings
  # --------

    # if var cols_by names larger than by row name, pad with "|"
    mx <- max(nchar(names(a)[i.c]))
    nm.r1 <- names(a)[i.r[1]]
    if (nchar(nm.r1) < mx) {
      dff <- mx - nchar(nm.r1)
      buf <- paste(rep("|", dff), sep="", collapse="")
      nm.buf <- paste(names(a)[i.r[1]], buf, sep="")
      names(w)[1] <- nm.buf
    }

    # remove all by rows_by names, get " -" locations from kable
    for (i in (n.r.by+1):length(names(w))) names(w)[i] <- " "
    k <- kable(w, format="pandoc", digits=2, caption=" ", align="r")
    g <- gregexpr(" -", k[4], fixed=TRUE)
    g <- unlist(g)

    # get line of blank chars to fill later, including 1st by col name
    nc <- nchar(k[4])
    ln1 <- paste(rep(" ", nc), sep="", collapse="")
    ln1 <- paste(" ", names(a)[i.c[1]], ln1, sep="")

    # fill level labels for one cols_by var
    if (n.c.by == 1) {
      c1.nm <- names(a)[i.c[1]]
      substr(ln1, 2, 2 + nchar(c1.nm)) <- c1.nm
      lvl1 <- levels(a[,i.c[1]])

      for (i in 1:length(lvl1)) {
        start <- g[n.r.by + (i-1)] + 1
        stop <- start + nchar(lvl1[i])
        substr(ln1, start, stop) <- lvl1[i]
      }
    }

    # fill level labels for two cols_by vars
    else if (n.c.by == 2) {

      lvl1 <- levels(a[,i.c[1]])
      lvl2 <- levels(a[,i.c[2]])

      ln2 <- paste(rep(" ", nc), sep="", collapse="")  # blank line
      ln2 <- paste(" ", names(a)[i.c[2]], ln2, sep="") # add var name

      # labels for first cols_by var
      for (i in 1:length(lvl1)) {
        start <- g[n.r.by + (i-1)*length(lvl2)] + 1
        stop <- start + nchar(lvl1[i])
        substr(ln1, start, stop) <- lvl1[i]
      }

      # labels for second cols_by var
      c2.nm <- names(a)[i.c[2]]
      substr(ln2, 2, 2 + nchar(c2.nm)) <- c2.nm
      for (k in 1:length(lvl1)) {
        for (i in 1:length(lvl2)) {
          start <- g[n.r.by + (k-1)*length(lvl2) + (i-1)] + 1
          stop <- start + nchar(lvl2[i])
          substr(ln2, start, stop) <- lvl2[i]
        }
      }
    }  # end n.c.by == 2

    # caption
    cpt <- paste(cmpt.nm, "of", names(a)[i.s], "\n")

    w <- kable(w, format="pandoc", digits=2, caption=cpt, align="r")
    if (n.c.by == 1)
      w[2] <- ln1
    else
      w[2] <- paste(ln1, "\n", ln2, sep="")

    # clear any "|" padding that may be present
    for (i in 1:nchar(w[3]))
      if (substr(w[3],i,i) == "|") substr(w[3],i,i) <- " "

    # when knitr processing, wrap kable table with "```"
    if (!is.null(options()$knitr.in.progress)) {
      w[1] <- paste("```", "\n", w[1], sep="")
      w[length(w)] <- paste(w[length(w)], "\n", "```", sep="")
    }

    a <- w
  }

  return(a)
}
