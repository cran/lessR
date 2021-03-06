pivot <-
function(data, compute, variable, by=NULL, by_cols=NULL, rows=NULL,
         na_by_show=TRUE, na_remove=TRUE, sort=NULL, 
         out_names=NULL, factors=FALSE, q_num=4,
         table_prop=c("none", "all", "row", "col"),
         show_n=TRUE, digits_d=3, quiet=getOption("quiet")) {

  table_prop <- match.arg(table_prop)
  out.nm.miss <- missing(out_names)

  # length(x) is count_n(x) + count_NA(x)
  count_n <- function(x) sum(!is.na(x))
  count_NA <- function(x) sum(is.na(x))


  # -----------------------------------------------------------
  # ---- identify the compute functions, variable and by variables

  # missing variables in by vars, automatically dropped
  # missing variables in vars, NA in aggregated output

  data.vars <- as.list(seq_along(data))
  names(data.vars) <- names(data)

  # subset rows of input data frame
  ind <- eval(substitute(variable), envir=data.vars)  # col num of each var
  if (!missing(rows)) {  # subset rows
    r <- eval(substitute(rows), envir=data, enclos=parent.frame())
    r <- r & !is.na(r)  # set missing for a row to FALSE
    data <- data[r,,drop=FALSE]
  }

  # -----------------
  # compute functions

  # value of compute must be a recognized object, such as a function
  # cmpt.vc <- eval(parse(text=nm.cmp))  # access functions from their names
  raw.cmp <- deparse(substitute(compute))  # recognized as class function
  n.cmp <- length(compute)  # n of compute functions, not including quantile

  if (n.cmp == 1)
    nm.cmpt <- raw.cmp
  else {
    raw.cmp <- substr(raw.cmp, 3, nchar(raw.cmp)-1)  # remove "c(", ")"
    nm.cmpt <- unlist(strsplit(raw.cmp, ",", fixed=TRUE))  # get each function
    nm.cmpt <- gsub(" ", "", nm.cmpt, fixed=TRUE)  # remove spaces
  }

  qflag <- FALSE
  if ("quantile" %in% nm.cmpt) {  # remove and process quantile separately
    qflag <- TRUE
    n.cmp <- n.cmp - 1
    if (n.cmp > 0) {
      ipos <- which(nm.cmpt %in% "quantile")
      nm.cmpt <- nm.cmpt[-ipos]  # remove from name list
      compute <- compute[-ipos]  # remove from function list
    }
  }

  tflag <- FALSE
  if ("table" %in% nm.cmpt) {  # remove and process quantile separately
    tflag <- TRUE
    n.cmp <- n.cmp - 1
    if (n.cmp > 0) {
      ipos <- which(nm.cmpt %in% "table")
      nm.cmpt <- nm.cmpt[-ipos]  # remove from name list
      compute <- compute[-ipos]  # remove from function list
    }
  }

  # abbreviation dictionary for function names
  fun.vec <- c("sum", "mn", "md", "min", "max", "sd", "var", "IQR", "mad",
               "", "", "sk", "kt", "tbl")
  names(fun.vec) <- c("sum", "mean", "median", "min", "max",
                      "sd", "var", "IQR", "mad", "range", "quantile",
                      "skew", "kurtosis", "table")


  # ---------
  # variables

  # get index of variable(s)
  ind.var.d <- eval(substitute(variable), envir=data.vars, parent.frame())
  n.var <- length(ind.var.d)  # n of variables
  nm.var.d <- names(data)[ind.var.d]
  if (is.null(out_names))
    out_names <- paste(nm.var.d, "_", fun.vec[nm.cmpt], sep="")


  # ------- 
  # by vars

  if (!missing(by) || nm.cmpt[1] == "tabulate") {
    # get indices of by variable(s)
    x <- str2lang(deparse(substitute(by)))  # char string to type calls
    ind.r.by <- eval(substitute(x), envir=data.vars, parent.frame())
    nm.row.by <- names(data)[ind.r.by]

    if (missing(by))
      ind.r.by <- ind.var.d  # for tabulate do not have variables

    # save original variable types of by vars because aggregate makes factors
    n.r.by <- length(ind.r.by)
    cls <- character(length=n.r.by)
    for (i in 1:n.r.by) cls[i] <- class(data[,ind.r.by[i]])

    # get indices of by_cols variable(s)
    if (!missing(by_cols)) {
      x <- str2lang(deparse(substitute(by_cols)))  # char string to type calls
      ind.c.by <- eval(substitute(x), envir=data.vars, parent.frame())
      n.c.by <- length(ind.c.by)
      nm.col.by <- names(data)[ind.c.by]
    }
    else {
      ind.c.by <- NULL
      n.c.by <- 0
    }

    # collapse by variables to a single by for aggregation into long form
    n.by <- n.r.by + n.c.by
    ind.by <- c(ind.r.by, ind.c.by)

    # convert by variables to factors
    if (n.by > 1) {
      data[, ind.by] <- lapply(data[, ind.by], factor)
      by.vars <- as.list(data[, ind.by])
    }
    else {
      data[, ind.by] <- factor(data[, ind.by])
      by.vars <- list(data[, ind.by])
      names(by.vars) <- deparse(substitute(by))
    }
  }  # end by is specified or tabulate

  else {
    n.by <- 0
    n.c.by <- 0
  }


  # ------------
  # Not Possible

  if (tflag && n.cmp > 0 && missing(by))  {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Cannot have more than one statistic specified\n",
      "  when computing a frequency table over all the data.\n\n")
  }

  if (n.c.by > 2)  {
    nms.c <- ""
    for (i in 1:n.c.by) nms.c <- paste(nms.c, names(data)[ind.c.by[i]])
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Specified column  by  variables: ", nms.c, "\n",
      "Only two column  by  variables permitted\n\n")
  }
  if (tflag && n.var > 1)  {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Cannot have more than one variable to aggregate\n",
      "  when computing a frequency table.\n\n")
  }
  if (n.var > 1 && n.cmp > 1 && n.by > 0)  {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Cannot have multiple compute functions, multiple variables, \n",
      "  and do aggregation with a by variable.\n\n")
  }

  if (!is.null(sort)) {
    if (!(sort %in% c( "+", "-"))) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Parameter  sort  can only be \"+\" or \"-\"\n\n")
    }
  }


  # -------------------------------------------
  # ------- no aggregation, all the data ------

  # if table over all the data, then a single freq dist
  if (missing(by) && nm.cmpt[1] == "table") {
    tbl <- table(data[,ind.var.d])
    a <- data.frame(tbl)
    names(a)[1] <- nm.var.d[1]
    a$Prop <- round(a$Freq / sum(tbl), digits_d)
    return(a) 
  }

  if (missing(by) && nm.cmpt[1] != "tabulate") {
    a <- NULL
    out <- double(length=n.cmp)

    if (n.cmp > 0) {
      # one of compute or variable, output same format as with by vars, 1-line

      if (!(n.cmp>1 && n.var>1)) {
        for (i in 1:n.var) {
          n <- count_n(data[,nm.var.d[i]])
          na <- count_NA(data[,nm.var.d[i]])
          for (j in 1:n.cmp) {
            txt <- paste("data[,", ind.var.d[i], "], na.rm=na_remove", sep="")
            f.call <- paste(nm.cmpt[j], "(", txt, ")", sep="")
            out[j] <- eval(parse(text=f.call))  # evaluate compute function
            a.l = length(a)
            if (j==1) {  # first (possibly only) compute function
                a <- c(a, n, na, out[j])
                names(a)[a.l+1] <- paste("n_",nm.var.d[i],sep="")
                names(a)[a.l+2] <- paste("na_",nm.var.d[i],sep="")
                names(a)[a.l+3] <- out_names[i]
            }
            else {  # j > 1, so multiple compute functions
              a = c(a, out[j])
              names(a)[a.l+1] <- out_names[j]
            }
          }  # end j
        }  # end i

        a <- as.matrix(a)  # turn vector a into a data frame
        a <- data.frame(t(a))
        txt <- ifelse (n.cmp == 1, nm.cmpt[1], "Stat")
        if (!(txt %in% c("Stat", "sd")))
          substr(txt,1,1) <- toupper(substr(txt,1,1))
        row.names(a)[1] <- paste("Grand_", txt, sep="")
      }  # end just one of compute or variables

      # do both multiple computes and multiple variables with tabular format
      else {  
        for (i in 1:n.var) {
          n <- count_n(data[,nm.var.d[i]])
          na <- count_NA(data[,nm.var.d[i]])
          txt <- paste("data[,", ind.var.d[i], "], na.rm=na_remove", sep="")
          for (j in 1:n.cmp) {
            f.call <- paste(nm.cmpt[j], "(", txt, ")", sep="")
            out[j] <- eval(parse(text=f.call))  # evaluate compute function
          }  # end j
            a <- data.frame(rbind(a, c(n, na, out)))  # a has multiple rows
        }  # end i

        names(a) <- c("n", "na", nm.cmpt)
        row.names(a) <- nm.var.d
      }
    }  # end n.cmp > 0

    if (qflag) {  # do quantiles here
      for (i in 1:n.var) {
        n <- count_n(data[,nm.var.d[i]])
        na <- count_NA(data[,nm.var.d[i]])
        a2 <- quantile(data[,nm.var.d[i]], na.rm=TRUE, probs=seq(0,1,1/q_num))
        a2 <- c(n, na, a2)
        a2 <- data.frame(t(a2))
        names(a2)[n.by+1] <- "n_"
        names(a2)[n.by+2] <- "na_"
        if (i == 1)
          aq <- a2
        else
          aq <- rbind(aq, a2)
      }
      for (i in 1:length(names(aq))) {   # delete ending . in var names
        nm <- names(aq)[i]
        if (substr(nm, nchar(nm), nchar(nm)) == ".")
          names(aq)[i] <- substr(nm, 1, nchar(nm)-1)
      }
      names(aq) <- gsub("X", "p_", names(aq), fixed=TRUE) 
      row.names(aq) <- nm.var.d
    }  # end qflag

    # quant and non-quant done separately, here set final a
    if (qflag) {
      if (n.cmp == 0)
        a <- aq
      else 
        a <- cbind(a, aq[,3:ncol(aq)])
    }

    if (!show_n) {
      not.n <- which(c(substr(names(a),1,2)) == "n_")
      not.n <- c(not.n, which(c(substr(names(a),1,3)) == "na_"))
      a <- a[,-not.n]
    }
       
    # round, leaves true integers (1.00) displayed as an integer (1)
    if (!is.null(digits_d)) a <- round(a, digits_d)

    # sort
    if (!is.null(sort)) {
      direction <- ifelse (sort=="+", FALSE, TRUE)
#     if (!is.null(sort_var)) {
#       if (is.numeric(sort_var))
#         s.col <- sort_var
#       else
#         s.col <- which(names(a) == sort_var)
#     }
#     else
        s.col <- ncol(a)
      a <- a[order(a[,s.col], decreasing=direction), ]
    }

    return(a)
  }  # no by specified and not tabulate


  # ------------------------------------------
  # --------------- tabulate -----------------

    if (nm.cmpt[1] == "tabulate") {
      use_na <- ifelse (!na_by_show, "no", "ifany")
      a <- table(data[, ind.by], useNA=use_na)
      a <- as.data.frame(a)
      if (n.by == 1) names(a)[1] <- deparse(substitute(variable))
      names(a)[ncol(a)] <- "n"

      return(a)
    }

  # ------------------------------------------
  # --------------- aggregate ----------------

  # aggregate over a numerical variable (by vars become factors)
  if (n.cmp == 1) {
    if (qflag || tflag) 
      a <- aggregate(data[,ind.var.d], by=by.vars, drop=FALSE, FUN=compute[[1]],
                     na.rm=na_remove)  # compute for a cell even if missing
    else
      a <- aggregate(data[,ind.var.d], by=by.vars, drop=FALSE, FUN=compute,
                     na.rm=na_remove)  # compute for a cell even if missing
    if (n.var == 1)
      names(a)[ncol(a)] <- nm.var.d[1]
  }

  else if (n.cmp > 1) { 
    a <- aggregate(data[,ind.var.d], by=by.vars, drop=FALSE, FUN=compute[[1]],
                   na.rm=na_remove)  # compute for a cell even if a missing
    for (i in 2:n.cmp) {
      ac <- aggregate(data[,ind.var.d], by=by.vars, drop=FALSE,
               FUN=compute[[i]], na.rm=na_remove)
      a <- merge(a, ac, by=names(by.vars), sort=FALSE)
    }
  }

  n <- aggregate(data[,ind.var.d], by=by.vars, drop=FALSE, FUN=count_n)
  n_NA <- aggregate(data[,ind.var.d], by=by.vars, drop=FALSE, FUN=count_NA)
  # merge columns to form complete aggregate data frame: a
  # pull by vars and aggregated var from original a
  if (n.cmp > 0) {
    a <- merge(a, n, by=names(by.vars), sort=FALSE)
    a <- merge(a, n_NA, by=names(by.vars), sort=FALSE)
  }
  else
    a <- merge(n, n_NA, by=names(by.vars), sort=FALSE)

  # if there is a non-quantile computation function
  if (n.cmp > 0) {
    if (length(ind.var.d) == 1)
      names(a)[ncol(a)] <- deparse(substitute(variable))
    if (length(ind.by) == 1)
      names(a)[1] <- deparse(substitute(by))

    if (!factors) {  # retain original variable type of by vars
      for (i in 1:n.r.by) {
        if (cls[i] == "Date") a[,i] <- as.Date(as.character(a[,i]))
        if (cls[i] == "character") a[,i] <- as.character(a[,i])
        if (cls[i] == "integer") a[,i] <- as.integer(as.character(a[,i]))
        if (cls[i] == "logical") a[,i] <- as.logical(as.character(a[,i]))
      }
    }

    a2 <- a[, 1:n.by, drop=FALSE]  # temp, start by var(s) in the first col(s)

    if (n.cmp == 1) {
      col.val <- matrix(, nrow=0, ncol=3)
      for (i in 1:n.var) {  # identify variables in a
        if (n.var == 1)  # by position
          col.val <- rbind(col.val, c(n.by+1,n.by+2,n.by+3))  # next 3 cols
        else  # by name, agg structure includes names
          col.val <- rbind(col.val, which(grepl(nm.var.d[i], names(a),
                           fixed=TRUE)))
      }
      # add columns to temp a2 data frame
      for (i in 1:n.var) {
         a2 <- cbind(a2, a[, col.val[i,2:3]])
         a2 <- cbind(a2, a[, col.val[i,1]])
      }
      a <- a2

      k <- n.by
      for (i in 1:n.var) {  # for each variable, cover n, na, aggregated stat
        names(a)[k+1] <- paste("n", "_", nm.var.d[i], sep="")
        names(a)[k+2] <- paste("na", "_", nm.var.d[i], sep="")
        names(a)[k+3] <- nm.var.d[i]
        k <- k + 3
      }
    }  # end n.cmp==1

    else {  # n.cmp > 1 and n.var == 1
      a2 <- cbind(a2, a[, (ncol(a)-1):ncol(a)])
      a2 <- cbind(a2, a[, (n.by+1):(n.by+n.cmp)])
      a <- a2

      k <- n.by
      names(a)[k+1] <- paste("n", "_", nm.var.d[1], sep="")
      names(a)[k+2] <- paste("na", "_", nm.var.d[1], sep="")
      for (i in 1:n.cmp) {
        names(a)[k+2+i] <- nm.var.d[1]  # here all agg vars get the var name
      }
    }  # end n.cmp > 1 and n.var==1

      ind.var <- which(names(a) %in% names(data)[ind.var.d])  # variables in a
      names(a)[ind.var] <- out_names
  }  # end n.cmp>0


  # if there is a table computation
  if (tflag) {

    # get n_ and na_ variables
    a1 <- aggregate(data[,ind.var.d], by=by.vars, drop=FALSE, FUN=count_n)
    a2 <- aggregate(data[,ind.var.d], by=by.vars, drop=FALSE, FUN=count_NA)
    amd <- merge(a1, a2, by=names(by.vars), sort=FALSE)
    names(amd)[n.by+1] <- paste("n", "_", nm.var.d[1], sep="")
    names(amd)[n.by+2] <- paste("na", "_", nm.var.d[1], sep="")

    # calculate the freqs level by level of the aggregation variable
    lvl <- sort(unique(data[,ind.var.d]))
    for (i in 1:length(lvl)) {
      dtmp <- data[data[,ind.var.d]==lvl[i], , drop=FALSE]  # lvl[i] subset
      if (n.by > 1)
        by.v <- as.list(dtmp[,ind.by])
      else
        by.v <- list(dtmp[,ind.by])
      names(by.v) <- nm.row.by
      a2=aggregate(dtmp[,ind.var.d], by=by.v, drop=FALSE, FUN=count_n)
      for (j in 1:nrow(a2)) if (is.na(a2[j,ncol(a2)])) a2[j,ncol(a2)] <- 0
      amd <- merge(amd,a2, by=names(by.vars), sort=FALSE)
      if (out.nm.miss)
        l.nm <-lvl[i]
#       l.nm <- paste(abbreviate(nm.var.d[1], 3), "_", lvl[i], sep="")
      else
        l.nm <- out_names[i]
      names(amd)[ncol(amd)] <- l.nm # lvl[i]
    }

    if (table_prop %in% c("all", "row", "col")) {
      nca <- ncol(amd)
      ll <- length(lvl)

      dp <- amd[,(nca-ll+1):nca, drop=FALSE]  # pull out freq table
      tdb <- as.matrix(dp)

      if (table_prop == "all") tdb <- proportions(tdb)
      if (table_prop == "row") tdb <- proportions(tdb,1)
      if (table_prop == "col") tdb <- proportions(tdb,2)

      if (!quiet) {
        txt <- ifelse (table_prop=="col", "column", table_prop)
        cat("\nProportions computed over", txt, "cells\n\n")
      }
      amd <- cbind(amd[,1:(nca-ll)], tdb)  # replace freqs with props
    }

    if (n.cmp == 0)  
      a <- amd
    else {  # more than just table already processed
      not.n <- which(c(substr(names(amd),1,2)) == "n_")
      not.n <- c(not.n, which(c(substr(names(amd),1,3)) == "na_"))
      amd <- amd[,-not.n]  # remove what will be redundant columns
      a <- cbind(a,amd[(n.by+1):ncol(amd)])
    }
  }  # end tflag


  # if there is a quantile computation
  if (qflag) {
    by_i <- 1 / q_num
    aq <- aggregate(data[,ind.var.d], by=by.vars, drop=FALSE, FUN=quantile,
                   prob=seq(0,1,by_i), na.rm=na_remove) 
    am <- as.matrix(aq)  # convert aggregation to a data frame
    amd <- data.frame(am)
    nm.v <- paste(nm.var.d[1], "_", sep="")
    names(amd) <- gsub("x.", nm.v, names(amd), fixed=TRUE) 
    for (i in 1:length(names(amd))) {   # delete ending . in var names
      nm <- names(amd)[i]
      if (substr(nm, nchar(nm), nchar(nm)) == ".")
        names(amd)[i] <- substr(nm, 1, nchar(nm)-1)
    }
    amd[] <- lapply(amd[], type.convert, as.is=TRUE)  # recover true var types
    if (n.cmp == 0) {  # quantile is only specified compute function
      ss <- merge(n, n_NA, by=names(by.vars), sort=FALSE)
      a <- merge(ss, amd, by=names(by.vars), sort=FALSE)
      names(a)[n.by+1] <- "n_"
      names(a)[n.by+2] <- "na_"
    }
    else {  # more than just quantiles already processed
      a <- cbind(a,amd[(n.by+1):ncol(amd)])
    }
  }  # end qflag


  # ----- post-computation processing -----

  # missing by variables data: set n to 0 or remove if specified
  n_.ind <- which(substr(names(a), 1,2) == "n_")
  nm.n_.var <- names(a)[n_.ind]
  nm.na_.var <- names(a)[n_.ind+1] 
  ind0 <- logical(nrow(a))  # initializes to FALSE
  for (i in 1:nrow(a)) {
    for (j in 1:length(nm.n_.var)) {
      if (is.na(a[i,nm.na_.var[j]])) a[i,nm.na_.var[j]] <- 0
      if (is.na(a[i,nm.n_.var[j]])) {
        a[i,nm.n_.var[j]] <- 0
        ind0[i] <- TRUE
      }
    }
  }  # end 1:nrow(a)
  if (!na_by_show) a <- a[!ind0,]

  # drop n_ and na_ columns if specified
  if (!show_n) {
    not.n <- which(c(substr(names(a),1,2)) == "n_")
    not.n <- c(not.n, which(c(substr(names(a),1,3)) == "na_"))
    a <- a[,-not.n]
  }

  # round if specified
  ind.var <- (n.by+1):ncol(a)
  i.s <- which(names(a) %in% names(a)[ind.var])
  if (!is.null(digits_d)) a[,i.s] <- round(a[,i.s], digits_d)

  # sort if specified
  if (!is.null(sort)) {
    direction <- ifelse (sort=="+", FALSE, TRUE)
#   if (!is.null(sort_var)) {
#     if (is.numeric(sort_var))
#       s.col <- sort_var
#     else
#       s.col <- which(names(a) == sort_var)
#   }
#   else
      s.col <- ncol(a)
    a <- a[order(a[,s.col], decreasing=direction), ]
  }



  # --------------------------------------------------
  # --- option to reshape long form a to wide form ---

  if (n.c.by > 0) {

    if (n.cmp > 1)  {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Cannot have both more than 1 compute function for making a table.\n\n")
    }

    if (n.var > 1)  {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Cannot have more than 1 variable to aggregate for making a table.\n\n")
    }

    nm.var.a <- names(a)[ind.var]  # preserve original names

    # delete unneeded variables
    n.ind <- which(substr(names(a),1,2) == "n_")
    miss.ind <- which(substr(names(a),1,3) == "na_")
    a <- a[, -c(n.ind, miss.ind)]

    # re-reference pivot variables in aggregated data frame a
    i.s <- which(names(a) %in% nm.var.a)
    i.r <- which(names(a) %in% nm.row.by)
    i.c <- which(names(a) %in% nm.col.by)

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

    # if var by_cols names larger than by row name, pad with "|"
    mx <- max(nchar(names(a)[i.c]))
    nm.r1 <- names(a)[i.r[1]]
    if (nchar(nm.r1) < mx) {
      dff <- mx - nchar(nm.r1)
      buf <- paste(rep("|", dff), sep="", collapse="")
      nm.buf <- paste(names(a)[i.r[1]], buf, sep="")
      names(w)[1] <- nm.buf
    }

    # remove all by names, get " -" locations from kable
    for (i in (n.r.by+1):length(names(w))) names(w)[i] <- " "
    k <- kable(w, format="pandoc", digits=2, caption=" ", align="r")
    g <- gregexpr(" -", k[4], fixed=TRUE)
    g <- unlist(g)

    # get line of blank chars to fill later, including 1st by col name
    nc <- nchar(k[4])
    ln1 <- paste(rep(" ", nc), sep="", collapse="")
    ln1 <- paste(" ", names(a)[i.c[1]], ln1, sep="")

    # fill level labels for one by_cols var
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

    # fill level labels for two by_cols vars
    else if (n.c.by == 2) {
      lvl1 <- levels(a[,i.c[1]])
      lvl2 <- levels(a[,i.c[2]])

      ln2 <- paste(rep(" ", nc), sep="", collapse="")  # blank line
      ln2 <- paste(" ", names(a)[i.c[2]], ln2, sep="") # add var name

      # labels for first by_cols var
      for (i in 1:length(lvl1)) {
        start <- g[n.r.by + (i-1)*length(lvl2)] + 1
        stop <- start + nchar(lvl1[i])
        substr(ln1, start, stop) <- lvl1[i]
      }

      # labels for second by_cols var
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

    # caption, with added blank line
    cpt <- paste(nm.cmpt[1], "of", names(data)[ind.var.d], "\n")
    w <- kable(w, format="pandoc", digits=2, caption=cpt, align="r")
    # wc <- character(length=length(w))
    # for (i in 1:length(w)) wc[i] <- w[i]

    if (n.c.by == 1)  # set second line of table w
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
  }  # end n.c.by > 0
  # ------------------------------------------


  # display all unique non-numeric vars, data drilled down to specific cases
  # data <- na.omit(data)  # remove missing data for comparison of values
  nm <- character(length=ncol(data))
  k <- 0
  for (i in 1:ncol(data)) {  # identify unique non-numeric variables
    if (length(unique(data[,i])) == 1) {
      k <- k + 1
      nm[k] <- names(data)[i]  # name of variable that is not unique
    }
  }
  nm.TF <- (names(data) %in% nm)  # names of unique variables set to TRUE
  if (any(nm.TF)) {
    for (i in 1:ncol(data)) {
      if (nm.TF[i]) if (is.numeric(data[[i]]) && !.is.integer((data[[i]])))
        nm.TF[i] <- FALSE  # drop numeric vars that are not integers
    }
    for (i in 1:ncol(data)) {
      dval <- data[1,i]
      if (class(data[1,i]) == "Date") dval <- as.character(data[1,i])
      if (is.factor(data[1,i])) dval <- as.character(data[1,i])
      if (nm.TF[i])
        cat(names(data)[i], ": ", dval, sep="", "\n")
    }
    cat("\n")
  }

  return(a)
}
