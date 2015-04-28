SummaryStats <-
function(x=NULL, by=NULL, data=mydata, n.cat=getOption("n.cat"), 
    digits.d=NULL, brief=getOption("brief"), ...)  {


  x.name <- deparse(substitute(x))
  options(xname = x.name)

  is.df <- FALSE  # is data frame


# -----------------------------------------------------------------
# determine if x is multiple variables, a data frame or a vars list

  if (missing(x)) {
    x.name <- ""  # in case x is missing, i.e., data frame mydata
    is.df <- TRUE
    if (missing(data)) data <- eval(substitute(mydata))
  }

  else if ( (!grepl(":", x.name) && !grepl(",", x.name)) ) {  # not a var list
    if (exists(x.name, where=1)) if (is.data.frame(x)) {
      data <- x
      is.df <- TRUE
    }
  }

  # proceed here only if x.name is a var list
  else if (grepl(":", x.name) || grepl(",", x.name) ) {
    all.vars <- as.list(seq_along(data))
    names(all.vars) <- names(data)
    x.col <- eval(substitute(x), envir=all.vars, enclos=parent.frame())
    data <- data[, x.col]  # create subset data frame
    is.df <- TRUE
  }


# -----------------------------------------------------
# do the analysis for a single variable or a data frame 

  if (!is.df) {

    dname <- deparse(substitute(data))
    options(dname = dname)

    # get conditions and check for data existing
    xs <- .xstatus(x.name, dname)
    is.frml <- xs$ifr
    in.global <- xs$ig 

    # warn user that old formula mode no longer works
    if (is.frml) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Instead, of 'Y ~ X', now use the by option, 'Y, by=X' \n\n")
    }

    # see if the variable exists in data frame, if x not in Global Env 
    if (!in.global) .xcheck(x.name, dname, data)

    if (!in.global) x.call <- eval(substitute(data$x))
    else {  # vars that are function names get assigned to global
      x.call <- x
      if (is.function(x.call)) x.call <- eval(substitute(data$x))
    }

    # evaluate by
    if (!missing(by)) {

      # get actual variable name before potential call of data$x
      y.name <- deparse(substitute(by)) 
      options(yname = y.name)

      # see if y exists from a function call
      # indicate a function call with sys.frame returns larger than 1 
      if (exists(y.name, where=parent.frame(n=1)) && sys.nframe() > 1) 
        in.call <- TRUE else in.call <- FALSE

      # get conditions and check for data existing
      if (!in.call) {
        xs <- .xstatus(y.name, dname)
        in.global <- xs$ig 
      }
      else in.global <- FALSE

      # see if var exists in data frame, if x not in Global Env or function call 
      if (!in.global && !in.call) .xcheck(y.name, dname, data)

      if (!in.global) y.call <- eval(substitute(data$by))
      else {  # vars that are function names get assigned to global
        y.call <- by
        if (is.function(y.call)) y.call <- eval(substitute(data$by))
      }
    }
    else y.call <- NULL

  }  # x not data frame


  if (!is.df) nu <- length(unique(na.omit(x.call)))

  if (is.df)
    .ss.data.frame(data, n.cat, brief, ...) 

  else if (is.numeric(x.call)) {
    if (nu > n.cat) { 
      sk <- NA; kt <- NA; q1 <- NA; q3 <- NA;  qr <- NA;
      stuff <- .ss.numeric(x.call, y.call, data, digits.d, brief, ...)
      txsts <- stuff$tx
      txotl <- .outliers2(x.call)
    }
    else {
      cat("\n>>> Variable is numeric, but only has", nu, "<= n.cat =", n.cat, "levels,",
        "so treat as categorical.\n",
        "   To obtain the numeric summary, decrease  n.cat  to indicate a lower\n",
        "   number of unique values such as with function: set.\n", 
        "   Perhaps make this variable a factor with the R factor function.\n")
      stuff <- .ss.factor(x.call, y.call, brief, digits.d, ...)
    }
  }

  # ordered factors have two attributes, "ordered" and "factor"
  else if (is.factor(x.call)  ||  is.character(x.call)) {

    if (is.factor(x.call)) {
      stuff <- .ss.factor(x.call, y.call, brief, digits.d, ...)
      n.dim <- stuff$n.dim
      if (n.dim == 1) {
        txttl <- stuff$title
        txsts <- stuff$tx
        frq <- stuff$frq
        prp <- stuff$prp
      }
      else if (n.dim == 2) {
        txttl <- stuff$txttl
        txfrq <- stuff$txfrq
        txprp <- stuff$txprp
        txcol <- stuff$txcol
        txrow <- stuff$txrow
      }
    }

  else if (is.character(x.call))
    if (nlevels(factor(x.call)) < length(x.call)) { 
      stuff <- .ss.factor(factor(x.call), by, brief, n.cat, digits.d, ...)
      n.dim <- stuff$n.dim
      if (n.dim == 1) {
        txttl <- stuff$title
        txsts <- stuff$tx
        frq <- stuff$frq
        prp <- stuff$prp
      }
      else if (n.dim == 2) {
        txttl <- stuff$txttl
        txfrq <- stuff$txfrq
        txprp <- stuff$txprp
        txcol <- stuff$txcol
        txrow <- stuff$txrow
      }

    }
    else cat("\n Appears to contain unique Names or IDs", "\n")

  }

  else {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "The variable to be analyzed must be numeric or a factor, or have\n",
        "character values that can be converted to a factor, or logical values\n",
        "that can be converted to numerical 0 and 1.\n")
  }

  if (!is.df) { 

    if (is.numeric(x.call)) {
      class(txsts) <- "out_piece"
      class(txotl) <- "out_piece"

      output <- list(out_stats=txsts, out_outliers=txotl, n=stuff$n,
           n.miss=stuff$n.miss, mean=stuff$m, sd=stuff$s, skew=stuff$sk,
           kurtosis=stuff$kt, min=stuff$mn, quartile1=stuff$q1,
           median=stuff$md, quartile3=stuff$q3, max=stuff$mx, IQR=stuff$qr)
    }

    else if (is.factor(x.call)  ||  is.character(x.call)) {
      if (n.dim == 1) {
        class(txttl) <- "out_piece"
        class(txsts) <- "out_piece"
        class(frq) <- "out_piece"
        class(prp) <- "out_piece"
        output <- list(out_title=txttl, out_stats=txsts, freq=frq, prop=prp)
      }
      else if (n.dim == 2) {
        if (brief) {
          class(txttl) <- "out_piece"
          class(txfrq) <- "out_piece"
          output <- list(out_title=txttl, out_freq=txfrq)
        }
        else {
          class(txprp) <- "out_piece"
          class(txcol) <- "out_piece"
          class(txrow) <- "out_piece"
          output <- list(out_title=txttl, out_freq=txfrq,
             out_prop=txprp, out_colsum=txcol, out_rowsum=txrow)
        }
      }
    }

    class(output) <- "out_all"

    return(output)

  }

}
