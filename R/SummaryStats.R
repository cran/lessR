SummaryStats <-
function(x=NULL, by=NULL, data=d, rows=NULL, n_cat=getOption("n_cat"), 
    digits_d=NULL, brief=getOption("brief"), label_max=20, ...)  {

  message(">>> Deprecated<<<\n\n",
      "New, more flexible replacement is the pivot() function.\n",
      "For inferential tests of proportions, the Prop_test() function.\n",
      "For explanation see the vignettes.\n\n",
      "Enter: browseVignettes(\"lessR\")\n",
      "Select: Summary Statistics with a Pivot Table\n",
      "Select: Proportion Inference\n\n")

  # a dot in a parameter name to an underscore
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (names(dots)[i] == "values.cex")  values_size <- dots[[i]]
      if (grepl(".", names(dots)[i], fixed=TRUE)) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }

  # get variable name before potential call of data$x
  x.name <- deparse(substitute(x))  # could be a vars list
  options(xname = x.name)

  data.miss <- ifelse (missing(data), TRUE, FALSE) 

  # let deprecated mydata work as default
  dfs <- .getdfs() 
  mydata.ok <- FALSE
  if ("mydata" %in% dfs  &&  !("d" %in% dfs)) {
    d <- mydata
    df.name <- "mydata"
    mydata.ok <- TRUE
    options(dname = df.name)
  }
  if (!mydata.ok) {
    df.name <- deparse(substitute(data))  # get name of data table
    options(dname = df.name)
  }
  
  x.in.df <- FALSE
  if (nchar(x.name) > 0)
    if (exists(df.name, where=.GlobalEnv, inherits=FALSE))
      if (exists(x.name, where=data)) x.in.df <- TRUE
 
  # if a tibble convert to data frame
  if (df.name %in% ls(name=.GlobalEnv)) {  # tibble to df
   if (any(grepl("tbl", class(data), fixed=TRUE))) {
      data <- data.frame(data)
   }
  }

  is.df <- FALSE  # is data frame


  # evaluate by, i.e., get y.call
  if (!missing(by)) {

    # get actual variable name before potential call of data$x
    y.name <- deparse(substitute(by)) 
    options(yname = y.name)

    # see if y exists from a function call
    # indicate a function call with sys.frame returns larger than 1 
    if (exists(y.name, where=parent.frame(n=1)) && sys.nframe() > 1) 
      in.call <- TRUE else in.call <- FALSE

    # get conditions and check for data existing
    if (!in.call)
      in.style <- .in.global(y.name, quiet=FALSE)
    else
      in.style <- FALSE

    # see if var exists in data frame, if x not in style Env or function call 
    if (!in.style && !in.call) .xcheck(y.name, df.name, names(data))

    if (!in.style) y.call <- eval(substitute(data$by))
    else {  # vars that are function names get assigned to style
      y.call <- by
      if (is.function(y.call)) y.call <- eval(substitute(data$by))
    }
  }
  else
    y.call <- NULL


# -----------------------------------------------------------------
# establish if a data frame, if not then identify variable(s)

  if (!missing(x)) {

    # x not in global env, in df, specify data= forces to data frame
    if (!exists(x.name, where=.GlobalEnv) || x.in.df) {
      .nodf(df.name)  # check to see if data frame container exists     
      .xcheck(x.name, df.name, names(data))  # var in df?, vars lists not checked
      all.vars <- as.list(seq_along(data))  # even if only a single var
      names(all.vars) <- names(data)  # all data in data frame
      ind <- eval(substitute(x), envir=all.vars)  # col num selected vars
      if (!missing(rows)) {  # subset rows
        r <- eval(substitute(rows), envir=data, enclos=parent.frame())
        r <- r & !is.na(r)  # set missing for a row to FALSE
        data <- data[r,,drop=FALSE]
      }
      if (!("list" %in% class(data))) {
        data <- data[, ind]
        if (length(ind) == 1) {  # x is 1 var
          data <- data.frame(data, stringsAsFactors=TRUE)
          names(data) <- x.name
         }
      }
      else {  # class of data is "list"
        data <- data.frame(data[[ind]], stringsAsFactors=TRUE)
        names(data) <- x.name
      }
    }

    else {  # x is in the global environment (vector or data frame)
      if (is.data.frame(x))  # x a data frame
        data <- x
      else {  # x a vector in style
        .in.global(x.name, quiet=FALSE) 
        if (!is.function(x))
          data <- data.frame(x, stringsAsFactors=TRUE)  # x is 1 var
        else  # x is 1 var
          data <- data.frame(eval(substitute(data$x)), stringsAsFactors=TRUE)
        names(data) <- x.name
      }
    }

  }


# -----------------------------------------------------
# data is now set
# do the analysis for a single variable or a data frame 

  # see if a single variable is categorical; if so, make a factor
  if (ncol(data) == 1) {
    x.call <- data[,1]

    num.cat <- .is.num.cat(x.call, n_cat)
    nu <- length(unique(na.omit(x.call)))

    if (!num.cat && is.integer(x.call) && nu <= n_cat) {
      cat("\n")
      cat(">>> ", x.name, " has only only ", nu, " unique ",
          "integer values, but not equally spaced,\n",
          "      so treat as numerical in this analysis\n",
          "    Convert to an R factor to treat as categorical\n\n",
          sep="")
    }

    if (num.cat || is.character(x.call)) {
      x.call <- as.factor(x.call)
     .ncat("summary statistics", x.name, nu, n_cat)
    }
  }

  if (ncol(data) > 1)
    .ss.data.frame(data, n_cat, brief, ...) 

  else if (!is.factor(x.call)) {  # numeric analysis

    sk <- NA; kt <- NA; q1 <- NA; q3 <- NA;  qr <- NA;
    if (exists("y.name"))
      nm <- y.name
    else
      nm <- NULL
    stuff <- .ss.numeric(x.call, y.call, digits_d, brief, y.name=nm, ...)
    txsts <- stuff$tx
    txotl <- .bx.stats(x.call)$txotl

  }

  # ordered factors have two attributes, "ordered" and "factor"
  else {  # is.factor(x.call)

    gl <- .getlabels(graph.win=FALSE)
    x.name <- gl$xn; x.lab <- gl$xb; x.lbl <- gl$xl
    y.name <- gl$yn; y.lab <- gl$yb; y.lbl <- gl$yl

    stuff <- .ss.factor(x.call, y.call, brief, digits_d,
                      x.name, y.name, x.lbl, y.lbl, label_max, ...)

    n.dim <- stuff$n.dim
    if (n.dim == 1) {
      txttl <- stuff$title
      txsts <- stuff$counts
      txchi <- stuff$chi
      txlbl <- stuff$lbl
      frq <- stuff$freq
      prp <- stuff$prop
    }
    else if (n.dim == 2) {
      txttl <- stuff$txttl
      txXV <- stuff$txXV
      txfrq <- stuff$txfrq
      txprp <- stuff$txprp
      txcol <- stuff$txcol
      txrow <- stuff$txrow
    }
  }

  # else {
  #   cat("\n"); stop(call.=FALSE, "\n","------\n",
  #       "The variable to be analyzed must be numeric or a factor, or have\n",
  #       "character values that can be converted to a factor.\n")
  # }


  # not a data frame analysis
  if (ncol(data) == 1) { 

    if (!is.factor(x.call)) {
      class(txsts) <- "out"
      class(txotl) <- "out"

      output <- list(out_stats=txsts, out_outliers=txotl, n=stuff$n,
           n.miss=stuff$n.miss, mean=stuff$m, sd=stuff$s, skew=stuff$sk,
           kurtosis=stuff$kt, min=stuff$mn, quartile1=stuff$q1,
           median=stuff$md, quartile3=stuff$q3, max=stuff$mx, IQR=stuff$qr)
    }

    else {  # is.factor(x.call)
      if (n.dim == 1) {
        class(txttl) <- "out"
        class(txsts) <- "out"
        class(txchi) <- "out"
        class(txlbl) <- "out"
        output <- list(out_title=txttl, out_stats=txsts, out_chi=txchi,
                       out_lbl=txlbl, freq=frq, prop=prp)
      }
      else if (n.dim == 2) {
        class(txttl) <- "out"
        class(txfrq) <- "out"
        if (brief) {
          output <- list(out_title=txttl, out_freq=txfrq, out_chi=txXV)
        }
        else {
          class(txXV) <- "out"
          class(txprp) <- "out"
          class(txcol) <- "out"
          class(txrow) <- "out"
          output <- list(out_title=txttl, out_freq=txfrq, out_XV=txXV,
             out_prop=txprp, out_colsum=txcol, out_rowsum=txrow)
        }
      }
    }

    class(output) <- "out_all"

    return(output)

  }

}
