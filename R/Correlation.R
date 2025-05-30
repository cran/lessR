Correlation <-
function(x, y, data=d, # x can be a data frame, or variables in a data frame
         miss=c("pairwise", "listwise", "everything"),
         show=c("cor", "missing"),
         fill_low=NULL, fill_hi=NULL,
         brief=FALSE, digits_d=NULL, heat_map=TRUE,
         main=NULL, bottom=3, right=3, quiet=getOption("quiet"),
         pdf_file=NULL, width=5, height=5, ...) {


  # a dot in a parameter name to an underscore
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (length(grep(".", names(dots)[i], fixed=TRUE)) > 0) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }

  miss <- match.arg(miss)
  show <- match.arg(show)

  # replace older names with current name
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (names(dots)[i] == "graphics")  heat_map <- dots[[i]]
      if (names(dots)[i] == "show_n") {
      cat("\n"); stop(call.=FALSE, "\n------\n",
        "New parameter  show  replaces  show_n.\n",
        "Obtain pairwise missing value counts with  show=\"missing\", \n\n")
    }
    }
  }
  
  x.name <- deparse(substitute(x))
  options(xname = x.name)

  is.df <- FALSE  # is data frame

  if (missing(y)) {  # is a data frame or a list of variables
    is.df <- TRUE
    if (missing(x)) {
      x.name <- "$"  # in case x is missing, i.e., data frame d
      if (missing(data)) data <- eval(substitute(d))
    }
    else if ( (!grepl(":", x.name) && !grepl(",", x.name)) ) {
      if (is.data.frame(x)) {  # specified data name
        if (exists(x.name, where=.GlobalEnv)) data <- x
      }
    } 

    else {  # two variables
      all.vars <- as.list(seq_along(data))
      names(all.vars) <- names(data)
      # proceed here only if x.name is in data or is a list
      if ((x.name %in% names(all.vars)) || grepl(":", x.name)
            || grepl(",", x.name) ) {
        x.col <- eval(substitute(x), envir=all.vars, enclos=parent.frame())

        # if x is a variable list, create subset data frame
        if (length(x.col) > 1) {
          data <- data[, x.col]
          is.df <- TRUE
        }
      }
    }
  }  # end missing y


  if (!is.df) {  # pairwise

    dname <- deparse(substitute(data))
    options(dname = dname)

    # get conditions and check for data existing
    is.frml <- ifelse (grepl("~", x.name), TRUE, FALSE)
    in.style <- .in.global(x.name, dname)

    # see if the variable exists in data frame, if x not in style Env 
    if (!in.style) .xcheck(x.name, dname, names(data))

    if (in.style) x.call <- x else x.call <- eval(substitute(data$x))

    # evaluate y
    if (!missing(y)) {

      # get actual variable name before potential call of data$x
      y.name <- deparse(substitute(y)) 
      options(yname = y.name)

      # see if y exists from a function call
      # indicate a function call with sys.frame returns larger than 1 
      #if (exists(y.name, where=parent.frame(n=1)) && sys.nframe() > 1) 
        #in.call <- TRUE else in.call <- FALSE
      in.call <- FALSE

      # get conditions and check for data existing
      if (!in.call)
        in.style <- .in.global(y.name, dname)
      else in.style <- FALSE

      # see if var exists in data frame, if x not in style Env or function call 
      if (!in.style && !in.call) .xcheck(y.name, dname, names(data))

      if (in.style) y.call <- y 
      else y.call <- eval(substitute(data$y))
    }

    else
      y.call <- NULL

  }  # x not data frame


  if (is.df) { 
    stuff <- .cr.data.frame(data, miss, show, digits_d,
                   heat_map, fill_low, fill_hi, main, bottom, right, quiet, 
                   pdf_file, width, height, ...) 

    txmis <- stuff$txm

    class(txmis) <- "out"

    if (show == "cor") {
      if (!quiet)
        return(stuff$R)
      else
        invisible(stuff$R)
      }
    else if (show == "missing") return(txmis)
  }

  else {  # pairwise
  
    if (!is.null(pdf_file)) {
      cat("\n");   warning(call.=FALSE, "\n","------\n",
        " To produce a scatter plot, pass a vector to:  Plot\n",
        " No heat map produced here and yet a pdf file is specified.\n\n",
         sep="")
     }

    stuff <- .cr.main(x.call, y.call, brief, ...) 
    txbck <- stuff$txb
    txdsc <- stuff$txd
    txinf <- stuff$txi

    class(txbck) <- "out"
    class(txdsc) <- "out"
    class(txinf) <- "out"

    output <- list(out_background=txbck, out_describe=txdsc, out_inference=txinf,
      r=stuff$r, tvalue=stuff$tvalue, df=stuff$df, pvalue=stuff$pvalue,
      lb=stuff$lb, ub=stuff$ub)

    class(output) <- "out_all"
    return(output)
  }  # end pairwise

}
