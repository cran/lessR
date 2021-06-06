.<-
  function (x, ...) {  # return selected row or column indices

  random <- function (n) {  # random selection of rows
    n.obs <- nrow(data)
    n.obs_new <- ifelse (n > 1, n, round(n*n.obs,0))
    rand.rows <- sample(1:n.obs, size=n.obs_new, replace=FALSE)
  }

  # not necessarily the full col expression if there is a , in the col string
  x.call <- deparse(substitute(x))  # expressions passed to ., row or col

  sc <- sys.calls()
  scu <- unlist(sc)  # all function calls that lead to x, many in .Rmd
  f.call <- as.character(scu[[length(sc)-2]])  # full d[ ] function call
  df <- f.call[2]   # data frame name from full d[ ] call
  x.r <- f.call[3]  # function call for rows from full d[ ] call
  x.c <- f.call[4]  # function call for cols from full d[ ] call

  # correct for user mistake in x.r of only one = sign
  # this is evaluated for columns also, presumably no = signs there
  if (nchar(x.call) == 0) {  # happens with only one = sign
    n.eq <- length(unlist(gregexpr("=", x.r, fixed=TRUE)))
    if (n.eq == 1) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
         "Need two equal signs == for logical equality.\n\n")
    }
  }

  data <- get(df, envir=parent.frame())
  xdt <- data  # not work if data is a function name, so work on copy
  df <- paste("xdt", "$", sep="")

  # select rows, expression precedes the , in originating d[ ] call
  not.flag <- FALSE
  if (substr(x.r, 3, 3) == "!" || x.r == ".(!rows)") {  # needed for literal !rows
    not.flag <- TRUE
    if (x.r ==  ".(!rows)") x.r = ".(rows)"
  }

  # need to strip x.r down to xcall which does not contain . and !
  if (!not.flag || grepl("rows", x.r, fixed=TRUE)) {
    x.r <- substr(x.r, 3, nchar(x.r))    # remove leading .(
    x.r <- substr(x.r, 1, nchar(x.r)-1)  # remove trailing )
  }
  else {
    x.r <- substr(x.r, 5, nchar(x.r))    # remove leading .(!(
    x.r <- substr(x.r, 1, nchar(x.r)-2)  # remove trailing )
  }
  if (x.r == "rows") x.r <- eval(parse(text=x.r))
  if (!grepl("rows", x.r, fixed=TRUE))
    x.rr <- ifelse(!not.flag,  x.r, paste("!(", x.r, ")" , sep=""))
  else
    x.rr <- x.r

  # see if process rows
  if (x.call %in% c(x.rr, "rows", "!rows")) {  # select rows
    x <- x.r  # character string of the logical expression for rows
    # problem is that a name could be a subset of another name
    #   so add an initial " " for whole name to later be extracted
   x <- paste(" ", x, sep="")  
    # proceed through all xdt names, when a match add "xdt$" to the name in x
    for (i in 1:length(names(xdt))) {  # see if a variable is specified
      # is the variable name in xdt in the x logical expression for rows?
      # position of ith variable name that is followed by a non-alphabetic char
      ind <- gregexpr(paste(" ", names(xdt)[i], "[^a-zA-Z]", sep=""), x)
      n <- unlist(ind)  # starting position of matche(s), -1 if no match
#cat(i," name=", names(xdt)[i], "  x=\'",x,"\'  start=",
#    n, " length=", attr(ind[[1]],which="match.length"), "\n")
      if (any(n>0)) {  # in x, prefix specified variable name with df=xdt$
        for (j in 1:length(n)) {  # variable name can occur more than once
          ind2 <- gregexpr(paste(" ", names(xdt)[i], "[^a-zA-Z]", sep=""), x)
          np <- unlist(ind2)
          pre <- substr(x, 1, np-1)
          post <- substr(x, np, nchar(x))
          x <- paste(pre, df, post, sep="")
          x <- gsub("$ ", "$", x, fixed=TRUE) # get rid of extra inserted " "
        }
      }  # end any(n > 0)
    }
    # x is either a logical expression or a function (e.g. random)
    if (not.flag) x <- paste("!(", x, ")", sep="")
    if (!grepl("random(", x.r, fixed=TRUE))
      x <- paste("which(", x, ")", sep="")  # expression to select rows
    rn <- row.names(xdt[eval(parse(text=x)),])
    return(rn)  # selected rows
  }

  # select columns, expression follows the , in originating d[ ] call
  if (x.call == "cols") {
     x.c <- eval(parse(text=x.call))  # evaluate cols
     x.c <- paste(".(", x.c, ")", sep="")  # as if read directly
  }
  if (grepl(x.call, x.c, fixed=TRUE)  ||  x.call=="cols") {  # x.call to ,
    if (substr(x.c, 1, 1) == "-")  # base R Extract handles the -
      x.c <- substr(x.c, 2, nchar(x.c))  # remove the -.
    substr(x.c, 1, 1) <- "c"  # replace . with combine function c
    x <- str2lang(x.c)  # transform character string to type calls
    xdt.vars <- as.list(seq_along(xdt))
    names(xdt.vars) <- names(xdt)
    return(eval(x, envir=xdt.vars))  # selected column index numbers
  }

}
