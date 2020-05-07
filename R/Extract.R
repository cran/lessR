. <-
  function (x, ...) {

  random <- function (n) {  # random selection of rows
    n.obs <- nrow(data)
    n.obs_new <- ifelse (n > 1, n, round(n*n.obs,0))
    rand.rows <- sample(1:n.obs, size=n.obs_new, replace=FALSE)
  }

  x.call <- deparse(substitute(x))  # expressions passed to .

  sc <- sys.calls()
  scu <- unlist(sc)  # all function calls that lead to x, many in .Rmd
  f.call <- as.character(scu[[length(sc)-2]])  # full d[ ] function call
  df <- f.call[2]   # data frame full d[ ] call
  x.r <- f.call[3]  # function call for rows from full d[ ] call
  x.c <- f.call[4]  # function call for cols from full d[ ] call

  # correct for user mistake in x.r of only one = sign
  # this is evaluated for columns also, presumably no = signs there
  if (nchar(x.call) == 0) {  # happens with only one = sign
    n.eq <- length(unlist(gregexpr("=", x.r, fixed=TRUE)))
    cat("n.eq:", n.eq, "\n")
    if (n.eq == 1) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
         "Need two equal signs == for logical equality.\n\n")
    }
  }

  data <- get(df, envir=parent.frame())

  # select rows, expression precedes the , in originating d[ ] call
  x.r <- substr(x.r, 3, nchar(x.r))    # remove leading .(
  x.r <- substr(x.r, 1, nchar(x.r)-1)  # remove trailing )
  not.flag <- FALSE
  if (substr(x.r, 1, 1) == "!") {  # needed for literal !rows
    not.flag <- TRUE
    x.r <- substr(x.r, 2, nchar(x.r))  # remove ! )
  }
  if (x.r == "rows") x.r <- eval(parse(text=x.r))
  x.rr <- ifelse (not.flag, paste("!", x.r , sep=""), x.r)
  if (x.call %in% c(x.rr, "rows", "!rows")) {  # select rows
    x <- x.r  # character string of the logical expression for rows
    df <- paste(df, "$", sep="")
    for (i in 1:length(names(data))) {  # see if a variable is specified
      # position of ith variable name that is followed by a non-alphabetic char
      ind <- gregexpr(paste(names(data)[i], "[^a-zA-Z]", sep=""), x)
      n <- unlist(ind)
      if (any(n > 0)) { # prefix the specified variable name with df$
        for (j in 1:length(n)) {  # variable name can occur more than once
          np <- n[j] + (2*(j-1))  # account for d$ added each iteration
          x <- paste(substr(x, 1, np-1), df, substr(x, np, nchar(x)), sep="")
        }
      }
    }
    # x is either a logical expression or a function (e.g. random)
    if (not.flag) x <- paste("!(", x, ")", sep="")
    if (!grepl("random(", x.rr, fixed=TRUE))
        x <- paste("which(", x, ")", sep="")  # get rid of NA's
    return(row.names(data[eval(parse(text=x)),]))
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
    data.vars <- as.list(seq_along(data))
    names(data.vars) <- names(data)
    return(eval(x, envir=data.vars)) # selected column index numbers
  }

}
