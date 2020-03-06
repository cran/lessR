ic <-
  function (x, ...) {

  sc <- sys.calls()  # all function calls that lead to ic (many in .Rmd)
  scu <- unlist(sc)
  scu2 <- as.character(scu[[length(sc)-2]])  # full d[ ] function call
  df <- scu2[2]       # data frame
  ic_call <- scu2[4]  # ic function call

  data <- get(df, envir=parent.frame())
  xc <- substr(ic_call, 2, nchar(ic_call))  # remove the i from ic

  n <- regexpr("-", xc)[1]  # locate and remove any - in the function call
  delFlag <- ifelse (n > 0, TRUE, FALSE)
  if (n > 4)  # "-" not in first position
    xc <- sub(", \"-\"", "", xc, fixed=TRUE)
  else if (n == 4)
    xc <- sub("\"-\", ", "", xc, fixed=TRUE)  # "-" in the first position

  x <- str2lang(xc)  # transform character string to type calls

  data.vars <- as.list(seq_along(data))
  names(data.vars) <- names(data)
  ind <- eval(x, envir=data.vars) # index numbers of relevant columns

  if (delFlag) ind <- -ind
  return(ind)
}
