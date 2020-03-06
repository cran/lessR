ir <-
  function (x) {
    sc <- sys.calls()  # all function calls that lead to ir (many in .Rmd)
    scu <- unlist(sc)
    scu2 <- as.character(scu[[length(sc)-2]])  # full d[ ] function call
    df <- scu2[2]       # data frame
    ir_call <- scu2[3]  # ir function call

    data <- get(df, envir=parent.frame())

    x <- substr(ir_call, 4, nchar(ir_call))  # remove the ir( from ir
    x <- substr(x, 1, nchar(x)-1)  # remove the ) from ir

    df <- paste(df, "$", sep="")
    for (i in 1:length(names(data))) {
      n <- regexpr(names(data)[i], x)  # position of ith variable name
      if (n > 0)  # n is -1 if name not present
        x <- paste(substr(x, 1, n-1), df, substr(x, n, nchar(x)), sep="")
    }

    r.names <- row.names(data[eval(parse(text=x)),])
    return(r.names)
}