train_test <-
  function(data, response=NULL, p_train=0.75, seed=NULL) {

  # if a tibble convert to data frame
  df.name <- deparse(substitute(data))  # get name of data table
  dfs <- .getdfs()
  if (!is.null(dfs)) {
    if (df.name %in% dfs) {  # tibble to df
      if (any(grepl("tbl", class(data), fixed=TRUE))) {
        data <- data.frame(data)
      }
    }
  }

  index <- 1:nrow(data)

  if (!is.null(seed)) set.seed(seed)
  train_index <- sample(index, round(p_train*nrow(data)), replace=FALSE)
  test_index <- setdiff(index, train_index)

    data.vars <- as.list(seq_along(data))
    names(data.vars) <- names(data)
    ind <- eval(substitute(response), envir=data.vars, parent.frame())

   if (!is.null(ind)) {
    train_x <- d[train_index, -ind, drop=FALSE]
    train_y <- d[train_index, ind, drop=FALSE]

    test_x <- d[test_index, -ind, drop=FALSE]
    test_y <- d[test_index, ind, drop=FALSE]

    return(list(train_x=train_x, train_y=train_y, test_x=test_x, test_y=test_y))
  }

  else {
    train <- d[train_index, , drop=FALSE]
    test <- d[test_index, , drop=FALSE]
    return(list(train=train, test=test))
  }

}
