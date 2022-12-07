train_test <-
  function(data, response=NULL, p_train=0.75, seed=NULL, matrix_out=FALSE) {

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

  n.rows <- 1:nrow(data)

  if (!is.null(seed)) set.seed(seed)
  train_index <- sample(n.rows, round(p_train*nrow(data)), replace=FALSE)
  test_index <- setdiff(n.rows, train_index)

  data.vars <- as.list(seq_along(data))  # locate response variable
  names(data.vars) <- names(data)
  ind <- eval(substitute(response), envir=data.vars, parent.frame())

  if (!is.null(ind)) {
    train_x <- d[train_index, -ind, drop=FALSE]
    train_y <- d[train_index, ind, drop=FALSE]

    test_x <- d[test_index, -ind, drop=FALSE]
    test_y <- d[test_index, ind, drop=FALSE]

    if (matrix_out) {
      train_x <- as.matrix(train_x)
      train_y <- as.matrix(train_y)
      test_x <- as.matrix(test_x)
      test_y <- as.matrix(test_y)
    }

    return(list(train_x=train_x, train_y=train_y, test_x=test_x, test_y=test_y))
  }

  else {  # no response variable, all variables treated equally
    train <- d[train_index, , drop=FALSE]
    test <- d[test_index, , drop=FALSE]
    if (matrix_out) {
      train <- as.matrix(train)
      test <- as.matrix(test)
    }
    return(list(train=train, test=test))
  }

}
