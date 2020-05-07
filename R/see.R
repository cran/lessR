see <- function(data, n_row=min(nrow(data), 5), n_col=min(ncol(data), 8)) {

  d_nm <- deparse(substitute(data))

  cat("\n")
  h1 <- paste("First", n_row, "rows and first", n_col, "columns of", d_nm)
  cat(h1, "\n")
  .dash(nchar(h1))
  print(data[1:n_row, 1:n_col])

  cat("\n")
  h2 <- paste("Last", n_row, "rows and last", n_col, "columns of", d_nm)
  cat(h2, "\n")
  .dash(nchar(h2))
  print(data[(nrow(data)-n_row+1):nrow(data), (ncol(data)-n_col):ncol(data)])
}
