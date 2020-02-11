corRead <-
function(from=NULL, var_names=NULL, ...) {

  cat("\n")

  if (!is.null(from))
    if (nchar(from) == 0) from <- NULL  #  "" to NULL

  if (is.null(from)) {
    from <- file.choose()
    .dash(64)
    cat("File: \n")
    cat("   ", from, "\n")
    .dash(64)
    cat("\n")
  }

  myc <- as.matrix(read.table(from, ...))

  if (!is.null(var_names)) 
    colnames(myc) <- var_names
  else
    colnames(myc) <- to("X", ncol(myc))
  rownames(myc) <- colnames(myc)

  return(myc)

}
